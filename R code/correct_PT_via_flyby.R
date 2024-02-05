Sys.umask('002')
#dank flyby script
correct_PT_via_flyby=function(pt_file_in,munged_PT_directory,munged_GNSS_directory,
                              gnss_sd_thresh,time_thresh,dist_thresh,output_dir){
  
  if (file.exists(paste0('/nas/cee-water/cjgleason/calval/Processed data/PT_stories/','flyby_',pt_file_in))){
    file.remove(paste0('/nas/cee-water/cjgleason/calval/Processed data/PT_stories/','flyby_',pt_file_in))}
  
  
  
  # 
  # munged_PT_directory= '/nas/cee-water/cjgleason/calval/Processed data/UMass/Munged PT/reprocessed_2023_12_20/'
  # pt_file_in=list.files(munged_PT_directory)[68]
  # munged_GNSS_directory='/nas/cee-water/cjgleason/calval/Processed data/UMass/Munged drifts/reprocessed_2023_12_20/'
  # gnss_sd_thresh=0.05
  # time_thresh=7.5*60
  # dist_thresh=200
  
  
  library(dplyr)
  library(parallel)
  #read in pt----------
  #first line only to get flag with minimum data read
  #already munged, so no in the water or other filters
  
  #check flag
  #if flag === 0,1, were good, do nothing
  #if flag === else, we need to do stuff
  flag=read.csv(paste0(munged_PT_directory,pt_file_in), header = TRUE, nrow = 1)$flag
  # stop here if flag is 0, the file is good, or if flag =1,
  #the file has only a shift in the raw record
  pt_data=read.csv(paste0(munged_PT_directory,pt_file_in), header = TRUE, stringsAsFactors = FALSE)%>%
    mutate(datetime=as.POSIXct(pt_time_UTC))
  new_flag=flag
  
  if ( (flag ==0) | (flag ==1) ){
    new_flag=new_flag
    pt_data_out=pt_data%>%
      mutate(flyby_correction_m= NA,
             flyby_total_error_m=NA,
             pt_with_correction_mean_offset_sd_m=pt_correction_offset_sd_m_install,
             pt_with_correction_total_error_m=pt_correction_mean_total_error_m,
             pt_with_flyby_wse_m=pt_wse_m,
             flag=new_flag)
    
    write.csv(pt_data_out,paste0(output_dir,'flyby_',pt_file_in),row.names = FALSE)
    return(NA)} 
  
  #-------------------
  
  #get flybys----------
  #read all this river's GNSS data
  #for each one read first and last line only, which defines the start and end of the record
  #return a list of all GNSS files that have the GNSS record within the PT record
  #also, return GNSS only where the PT is within the min/max lat/lon box of the GNSS
  gnss_files=list.files(munged_GNSS_directory,full.names=TRUE)
  
  #there are 'bad' files within the GNSS record that have been reprocessed, and have a new third date.
  # positions 9 and 10 are the start and end of the data
  # position 11 is the repro date- we want the most recent repro
  # position 12 is the turning point
  parse_gnss=function(gnss_file){
    splitter= strsplit(gnss_file,'_')
    first_two_dates=paste(splitter[[1]][9],splitter[[1]][10],sep="_")
    
    #the standard date format has an 'nT' character in the middle of thedates
    third_date=as.POSIXct(paste0(substring(splitter[[1]][11],1,8),
                                 substring(splitter[[1]][11],10,15) ),
                          format='%Y%m%d%H%M%S')
    tp=splitter[[1]][[12]]
    return(data.frame(cbind(keystring=paste(first_two_dates,tp),third_date)))
  }
  
  
  
  gnss_index=do.call(rbind,lapply(gnss_files,parse_gnss))%>%
    #this is implicit, so when we filter things we'll mess up the order. 
    #add an index favlue
    mutate(gnss_file_index=1:nrow(.))%>%
    group_by(keystring)%>%
    filter(third_date==max(third_date))
  
  
  #remove old files
  gnss_files=gnss_files[gnss_index$gnss_file_index]
  pt_lat=pt_data$pt_lat[1]
  pt_lon=pt_data$pt_lon[1]
  
  create_offset_per_flyby=function(gnss_file, pt_lat,pt_lon,time_thresh, dist_thresh,
                                   pt_data, minimum_gnss_points_for_offset){
    library(dplyr)
    library(fuzzyjoin)
    library(geodist)
    
    #get max and min lat/lon covered by the dataframe
    gnss_data=read.csv(gnss_file, header = TRUE,  stringsAsFactors = FALSE)%>%
      mutate(datetime=gnss_time_UTC)%>%
      mutate(datetime=as.POSIXct(datetime))%>%
      select(-X) #written with row names. if those are later removed this will bonk
    
    #check to see if the pt file is within the box
    if( (pt_lat >= min(gnss_data$gnss_Lat)) & (pt_lat <= max(gnss_data$gnss_Lat)) & 
        (pt_lon >= min(gnss_data$gnss_Lon)) & (pt_lon <= max(gnss_data$gnss_Lon)) ){
      
      
      
      #join the PT data and GNSS data------
      #now, join the GNSS 1Hz data to the PT 15 minute data 
      
      #force to integers to remove fuzzyjoin's scale dependency
      gnss_data$datetime=as.integer(as.POSIXct(gnss_data$datetime))
      pt_data$datetime=as.integer(as.POSIXct(pt_data$datetime))
      
      pt_with_gnss_time=difference_inner_join(pt_data,gnss_data,by='datetime',max_dist=time_thresh,distance_col='sec_btw_gnss_pt')
      
      #----------------
      
      #take that file and calculate a distance vector
      #this vector is in order of the dataframe above, and computes the pairwise difference between
      #every row's GNSS and PT. the time join above is many to one, so all GNSS within some time are joined to each
      #PT record. This then combs through those and calculates the distances, that is- does this file have
      #GNSS data from teh same time but 5km away? The earlier min/max lat/on filter might have solved this, but
      #we want things within quite precise distances-----
      distance_m=geodist(cbind(pt_with_gnss_time$pt_lon, pt_with_gnss_time$pt_lat),  
                         cbind(pt_with_gnss_time$gnss_Lon, pt_with_gnss_time$gnss_Lat), paired=TRUE,measure='haversine')
      #-----------------------
      
      #make a dataframe out of everything and filter by distance
      spacetime_pt=cbind(pt_with_gnss_time,distance_m)%>%
        filter(distance_m<dist_thresh)
      #-----------------------------------
      
      
      #calculate an offset dataframe------
      #since we're doing this one gnss file at a time, the grouping by gnss drift id
      #is implicit. doing it as a giant df is too memory intensive
      offset_df=spacetime_pt%>%
        group_by(pt_time_UTC)%>%
        filter(n()>minimum_gnss_points_for_offset)%>%
        mutate(flyby_offset= gnss_wse-pt_level)%>%
        select(-c(gnss_ellipsoid)) # comma in column that messes with append, so I rid.
      #elegance in klugosity itself
      # writing row in the csv file 
      
      write.table(offset_df, file = paste0('/nas/cee-water/cjgleason/calval/Processed data/PT_stories/','flyby_',pt_file_in),
                  sep = ",", 
                  append = TRUE, quote = FALSE, 
                  col.names = TRUE, row.names = FALSE) 
      
      
      offset_df=offset_df%>%
        summarize( flyby_pt_correction_m= mean(flyby_offset,na.rm=TRUE),
                   
                   #error is equal to the sd of the wse (boat bobbing) and the gnss average error, propogated
                   flyby_pt_correction_total_error_m=  sqrt( sd(flyby_offset,na.rm=TRUE)^2 + (sum(gnss_uncertainty_m)/n()) ^2 ),
                   
                   #this is the error due to changing wse without GNSS error
                   flyby_pt_correction_offset_sd_m=sd(flyby_offset,na.rm=TRUE),
                   
                   #this is the error due to GNSS error
                   flyby_pt_correction_gnss_average_error_m=sum(gnss_uncertainty_m)/n(),
                   
                   #what drift ids were used here
                   flyby_drift_id=gnss_file,
                   
                   pt_serial=unique(pt_serial),
                   
                   gnss_pings_used=n()
                   
        )
      
      return(offset_df)
      
    }
    
    
  }
  
  
  #-------------------
  time_thresh=7.5*60
  dist_thresh=200
  minimum_gnss_points_for_offset=5
  
  # test= create_offset_per_flyby(gnss_files[41], pt_lat=pt_lat,pt_lon=pt_lon,
  #                               time_thresh = time_thresh,dist_thresh = dist_thresh,
  #                               pt_data=pt_data,
  #                               minimum_gnss_points_for_offset=minimum_gnss_points_for_offset)
  
  # 
  cl=makeCluster(38)
  flyby_offsets=do.call(rbind,parLapply(cl,gnss_files,create_offset_per_flyby,
                                        pt_lat=pt_lat,pt_lon=pt_lon,
                                        time_thresh = time_thresh,dist_thresh = dist_thresh,
                                        pt_data=pt_data,
                                        minimum_gnss_points_for_offset=minimum_gnss_points_for_offset))
  
  
  stopCluster(cl)
  
  
  
  
  if (is.null(flyby_offsets) ){
    new_flag=new_flag + 100000
    pt_data_out=pt_data%>%
      mutate(flyby_correction_m= NA,
             flyby_total_error_m=NA,
             pt_with_correction_mean_offset_sd_m=pt_correction_offset_sd_m_install,
             pt_with_correction_total_error_m=pt_correction_mean_total_error_m,
             pt_with_flyby_wse_m=pt_wse_m,
             flag=new_flag)
    
    write.csv(pt_data_out,paste0(output_dir,'flyby_',pt_file_in),row.names = FALSE)
    
    #filter for noise
    return(NA)}else{
      flyby_offsets=  flyby_offsets%>%
        filter(flyby_pt_correction_total_error_m< gnss_sd_thresh)
    }
  
  if (nrow(flyby_offsets)==0 ){
    new_flag=new_flag + 100000
    pt_data_out=pt_data%>%
      mutate(flyby_correction_m= NA,
             flyby_total_error_m=NA,
             pt_with_correction_mean_offset_sd_m=pt_correction_offset_sd_m_install,
             pt_with_correction_total_error_m=pt_correction_mean_total_error_m,
             pt_with_flyby_wse_m=pt_wse_m,
             flag=new_flag)
    write.csv(pt_data_out,paste0(output_dir,'flyby_',pt_file_in),row.names = FALSE)
    
    return(NA)
  }
  
  
  # plot(as.POSIXct(flyby_offsets$pt_time_UTC),flyby_offsets$flyby_pt_correction_m)
  print(paste('flag is',pt_data$flag[1]))
  print(paste('install offset is',pt_data$pt_correction_m_install[1]))
  print(paste('uninstall offset is',pt_data$pt_correction_m_uninstall[1]))
  print(paste('min of flyby offsets is ',min(flyby_offsets$flyby_pt_correction_m)))
  print(paste('max of flyby offsets is ',max(flyby_offsets$flyby_pt_correction_m)))
  print(paste('number of flyby offsets is', nrow(flyby_offsets)))
  
  #parse flags--------
  #if flag is 10, there was an install only
  #do the grouped offsets agree with the install?
  # check could be # that do, or overall variance of offsets, or else?
  #first throw out outliers
  #if good, close
  #if bad, check for settling
  #progressively increasing offsets over time (with some tolerance)
  #if no settling
  #flag and close- irrecoverable
  #if settling
  #lienar fit, apply, close  
  
  #missing an uninstall?
  #check the flags ending in 10
  if (substr(as.character(flag),(nchar(as.character(flag))-1),nchar(as.character(flag))) == '10' | 
      substr(as.character(flag),(nchar(as.character(flag))-1),nchar(as.character(flag))) == '11'  ){
    #do the flybys agree with the occupy?
    mean_flyby=mean(flyby_offsets$flyby_pt_correction_m)
    sd_flyby=sd(flyby_offsets$flyby_pt_correction_m)
    #install offsets repeat, so take just the first
    offset_vs_flyby=pt_data$final_offset_m[1] - mean_flyby
    
    
    if (offset_vs_flyby < gnss_sd_thresh){
      if(nrow(flyby_offsets)>1){
        #we're good! the offsets match the install
        #add these two fields, rewrite the file
        new_flag=new_flag + 10000000
        pt_data_out=pt_data%>%
          mutate(flyby_correction_m= NA,
                 flyby_total_error_m=NA,
                 pt_with_correction_mean_offset_sd_m=pt_correction_offset_sd_m_install,
                 pt_with_correction_total_error_m=pt_correction_mean_total_error_m,
                 pt_with_flyby_wse_m=pt_wse_m,
                 flag=new_flag)
        
        # write.csv(pt_data_out,paste0(output_dir,'flyby_',pt_file_in),row.names = FALSE)
        # return(NA)
      } else {
        new_flag=new_flag + 100000
        pt_data_out=pt_data%>%
          mutate(flyby_correction_m= NA,
                 flyby_total_error_m=NA,
                 pt_with_correction_mean_offset_sd_m=pt_correction_offset_sd_m_install,
                 pt_with_correction_total_error_m=pt_correction_mean_total_error_m,
                 pt_with_flyby_wse_m=pt_wse_m,
                 flag=new_flag)
        # 
        # write.csv(pt_data_out,paste0(output_dir,'flyby_',pt_file_in),row.names = FALSE)
      }
      
    } else {
      
      #not good. write with a flag
      new_flag=new_flag + 10000
      mean_flyby=mean(flyby_offsets$flyby_pt_correction_m)
      sd_flyby=sd(flyby_offsets$flyby_pt_correction_m)
      install_mean=pt_data$pt_correction_m_install[1]
      
      flyby_correction=mean(c(mean_flyby,install_mean))
      flyby_correction_sd=sd(c(mean_flyby,install_mean))
      
      flyby_total=(sum(flyby_offsets$flyby_pt_correction_total_error_m)/nrow(flyby_offsets))
      
      pt_data_out=pt_data%>%
        mutate(flyby_correction_m= flyby_correction,
               #                               #error from each   +     error in summarizing across offsets
               flyby_total_error_m=sqrt(         (flyby_total)^2  + flyby_correction_sd^2 ),
               pt_with_correction_mean_offset_sd_m=flyby_correction_sd,
               pt_with_correction_total_error_m=flyby_total,
               pt_with_flyby_wse_m=pt_level+flyby_correction,
               flag=new_flag)
      # write.csv(pt_data_out,paste0(output_dir,'flyby_',pt_file_in),row.names = FALSE)
      # return(NA)
      
      
    }
    
    
    
  }else{ # end of 10 or 11 flag checks
    
    #We check all flags not 10 or 11#
    mean_flyby=mean(flyby_offsets$flyby_pt_correction_m)
    sd_flyby=sd(flyby_offsets$flyby_pt_correction_m)
    #install offsets repeat, so take just the first
    offset_vs_flyby=pt_data$final_offset_m[1] - mean_flyby
    
  
    if (offset_vs_flyby < gnss_sd_thresh){
      if(nrow(flyby_offsets)>1){
        #we're good! the offsets match the install
        #add these two fields, rewrite the file
        new_flag=new_flag + 10000000
        pt_data_out=pt_data%>%
          mutate(flyby_correction_m= NA,
                 flyby_total_error_m=NA,
                 pt_with_correction_mean_offset_sd_m=pt_correction_offset_sd_m_install,
                 pt_with_correction_total_error_m=pt_correction_mean_total_error_m,
                 pt_with_flyby_wse_m=pt_wse_m,
                 flag=new_flag)
        
        # write.csv(pt_data_out,paste0(output_dir,'flyby_',pt_file_in),row.names = FALSE)
        # return(NA)
      } else {
        #all non 10, 11 flags 
        if (nrow(flyby_offsets>=3)){
          
          #add the install and uninstall here
          
          #### get the og correction
          install_mean=pt_data$pt_correction_m_install[1]
          uninstall_mean=pt_data$pt_correction_m_uninstall[1]
          install_time=pt_data$gnss_install_UTC_start[1]
          uninstall_time=pt_data$gnss_uninstall_UTC_start[1]
          
          #### add them to the offset dataframe
          dummy_df=data.frame(pt_time_UTC =rbind(install_time,uninstall_time),
                              flyby_pt_correction_m=rbind(install_mean,uninstall_mean),
                              flyby_pt_correction_total_error_m=NA,
                              flyby_pt_correction_gnss_average_error_m=NA,
                              flyby_pt_correction_offset_sd_m=NA,
                              flyby_drift_id=rbind('install','uninstall'),
                              pt_serial=cbind(flyby_offsets$pt_serial[1:2]),
                              gnss_pings_used=600
          )
          
          settling_df=rbind(flyby_offsets,dummy_df)
          
          
          ###
          
          #is linear fit positive slope? use a linear fit
          linear_fit=lm(flyby_pt_correction_m ~ as.integer(as.POSIXct(pt_time_UTC)), data = settling_df)
          slope=linear_fit$coefficients[2]
          intercept=linear_fit$coefficients[1]
          residual_sigma=summary(linear_fit)$sigma
          #if so, how monotonic is that increase? use a difference vector
          #shift = (n+1)-n
          monotonizer=(c(settling_df$flyby_pt_correction_m,0) -c(0,settling_df$flyby_pt_correction_m))[2:(nrow(settling_df))]
          #add all those differences. if total is positive, that means more increases than decreases
          monotic_sum=sum(monotonizer)
          
          if (slope >0 & monotic_sum >0){
            #pt is settling
            #linear fit. no way we know it is linear!!!!
            new_flag= new_flag + 1000000
            flyby_total=(sum(flyby_offsets$flyby_pt_correction_total_error_m)/nrow(flyby_offsets))
            
            pt_data_out=pt_data%>%
              mutate(flyby_correction_m= (slope*as.integer(as.POSIXct(pt_time_UTC)))+intercept)%>%
              #                    #error from each                             + error from linear fit
              mutate(flyby_total_error_m=sqrt( (flyby_total)^2 + residual_sigma^2),
                     pt_with_correction_mean_offset_sd_m=NA,
                     pt_with_correction_total_error_m=flyby_total_error_m,
                     pt_with_flyby_wse_m=pt_level+flyby_correction_m,
                     flag=new_flag)
            # write.csv(pt_data_out,paste0(output_dir,'flyby_',pt_file_in),row.names = FALSE)
            # return(NA)
            
          } else {
            #pt not settling 
            new_flag= new_flag + 10000
            mean_flyby=mean(flyby_offsets$flyby_pt_correction_m)
            sd_flyby=sd(flyby_offsets$flyby_pt_correction_m)
            install_mean=pt_data$pt_correction_m_install[1]
            uninstall_mean=pt_data$pt_correction_m_uninstall[1]
            
            flyby_correction=mean(c(mean_flyby,install_mean,uninstall_mean))
            flyby_correction_sd=sd(c(mean_flyby,install_mean,uninstall_mean))
            
            flyby_total=(sum(flyby_offsets$flyby_pt_correction_total_error_m)/nrow(flyby_offsets))
            
            pt_data_out=pt_data%>%
              mutate(flyby_correction_m= flyby_correction,
                     ##error from each              +     error in summarizing across offsets
                     flyby_total_error_m=sqrt(         (flyby_total)^2  + flyby_correction_sd^2 ),
                     pt_with_correction_mean_offset_sd_m=flyby_correction_sd,
                     pt_with_correction_total_error_m=flyby_total,
                     pt_with_flyby_wse_m=pt_level+flyby_correction,
                     flag=new_flag)
            # write.csv(pt_data_out,paste0(output_dir,'flyby_',pt_file_in),row.names = FALSE)
            # return(NA)
          } # end of the settlign check
          
        } else { # less than than 3 datapoints
          new_flag= new_flag + 100000
          #write corrections
          pt_data_out=pt_data%>%
            mutate(flyby_correction_m= NA,
                   flyby_total_error_m=NA,
                   pt_with_correction_mean_offset_sd_m=pt_correction_offset_sd_m_install,
                   pt_with_correction_total_error_m=pt_correction_mean_total_error_m,
                   pt_with_flyby_wse_m=pt_wse_m,
                   flag=new_flag)
          
          # write.csv(pt_data_out,paste0(output_dir,pt_file_in),row.names = FALSE)
          # return(NA)
        }
        
        
      }
      
    }else{#flybyoffset > gnss_thresh - we rid of this, what flag and do what??
      new_flag=new_flag + 10000
      mean_flyby=mean(flyby_offsets$flyby_pt_correction_m)
      sd_flyby=sd(flyby_offsets$flyby_pt_correction_m)
      install_mean=pt_data$pt_correction_m_install[1]
      
      flyby_correction=mean(c(mean_flyby,install_mean))
      flyby_correction_sd=sd(c(mean_flyby,install_mean))
      
      flyby_total=(sum(flyby_offsets$flyby_pt_correction_total_error_m)/nrow(flyby_offsets))
      
      pt_data_out=pt_data%>%
        mutate(flyby_correction_m= flyby_correction,
               #                               #error from each   +     error in summarizing across offsets
               flyby_total_error_m=sqrt(         (flyby_total)^2  + flyby_correction_sd^2 ),
               pt_with_correction_mean_offset_sd_m=flyby_correction_sd,
               pt_with_correction_total_error_m=flyby_total,
               pt_with_flyby_wse_m=pt_level+flyby_correction,
               flag=new_flag) 
    }
  }
  
  write.csv(pt_data_out,paste0(output_dir,'flyby_',pt_file_in),row.names = FALSE)
  
  
}#end function

###Flag Description###
#0- all good
#1- shift in raw PT (15cm within 15 mins)
#10- no uninstall data
#100- total error of install/uninstall correction > threshold
#1000- (install - uninstall) > threshold 
#10000- Flybys don’t agree with the offset. All install, uninstall, flybys averaged.
#100000- there are not enough flybys: not appropriate to do anything
#1000000- pt is likely settling. Linear fit was applied.
#10000000 - flybys agree with OG file. we did nothing

# munged_PT_directory= '/nas/cee-water/cjgleason/calval/Processed data/Brown/Munged PT/reprocessed_2023_12_20/'
# output_dir=          '/nas/cee-water/cjgleason/calval/Processed data/Brown/Flyby PT/reprocessed_2023_12_20/'
# pt_file_in=list.files(munged_PT_directory)
# munged_GNSS_directory='/nas/cee-water/cjgleason/calval/Processed data/Brown/Munged drifts/reprocessed_2023_12_20/'
# gnss_sd_thresh=0.05
# time_thresh=7.5*60
# dist_thresh=200
# 
# 
# lapply(pt_file_in,correct_PT_via_flyby,
#        munged_GNSS_directory=munged_GNSS_directory,
#        munged_PT_directory=munged_PT_directory,
#        time_thresh=time_thresh,
#        dist_thresh=dist_thresh,
#        gnss_sd_thresh=gnss_sd_thresh,
#        output_dir=output_dir)
