setwd("D:/CTR Lidar 2021")

requiredPackages = c('ggplot2','dplyr','data.table','egg','latticeExtra', 'tidyr', 'extrafont', 'lidR', 'sf', 'stringr', 'lubridate', 'ggrepel')
for(p in requiredPackages){
  if(!require(p,character.only = TRUE)) install.packages(p)
  library(p,character.only = TRUE)
}

# Matching GNSS and PTs ---------------------------------------------------
PT_loc <- as.data.frame(read.csv("D:/CTR Lidar 2021/Bend_PTs/PT_UTM.csv"))
PT_loc$start <- as.POSIXct(paste(PT_loc$Date_inst, PT_loc$UTC_time_start), format = "%Y-%m-%d %I:%M:%S %p", tz = "UTC")
PT_loc$end <- as.POSIXct(paste(PT_loc$Date_inst, PT_loc$UTC_time_end), format = "%Y-%m-%d %I:%M:%S %p", tz = "UTC")
PT_loc$roundtime <- ceiling_date(PT_loc$end, "15 minutes")
PT_loc <- PT_loc[, c(1, 5, 11:16)]

# Fix incorrect times #
PT_loc$roundtime[4] <- PT_loc$roundtime[4] - 60*15
PT_loc$roundtime[24] <- PT_loc$roundtime[24] + 60*15
PT_loc$roundtime[27] <- PT_loc$roundtime[27] + 60*15

# GNSS files used during PT install on the bend
PPP_files <- list.files(path = "D:/CTR Lidar 2021/GNSS/PT_install/g18_UTM/", pattern = "*.csv", full.names=TRUE, recursive=FALSE)

### Create list of GNSS files as separate dataframes and format date/time ###
PPP_list <- list()

for (i in PPP_files){
  PPP <- read.csv(i)
  PPP$name <- str_sub(gsub(".*[/]([^.]+)[.].*", "\\1", i), end = -7)
  PPP$date <- as.Date((PPP[,5] -1), origin=as.Date(paste0(as.character(PPP$year[1]),"-01-01")), format = "%Y-%m-%d")
  PPP$DT <- (with_tz(as.POSIXct(PPP$date), tz = "UTC") + (3600*PPP[,4]))
  PPP_list[[i]] <- PPP[,c(1:3,7,9:13,15)] 
}

### Create list of groups of PTs by GNSS file at install ###
PT_grp <- split(PT_loc, PT_loc$GNSS_file)

### Loop through each GNSS and PT grps to get offset of GPS at each PT install ###  
PPP_list <- PPP_list[order(names(PPP_list))]
PT_grp <- PT_grp[order(names(PT_grp))]

offset_list <- data.frame()
for (i in 1:length(PT_grp)){
  for (k in 1:nrow(PT_grp[[i]])){
    PPP_filt <- PPP_list[[i]] %>%
      filter(DT >= PT_grp[[i]]$start[k], DT <= PT_grp[[i]]$end[k])
    avg = mean(PPP_filt[,6])
    sd = sd(PPP_filt[,6])
    avgX = mean(PPP_filt$POINT_X)
    avgY = mean(PPP_filt$POINT_Y)
    id = PT_grp[[i]]$PT[k]
    gnss_offset = PT_grp[[i]]$GNSS_offset[k]
    DT_UTC = PT_grp[[i]]$roundtime[k]
    offset <- data.frame(avg, sd, avgX, avgY, id, gnss_offset, DT_UTC)
    offset_list <- rbind(offset_list, offset)
    rm(PPP_filt)
  }
}

offset_list$PT_elev <- offset_list$avg - offset_list$gnss_offset
offset_list <- offset_list[order(offset_list$id),]

### Remove unnecessary variables ###
rm(offset, PPP, PPP_list, PT_grp, avg, DT_UTC, gnss_offset, i, id, k, PPP_files)

# Appply offset to PT data ------------------------------------------------
PT_files <- list.files(path = "D:/CTR Lidar 2021/Bend_PTs/PTdata/", pattern="*.csv", full.names=TRUE, recursive=FALSE) 

# lidDate1 = as.Date("08-31-2021", "%m-%d-%Y")
# lidDate2 = as.Date("09-02-2021", "%m-%d-%Y")

PTdf <- data.frame(Date = as.Date(NA),
                   Time = NA, 
                   LEVEL = NA, 
                   ID = NA,
                   name = NA) 
for (j in PT_files){
  PT <- read.csv(j, skip = 11, header = T)
  id <- str_sub(j, start = -15, -14) 
  name <- str_sub(gsub(".*[/]([^.]+)[.].*", "\\1", j))
  PT$Date <- as.Date(PT$Date, format = "%m/%d/%Y")
  PT$ID <- id
  PT$name <- name
  PT_frame <- PT[,c(1:2,4,6:7)]
  PTdf <- rbind(PTdf, c(PT_frame))
}

PTdf <- PTdf[-1,]
rm(PT, PT_frame)
PTdf$ID <- as.factor(PTdf$ID)
PTdf$DT <- as.POSIXct(paste(PTdf$Date, PTdf$Time), format = "%Y-%m-%d %I:%M:%S %p")
PTdf$DT_UTC <- force_tz((PTdf$DT + hours(4)), "UTC") # Need to address if EST vs EDT/other time zones

PT_series_grp <- split(PTdf, PTdf$ID)

### Get PT offset based on time and apply to each file, WRITE AND SAVE###
for (i in 1:length(PT_series_grp)){
  mrg <- merge(offset_list[i,], PT_series_grp[[i]][c("DT_UTC","LEVEL")],by="DT_UTC")
  PT_offset <- mrg$PT_elev - mrg$LEVEL
  PT_series_grp[[i]]$PT_err <- mrg$sd + 0.008
  PT_series_grp[[i]]$PT_elev <- PT_series_grp[[i]]$LEVEL + PT_offset
  # write.csv(PT_series_grp[[i]], paste0("D:/CTR Lidar 2021/Bend_PTs/PTdata/corrected/", PT_series_grp[[i]]$name[1],"_GNSS.csv"))
}

### Unsplit from grouped PTs ###

PTdf <- bind_rows(PT_series_grp)

rm(PT_series_grp, mrg)

# LID CLIPPING ------------------------------------------------------------
### Test to clip only buffered PT files ###
lid <- readLAS("D:/CTR Lidar 2021/Raw/CT_River_01001.las", filter = "-drop_z_above 34")
# id <- readLAS("D:/CTR Lidar 2021/Raw/new/CTRbendcomb.las", filter = "-drop_z_above 34")
projection(lid) <- 6347
writeLAS(lid, file = "D:/CTR Lidar 2021/Raw/CT_River_01001_filt_34.las")
# lid <- catalog("D:/CTR Lidar 2021/Raw/CT_River_02001.las")
# lid2 <- catalog("D:/CTR Lidar 2021/Raw/CT_River_02005.las")
# lid3 <- catalog("D:/CTR Lidar 2021/CTR_bendcomb_Rclip.las")
lid3 <- catalog("D:/CTR Lidar 2021/Raw/CT_River_01001_filt_34.las")
# crs(lid) <- ("+init=epsg:6347")
# crs(lid2) <- ("+init=epsg:6347")
# projection(lid) <- 6347
# shp <- st_read("D:/CTR Lidar 2021/CT Sentinel-2 water mask/ctr_ndwi_buf10.shp")
# Extract X and Y locations from avg of GPS time at PT
x <- offset_list$avgX
y <- offset_list$avgY
# Set radius
r <- 30 # meters # was 10, but does not capture some PT locations... 
# Save clipped las files 
# name <- lid@data$File.Source.ID
# name2<- lid2@data$File.Source.ID
name3<- lid3@data$File.Source.ID

# opt_output_files(lid) <-  paste0("D:/CTR Lidar 2021/tmp/bend/atGNSS/", name, "_{XCENTER}_{YCENTER}")
# opt_output_files(lid2) <- paste0("D:/CTR Lidar 2021/tmp/bend/atGNSS/", name2, "_{XCENTER}_{YCENTER}")
opt_output_files(lid3) <- paste0("D:/CTR Lidar 2021/tmp/atGNSS/", name3, "_{XCENTER}_{YCENTER}")
# Clip is a circle with diameter XX m
# rois <- lasclipCircle(lid, x, y, r)
# rois2 <- lasclipCircle(lid2, x, y, r)
rois3 <- clip_circle(lid3, x, y, r)

rm(rois3)

### Second clip to with water mask - this gets hung up when no points are there...must do crs first ### 1.16.2022 - Do big clip first, we can rerun buffer size and will be faster.
LAS_files <- list.files(path="D:/CTR Lidar 2021/tmp/atGNSS/01001", pattern="*.las", full.names=TRUE, recursive=FALSE)
shp <- st_read("D:/CTR Lidar 2021/CT Sentinel-2 water mask/ctr_ndwi_poly_prj1.shp")
shp <- st_transform(shp, "epsg:6347")

### Clip and rewrite with _clip extension ###
for (i in 1:length(LAS_files)){
  las <- readLAS(LAS_files[i])
  out <- clip_roi(las, shp)
  writeLAS(out, file = paste0(tools::file_path_sans_ext(LAS_files[i]), "_clip.las"))
}

# PT vs. Lidar (50 m buffer) ----------------------------------------------
# clipped_LAS_files <- list.files(path="D:/CTR Lidar 2021/tmp/atGNSS/01001/", pattern="*.las", full.names=TRUE, recursive=FALSE)

clipped_LAS_files <- list.files(path="D:/CTR Lidar 2021/tmp/atGNSS/", pattern="*_clip.las", full.names=TRUE, recursive=FALSE)

df <- data.frame(ID = NA,
                 Z = NA,
                 Zref = NA,
                 gpstime = NA)
# change column to character to match lid id
offset_list$avgYchr <- as.character(round(offset_list$avgY, digits = 0)) # Match y instead because of rounding and decimal
# Format as character with 2 digits for matching in loop #
offset_list$id <- sprintf("%02d", PT_loc$PT)

for (i in clipped_LAS_files) {
  las <- readLAS(i)
  lidid <- str_sub(i, start = -16, -10)# for clip extension
  # lidid <-str_sub(i, start = -11, -5)
  PTid <- filter(offset_list, avgYchr ==lidid)$id
  Z <- data.frame(Z = las@data$Z)
  Zref <- data.frame(Zref = mean(las@data$Z)- las@data$Z)
  gpstime <- data.frame(gpstime = las@data$gpstime)
  ID <- data.frame(ID = rep.int(PTid, nrow(Z)))
  las_frame <- data.frame(ID, Z, Zref, gpstime)
  df <- rbind(df, las_frame)
}

df <- df[-1,] # removes first row of NAs
rm(i, las, Z, ID, las_frame, gpstime, Zref)

# Convert GPS time to hour, minutes, seconds
lidDate = as.Date("08-31-2021", "%m-%d-%Y")
# lidDate = as.Date("09-02-2021", "%m-%d-%Y")

df$localtime <- seconds_to_period(df$gpstime)
df$localtime <- sprintf('%02i:%02i:%.0f', hour(df$localtime), minute(df$localtime), second(df$localtime))## make it a time/date col like the PT's
df$DT_UTM <- as.POSIXct(paste(lidDate, df$localtime), format="%Y-%m-%d %H:%M:%S", tz = "UTC")
df$ID <- as.factor(df$ID)

### Match lidar and PT data ###

avgtime <- aggregate(df[,6], list(df$ID), mean)
avgtime$x <- with_tz(avgtime$x, tz = "UTC") ## why is this necessary?
avgtime$DT <- round_date(avgtime$x, "15 minutes")
avgtime$comb <- paste(avgtime$Group.1, avgtime$DT)
PTdf$comb <- paste(PTdf$ID, PTdf$DT_UTC)

avgtime <- merge(avgtime,PTdf[c("comb","PT_elev", "PT_err")],by = "comb")


### Add PT elev at time of lidar to lidar df ###
names(avgtime)[2] <- 'ID'
df<- merge(df, avgtime[c("ID", "PT_elev", "PT_err")], by = "ID")

### Remove likely vegetation points ###
# Use the highest PT value, add 1m, and subset any lidar points less than this #
sub <- max(df$PT_elev) + 1
df_subveg <- subset(df, Z <= sub)

# Lidar vs. PT Plots ------------------------------------------------------
### Plots in one place ###
ggplot(df, aes(x=Z)) +
  scale_color_brewer(palette = "Greys", direction = -1) +
  scale_fill_brewer(palette = "Greys", direction = -1) +
  geom_density(alpha=0.7, size = 0.65, aes(y = ..count..)) +
  geom_vline(xintercept = elev, colour = "black", size =0.25) +
  facet_wrap(~ID, ncol = 5) +
  xlab("Residuals (cm)") + ylab("Count") +
  ggtitle("Distribution of lidar heights at each PT") +
  xlim(29, 33) +
  theme_classic() +
  theme(plot.title = element_text(size = 11, hjust = 0.5),
        legend.position = 'none',
        strip.background = element_blank(),
        axis.title.x = element_text(size=8),
        axis.title.y = element_text(size=8),
        axis.text.x = element_text(size=6),
        axis.text.y = element_text(size=6),
        text = element_text(family = "Times New Roman"), 
        panel.spacing.x = unit(1, "lines"),
        panel.grid.major.x = element_line(color ="gray91", size = 0.25), 
        panel.grid.major.y = element_line(color = "gray91", size = 0.25))

### Individual Plots ###
df_grp <- split(df_subveg, df_subveg$ID)

### SCATTER PLOT ###
RMSE = data.frame()
scatter = data.frame()
for (i in 1:length(df_grp)){
  calc <- data.frame(RMSE = (sqrt(mean((df_grp[[i]]$PT_elev - df_grp[[i]]$Z)^2))), ID = df_grp[[i]]$ID[1])
  RMSE <- rbind(RMSE, calc) 
  d <- density.default(df_grp[[i]]$Z)
  modes <- function(d){
    j <- which(diff(sign(diff(d$y))) < 0) +1
    data.frame(x = d$x[j], y = d$y[j])
  }
  pdfmax <- d$x[which.max(d$y)]
  PT_elev <- df_grp[[i]]$PT_elev[1]
  PT_ID <- df_grp[[i]]$ID[1]
  PTerr <- df_grp[[i]]$PT_err[1]
  liderr <- sd(df_grp[[i]]$Z)
  # PTerr <- 0.068
  bind <- cbind(PT_ID, PT_elev, pdfmax, liderr, PTerr)
  scatter <- rbind(scatter, bind)
}

# scatter$PT_ID <- c(11,12,13,14,15,16,17,18,19,20)
# scatter <- scatter[11:20,]
rm(calc, d, modes, pdfmax, PT_elev, PT_ID, liderr, PTerr)


sp <- ggplot(scatter, aes(x = PT_elev, y = pdfmax)) +
  geom_point(size = 2) +
  # geom_text(aes(label = PT_ID, size = 2.5, hjust= -1.1)) +
  geom_errorbar(aes(ymin = pdfmax-liderr, ymax = pdfmax+liderr), width = 0.05) +
  geom_errorbarh(aes(xmin = PT_elev-PTerr, xmax = PT_elev+PTerr), height = 0.05) +
  geom_abline(slope = 1, intercept = 0) +
  xlab("PT Elevation (m)") + ylab("Lidar Elevation (m)") +
  xlim(30.25, 33.25) +
  ylim(30.25, 33.25) +
  theme_classic() +
  theme(# plot.title = element_text(size = 11, hjust = 0.5),
    legend.position = 'none',
    strip.background = element_blank(),
    axis.title.x = element_text(size=16),
    axis.title.y = element_text(size=16),
    axis.text.x = element_text(size=14),
    axis.text.y = element_text(size=14),
    # text = element_text(family = "Times New Roman"), 
    #panel.spacing.x = unit(1, "lines"),
    panel.grid.major.x = element_line(color ="gray91", size = 0.15), 
    panel.grid.major.y = element_line(color = "gray91", size = 0.15))
sp
png(file = paste0("D:/CTR Lidar 2021/figs/scatterbendg18gnssxy_labs01001.png"))
plot(sp)
dev.off()

### Individual PDF plots ###
for (i in 1:length(df_grp)){
  mean <- mean(df_grp[[i]]$Z)
  sd <- sd(df_grp[[i]]$Z)
  ### get RMSE for each comp - add as plot labels - see reed script ###
  testplot <- 
    ggplot(df_grp[[i]], aes(x=Z, color = Z, fill = Z)) +
    scale_color_brewer(palette = "Greys", direction = -1) +
    scale_fill_brewer(palette = "Greys", direction = -1) +
    geom_density(alpha=0.7, size = 0.65, aes(y = ..count..)) +
    geom_vline(xintercept = df_grp[[i]]$PT_elev[1], colour = "black", size =0.25) +
    xlab("Elev (m)") + ylab("Count") +
    ggtitle(paste0("PT", df_grp[[i]]$ID[1])) +
    xlim((mean - 1.5), (mean + 1.5)) +
    theme_classic() +
    theme(plot.title = element_text(size = 11, hjust = 0.5),
          legend.position = 'none',
          strip.background = element_blank(),
          axis.title.x = element_text(size=8),
          axis.title.y = element_text(size=8),
          axis.text.x = element_text(size=6),
          axis.text.y = element_text(size=6),
          text = element_text(family = "Times New Roman"), 
          panel.spacing.x = unit(1, "lines"),
          panel.grid.major.x = element_line(color ="gray91", size = 0.25), 
          panel.grid.major.y = element_line(color = "gray91", size = 0.25))
  # Save files
  png(file = paste0("D:/CTR Lidar 2021/figs/lidvPTg18gnssxy_", df_grp[[i]]$ID[1], ".png"))
  plot(testplot)
  dev.off()
}

# Lidar vs. Drifter -------------------------------------------------------


