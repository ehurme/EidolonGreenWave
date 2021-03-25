# animate migration
# http://animove.org/wp-content/uploads/2019/04/Daniel_Palacios_animate_moveVis.html

library("tidyverse")
library("lubridate") # masks base::date()
library("RColorBrewer")
library("move") # masks dplyr::select() and tidyr::extract()
library("moveVis")
library("argosfilter") # masks move::distance, etc.


login = movebankLogin(username = "edwardhurme", password = "V!vesi78212")
zambats <- getMovebankData(study="2019 Bats - Rwanda", login=login)
zambats <- zambats[zambats$location_lat < 10 & zambats$location_long < 35,]

dat.trks <- zambats

# Inspect the contents of the data file:
# show(dat.trks)

# Number of individual animals:
# n.indiv(dat.trks) # 143

# Longitude and latitude extent of the tracks:
# extent(dat.trks)


# Temporal extent of the tracks:
#range(dat.trks@timestamps)
# "1993-08-28 18:20:00 UTC" "2009-04-10 12:34:34 UTC"

# Temporal extent of the tag deployments:
range(as.POSIXct(dat.trks@idData$timestamp_start, tz = "UTC"))
# "1993-08-28 18:20:00 UTC" "2008-07-28 23:00:00 UTC"

# Number of locations per track:
trks.len <- n.locs(dat.trks)
#range(trks.len) # 1-456

# Duration (in days) of each track:
trks.dur <- as.Date(dat.trks@idData$timestamp_end) - 
  as.Date(dat.trks@idData$timestamp_start)
range(trks.dur) # 0-504 days
median(trks.dur) # 58 days

plot(dat.trks, type = "b", xlab = "location_long", ylab = "location_lat")

# Convert the MoveStack object to a data frame:
dat.trks.df <- as(dat.trks, "data.frame")
#glimpse(dat.trks.df) # 15777x37

# Identify the very short tracks:
trks.short <- dat.trks.df %>% 
  group_by(trackId) %>% 
  summarise(nlocs = n(),
            duration = as.Date(first(timestamp_end)) - 
              as.Date(first(timestamp_start))) %>% 
  filter(nlocs < 7 | duration < 7) # 37x3
#glimpse(trks.short) # there are 37 short tracks

# Subset data to the long tracks:
dat.trks.long.df <- dat.trks.df %>% 
  filter(!trackId %in% trks.short$trackId) %>% 
  droplevels() # 15585x37

# Create a summary for the long tracks, including track ID, the number of
# locations (i.e., rows) per track, the tracking duration, the deployment on and
# off timestamps, and the deployment on and off year. Also add a column with the
# difference between year on and year off, which will come in handy later.
trks.long.info <- dat.trks.long.df %>% 
  group_by(trackId) %>% 
  summarise(nlocs = n(),
            duration = as.Date(first(timestamp_end)) - 
              as.Date(first(timestamp_start)), 
            date.on = as.POSIXct(first(timestamp_start)), 
            date.off = as.POSIXct(first(timestamp_end)), 
            year.on = year(first(timestamp_start)), 
            year.off = year(first(timestamp_end))) %>% 
  mutate(year.diff = year.off - year.on) # 106x8
#glimpse(trks.long.info)
#unique(trks.long.info$year.diff) # 0 1 2

# Create another data frame summarizing the number of tags deployed in each year:
n.trks.yr <- trks.long.info %>% 
  count(year.on) # 12x2
#range(n.trks.yr$year.on) # on 12 years between 1994 and 2008...
#range(n.trks.yr$n) # between 1 and 16 tags were deployed

# Create additional columns in the data frame containing the necessary pieces
# for date manipulation. Put all years into a common one while accounting for
# tracks that cross into the next year (or two, in one case). For tracks that
# span across one or more years, add a year according to trks.long.info$year.diff
# using lubridate::update().
dat.trks.long.df <- dat.trks.long.df %>% 
  mutate(year.diff.full = rep.int(trks.long.info$year.diff, 
                                  trks.long.info$nlocs), 
         year.diff.inst = rep.int(trks.long.info$year.off, 
                                  trks.long.info$nlocs) - year(timestamps), 
         year.shift = year.diff.full - year.diff.inst, 
         timestamps.new = update(timestamps, 
                                 year = min(trks.long.info$year.on) + year.shift))
#glimpse(dat.trks.long.df) # NOW 10312x42
#range(dat.trks.long.df$timestamps)
# "1994-09-14 21:22:00 UTC" "2009-04-10 12:34:34 UTC"
#range(dat.trks.long.df$timestamps.new)
# "1994-01-14 05:00:00 UTC" "1996-01-06 16:08:02 UTC"
#plot(dat.trks.long.df$timestamps, dat.trks.long.df$timestamps.new)

# Create a column in dat.trks.long.df identifying locations occurring on
# February 29 of a leap year:
dat.trks.long.df <- dat.trks.long.df %>% 
  mutate(ind.bad.date = leap_year(timestamps) & yday(timestamps) == 60) # NOW 10312x43
#which(dat.trks.long.df$ind.bad.date) # 14

# Exclude the 14 offending locations from the data frame version of the data set:
dat.trks.long.df <- dat.trks.long.df %>% 
  filter(!ind.bad.date)
#glimpse(dat.trks.long.df) # NOW 10298x43

# Determine the last date of the tracking data set without the long track:
#trk.vlong <- dat.trks.long.df %>% 
#  filter(trackId == "X2004CA.Bmu.00840") # 226x43
#range(trk.vlong$timestamps.new)
# "1994-08-20 17:06:00 UTC" "1996-01-06 16:08:02 UTC"
trk.rest <- dat.trks.long.df %>% 
  filter(trackId != "X2004CA.Bmu.00840") # 10072x43
#range(trk.rest$timestamps.new)
# "1994-01-14 05:00:00 UTC" "1995-08-05 23:51:58 UTC"

# Exclude the locations from the long track occurring after "1995-08-05 23:51:58
# UTC" from the data set:
dat.trks.long.df <- dat.trks.long.df %>% 
  filter(timestamps.new <= max(trk.rest$timestamps.new)) # NOW 10262x43
#10298-10262 # 36 locations from the very long track excluded

# Convert the final data frame back to a MoveStack object:
dat.trks.final <- df2move(dat.trks.long.df, 
                          proj = "+init=epsg:4326 +proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0", 
                          track_id = "trackId", 
                          x = "location_long", 
                          y = "location_lat", 
                          time = "timestamps.new")
# A Large MoveStack (10262 elements, 1.4 Mb)
#glimpse(dat.trks.final)
#plot(dat.trks.final, type = "b", pch = 20)

# We can also add the original timestamps to keep it together with the new
# MoveStack:
dat.trks.final@data$timestamps.orig <- dat.trks.long.df$timestamps


#  Use 1-day intervals:
dat.trks.int <- align_move(dat.trks.final, res = 1, digit = 0, unit = "days", 
                           spaceMethod = "greatcircle")
# A Large MoveStack (11373 elements, 4.5 Mb)
#show(dat.trks.int) # 11373 timestamps

# Extract the unique timestamps from the aligned data frame, indicating the
# number of individual frames that will be contained in the animation. Use this
# timestamp to derive the month and year day in the common year for labeling the
# maps.
#n_distinct(timestamps(dat.trks.int)) # 564 unique timestamps
frames.ts <- sort(unique(timestamps(dat.trks.int))) # 564x1
lbl.tstamp <- paste("Month:", toupper(month(frames.ts, label = TRUE)), 
                    "| Year day:", yday(frames.ts), sep = " ") # 564x1

# Select 12 colors from the ColorBrewer palette "Set3":
#col12CB <- rev(brewer.pal(12, "Set3"))
#display.brewer.pal(12, "Set3")
# Or join two smaller palettes with bolder colors to obtain 12 colors (avoid
# light yellow, which offers poor contrast):
# col12CB <- rev(c(brewer.pal(n.trks.yr$n[1], "Dark2"), 
#                  brewer.pal(n.trks.yr$n[2], "Accent")[c(1, 2, 3, 5)])) # 12 colors
col12CB <- rev(brewer.pal(n.trks.yr$n %>% sum, "Dark2"))


# Replicate each color according to the number of tracks in each year:
col12CBx106 <- rep.int(col12CB, n.trks.yr$n %>% sum) # 106

# Generate frames for the animation. Supply a fake timestamp and guide legend:
frames <- frames_spatial(m = dat.trks.int, 
                         path_colours = col12CBx106, 
                         path_size = 2, # default is 3
                         map_service = "carto", 
                         map_type = "voyager_no_labels", 
                         path_legend = FALSE) %>% 
  add_labels(x = "Longitude", y = "Latitude", 
             title = "Blue whale Argos tracks, 1994-2008", 
             subtitle = "Oregon State University, Marine Mammal Institute") %>% 
  add_text(labels = lbl.tstamp, x = -117, y = 57, colour = "black", 
           size = 6, type = "label") %>% 
  add_text(labels = "D.Palacios, OSU", x = -90, y = 1.5, colour = "black", 
           size = 3, type = "text") %>% 
  add_gg(gg = expr(annotate(geom = "label", x = -90, 
                            y = seq(from = 54, by = -2.25, len = 12), 
                            label = n.trks.yr$year.on, colour = "white", 
                            fill = col12CB, size = 5, fontface = "bold")))
# A Large list (564 elements, 40 Mb)
# Preview one of the frames (total = 564 @ 1-day resolution):
#frames[[320]]
