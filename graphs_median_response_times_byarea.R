# set the three variables below to the appropriate path on your system
# working_directory
# allcalls_bydivision - is the path to the list of calls 
# boundary_files - is the path to the electoral division .shp boundary file

# once you have set the paths for the three variables above to the relevant files on your machine you run the following

library (sf)
library (readxl)
library (plyr)
library (dplyr)
setwd(working_directory)
all_times <- read_excel(allcalls_bydivision)
summarised <- ddply(all_times[!is.na(all_times$ED_ID),],.(ED_ID),summarize,median=median(`Response Time Conveying Resource`),number_calls=length(unique(`Randomised Number`)), max=max(`Response Time Conveying Resource`), min=min(`Response Time Conveying Resource`), mean=mean(`Response Time Conveying Resource`))
summarised$mediantimes <- hms::as_hms(summarised$median)
summarised$mediantimes_mins <- (as.numeric(summarised$mediantimes))/60
summarised$mediantimes_minscat <- as.factor(ifelse(summarised$mediantimes_mins < 11, '0-10 mins',
                                                   ifelse(summarised$mediantimes_mins < 21, '11-20 mins', 
                                                          ifelse(summarised$mediantimes_mins < 31 , '21-30 mins', 
                                                                 ifelse(summarised$mediantimes_mins <41, '31-40 mins', 
                                                                        ifelse (summarised$mediantimes_mins <51, '41-50 mins',
                                                                                ifelse(summarised$mediantimes_mins < 61, '51-60mins','more than 60 mins')))))))
bounds <- st_read (boundary_files)
bounds$ED_ID <- as.numeric(bounds$ED_ID)
ireland <- left_join(bounds, summarised, by="ED_ID")

library (viridis)
library (ggplot2)

# to plot the whole country
ggplot (data=ireland) + geom_sf(aes(fill=`mediantimes_minscat`))  + scale_fill_viridis(option = "C", discrete=TRUE ) + labs (fill="Median Response Time -mins") 

# to just plot an individual county e.g. DUBLIN plot the code below
ggplot (data=ireland[ireland$COUNTY=="DUBLIN",]) + geom_sf(aes(fill=`mediantimes_minscat`))  + scale_fill_viridis(option = "C", discrete=TRUE, drop=FALSE, na.translate=FALSE ) + labs (fill="") + ggtitle("Dublin- Median Response Times")