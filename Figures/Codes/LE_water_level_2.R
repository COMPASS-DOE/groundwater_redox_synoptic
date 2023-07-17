# LE water level study
# Fausto Silva 12/30/2022

# Lake Erie Water Level from NOAA/NOS/CO-OPS at 9063085, Toledo OH
# https://tidesandcurrents.noaa.gov/stationhome.html?id=9063085

# Data achired from:
# https://tidesandcurrents.noaa.gov/waterlevels.html?id=9063085&units=standard&bdate=20220101&edate=20221229&timezone=LST&datum=IGLD&interval=h&action=data


# set packages
require(pacman)
p_load(readr,lubridate,tidyverse,viridis,MASS, M3)

# bring df Toledo
#Note: I changed the name "Time(LST/DST)" to TimeLST in excel
df_LE1<-read_csv("CO-OPS_9063085_met_Toledo.csv")

# df structure
str(df_LE1)
head(df_LE1)

#declaring date
date_obj <- df_LE1$Date
date_obj <- as.Date(date_obj, "%m/%d/%Y")

# declaring time
time_obj <- df_LE1$TimeLST

# specifying the format
format <- "%Y-%m-%d %H:%M:%S"

# combining date and time into single object
df_LE1$datetime <-as.POSIXct(paste(date_obj, time_obj), format=format)

#add column station
df_LE1b <- df_LE1 %>%
               add_column(station = "Toledo")
head(df_LE1b)

df1<-df_LE1b


# bring df Marblehead
df_LE2<-read_csv("CO-OPS_9063079_met_Marblehead.csv")

# df structure
str(df_LE2)
head(df_LE2)

#declaring date
date_obj <- df_LE2$Date
date_obj <- as.Date(date_obj, "%m/%d/%Y")

# declaring time
time_obj <- df_LE2$TimeLST

# specifying the format
format <- "%Y-%m-%d %H:%M:%S"

# combining date and time into single object
df_LE2$datetime <-as.POSIXct(paste(date_obj, time_obj), format=format)

#add column station
df_LE2b <- df_LE2 %>%
  add_column(station = "Marblehead")

head(df_LE2b)

df2<-df_LE2b
df1<-df_LE1b

head(df1)
head(df2)

df3 <- rbind(df1, df2)
str(df3)
df4<-clean_names(df3)
df4$station<-as.factor(df4$station)
df4$verified_m<-as.numeric(df4$verified_m)
head(df4)

write_csv(df4, "df_LE_M&T.csv")

df_LE<-read_csv("df_LE_M&T.csv")

# ft to m
#df_LE$m <- df_LE$verified_ft * 0.3048
# Set ggplot theme
theme_set(theme_bw())
str(df_LE)

##########################################################################
# LE level vs time

window_size <- 5
df_LE <- df_LE %>%
  mutate(smoothed_verified_m = rollmeanr(verified_m, k = window_size, fill = NA))

df_LE %>% 
  filter(datetime>"2022-09-22 11:00:00", 
         datetime<"2022-10-07 11:00:00") %>%
  ggplot(aes(x = datetime, 
             y = smoothed_verified_m, 
             color=station)
  )+
  geom_point(size = 3, 
             alpha=0.3
  )


+
  scale_color_gradientn(colours = viridis(7),
                        breaks = seq(as.POSIXct("2022-03-01 11:00:00"),
                                     as.POSIXct("2022-11-01 11:00:00"), 
                                     "months"),
                        limits = c(as.POSIXct("2022-03-01 11:00:00"), 
                                   as.POSIXct("2022-11-15 11:00:00")
                        ),
                        trans = "time",
                        labels = month(label=TRUE,
                                       seq(as.POSIXct("2022-03-01 11:00:00"),
                                           as.POSIXct("2022-11-01 11:00:00"), 
                                           "months"))
  )+
  scale_x_continuous(breaks = seq(as.POSIXct("2022-03-01 11:00:00"),
                                  as.POSIXct("2022-11-01 11:00:00"), 
                                  "months"
  ),
  labels = month(label=FALSE,
                 seq(as.POSIXct("2022-03-01 11:00:00"),
                     as.POSIXct("2022-11-01 11:00:00"), 
                     "months"
                 )
  )
  )+
  labs(title="Lake Erie Water Level", y="Depth (m)", x= "Time (Months)")+
  guides(color = guide_colorbar(reverse = TRUE, nbin=10, raster=TRUE, barheight=10))+
  labs(color = "Month")+
  theme(text = element_text(size = 18))->LE


str(df_LE)
install.packages("fpp")
library(fpp)

df4<-clean
head(df4)

# LE level vs time
df4 %>% 
  filter(datetime>"2022-03-20 11:00:00", 
         datetime<"2022-11-01 11:00:00") %>%
  ggplot(aes(x = datetime, 
             y = verified_m,
             color=station)
  )+
  geom_point(size = 3, 
             alpha=0.3
            )+
  scale_x_continuous(breaks = seq(as.POSIXct("2022-03-01 11:00:00"),
                                  as.POSIXct("2022-11-01 11:00:00"), 
                                  "months"),
                     labels = month(label=TRUE,
                                    seq(as.POSIXct("2022-03-01 11:00:00"),
                                        as.POSIXct("2022-11-01 11:00:00"), 
                                        "months"))
                     )+
  labs(title="Lake Erie Water Level 2022", y="Depth (m)", x= "Time (Months)")+
  theme(text = element_text(size = 18))+
  scale_color_manual(values=c('blue4','#56B4E9'), guide=FALSE)+
  guides(colour = guide_legend(override.aes = list(alpha = 1)))



df_LE %>% 
  filter(datetime>"2022-09-22 11:00:00", 
         datetime<"2022-10-07 11:00:00") %>%
  ggplot(aes(x = datetime, 
             y = smoothed_verified_m, 
             color=station)
  )+
  geom_point(size = 3, 
             alpha=0.3
  )+
  scale_x_continuous(breaks = seq(as.POSIXct("2022-09-22 11:00:00"),
                                    as.POSIXct("2022-10-07 11:00:00"), 
                                    "3 days"),
                     labels = seq(as.POSIXct("2022-09-22"),
                                        as.POSIXct("2022-10-07"), 
                                        "3 days"),
                     df_LE %>% 
  filter(datetime>"2022-09-22 11:00:00", 
         datetime<"2022-10-07 11:00:00") %>%
  ggplot(aes(x = datetime, 
             y = smoothed_verified_m, 
             color=station)
  )+
  geom_point(size = 3, 
             alpha=0.3
  )+
  scale_x_continuous(breaks = seq(as.POSIXct("2022-09-22 11:00:00"),
                                    as.POSIXct("2022-10-07 11:00:00"), 
                                    "3 days"),
                     labels = seq(as.POSIXct("2022-09-22"),
                                        as.POSIXct("2022-10-07"), 
                                        "3 days")
                     
                     

df_LE %>% 
  filter(datetime>"2022-09-22 11:00:00", 
         datetime<"2022-10-07 11:00:00") %>%
  ggplot(aes(x = datetime, y = smoothed_verified_m, color=station))+
  geom_point(size = 3, alpha=0.3)+
  scale_x_datetime(breaks = seq(as.POSIXct("2022-09-22 11:00:00"),
                                  as.POSIXct("2022-10-07 11:00:00"), 
                                  "2 days"),
                     labels = seq(as.POSIXct("2022-09-22"),
                                        as.POSIXct("2022-10-07"), 
                                        "2 days"),
                     date_labels = "%d/%b")+
  labs(title="Lake Erie Seiche (NOAA/NOS/CO-OPS)", y="Depth (m)", x= "", fill = "NOAA Station")+
  theme(legend.position = "bottom")
                    
  
  