# Install packages
require(pacman)
options(repos = c(CRAN = "https://cran.cnr.berkeley.edu/"))
p_load(dplyr,
       tidyr,
       ggplot2, 
       tidyverse, 
       plotly, 
       viridis, 
       lubridate, 
       egg, 
       mgcv,
       cowplot,
       data.table,
       grid,
       gridExtra,
       cowplot)

# Set ggplot theme
theme_set(theme_bw())

#bring df
df_troll<-data.frame(fread("trolls.csv"))

troll<-data.frame(fread("trolls.csv"))


#Change OWC UP to WTE, order sites and locations
df_troll <- df_troll %>% 
                 mutate(location = ifelse(site == "OWC" & 
                                          location == "UP", 
                                          "WTE", location))

df_troll$site <- factor(df_troll$site, levels=c("CRC", "PTR", "OWC", "MSM", "GWI"))
df_troll$location <- factor(df_troll$location, levels=c("UP", "TR", "WTE", "W"))

#create season
#df_troll<-df_troll %>%
#  mutate(season = lubridate::quarter(datetime))

#df_troll$season <- as.factor(df_troll$season)
#levels(df_troll$season)<- c("Winter","Spring","Summer","Fall")

#create fresh and salt water
#df_troll<-df_troll %>%
#  mutate(water = case_when(site!="MSM" & site!="GWI" ~ "Fresh", site!="CRC" & site!="PTR" & site!= "OWC" ~ "Salt"))

#df_troll$water <- as.factor(df_troll$water)

#concatenate Site-location
#df_troll$siloc <- as.factor(paste(df_troll$site, "-", df_troll$location))

#QAQC
#df_troll<-df_troll %>% 
#  mutate(flag_out_of_water = ifelse(wl_below_surface_m < ((ground_to_sensor_cm/100) * -0.98), "TRUE","FALSE"))

df_troll<-df_troll %>% 
  mutate(flag_out_of_water = ifelse((wl_below_surface_m < ((ground_to_sensor_cm/100)* -0.98)|
                                    (site=="CRC" & datetime>"2022-07-19 00:00:00" & datetime<"2022-07-28 00:00:00")| 
                                    (site=="OWC" & datetime>"2022-07-21 08:00:00" & datetime<"2022-07-31 08:00:00")| 
                                    (site=="PTR" & location =="UP" & datetime>"2022-08-02 08:00:00" & datetime<"2022-08-12 08:00:00")|
                                    (site=="PTR" & location !="UP" & datetime>"2022-07-19 08:00:00" & datetime<"2022-07-29 08:00:00")
                                     ),"TRUE","FALSE"
                                    )
         )

df_troll$month <- month(df_troll$datetime)

df_troll$month2 <- factor(df_troll$month, 
                          levels = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12), 
                          labels = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", 
                                     "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"))
                          

str(df_troll)

#check TZ
tz <- attr(df_troll$datetime, "tzone")
print(tz)

# The datetime column is in "America/New_York", but it's not set, so R understands UTC
# then, you need to specify "datetime UTC" is "America/New_York"in your dataframe
df_troll$datetime <- as.POSIXct(format(df_troll$datetime, tz = "UTC"), tz = "America/New_York")

tz <- attr(df_troll$datetime, "tzone")
print(tz)

df_troll %>%
  filter(datetime>"2022-03-01 00:00:00", 
         datetime<"2022-11-02 00:00:00" ,
         #flag_out_of_water=="FALSE",
         site=="PTR",
         location=="UP"
         ) %>%
  ggplot(aes(x = datetime, y = p_h))+
  geom_point(size = 5, alpha=0.5, color="purple")+
  labs(title="", y="ph (mV)", x="")+
  theme(text = element_text(size = 18))+
  guides(colour = guide_legend(override.aes = list(alpha = 1)))+
  theme(plot.margin = margin(0, 0, 0, 0))+
  scale_x_datetime(date_breaks = "1 hour",
                   #limits= klimits,
                   date_labels = "%H")->p0check 

p0check 

##########################################################################################

# (0) To transform a dataframe with 24 variables measured each 15 minutes to a 
# new dataframe with all data per hour in R, you can use the dplyr and tidyr packages.

# Create a new column with the hourly interval using floor_date from the lubridate package.
# This code creates a new column hourly_interval that rounds the datetime column to the nearest hour.
df_troll$hourly_interval <- floor_date(df_troll$datetime, unit = "hour")

# Use group_by and summarise functions from the dplyr package 
# to aggregate the data by site, location, month2, and hourly_interval, 
# and compute the mean for each variable. The code would be:

## This code groups the data by site, location, month2, and hourly_interval, 
## and then calculates the mean for each variable. 
## The argument across(c(temperature:wl_below_surface_m), mean, na.rm = TRUE) 
## applies the mean function to columns temperature through wl_below_surface_m.

df_troll_hourly <- df_troll %>%
  group_by(site, location, month2, hourly_interval, flag_out_of_water) %>%
  summarise(across(c(temperature:wl_below_surface_m), mean, na.rm = TRUE))

# check structure
head(df_troll_hourly)
str(df_troll_hourly)

# Change column name from "hourly_interval" to "datetime"
names(df_troll_hourly)[names(df_troll_hourly) == "hourly_interval"] <- "datetime"

#check timezone
tz <- attr(df_troll_hourly$datetime, "tzone")
print(tz)

write.csv(df_troll_hourly, "df_troll_hourly.csv", row.names = FALSE)

##########################################################################################

# bring df trolls per hour
df_troll2<-data.frame(fread("df_troll_hourly.csv"))

str(df_troll2)


#Check timezone
tz <- attr(df_troll2$datetime, "tzone")
print(tz)

# Datetime column is in "America/New_York" but not setted up
#then, you need to specify "datetime UTC" is "America/New_York"in your dataframe
df_troll2$datetime <- as.POSIXct(format(df_troll2$datetime, tz = "UTC"), tz = "America/New_York")


# (1) Eh vs time 

df_troll2 %>% 
  filter(datetime>"2022-08-31 23:00:00", 
         datetime<"2022-09-04 00:00:00" ,
         flag_out_of_water=="FALSE",
         site=="PTR",
         location=="UP") %>%
  ggplot(aes(x = datetime, y = p_h_orp))+
  geom_point(size = 5, alpha=0.5, color="forestgreen")+
  labs(title="", y="Eh (mV)", x="")+
  theme(text = element_text(size = 18))+
  guides(colour = guide_legend(override.aes = list(alpha = 1)))+
  theme(plot.margin = margin(0, 0, 0, 0))+
  scale_x_datetime(breaks = seq(as.POSIXct("2022-08-31 00:00:00"), 
                                as.POSIXct("2022-09-04 00:00:00"), 
                                by = "12 hours"), 
                   date_labels = "%H:%M\n%d/%m")->p1


# (2) ph vs timeforestgreen

df_troll %>% 
  filter(datetime>"2022-03-01 23:00:00", 
         datetime<"2022-12-04 00:00:00" ,
         flag_out_of_water=="FALSE",
         site=="CRC",
         p_h>9,
         location=="W") %>%
  ggplot(aes(x = datetime, y = p_h))+
  geom_point(size = 5, alpha=0.5, color="purple4")+
  labs(title="", y="pH", x="")+
  theme(text = element_text(size = 18))+
  guides(colour = guide_legend(override.aes = list(alpha = 1)))+
  theme(plot.margin = margin(0, 0, 0, 0))+
  scale_x_datetime(breaks = seq(as.POSIXct("2022-08-31 00:00:00"), 
                                as.POSIXct("2022-09-04 00:00:00"), 
                                by = "12 hours"), 
                   date_labels = "%H:%M\n%d/%m")->p2


# (3) GWL vs time 

df_troll2 %>% 
  filter(datetime>"2022-08-31 23:00:00", 
         datetime<"2022-09-04 00:00:00" ,
         flag_out_of_water=="FALSE",
         site=="PTR",
         location=="UP") %>%
  ggplot(aes(x = datetime, y = wl_below_surface_m))+
  geom_point(size = 5, alpha=0.5, color="blue4")+
  labs(title="", y="GW Level Below Surface (m)", x="")+
  theme(text = element_text(size = 18))+
  guides(colour = guide_legend(override.aes = list(alpha = 1)))+
  theme(plot.margin = margin(0, 0, 0, 0))+
  scale_x_datetime(breaks = seq(as.POSIXct("2022-08-31 00:00:00"), 
                                as.POSIXct("2022-09-04 00:00:00"), 
                                by = "12 hours"), 
                   date_labels = "%H:%M\n%d/%m")->p3

# (4) SPC vs time 

df_troll2 %>% 
  filter(datetime>"2022-08-31 23:00:00", 
         datetime<"2022-09-04 00:00:00" ,
         flag_out_of_water=="FALSE",
         site=="PTR",
         location=="UP")%>%
  ggplot(aes(x = datetime, y = specific_conductivity))+
  geom_point(size = 5, alpha=0.5, color="grey30")+
  labs(title="", y="SPC (uS/cm)", x="")+
  theme(text = element_text(size = 18))+
  guides(colour = guide_legend(override.aes = list(alpha = 1)))+
  theme(plot.margin = margin(0, 0, 0, 0))+
  scale_x_datetime(breaks = seq(as.POSIXct("2022-08-31 00:00:00"), 
                                as.POSIXct("2022-09-04 00:00:00"), 
                                by = "12 hours"), 
                   date_labels = "%H:%M\n%d/%m")->p4



##########################################################################################

#(5) rain and PAR

df_clima<-read.csv("df_clima.csv")
str(df_clima)

# site and location as factor
df_clima$site<-as.factor(df_clima$site)
df_clima$location<-as.factor(df_clima$location)

# Datetime column is in "America/New_York" but not setted up
#then, you need to specify "datetime UTC" is "America/New_York"in your dataframe
df_clima$datetime <- as.POSIXct(format(df_clima$datetime, tz = "UTC"), tz = "America/New_York")

#Check timezone
tz <- attr(df_troll_hourly$datetime, "tzone")
print(tz)

df_clima %>% 
  filter(datetime>"2022-08-31 23:00:00", 
         datetime<"2022-09-04 00:00:00" ,
         site=="PTR",
         par_tot_c_tot<7000)%>%
  ggplot(aes(x = datetime, y = par_tot_c_tot))+
  geom_point(size = 5, alpha=0.5, color="red4")+
  labs(title="", y="PAR (mmol/m2s)", x="")+
  theme(text = element_text(size = 18))+
  guides(colour = guide_legend(override.aes = list(alpha = 1)))+
  theme(plot.margin = margin(0, 0, 0, 0))+
  scale_x_datetime(breaks = seq(as.POSIXct("2022-08-31 00:00:00"), 
                                as.POSIXct("2022-09-04 00:00:00"), 
                                by = "12 hours"), 
                   date_labels = "%H:%M\n%d/%m")->p3


pp1 <- p1+theme(plot.margin = unit(c(0.1, 1, 0.1, 0.5), "cm"))
pp2 <- p2+theme(plot.margin = unit(c(0.1, 1, 0.1, 0.5), "cm"))
pp3 <- p3+theme(plot.margin = unit(c(0.1, 1, 0.1, 0.5), "cm"))

pp <- list(pp3, pp2, pp1)
plot_grid(plotlist=pp, nrow = 3, ncol=1, align='hv', axis = "tb")
          


############################################################
############# Monthly ######################
#################################


# (1) Eh vs time 

df_troll2 %>% 
  filter(datetime>"2022-08-31 23:00:00", 
         datetime<"2022-09-30 00:00:00" ,
         flag_out_of_water=="FALSE",
         site=="PTR",
         location=="UP") %>%
  ggplot(aes(x = datetime, y = p_h_orp))+
  geom_point(size = 3, alpha=0.5, color="forestgreen")+
  labs(title="", y="Eh (mV)", x="")+
  theme(text = element_text(size = 16))+
  guides(colour = guide_legend(override.aes = list(alpha = 1)))+
  theme(plot.margin = margin(0, 0, 0, 0))+
  scale_x_datetime(breaks = seq(as.POSIXct("2022-08-31 00:00:00"), 
                                as.POSIXct("2022-09-30 00:00:00"), 
                                by = "24 hours"), 
                   date_labels = "%d")->pm1


# (2) ph vs timeforestgreen

df_troll2 %>% 
  filter(datetime>"2022-08-31 23:00:00", 
         datetime<"2022-09-30 00:00:00" ,
         flag_out_of_water=="FALSE",
         site=="PTR",
         location=="UP") %>%
  ggplot(aes(x = datetime, y = p_h))+
  geom_point(size = 3, alpha=0.5, color="purple4")+
  labs(title="", y="pH", x="")+
  theme(text = element_text(size = 16))+
  guides(colour = guide_legend(override.aes = list(alpha = 1)))+
  theme(plot.margin = margin(0, 0, 0, 0))+
  scale_x_datetime(breaks = seq(as.POSIXct("2022-08-31 00:00:00"), 
                                as.POSIXct("2022-09-30 00:00:00"), 
                                by = "24 hours"), 
                   date_labels = "%d")->pm2


# (3) GWL vs time 

df_troll2 %>% 
  filter(datetime>"2022-08-31 23:00:00", 
         datetime<"2022-09-30 00:00:00" ,
         flag_out_of_water=="FALSE",
         site=="PTR",
         location=="UP") %>%
  ggplot(aes(x = datetime, y = wl_below_surface_m))+
  geom_point(size = 3, alpha=0.5, color="blue4")+
  labs(title="", y="GWL (m)", x="")+
  theme(text = element_text(size = 16))+
  guides(colour = guide_legend(override.aes = list(alpha = 1)))+
  theme(plot.margin = margin(0, 0, 0, 0))+
  scale_x_datetime(breaks = seq(as.POSIXct("2022-08-31 00:00:00"), 
                                as.POSIXct("2022-09-30 00:00:00"), 
                                by = "24 hours"), 
                   date_labels = "%d")->pm3

# (4) SPC vs time 

df_troll2 %>% 
  filter(datetime>"2022-08-31 23:00:00", 
         datetime<"2022-09-30 00:00:00" ,
         flag_out_of_water=="FALSE",
         site=="PTR",
         location=="UP")%>%
  ggplot(aes(x = datetime, y = specific_conductivity))+
  geom_point(size = 3, alpha=0.5, color="grey30")+
  labs(title="", y="SPC\n(uS/cm)", x="")+
  theme(text = element_text(size = 16))+
  guides(colour = guide_legend(override.aes = list(alpha = 1)))+
  theme(plot.margin = margin(0, 0, 0, 0))+
  scale_x_datetime(breaks = seq(as.POSIXct("2022-08-31 00:00:00"), 
                                as.POSIXct("2022-09-30 00:00:00"), 
                                by = "24 hours"), 
                   date_labels = "%d")->pm4



##########################################################################################

#(5) rain and PAR

#df_clima<-read.csv("df_clima.csv")
#str(df_clima)

# site and location as factor
#df_clima$site<-as.factor(df_clima$site)
#df_clima$location<-as.factor(df_clima$location)

# Datetime column is in "America/New_York" but not setted up
#then, you need to specify "datetime UTC" is "America/New_York"in your dataframe
#df_clima$datetime <- as.POSIXct(format(df_clima$datetime, tz = "UTC"), tz = "America/New_York")

#Check timezone
#tz <- attr(df_troll_hourly$datetime, "tzone")
#print(tz)

df_clima %>% 
  filter(datetime>"2022-08-31 23:00:00", 
         datetime<"2022-09-30 00:00:00" ,
         site=="PTR",
         par_tot_c_tot<7000)%>%
  ggplot(aes(x = datetime, y = par_tot_c_tot))+
  geom_point(size = 3, alpha=0.5, color="red4")+
  labs(title="", y="PAR\n(mmol/m2s)", x="")+
  theme(text = element_text(size = 16))+
  guides(colour = guide_legend(override.aes = list(alpha = 1)))+
  theme(plot.margin = margin(0, 0, 0, 0))+
  scale_x_datetime(breaks = seq(as.POSIXct("2022-08-31 00:00:00"), 
                                as.POSIXct("2022-09-30 00:00:00"), 
                                by = "24 hours"), 
                   date_labels = "%d")->pm5


df_clima %>% 
  filter(datetime>"2022-08-31 23:00:00", 
         datetime<"2022-09-30 00:00:00" ,
         site=="PTR")%>%
  ggplot(aes(x = datetime, y = rain_mm_tot))+
  geom_point(size = 3, alpha=0.5, color="blue")+
  labs(title="", y="Rain\n(mm)", x="")+
  theme(text = element_text(size = 16))+
  guides(colour = guide_legend(override.aes = list(alpha = 1)))+
  theme(plot.margin = margin(0, 0, 0, 0))+
  scale_x_datetime(breaks = seq(as.POSIXct("2022-08-31 00:00:00"), 
                                as.POSIXct("2022-09-30 00:00:00"), 
                                by = "24 hours"), 
                   date_labels = "%d")->pm6


ppm1 <- pm1+theme(plot.margin = unit(c(0.1, 1, 0.1, 0.5), "cm"))
ppm2 <- pm2+theme(plot.margin = unit(c(0.1, 1, 0.1, 0.5), "cm"))
ppm3 <- pm3+theme(plot.margin = unit(c(0.1, 1, 0.1, 0.5), "cm"))
ppm4 <- pm4+theme(plot.margin = unit(c(0.1, 1, 0.1, 0.5), "cm"))
ppm5 <- pm5+theme(plot.margin = unit(c(0.1, 1, 0.1, 0.5), "cm"))
ppm6 <- pm6+theme(plot.margin = unit(c(0.1, 1, 0.1, 0.5), "cm"))

ppm <- list(ppm1, ppm2, ppm4, ppm5, ppm6)
plot_grid(plotlist=ppm, nrow = 5, ncol=1, align='hv', axis = "tb")





