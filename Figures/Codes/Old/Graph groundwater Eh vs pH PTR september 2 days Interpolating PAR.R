
install.packages("devtools") 
devtools::install_github("kaizadp/soilpalettes")

# Install packages
require(pacman)
p_load(readr, 
       dplyr, 
       ggplot2, 
       tidyverse, 
       plotly, 
       viridis, 
       lubridate, 
       egg, 
       mgcv-package,
       cowplot)

# Set ggplot theme
theme_set(theme_bw())

#bring df
df_troll<-read_csv("trolls.csv")

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

df_troll$month2 <- factor(df_troll$month, levels = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12), 
                          labels = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"))
                          

str(df_troll)


##########################################################################################

# (1) eh vs time 

klimits <- c(as.POSIXct("2022-09-01 00:00:00"), as.POSIXct("2022-09-04 00:00:00"))

df_troll %>% 
  filter(datetime>"2022-09-01 00:00:00", 
         datetime<"2022-09-04 00:00:00" ,
         flag_out_of_water=="FALSE",
         site=="PTR",
         location=="UP") %>%
  ggplot(aes(x = datetime, y = p_h_orp))+
  geom_point(size = 2, alpha=0.5, color="forestgreen")+
  labs(title="", y="Eh (mV)", x="")+
  theme(text = element_text(size = 18))+
  guides(colour = guide_legend(override.aes = list(alpha = 1)))+
  theme(plot.margin = margin(0, 0, 0, 0))+
  scale_x_datetime(date_breaks = "12 hours",
                   limits= klimits,
                   date_labels = "%H:%M\n%d/%m")->p1


# (3) ph vs time

df_troll %>% 
  filter(datetime>"2022-09-01 00:00:00", 
         datetime<"2022-09-04 00:00:00" ,
         flag_out_of_water=="FALSE",
         site=="PTR",
         location=="UP") %>%
  ggplot(aes(x = datetime, y = p_h))+
  geom_point(size = 2, alpha=0.5, color="grey20")+
  labs(title="", y="ph", x="")+
  theme(text = element_text(size = 18))+
  guides(colour = guide_legend(override.aes = list(alpha = 1)))+
  theme(plot.margin = margin(0, 0, 0, 0))+
  scale_x_datetime(date_breaks = "12 hours",
                   limits= klimits,
                   date_labels = "%H:%M\n%d/%m")->p2


##########################################################################################

#(3) rain and PAR

df_clima<-read.csv("df_clima.csv")
str(df_clima)
#x <- as.POSIXct(df_clima$datetime, format="%Y-%m-%dT%H:%M:%OSZ", tz="EST")
#df_clima$datetime <- as.POSIXct(gsub("EST", "", x))
df_clima$site<-as.factor(df_clima$site)
df_clima$datetime<-as.POSIXct(df_clima$datetime)


df_clima %>% 
  filter(datetime>"2022-09-01 00:00:00", 
         datetime<"2022-09-04 00:00:00" ,
         site=="PTR",
         par_tot_c_tot<7000)-> hourly_data

# create a sequence of timestamps at 15-minute intervals
new_times <- seq(from = min(hourly_data$datetime),
                 to = max(hourly_data$datetime),
                 by = "15 min")

# use linear interpolation to calculate the values at each 15-minute interval
new_values <- approx(x = hourly_data$datetime, y = hourly_data$par_tot_c_tot, xout = new_times)$y

# create a data frame with the new values and timestamps
interpolated_data <- data.frame(datetime = new_times, value = new_values)


interpolated_data%>%
  ggplot(aes(x = datetime, y = value))+
  geom_point(size = 2, alpha=0.8, color="red")+
  #geom_line(color="red")+
  labs(title="", y="PAR (mmol/m2s)", x="")+
  theme(text = element_text(size = 18))+
  guides(colour = guide_legend(override.aes = list(alpha = 1)))+
  theme(plot.margin = margin(0, 0, 0, 0))+
  scale_x_datetime(date_breaks = "12 hours",
                   limits= klimits,
                   date_labels = "%H:%M\n%d/%m")->p3


title <- ggdraw() +
  draw_label(
    "PTR-UP",
    fontface = 'bold',
    x = 0,
    hjust = 0
  )
  
  
pp <- list(p1, p2, p3)
plot_grid(plotlist=pp, nrow = 3, ncol=1, align='hv', axis = "tb",
          rel_widths= c(1,1,1),rel_heights = c(1,1,1))



########################################################

