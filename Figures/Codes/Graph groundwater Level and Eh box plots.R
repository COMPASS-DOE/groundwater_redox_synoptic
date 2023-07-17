# Analysis Redox (Eh)

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
       mgcv-package)

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

# Eh factors Site and location
df_troll %>% 
  filter(datetime>"2022-03-21 11:00:00", 
         datetime<"2022-11-01 11:00:00" ,
         flag_out_of_water=="FALSE",
         specific_conductivity>30,
         site!="GWI", site!="MSM", #site!="OWC",
         #location=="W"
  ) %>%
  ggplot(aes(x = factor(site), 
             y = p_h_orp,
             fill=location
  )
  )+
  geom_boxplot()+
  scale_fill_manual(values=c("forestgreen","maroon", "purple1", "cornflowerblue"))+
  labs(x="Sites", y="Eh (mV)")+
  ggtitle("Groundwater Monitoring Lake Erie TAI - Redox Potential") +
  theme(text = element_text(size = 18))+
  guides(fill=guide_legend(title="TAI Zone"))

df_troll %>% 
  filter(datetime>"2022-03-21 11:00:00", 
         datetime<"2022-11-01 11:00:00" ,
         flag_out_of_water=="FALSE",
         specific_conductivity>30,
         site!="GWI", site!="MSM", #site!="OWC",
         #location=="W"
  )->df

shapiro.test(df$p_h_orp)

# Eh factor month grid site~location
df_troll %>% 
  filter(datetime>"2022-03-21 11:00:00", 
         datetime<"2022-11-01 11:00:00" ,
         flag_out_of_water=="FALSE",
         specific_conductivity>30,
         site!="GWI", site!="MSM", #site!="OWC",
         #location=="W"
  ) %>%
  ggplot(aes(x = factor(month2), 
             y = p_h_orp))+
  geom_boxplot()+
  facet_grid(site~location)+
  labs(x="Months", y="Eh (mV)")+
  ggtitle("Groundwater Monitoring Lake Erie TAI - Redox Potential") +
  scale_y_continuous(limits = c(-500, 500))+
  theme(text = element_text(size = 16),
        axis.text.x = element_text(angle = 45, hjust = 1))

# GWL factors Site and location
df_troll %>% 
  filter(datetime>"2022-03-21 11:00:00", 
         datetime<"2022-11-01 11:00:00" ,
         flag_out_of_water=="FALSE",
         specific_conductivity>30,
         site!="GWI", site!="MSM", #site!="OWC",
         #location=="W"
  ) %>%
  ggplot(aes(x = factor(site), 
             y = wl_below_surface_m,
             fill=location
  )
  )+
  geom_boxplot()+
  scale_fill_manual(values=c("forestgreen","maroon", "purple1", "cornflowerblue"))+
  labs(x="Sites", y="Groundwater level below the surface (m)")+
  ggtitle("Groundwater Monitoring Lake Erie TAI - Groundwater Level") +
  theme(text = element_text(size = 18))

# GWL factor month grid site~location
df_troll %>% 
  filter(datetime>"2022-03-21 11:00:00", 
         datetime<"2022-11-01 11:00:00" ,
         flag_out_of_water=="FALSE",
         specific_conductivity>30,
         site!="GWI", site!="MSM", #site!="OWC",
         #location=="W"
  ) %>%
  ggplot(aes(x = factor(month2), 
             y = wl_below_surface_m))+
  geom_boxplot(fill="cyan")+
  facet_grid(site~location, scale="free")+
  labs(x="Months", y="Groundwater level below the surface (m)")+
  ggtitle("Groundwater Monitoring Lake Erie TAI - Groundwater Level") +
  theme(text = element_text(size = 16),
        axis.text.x = element_text(angle = 45, hjust = 1))
