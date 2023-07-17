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

str(df_troll)

# Graph Groundwater level below surface (m) vs time, color Eh
df_troll %>% 
  filter(datetime>"2022-03-01 11:00:00", 
         datetime<"2022-11-05 11:00:00" ,
         flag_out_of_water=="FALSE",
         specific_conductivity>30,
         site!="GWI", site!="MSM", #site!="OWC",
         #location=="W"
         ) %>%
  ggplot(aes(x = datetime, 
             y = wl_below_surface_m,
             color=p_h_orp)
  )+
  geom_point(size = 3, 
             alpha=0.3
  )+
  scale_color_gradientn(colours = viridis(7),
                        breaks = seq(-500,400,100)
  )+
  labs(title="", y="Groundwater level below surface (m)", x="Time")+
  facet_grid(site~location, 
             scales="free"
  )+
  guides(color = guide_colorbar(reverse = FALSE, nbin=10, raster=TRUE, barheight=10))+
  labs(color = "Eh (mV)")+
  theme(text = element_text(size = 18)) + 
  theme(panel.spacing.x = unit(1, "lines"))+
  theme(strip.background = element_rect(fill = "white", color = "black"))
