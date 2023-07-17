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
       mgcv-package,
       ggh4x)

# Set ggplot theme
theme_set(theme_bw())

#bring df
setwd("C:/Users/fsilva/OneDrive - University of Toledo/Documents/Fausto UT/COMPASS/Manuscript 1 Redox/Figures/Codes")

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

df_troll<-df_troll %>%mutate(site_location = paste(site, location, sep = " - "))

str(df_troll)

# Graph Groundwater level below surface (m) vs time, color Eh

str(df_troll)

f_labels <- data.frame(site = c("CRC", "CRC", "CRC",
                                "PTR", "PTR", "PTR",
                                "OWC", "OWC", "OWC"), 
                       location = c("UP", "TR", "W",
                                    "UP", "TR", "W",
                                    "TR", "WTE", "W"),
                       label = c("CRC - UP", "CRC - TR", "CRC - W",
                                 "PTR - UP", "PTR - TR", "PTR - W",
                                 "OWC - TR", "OWC - WTE", "OWC - W")
                       )

df_troll$site_location <- factor(df_troll$site_location, levels = c("CRC - UP", "CRC - TR", "CRC - W", "PTR - UP", "PTR - TR", "PTR - W", "OWC - TR", "OWC - WTE", "OWC - W"))

f_labels$site <- factor(f_labels$site, levels=c("CRC", "PTR", "OWC", "MSM", "GWI"))
f_labels$location <- factor(f_labels$location, levels=c("UP", "TR", "WTE", "W"))

df_troll <- df_troll %>%
  mutate(Eh = p_h_orp - (0.718 * temperature) + 224.41)

df_troll <- df_troll %>%
  arrange(datetime) %>%
  mutate(delta_DO = rdo_concen - lag(rdo_concen))

df_troll <- df_troll %>%
  arrange(datetime) %>%
  mutate(delta_WL = wl_below_surface_m - lag(wl_below_surface_m))

df_troll <- df_troll %>%
  arrange(datetime) %>%
  mutate(delta_Eh = Eh - lag(Eh))

df_troll %>%
  filter(datetime > "2022-03-01 11:00:00",
         datetime < "2022-11-05 11:00:00",
         flag_out_of_water == "FALSE",
         specific_conductivity > 30,
         site != "GWI", site != "MSM", location !="WTE") %>%
  ggplot(aes(x = datetime,
             y = wl_below_surface_m,
             color = Eh)) +
  geom_point(size = 3, alpha = 0.3) +
  scale_color_gradientn(colours = viridis(7),
                        breaks = seq(-500, 800, 100)) +
  labs(title = "", y = "Groundwater level (m)", x = "Time") +
  facet_grid(site ~ location, 
             #ncol = 1, 
             scales = "free_y"#,
             #labeller = as_labeller(unique(df_troll$site_location))
             ) +
  guides(color = guide_colorbar(reverse = FALSE, nbin = 10, raster = TRUE, barheight = 10)) +
  labs(color = "Eh (mV)") +
  theme(text = element_text(size = 15))->ff# +
  #scale_x_datetime(
   # limits = c(as.POSIXct("2022-03-15 00:00:00"), NA),
  #  date_breaks = "1 month",
  #  date_labels = "%b") +
 # theme(strip.background = element_blank(),
    #    strip.text = element_blank()) #+
  #geom_text(x = as.POSIXct("2022-11-01 00:00:00"), y = Inf, label = f_labels$label,
    #        data = f_labels, size = 4, vjust = 1.5, color = "black")->p
ff

ggsave("GWL_vs_datetime_Eh_color.png", plot = ff, dpi = 300)



############################################################################

df_troll %>%
  filter(datetime > "2022-03-01 11:00:00",
         datetime < "2022-11-05 11:00:00",
         flag_out_of_water == "FALSE",
         specific_conductivity > 30,
         site != "GWI", site != "MSM", location !="WTE") %>%
  ggplot(aes(x = datetime,
             y = rdo_concen,
             color = Eh)) +
  geom_point(size = 3, alpha = 0.3) +
  scale_color_gradientn(colours = viridis(7),
                        breaks = seq(-500, 800, 100)) +
  labs(title = "", y = "Dissolved Oxygen (mg/L)", x = "Time") +
  facet_grid(site ~ location, 
             #ncol = 1, 
             scales = "free_y"#,
             #labeller = as_labeller(unique(df_troll$site_location))
  ) +
  guides(color = guide_colorbar(reverse = FALSE, nbin = 10, raster = TRUE, barheight = 10)) +
  labs(color = "Eh (mV)") +
  theme(text = element_text(size = 15))->fff# +
#scale_x_datetime(
# limits = c(as.POSIXct("2022-03-15 00:00:00"), NA),
#  date_breaks = "1 month",
#  date_labels = "%b") +
# theme(strip.background = element_blank(),
#    strip.text = element_blank()) #+
#geom_text(x = as.POSIXct("2022-11-01 00:00:00"), y = Inf, label = f_labels$label,
#        data = f_labels, size = 4, vjust = 1.5, color = "black")->p
fff

ggsave("DO_vs_datetime_Eh_color.png", plot = fff, dpi = 300)








chooseCRANmirror()
options(repos = "https://cloud.r-project.org")
install.packages("pracma")
library(pracma)

?findpeaks

findpeaks(x, nups = 1, ndowns = nups, zero = "0", peakpat = NULL,
          minpeakheight = -Inf, minpeakdistance = 1,
          threshold = 0, npeaks = 0, sortstr = FALSE)





df_troll %>%
  filter(datetime > "2022-03-01 11:00:00",
         datetime < "2022-11-05 11:00:00",
         flag_out_of_water == "FALSE",
         specific_conductivity > 30,
         site != "GWI", site != "MSM") %>%
  ggplot(aes(x = delta_WL,
             y = delta_Eh,
             #color = Eh
             )
         ) +
  geom_point(size = 3, alpha = 0.3) +
  scale_color_gradientn(colours = viridis(7),
                        breaks = seq(-500, 800, 100)) +
  labs(title = "", y = "delta Eh", x = "delta WL") +
  facet_wrap(site ~ location, ncol = 2, scales = "free_y",
             labeller = as_labeller(unique(df_troll$site_location))
  ) +
  guides(color = guide_colorbar(reverse = FALSE, nbin = 10, raster = TRUE, barheight = 10)) +
  labs(color = "Eh (mV)") +
  theme(text = element_text(size = 15)) +
  theme(strip.background = element_blank(),
        strip.text = element_blank())->p
p

ggsave("DO_vs_datetime_Eh_color.png", plot = p, dpi = 300)









########################################
str(df_troll)
df_troll %>% 
  filter(datetime>"2022-03-01 11:00:00", 
         datetime<"2022-11-10 11:00:00" ,
         flag_out_of_water=="FALSE",
         specific_conductivity>30,
         site=="GWI",
         location=="UP")%>%
  ggplot(aes(x = datetime, 
             y = specific_conductivity))+
  geom_point(size = 3, alpha=0.3)+
  labs(title="GWI UP", y="SPC (uS/cm)", x="Time")
 

str(df_troll)
df_troll %>% 
  filter(datetime>"2022-03-01 11:00:00", 
         datetime<"2022-11-10 11:00:00" ,
         flag_out_of_water=="FALSE",
         specific_conductivity>30,
         site=="GWI",
         location=="UP")%>%
  ggplot(aes(x = datetime, 
             y = wl_below_surface_m))+
  geom_point(size = 3, alpha=0.3, color="blue")+
  labs(title="GWI UP", y="Groundwater level below the surface (m)", x="Time")

#######################################

df_filtered  %>% 
  filter(datetime>"2022-03-01 11:00:00", 
         datetime<"2022-11-05 11:00:00" ,
         flag_out_of_water=="FALSE"#,
         #site!="GWI", site!="MSM"#,
         #location=="WTE"
  ) %>%
  ggplot(aes(x = datetime, 
             y = Eh#,
             #color=location
             )
  )+
  geom_point(size = 3, 
             alpha=0.3)+
  labs(title="", y="EH", x="datetime")+
  facet_wrap(site~location, ncol=1, scales="free_y")+
  theme(text = element_text(size = 15))+ 
  theme(strip.background = element_blank(),
       strip.text = element_blank())#+
  #geom_text(x = as.POSIXct("2022-11-01 00:00:00"), y = Inf, label = f_labels$label, 
 #           data = f_labels, size = 4, vjust = 1.5, color = "black")


########################################

df_troll %>% 
  filter(datetime>"2022-03-01 11:00:00", 
         datetime<"2022-11-05 11:00:00" ,
         flag_out_of_water=="FALSE",
         specific_conductivity>30,
         site!="GWI", site!="MSM"#,
         #location!="WTE"
  ) %>%
  ggplot(aes(x = wl_below_surface_m, 
             y = specific_conductivity,
             color=location,
             shape=site)
  )+
  geom_point(size = 3, 
             alpha=0.3)+
  scale_color_gradientn(colours = viridis(7),
                        breaks = seq(-500,400,100)
  )+
  labs(title="", y="Dissolved Oxygen (mg/L)", x="Time")+
  facet_wrap(site~location, ncol=1, scales="free_y",
             labeller = as_labeller(unique(df_troll$site_location))
  )+
  guides(color = guide_colorbar(reverse = FALSE, nbin=10, raster=TRUE, barheight=10))+
  labs(color = "Eh (mV)")+
  theme(text = element_text(size = 15)) + 
  scale_x_datetime(limits = c(as.POSIXct("2022-03-15 00:00:00"), NA),
                   date_breaks = "1 month", 
                   date_labels = "%b")+
  theme(strip.background = element_blank(),
        strip.text = element_blank()
  )+
  geom_text(x = as.POSIXct("2022-11-01 00:00:00"), y = Inf, label = f_labels$label, 
            data = f_labels, size = 4, vjust = 1.5, color = "black")




#################################################################################











########################################

# Time series

df_troll %>% 
  filter(datetime>"2022-03-20 11:00:00", 
         datetime<"2022-11-05 11:00:00" ,
         flag_out_of_water=="FALSE",
         specific_conductivity>30,
         site!="GWI", site!="MSM")->df

# Split the data frame by site and location
mydata_split <- split(df, list(df$site, df$location))

#To diagnose if subsets may be empty, 
#you can check if any of your subsets are empty using the sapply() function, 
#which will return a logical vector indicating which subsets are empty:
empty_subsets <- sapply(mydata_split, function(subset) nrow(subset) == 0)

#This will return a logical vector with TRUE values for empty subsets and 
#FALSE values for non-empty subsets.  
#You can then print out the indices of the empty subsets using which():
which(empty_subsets)

#This will print out the indices of the empty subsets, 
#which you can use to remove those subsets from your mydata_split list:
mydata_split <- mydata_split[!empty_subsets]

#Once you have removed the empty subsets, 
#you can convert the remaining subsets to time series objects using the same code as before:
# Loop over each subset and convert the date-time column to a time series object

myts_list <- lapply(mydata_split, function(subset) {
  # Convert the date-time column to a time series object
  myts <- ts(subset$p_h_orp, frequency = 96)
  # Return the time series object
  return(myts)
})


# Apply the decompose function to each time series in myts_list
decomp_list <- lapply(myts_list, decompose)

# Extract the seasonal, trend, and residual components for each time series separately
seasonal_list <- lapply(decomp_list, function(x) x$seasonal)
trend_list <- lapply(decomp_list, function(x) x$trend)
residual_list <- lapply(decomp_list, function(x) x$random)
