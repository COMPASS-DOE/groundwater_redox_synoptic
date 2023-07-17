# Analysis Redox (Eh)

setwd("C:/Users/fsilva/OneDrive - University of Toledo/Documents/Fausto UT/COMPASS/Manuscript 1 Redox/Figures/Codes")

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
       ggh4x,
       plotly)

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
                                    (site=="CRC" & datetime>"2022-07-19 00:00:00" & datetime<"2022-08-08 00:00:00")| 
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
         site!="GWI", site!="MSM"#,
         #location!="WTE"
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
  facet_grid2(site~location, 
             scales="free_y", 
             independent = "y", 
             drop = TRUE)+
  guides(color = guide_colorbar(reverse = FALSE, nbin=10, raster=TRUE, barheight=10))+
  labs(color = "Eh (mV)")+
  theme(text = element_text(size = 15)) + 
  theme(panel.spacing.x = unit(1, "lines"))+
  theme(strip.background = element_rect(fill = "grey90", color = "black"))+
  scale_x_datetime(limits = c(as.POSIXct("2022-03-01 00:00:00"), NA),
                   date_breaks = "2 months", 
                   date_labels = "%b")
              


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



#########################################################################

# Graph Groundwater level below surface (m) vs time, color Eh
df_troll %>% 
  filter(datetime>"2022-03-10 11:00:00", 
         datetime<"2022-11-05 11:00:00" ,
         flag_out_of_water=="FALSE",
         specific_conductivity>30,
         site=="CRC"#,
         #location!="WTE"
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
  labs(title="", y="Groundwater level (m)", x="Time")+
  facet_grid2(site~location, 
              scales="free_y", 
              independent = "y", 
              drop = TRUE)+
  guides(color = guide_colorbar(reverse = FALSE, nbin=10, raster=TRUE, barheight=20))+
  labs(color = "ORP (mV)")+
  theme(text = element_text(size = 40)) + 
  theme(axis.text = element_text(size = 40))+
  theme(panel.spacing.x = unit(1, "lines"))+
  theme(strip.background = element_rect(fill = "grey90", color = "black"))+
  theme(strip.text.x = element_text(size = 40))+
  scale_x_datetime(limits = c(as.POSIXct("2022-03-01 00:00:00"), NA),
                   date_breaks = "2 months", 
                   date_labels = "%b")->pR

# Save the graph as a PDF with a width of 10 inches and a height of 8 inches
ggsave("mygraph2.png", pR, width = 20, height = 8, units = "in", dpi = 300)
