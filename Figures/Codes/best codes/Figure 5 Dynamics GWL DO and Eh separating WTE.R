# Install packages
require(pacman)
p_load(readr, 
       dplyr, 
       ggplot2, 
       tidyverse, 
       plotly, 
       viridis, 
       lubridate, 
       # egg, 
       #mgcv-package,
       #nortest,
       # coin,
       # PMCMR,
       multcomp,
       lme4,
       gridExtra,
       # patchwork,
       cowplot,
       ggforce,
       rstatix,
       #multcomp,
       #lme4,
       #nlme,
       #lsmeans,
       #nlme,
       ggpubr,
       rstatix, 
       zoo,
       ggh4x
)

#set directory
setwd("C:/Users/fsilva/OneDrive - University of Toledo/Documents/Fausto UT/COMPASS/Manuscript 1 Redox/Figures/Codes")

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

#flag out of water

df_troll<-df_troll %>% 
  mutate(flag_out_of_water = ifelse((wl_below_surface_m < ((ground_to_sensor_cm/100)* -0.98)|
                                       (site=="CRC" & datetime>"2022-07-19 08:00:00" & datetime<"2022-07-20 08:00:00")| 
                                       (site=="OWC" & datetime>"2022-07-21 08:00:00" & datetime<"2022-07-22 08:00:00")| 
                                       (site=="PTR" & location =="UP" & datetime>"2022-08-02 08:00:00" & datetime<"2022-08-03 08:00:00")|
                                       (site=="PTR" & location !="UP" & datetime>"2022-07-19 08:00:00" & datetime<"2022-07-20 08:00:00")
  ),"TRUE","FALSE"
  )
  )

df_troll <- df_troll %>%
  mutate(Eh = p_h_orp - (0.718 * temperature) + 224.41)

df_troll<-df_troll %>%mutate(site_location = paste(site, location, sep = " - "))

# need to smotgh DO?
smoothed <- rollmean(df$rdo_concen, k = 100, fill = NA)  # Adjust the window size as needed
df$smoothed_rdo_concen <- smoothed


### Graphs:

# Define the limits and breaks for the color scale
color_limits <- c(-300, 700)
color_breaks <- seq(-300, 700, 100)



# All but not OWC WTE #

df_troll %>% 
  filter(datetime>"2022-03-21 11:00:00", 
         datetime<"2022-11-01 11:00:00" ,
         flag_out_of_water=="FALSE",
         specific_conductivity>30,
         site!="MSM", site!= "GWI", location!="WTE")->dfx

# Run the secondary axis helper
sec <- help_secondary(dfx, primary = wl_below_surface_m, secondary = rdo_concen, name="Dissolved Oxygen (mg/L)")

# Making primary plot
p <- ggplot(dfx, aes(datetime)) +
  geom_point(aes(y = wl_below_surface_m, colour = Eh), size = 3, alpha = 0.5) +
  scale_color_gradientn(colours = viridis(7), limits = color_limits, breaks = color_breaks) +
  guides(color = guide_colorbar(reverse = FALSE, nbin = 10, raster = TRUE, barheight = 10)) +
  labs(title = "A", color = "Eh (mV)", y = "Groundwater level (m)") +
  theme(text = element_text(size = 12)) +
  scale_x_datetime(limits = c(as.POSIXct("2022-03-15 00:00:00"), 
                              as.POSIXct("2022-11-01 00:00:00")),
                   date_breaks = "1 month",
                   date_labels = "%b",
                   name="")+
  facet_grid(site~location, scale="free_y")+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
#geom_text(x = as.POSIXct("2022-10-28 00:00:00"), y = Inf, label = "OWC - W", 
#        size = 4, vjust = 1.5, color = "black")+


# For the secondary data, later we use the `proj` function from the helper
p <- p + geom_point(aes(y = sec$proj(rdo_concen)), colour = "black", size = 1, alpha = 0.5)

# We feed the scale to the secondary axis
px3 <- p + scale_y_continuous(sec.axis = sec) 
px3
ggsave("OWCW_GWL_Eh_DO_todos_3.png", plot = px3, dpi = 300)


# OWC WTE #

df_troll %>% 
  filter(datetime>"2022-03-21 11:00:00", 
         datetime<"2022-11-01 11:00:00" ,
         flag_out_of_water=="FALSE",
         specific_conductivity>30,
         site=="OWC", location=="WTE")->dfwte

# Run the secondary axis helper
sec <- help_secondary(dfwte, primary = wl_below_surface_m, secondary = c(0,30), name="Dissolved Oxygen (mg/L)")

# Making primary plot
p <- ggplot(dfwte, aes(datetime)) +
  geom_point(aes(y = wl_below_surface_m, colour = Eh), size = 3, alpha = 0.5) +
  scale_color_gradientn(colours = viridis(7), limits = color_limits, breaks = color_breaks) +
  guides(color = guide_colorbar(reverse = FALSE, nbin = 10, raster = TRUE, barheight = 10)) +
  labs(title = "OWC WTE", color = "Eh (mV)", y = " Groundwater level (m)") +
  theme(text = element_text(size = 16)) +
  scale_x_datetime(limits = c(as.POSIXct("2022-03-15 00:00:00"), 
                              as.POSIXct("2022-11-01 00:00:00")),
                   date_breaks = "1 month",
                   date_labels = "%b",
                   name="")
# geom_text(x = as.POSIXct("2022-10-28 00:00:00"), y = Inf, label = "OWC - WTE", 
#  size = 4, vjust = 1.5, color = "black")

# For the secondary data, later we use the `proj` function from the helper
p <- p + geom_point(aes(y = sec$proj(rdo_concen)), colour = "black", size = 1, alpha = 0.5)

# We feed the scale to the secondary axis
pwte <- p + scale_y_continuous(sec.axis = sec) 
pwte
ggsave("OWCWTE_GWL_Eh_DO2.png", plot = pwte, dpi = 300)



