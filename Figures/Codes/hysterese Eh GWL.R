# peaks

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
# Moving average

#DO
window_size <- 50
df_troll <- df_troll %>%
  mutate(smoothed_rdo_concen_mean = rollmean(rdo_concen, k = window_size, align = "center", fill = NA))

df_troll <- df_troll %>%
  mutate(smoothed_rdo_concen_mean_r = rollmeanr(rdo_concen, k = window_size, fill = NA))

#library(pracma)
#sigma <- 2
#df_troll <- df_troll %>%
#  mutate(smoothed_rdo_concen_gaussian = smooth(rdo_concen, sigma, kind = "gaussian"))

#Eh

df_troll <- df_troll %>%
  mutate(smoothed_Eh_mean = rollmean(Eh, k = window_size, align = "center", fill = NA))

df_troll <- df_troll %>%
  mutate(smoothed_Eh_mean_r = rollmeanr(Eh, k = window_size, fill = NA))




##################################################
# 1. CRC UP
################

df_troll %>%
  filter(datetime > "2022-04-15 00:00:00",
         datetime < "2022-04-19 00:00:00",
         flag_out_of_water == "FALSE",
         specific_conductivity > 30,
         rdo_concen==0,
         site == "CRC", location == "UP") %>%
  ggplot(aes(x = wl_below_surface_m, y = smoothed_Eh_mean, color = datetime)) +
  geom_point() +
  scale_color_gradientn(colours = viridis(7),
                        breaks = seq(as.POSIXct("2022-04-15 00:00:00"),
                                     as.POSIXct("2022-04-19 00:00:00"),
                                     "1 day"),
                        labels = format(seq(as.POSIXct("2022-04-15 00:00:00"),
                                            as.POSIXct("2022-04-19 00:00:00"),
                                            "1 day"),
                                        "%Y-%m-%d")) +
  guides(color = guide_colorbar(reverse = FALSE, nbin = 10, raster = TRUE, barheight = 5)) +
  labs(title = "", 
       y = "Eh (mV)", x = "Groundwater level (m)", color = "Date") -> PCU
PCU
ggsave("EH_GW_Hysteresis.png", plot = PCU, dpi = 300)


##################################################
# 2. CRC TR
################
df_troll %>% 
  filter(datetime > "2022-03-31 14:30:00", 
         datetime < "2022-04-02 06:00:00" ,
         flag_out_of_water == "FALSE",
         specific_conductivity > 30,
         rdo_concen==0,
         wl_below_surface_m>-0.35,
         site == "CRC", location == "TR") %>%
  ggplot(aes(x = wl_below_surface_m, y = smoothed_Eh_mean, color = datetime)) +
  geom_point() +
  scale_color_gradientn(colours = viridis(7),
                        breaks = seq(as.POSIXct("2022-03-10 11:00:00"),
                                     as.POSIXct("2022-04-08 06:00:00"),
                                     "1 week"),
                        labels = format(seq(as.POSIXct("2022-03-10 11:00:00"),
                                            as.POSIXct("2022-04-08 06:00:00"),
                                            "1 week"),
                                        format = "%Y-%m-%d")) +
  guides(color = guide_colorbar(reverse = FALSE, nbin = 10, raster = TRUE, barheight = 5)) +
  labs(title = "", 
       y = "Eh (mV)", x = "Groundwater level (m)", color = "Date") -> PCTR
PCTR
ggsave("Hysteresis_CRC_TR.png", plot = PCTR, dpi = 300)


##################################################
# 3 CRC W No hysteresis
################
df_troll %>% 
  filter(datetime>"2022-08-01 00:00:00", 
         datetime<"2022-11-01 00:00:00" ,
         flag_out_of_water=="FALSE",
         specific_conductivity>30,
         site=="CRC", location=="W",
         rdo_concen>0.01)%>%
  ggplot(aes(x = wl_below_surface_m, y = smoothed_Eh_mean, color = datetime)) +
  geom_point() 


##################################################
# 4 PTR UP
#################

timezone <- tz(df_troll$datetime)
print(timezone)
df_troll$datetime <- force_tz(df_troll$datetime, tzone = "EST5EDT")

df_troll %>% 
  filter(datetime>"2022-07-04 00:00:00", 
         datetime<"2022-07-20 00:00:00" ,
         flag_out_of_water=="FALSE",
         specific_conductivity>30,
         site=="PTR", location=="UP")%>%
  ggplot(aes(x = wl_below_surface_m, y = smoothed_Eh_mean, color = datetime)) +
  geom_point() +
  scale_color_gradientn(colours = viridis(7),
                        breaks = seq(as.POSIXct("2022-07-04 00:00:00"),
                                     as.POSIXct("2022-07-20 00:00:00"),
                                     "4 days"),
                        labels = format(seq(as.POSIXct("2022-07-04 00:00:00"),
                                            as.POSIXct("2022-07-20 00:00:00"),
                                            "4 days"),
                                        format = "%Y-%m-%d")) +
  guides(color = guide_colorbar(reverse = FALSE, nbin = 10, raster = TRUE, barheight = 5)) +
  labs(title = "", 
       y = "Eh (mV)", x = "Groundwater level (m)", color = "Date") -> PPU
PPU
ggsave("Hysteresis_PTR_UP.png", plot = PPU, dpi = 300)


##################################################
# 4 PTR TR
#################

df_troll %>%
  filter(datetime > "2022-04-17 11:00:00",
         datetime < "2022-06-17 11:00:00",
         flag_out_of_water == "FALSE",
         specific_conductivity > 30,
         site == "PTR",
         location == "TR") %>%
  ggplot(aes(x = wl_below_surface_m, y = smoothed_Eh_mean, color = datetime)) +
  geom_point() +
  scale_color_gradientn(
    colours = viridis(7),
    breaks = seq(as.POSIXct("2022-04-17 11:00:00"),
                 as.POSIXct("2022-07-28 11:00:00"),
                 "2 weeks"),
    labels = format(seq(as.POSIXct("2022-04-17 11:00:00"),
                        as.POSIXct("2022-07-28 11:00:00"),
                        "2 weeks"),
                    format = "%Y-%m-%d")
  ) +
  guides(color = guide_colorbar(reverse = FALSE, nbin = 10, raster = TRUE, barheight = 5)) +
  labs(title = "",
       y = "Eh (mV)", x = "Groundwater level (m)", color = "Date") -> PPT

PPT
ggsave("Hysteresis_CRC_TR.png", plot = PPT, dpi = 300)



##################################################
# 5 PTR W
#################

df_troll %>%
  filter(datetime > "2022-03-10 11:00:00",
         datetime < "2022-06-16 11:00:00",
         flag_out_of_water == "FALSE",
         specific_conductivity > 30,
         site == "PTR",
         location == "TR") %>%
  ggplot(aes(x = wl_below_surface_m, y = smoothed_Eh_mean, color = datetime)) +
  geom_point() +
  scale_color_gradientn(
    colours = viridis(7),
    breaks = seq(as.POSIXct("2022-03-10 11:00:00"),
                 as.POSIXct("2022-06-16 11:00:00"),
                 "2 weeks"),
    labels = format(seq(as.POSIXct("2022-03-10 11:00:00"),
                        as.POSIXct("2022-06-16 11:00:00"),
                        "2 weeks"),
                    format = "%Y-%m-%d")
  ) +
  guides(color = guide_colorbar(reverse = FALSE, nbin = 10, raster = TRUE, barheight = 5)) +
  labs(title = "",
       y = "Eh (mV)", x = "Groundwater level (m)", color = "Date") -> PPW

PPW
ggsave("Hysteresis_PTR_W.png", plot = PPW, dpi = 300)


##################################################
# 5 OWC TR
#################

df_troll %>%
  filter(datetime > "2022-03-10 11:00:00",
         datetime < "2022-09-16 11:00:00",
         flag_out_of_water == "FALSE",
         specific_conductivity > 30,
         site == "OWC",
         location == "TR") %>%
  ggplot(aes(x = wl_below_surface_m, y = smoothed_Eh_mean, color = datetime)) +
  geom_point() +
  scale_color_gradientn(
    colours = viridis(7),
    breaks = seq(as.POSIXct("2022-03-10 11:00:00"),
                 as.POSIXct("2022-06-16 11:00:00"),
                 "2 weeks"),
    labels = format(seq(as.POSIXct("2022-03-10 11:00:00"),
                        as.POSIXct("2022-06-16 11:00:00"),
                        "2 weeks"),
                    format = "%Y-%m-%d")
  ) +
  guides(color = guide_colorbar(reverse = FALSE, nbin = 10, raster = TRUE, barheight = 5)) +
  labs(title = "",
       y = "Eh (mV)", x = "Groundwater level (m)", color = "Date") -> OWCT

OWCT
ggsave("Hysteresis_OWC_T.png", plot = OWCT, dpi = 300)


################################################################################


# Sort the dataframe by datetime
df_cu <- df_cu %>% arrange(datetime)

# Calculate the lagged datetime values to determine the direction
df_cu <- df_cu %>%
  mutate(lagged_datetime = lag(datetime))

# Create a new column to store the arrow angle
df_cu <- df_cu %>%
  mutate(arrow_angle = ifelse(!is.na(lagged_datetime), atan2(Eh - lag(Eh), wl_below_surface_m - lag(wl_below_surface_m)), NA))

# Plot the graph with arrows
ggplot(df_cu, aes(x = wl_below_surface_m, y = Eh, color = datetime)) +
  geom_point() +
  geom_segment(aes(x = lag(wl_below_surface_m), y = lag(Eh),
                   xend = wl_below_surface_m, yend = Eh),
               arrow = arrow(length = unit(0.2, "cm"), type = "closed"),
               color = "black", alpha = 0.5) +
  scale_color_datetime(labels = date_format("%Y-%m-%d"),
                       limits = c(as.POSIXct("2022-03-25"), as.POSIXct("2022-04-25")),
                       breaks = seq(as.POSIXct("2022-03-25"), as.POSIXct("2022-04-25"), by = "1 week")) +
  labs(title = "", y = "Eh (mV)", x = "Groundwater level (m)") +
  theme(legend.position = "none")



###########################
## TRASH
#########################


df_troll %>% 
  filter(datetime>"2022-04-15 11:00:00", 
         datetime<"2022-04-19 05:00:00" ,
         flag_out_of_water=="FALSE",
         specific_conductivity>30,
         site=="CRC", location=="UP")%>%
  ggplot(aes(x = wl_below_surface_m, y = smoothed_Eh_mean, color = datetime)) +
  geom_point() +
  labs(title = "Redox Hysteresis in Groundwater Dynamics: CRC - UP", 
       y = "Eh (mV)", x = "Groundwater level (m)", color="date")->p1
p1
ggsave("Hysteresis_CRC_UP_2022_04_15_b.png", plot = p1, dpi = 300)



#######################################################

df_troll %>%
  filter(datetime > "2022-03-10 11:00:00",
         datetime < "2022-11-01 11:00:00",
         flag_out_of_water == "FALSE",
         specific_conductivity > 30,
         site != "MSM",
         site != "GWI",
         rdo_concen==0) %>%
  ggplot(aes(x = wl_below_surface_m, y = smoothed_Eh_mean, color = datetime)) +
  geom_point() +
  scale_color_gradientn(
    colours = viridis(7),
    breaks = seq(as.POSIXct("2022-03-10 11:00:00"),
                 as.POSIXct("2022-11-01 11:00:00"),
                 "4 weeks"),
    labels = format(seq(as.POSIXct("2022-03-10 11:00:00"),
                        as.POSIXct("2022-11-01 11:00:00"),
                        "4 weeks"),
                    format = "%Y-%m-%d")
  ) +
  guides(color = guide_colorbar(reverse = FALSE, nbin = 10, raster = TRUE, barheight = 5)) +
  labs(title = "",
       y = "Eh (mV)", x = "Groundwater level (m)", color = "Date")+
  facet_wrap(site~location, scale="free")

