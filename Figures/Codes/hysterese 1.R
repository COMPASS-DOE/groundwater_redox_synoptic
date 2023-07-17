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
smoothed <- rollmean(df$rdo_concen, k = 100, fill = NA)  # Adjust the window size as needed
df$smoothed_rdo_concen <- smoothed

install.packages("plotly")
library(plotly)


############################
# 1. CRC UP
################

df_troll %>% 
  filter(datetime>"2022-03-26 11:00:00", 
         datetime<"2022-04-24 11:00:00" ,
         flag_out_of_water=="FALSE",
         specific_conductivity>30,
         site=="CRC", location=="UP")->df_cu


df_cu %>%
  ggplot(aes(x = datetime, y = wl_below_surface_m, color = Eh)) +
  geom_point()->p

ggplotly(p)

df_cu %>%
  ggplot(aes(x = wl_below_surface_m, y = Eh, color = datetime)) +
  geom_point() +
  labs(title = "Redox Hysteresis in Groundwater Dynamics: CRC - UP", 
       y = "Eh (mV)", x = "Groundwater level (m)", color="date") +
  scale_color_gradientn(colours = rainbow(5),
                        limits = c(floor_date(as.POSIXct("2022-03-25 11:00:00"), "day"),
                                   floor_date(as.POSIXct("2022-04-25 11:00:00"), "day")),
                        breaks = seq(floor_date(as.POSIXct("2022-03-25 11:00:00"), "week"),
                                     floor_date(as.POSIXct("2022-04-25 11:00:00"), "week"),
                                     by = "1 week"),
                        labels = date_format("%Y-%m-%d"))->p



ggsave("Hysteresis_CRC_UP.png", plot = p, dpi = 300)



df_troll %>% 
  filter(datetime>"2022-04-19 11:00:00", 
         datetime<"2022-04-25 06:00:00" ,
         flag_out_of_water=="FALSE",
         specific_conductivity>30,
         site=="CRC", location=="UP")->df_cu


df_cu %>%
  ggplot(aes(x = datetime, y = wl_below_surface_m, color = Eh)) +
  geom_point()->p

ggplotly(p)

df_cu %>%
  ggplot(aes(x = wl_below_surface_m, y = Eh, color = datetime)) +
  geom_point() +
  labs(title = "Redox Hysteresis in Groundwater Dynamics: CRC - UP", 
       y = "Eh (mV)", x = "Groundwater level (m)", color="date") +
  scale_color_gradientn(colours = rainbow(5),
                        limits = c(floor_date(as.POSIXct("2022-04-15 11:00:00"), "day"),
                                   floor_date(as.POSIXct("2022-04-30 11:00:00"), "day")),
                        breaks = seq(floor_date(as.POSIXct("2022-04-15 11:00:00"), "week"),
                                     floor_date(as.POSIXct("2022-04-30 11:00:00"), "week"),
                                     by = "1 week"),
                        labels = date_format("%Y-%m-%d"))->p



ggsave("Hysteresis_CRC_UP.png", plot = p, dpi = 300)


##################################################


# 2a. CRC TRa

df_troll %>% 
  filter(datetime>"2022-03-10 11:00:00", 
         datetime<"2022-04-23 11:00:00" ,
         flag_out_of_water=="FALSE",
         specific_conductivity>30,
         site=="CRC", location=="TR")->df_cta


df_cta %>%
  ggplot(aes(x = datetime, y = wl_below_surface_m, color = Eh)) +
  geom_point()

df_cta %>%
  ggplot(aes(x = wl_below_surface_m, y = Eh, color = datetime)) +
  geom_point() +
  labs(title = "Redox Hysteresis in Groundwater Dynamics: CRC - TRa", 
       y = "Eh (mV)", x = "Groundwater level (m)", color = "date") +
  scale_color_gradientn(colours = rainbow(5),
                        limits = c(floor_date(as.POSIXct("2022-03-10 11:00:00"), "day"),
                                   floor_date(as.POSIXct("2022-04-24 11:00:00"), "day")),
                        breaks = seq(floor_date(as.POSIXct("2022-03-10 11:00:00"), "week"),
                                     floor_date(as.POSIXct("2022-04-24 11:00:00"), "week"),
                                     by = "1 week"),
                        labels = date_format("%Y-%m-%d"))->p2a


ggsave("Hysteresis_CRC_TR.png", plot = p2a, dpi = 300)


##################################################


# 2b. CRC TRb

df_troll %>% 
  filter(datetime>"2022-07-10 11:00:00", 
         datetime<"2022-09-15 11:00:00" ,
         flag_out_of_water=="FALSE",
         specific_conductivity>30,
         site=="CRC", location=="TR")->df_ctb


df_ctb %>%
  ggplot(aes(x = datetime, y = wl_below_surface_m, color = Eh)) +
  geom_point()

df_ctb %>%
  ggplot(aes(x = wl_below_surface_m, y = Eh, color = datetime)) +
  geom_point() +
  labs(title = "Redox Hysteresis in Groundwater Dynamics: CRC - TR", 
       y = "Eh (mV)", x = "Groundwater level (m)", color = "date") +
  scale_color_gradientn(colours = rainbow(5),
                        limits = c(floor_date(as.POSIXct("2022-07-10 11:00:00"), "day"),
                                   floor_date(as.POSIXct("2022-09-15 11:00:00"), "day")),
                        breaks = seq(floor_date(as.POSIXct("2022-07-10 11:00:00"), "week"),
                                     floor_date(as.POSIXct("2022-09-15 11:00:00"), "week"),
                                     by = "1 week"),
                        labels = date_format("%Y-%m-%d"))->p2b


ggsave("Hysteresis_CRC_TR.png", plot = p2b, dpi = 300)


##################################################

df_troll %>% 
  filter(datetime>"2022-08-01 11:00:00", 
         datetime<"2022-09-07 11:00:00" ,
         flag_out_of_water=="FALSE",
         specific_conductivity>30,
         site=="CRC", location=="W")->df_cw

df_cw %>%
  ggplot(aes(x = datetime, y = wl_below_surface_m, color = Eh)) +
  geom_point()

df_cw %>%
  ggplot(aes(x = wl_below_surface_m, y = Eh, color = datetime)) +
  geom_point() +
  labs(title = "Redox Hysteresis in Groundwater Dynamics: CRC - W", 
       y = "Eh (mV)", x = "Groundwater level (m)", color = "date") +
  scale_color_gradientn(colours = rainbow(5),
                        limits = c(floor_date(as.POSIXct("2022-08-01 11:00:00"), "day"),
                                   floor_date(as.POSIXct("2022-09-07 11:00:00"), "day")),
                        breaks = seq(floor_date(as.POSIXct("2022-08-01 11:00:00"), "week"),
                                     floor_date(as.POSIXct("2022-09-07 11:00:00"), "week"),
                                     by = "1 week"),
                        labels = date_format("%Y-%m-%d"))->p3


ggsave("Hysteresis_CRC_TR.png", plot = p3, dpi = 300)



##################################################

df_troll %>% 
  filter(datetime>"2022-03-17 11:00:00", 
         datetime<"2022-03-28 11:00:00" ,
         flag_out_of_water=="FALSE",
         specific_conductivity>30,
         site=="PTR", location=="UP")->df_pu

df_pu %>%
  ggplot(aes(x = datetime, y = wl_below_surface_m, color = Eh)) +
  geom_point()

df_pu %>%
  ggplot(aes(x = wl_below_surface_m, y = Eh, color = datetime)) +
  geom_point() +
  labs(title = "Redox Hysteresis in Groundwater Dynamics: CRC - W", 
       y = "Eh (mV)", x = "Groundwater level (m)", color = "date") +
  scale_color_gradientn(colours = rainbow(5),
                        limits = c(floor_date(as.POSIXct("2022-03-17 11:00:00"), "day"),
                                   floor_date(as.POSIXct("2022-03-28 11:00:00"), "day")),
                        breaks = seq(floor_date(as.POSIXct("2022-03-17 11:00:00"), "week"),
                                     floor_date(as.POSIXct("2022-03-28 11:00:00"), "week"),
                                     by = "3 day"),
                        labels = date_format("%Y-%m-%d"))->p4


ggsave("Hysteresis_CRC_TR.png", plot = p4, dpi = 300)



##################################################

df_troll %>% 
  filter(datetime>"2022-03-17 11:00:00", 
         datetime<"2022-03-28 11:00:00" ,
         flag_out_of_water=="FALSE",
         specific_conductivity>30,
         site=="PTR", location=="TR")->df_pt

df_pt %>%
  ggplot(aes(x = datetime, y = wl_below_surface_m, color = Eh)) +
  geom_point()

df_pu %>%
  ggplot(aes(x = wl_below_surface_m, y = Eh, color = datetime)) +
  geom_point() +
  labs(title = "Redox Hysteresis in Groundwater Dynamics: CRC - W", 
       y = "Eh (mV)", x = "Groundwater level (m)", color = "date") +
  scale_color_gradientn(colours = rainbow(5),
                        limits = c(floor_date(as.POSIXct("2022-03-17 11:00:00"), "day"),
                                   floor_date(as.POSIXct("2022-03-28 11:00:00"), "day")),
                        breaks = seq(floor_date(as.POSIXct("2022-03-17 11:00:00"), "week"),
                                     floor_date(as.POSIXct("2022-03-28 11:00:00"), "week"),
                                     by = "3 day"),
                        labels = date_format("%Y-%m-%d"))->p4


ggsave("Hysteresis_CRC_TR.png", plot = p4, dpi = 300)











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

