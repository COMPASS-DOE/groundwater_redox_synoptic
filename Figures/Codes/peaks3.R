install.packages("pracma")
library(pracma)

#prepare df

str(df_troll)
df_troll %>%
  filter(datetime > "2022-03-01 11:00:00",
         datetime < "2022-11-05 11:00:00",
         flag_out_of_water == "FALSE",
         specific_conductivity > 30,
         site == "CRC", location == "UP")->df_CRC_UP

#see the data  
df_CRC_UP%>%
  ggplot(aes(x = datetime,
             y = wl_below_surface_m,
             color = Eh)) +
  geom_point(size = 3, alpha = 0.3)

df_CRC_UP%>%
  filter(datetime > "2022-03-01 11:00:00",
         datetime < "2022-05-20 11:00:00")%>%
  ggplot(aes(x = datetime,
             y = wl_below_surface_m,
             color = temperature)) +
  geom_point(size = 3, alpha = 0.3)

df_CRC_UP%>%
  filter(datetime > "2022-03-15 11:00:00",
         datetime < "2022-11-15 11:00:00")%>%
  ggplot(aes(x = datetime,
             y = wl_below_surface_m,
             color = Eh)) +
  geom_point(size = 3, alpha = 0.3)

df_CRC_UP %>%
  filter(datetime > "2022-03-15 11:00:00",
         datetime < "2022-11-15 11:00:00") %>%
  ggplot(aes(x = datetime)) +
  geom_line(aes(y = rdo_concen), color = "cyan4", linewidth = 1.5) +
  geom_line(aes(y = Eh/200), color = "black", linewidth = 1.5) +
  scale_y_continuous(
    name = "DO (mg/L)",
    sec.axis = sec_axis(
      ~ . * 200,
      name = "Eh (mV)",
      breaks = seq(-200, 500, 100),
      labels = seq(-200, 500, 100)))+
  labs(title = "")+
  scale_x_datetime(
    name = "2022",
    breaks = "1 month",
    date_labels = "%b"
  )->CRCUPO2
  

df_CRC_UP %>%
  filter(datetime > "2022-03-15 11:00:00",
         datetime < "2022-11-15 11:00:00") %>%
  ggplot(aes(x = datetime)) +
  geom_line(aes(y = wl_below_surface_m), color = Eh, linewidth = 1.5) +
  geom_line(aes(y = rdo_concen), color = "black", linewidth = 1.5) +
  scale_y_continuous(
    name = "Groundwater level (m)",
    limits = c(-4,4))+
  scale_x_datetime(
    name = "2022",
    breaks = "1 month",
    date_labels = "%b"
  )->CRCUPGWL

CRCUPGWL

grid.arrange(CRCUPGWL, CRCUPO2, nrow = 2)

df_CRC_UP %>%
  filter(datetime > "2022-03-15 11:00:00",
         datetime < "2022-11-15 11:00:00") %>%
  ggplot(aes(x = datetime)) +
  geom_line(aes(y = wl_below_surface_m ), color = "blue", linewidth = 1.5) +
  geom_line(aes(y = Eh/100), color = "black", linewidth = 1.5) +
  scale_y_continuous(
    name = "Groundwater level (m)",
    limits = c(-4,4),
    sec.axis = sec_axis(
      ~ . *100,
      name = "Eh (mV)",
      breaks = seq(-500, 500, 100),
      labels = seq(-500, 500, 100)))+
  labs(title = "CRC UP")+
  scale_x_datetime(
    name = "2022",
    breaks = "1 month",
    date_labels = "%b"
  )->CRCUPGWL

#decreasing noise

# Round the datetime column to the nearest hour
df_hourly <- df_CRC_UP %>%
  mutate(datetime_hourly = floor_date(datetime, "hour"))

# Aggregate the data by hour, taking the mean or any other desired summary statistic of the variables
df_hourly <- df_hourly %>%
  group_by(datetime_hourly, site, location) %>%
  summarise(
    mean_temperature = mean(temperature),
    mean_actual_conductivity = mean(actual_conductivity),
    mean_specific_conductivity = mean(specific_conductivity),
    mean_salinity = mean(salinity),
    mean_tds = mean(tds),
    mean_rdo_concen = mean(rdo_concen),
    mean_rdo_perc_sat = mean(rdo_perc_sat),
    mean_rdo_part_pressure = mean(rdo_part_pressure),
    mean_p_h = mean(p_h),
    mean_p_h_orp = mean(p_h_orp),
    mean_depth = mean(depth),
    mean_water_density = mean(water_density),
    mean_resistivity = mean(resistivity),
    mean_pressure_mbar = mean(pressure_mbar),
    mean_pressure_psi = mean(pressure_psi),
    mean_ground_to_sensor_cm = mean(ground_to_sensor_cm),
    mean_density_gcm3_cor = mean(density_gcm3_cor),
    mean_pressurehead_m = mean(pressurehead_m),
    mean_wl_below_surface_m = mean(wl_below_surface_m),
    mean_Eh = mean(Eh),
    n = n()
  )

str(df_hourly)


#see the data  
df_hourly%>%
  ggplot(aes(x = datetime_hourly,
             y = mean_wl_below_surface_m,
             color = mean_Eh)) +
  geom_point(size = 3, alpha = 0.3)


library(zoo)
smoothed <- rollmean(df_hourly$mean_wl_below_surface_m, k = 1, fill = NA)  # Adjust the window size as needed
df_hourly$mean_wl_below_surface_m <- smoothed

#see the data  
df_hourly%>%
  ggplot(aes(x = datetime_hourly,
             y = mean_wl_below_surface_m,
             color = mean_Eh)) +
  geom_point(size = 3, alpha = 0.3)


# Find peaks in the rdo_concen column pracma


peaks <- findpeaks(df_hourly$mean_rdo_concen, 
                   #nups = 1,
                   #ndowns = 1,
                   #threshold = 1,
                   #npeaks = 10
                   )

#graph1
df_hourly%>%
  ggplot(aes(x = datetime_hourly,
             y = mean_rdo_concen,
             color = mean_Eh)) +
  geom_point(size = 3, alpha = 0.3)+
  geom_point(size = 3, color = ifelse(seq_along(df_hourly$mean_rdo_concen) %in% peaks, "red", "transparent"))


peaks2 <- findpeaks(df_CRC_UP$smoothed_rdo_concen, 
                   #nups = 1,
                   #ndowns = 1,
                   #threshold = 1,
                   npeaks = 10)

#graph2
df_CRC_UP%>%
  ggplot(aes(x = datetime,
             y = smoothed_rdo_concen,
             color = Eh)) +
  geom_point(size = 3, alpha = 0.3)+
  geom_point(size = 3, color = ifelse(seq_along(df_CRC_UP$smoothed_rdo_concen) %in% peaks2, "red", "transparent"))





df_troll %>%
  filter(datetime > "2022-03-01 11:00:00",
         datetime < "2022-11-15 11:00:00",
         flag_out_of_water == "FALSE",
         specific_conductivity > 30,
         site == "CRC", location== "UP")%>%
  ggplot(aes(x = datetime,
             y = Eh,
             color = Eh)) +
  geom_point(size = 3, alpha = 0.3)









#step 0 bring df 
df_troll %>%
  filter(datetime > "2022-04-01 11:00:00",
         datetime < "2022-04-15 11:00:00",
         flag_out_of_water == "FALSE",
         specific_conductivity > 30,
         site == "CRC", location== "UP")-> df_CRC_UP


# Step 1: Smooth the signal
smoothed_signal <- smooth(df_CRC_UP$rdo_concen, method = "moving", width = 5)

# Step 2: Find peaks using the smoothed signal
peaks <- findpeaks(smoothed_signal)

# Step 3: Refine peak selection based on 4 times increasing and 4 times decreasing
filtered_peaks <- vector("numeric", length = length(peaks$pks))
for (i in 1:length(peaks$pks)) {
  # Calculate the ratio between peak value and its neighboring points
  ratio_left <- peaks$pks[i] / smoothed_signal[peaks$locs[i] - 1]
  ratio_right <- peaks$pks[i] / smoothed_signal[peaks$locs[i] + 1]
  
  # Check if the ratios meet the criteria (4 times increasing and 4 times decreasing)
  if (ratio_left > 4 && ratio_right > 4) {
    filtered_peaks[i] <- peaks$pks[i]
  }
}

# Create a dataframe with peak information
peak_df <- data.frame(
  datetime = df_troll$datetime[peaks$locs],
  site = df_troll$site[peaks$locs],
  location = df_troll$location[peaks$locs],
  rdo_concen = filtered_peaks
)

# Plot the data with peaks highlighted in red
df_troll %>%
  ggplot(aes(x = datetime, y = rdo_concen, color = Eh)) +
  geom_point(size = 3, alpha = 0.3) +
  geom_point(data = peak_df, aes(color = "Peaks"), size = 3) +
  scale_color_manual(values = c("black", "red"), guide = FALSE) +
  facet_grid(site ~ location, scales = "free") +
  labs(title = "DO vs. Time", x = "Time", y = "Dissolved Oxygen (mg/L)") +
  theme_minimal()
Note that the code assumes you have already loaded the pracma package and have the required data fram



