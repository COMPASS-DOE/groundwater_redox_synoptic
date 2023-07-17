
#prepare df

str(df_troll)
df_troll %>%
  filter(datetime > "2022-03-01 11:00:00",
         datetime < "2022-11-05 11:00:00",
         flag_out_of_water == "FALSE",
         specific_conductivity > 30,
         site != "GWI", site != "MSM")-> df

str(df)

# Convert the datetime column to POSIXct if it's not already in that format
df$datetime <- as.POSIXct(df$datetime)

# Round the datetime column to the nearest hour
df_hourly <- df %>%
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
    n = n()
  )

# View the resulting hourly aggregated data frame
View(df_hourly)

# The resulting 'df_hourly' data frame will have data aggregated at the hourly level


# reduce noise 
library(zoo)

smoothed <- rollmean(df_CRC_UP$rdo_concen, k = 100, fill = NA)  # Adjust the window size as needed
df_CRC_UP$smoothed_rdo_concen <- smoothed

df_CRC_UP%>%
  ggplot(aes(x = datetime,
             y = smoothed_rdo_concen,
             color = Eh)) +
  geom_point(size = 3, alpha = 0.3)


# Find peaks in the rdo_concen column pracma
install.packages("pracma")
library(pracma)

peaks <- findpeaks(df_CRC_UP$smoothed_rdo_concen, 
                   threshholdnups = 1)

#graph
df_CRC_UP%>%
  ggplot(aes(x = datetime,
             y = smoothed_rdo_concen,
             color = Eh)) +
  geom_point(size = 3, alpha = 0.3)+
  geom_point(size = 3, color = ifelse(seq_along(df_CRC_UP$rdo_concen) %in% peaks, "red", "transparent"))


# Find peaks in the rdo_concen column

install.packages("findpeaksr")
library(findpeaksr)
findpeaks.multi()

install.packages("peakPantheR")



###################################

#################################

library(pracma)


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



