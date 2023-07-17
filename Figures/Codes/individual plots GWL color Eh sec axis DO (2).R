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

window_size <- 50
df_troll <- df_troll %>%
  mutate(smoothed_rdo_concen = rollmeanr(rdo_concen, k = window_size, fill = NA))
df_troll <- df_troll %>%
  mutate(smoothed_Eh = rollmeanr(Eh, k = window_size, fill = NA))

# 1 #
#  CRC UP

df_troll %>% 
  filter(datetime>"2022-03-21 11:00:00", 
         datetime<"2022-05-01 11:00:00" ,
         flag_out_of_water=="FALSE",
         specific_conductivity>30,
         rdo_concen>0.01,
         site=="CRC", location=="UP")->df

# Run the secondary axis helper
sec <- help_secondary(df, primary = smoothed_Eh, secondary = smoothed_rdo_concen, name="DO (mg/L)")

# Making primary plot
p <- ggplot(df, aes(datetime)) +
  geom_point(aes(y = smoothed_Eh, color="Eh"), size = 2, alpha = 0.5) +
  labs(title = "", color = "", y = "Eh (mV)") +
  theme(text = element_text(size = 12)) +
  scale_color_manual(values = c("darkblue", "#008080"))+
  theme(legend.position = "top")+
  labs(color = "CRC UP:")
  
# For the secondary data, later we use the `proj` function from the helper
p <- p + geom_point(aes(y = sec$proj(smoothed_rdo_concen), color="DO"), size = 2)

# We feed the scale the secondary axis
p + scale_y_continuous(sec.axis = sec)


model<-lm(Eh~rdo_concent, data=df)
summary(model)




# 2 #
# CRC TR #

df_troll %>% 
  filter(datetime>"2022-03-16 11:00:00", 
         datetime<"2022-11-01 11:00:00" ,
         flag_out_of_water=="FALSE",
         specific_conductivity>30,
         rdo_concen>0.05,
         site=="CRC", location=="TR")->dfct

# Run the secondary axis helper
sec <- help_secondary(dfct, primary = wl_below_surface_m, secondary = rdo_concen, name="DO (mg/L)")

# Making primary plot
p <- ggplot(dfct, aes(datetime)) +
  geom_point(aes(y = wl_below_surface_m, colour = Eh), size = 3, alpha = 0.5) +
  scale_color_gradientn(colours = viridis(7), limits = color_limits, breaks = color_breaks) +
  guides(color = guide_colorbar(reverse = FALSE, nbin = 10, raster = TRUE, barheight = 10)) +
  labs(title = "", color = "Eh (mV)", y = "Groundwater level (m)") +
  theme(text = element_text(size = 12)) +
  scale_x_datetime(limits = c(as.POSIXct("2022-03-15 00:00:00"), 
                              as.POSIXct("2022-11-01 00:00:00")),
                   date_breaks = "1 month",
                   date_labels = "%b",
                   name="")+
  geom_text(x = as.POSIXct("2022-10-28 00:00:00"), y = Inf, label = "CRC - TR", 
            data = f_labels, size = 4, vjust = 1.5, color = "black")

# For the secondary data, later we use the `proj` function from the helper
p <- p + geom_point(aes(y = sec$proj(rdo_concen)), colour = "black", size = 1)

# We feed the scale the secondary axis
pct <- p + scale_y_continuous(sec.axis = sec) 
pct


# 3 #
# CRC W #

df_troll %>% 
  filter(datetime>"2022-03-21 11:00:00", 
         datetime<"2022-11-01 11:00:00" ,
         flag_out_of_water=="FALSE",
         specific_conductivity>30,
         site=="CRC", location=="W")->dfcw

# Run the secondary axis helper
sec <- help_secondary(dfcw, primary = wl_below_surface_m, secondary = c(0,30), name="DO (mg/L)")

# Making primary plot
p <- ggplot(dfcw, aes(datetime)) +
  geom_point(aes(y = wl_below_surface_m, colour = Eh), size = 3, alpha = 0.5) +
  scale_color_gradientn(colours = viridis(7), limits = color_limits, breaks = color_breaks) +
  guides(color = guide_colorbar(reverse = FALSE, nbin = 10, raster = TRUE, barheight = 10)) +
  labs(title="", color = "Eh (mV)", y="Groundwater level (m)") +
  theme(text = element_text(size = 12))+
  scale_x_datetime(limits = c(as.POSIXct("2022-03-15 00:00:00"), 
                              as.POSIXct("2022-11-01 00:00:00")),
                   date_breaks = "1 month",
                   date_labels = "%b",
                   name="")+
  geom_text(x = as.POSIXct("2022-10-28 00:00:00"), y = Inf, label = "CRC - W", 
            data = f_labels, size = 4, vjust = 1.5, color = "black")

# For the secondary data, later we use the `proj` function from the helper
p <- p + geom_point(aes(y = sec$proj(rdo_concen)), colour = "black", size = 1)

# We feed the scale the secondary axis
pcw <- p + scale_y_continuous(sec.axis = sec) 
pcw


# 4 #
# PTR UP #

df_troll %>% 
  filter(datetime>"2022-03-21 11:00:00", 
         datetime<"2022-11-01 11:00:00" ,
         flag_out_of_water=="FALSE",
         specific_conductivity>30,
         site=="PTR", location=="UP")->dfpu

# Run the secondary axis helper
sec <- help_secondary(dfpu, primary = wl_below_surface_m, secondary = rdo_concen, name="DO (mg/L)")

# Making primary plot
p <- ggplot(dfpu, aes(datetime)) +
  geom_point(aes(y = wl_below_surface_m, colour = Eh), size = 3, alpha = 0.5) +
  scale_color_gradientn(colours = viridis(7), limits = color_limits, breaks = color_breaks) +
  guides(color = guide_colorbar(reverse = FALSE, nbin = 10, raster = TRUE, barheight = 10)) +
  labs(title="", color = "Eh (mV)", y="Groundwater level (m)") +
  theme(text = element_text(size = 12))+
  scale_x_datetime(limits = c(as.POSIXct("2022-03-15 00:00:00"), 
                              as.POSIXct("2022-11-01 00:00:00")),
                   date_breaks = "1 month",
                   date_labels = "%b",
                   name="")+
  geom_text(x = as.POSIXct("2022-10-28 00:00:00"), y = Inf, label = "PTR - UP", 
            data = f_labels, size = 4, vjust = 1.5, color = "black")

# For the secondary data, later we use the `proj` function from the helper
p <- p + geom_point(aes(y = sec$proj(rdo_concen)), colour = "black", size = 1)

# We feed the scale the secondary axis
ppu <- p + scale_y_continuous(sec.axis = sec) 
ppu



# 5 #
# PTR TR #

df_troll %>% 
  filter(datetime>"2022-03-21 11:00:00", 
         datetime<"2022-11-01 11:00:00" ,
         flag_out_of_water=="FALSE",
         specific_conductivity>30,
         site=="PTR", location=="TR")->dfpt

smoothed <- rollmean(dfpt$rdo_concen, k = 100, fill = NA)  # Adjust the window size as needed
dfpt$smoothed_rdo_concen <- smoothed


str(dfpt)
# Run the secondary axis helper
sec <- help_secondary(dfpt, primary = wl_below_surface_m, secondary = rdo_concen, name="DO (mg/L)")

# Making primary plot
p <- ggplot(dfpt, aes(datetime)) +
  geom_point(aes(y = wl_below_surface_m, colour = Eh), size = 3, alpha = 0.5) +
  scale_color_gradientn(colours = viridis(7), limits = color_limits, breaks = color_breaks) +
  guides(color = guide_colorbar(reverse = FALSE, nbin = 10, raster = TRUE, barheight = 10)) +
  labs(title="", color = "Eh (mV)", y="Groundwater level (m)") +
  theme(text = element_text(size = 12))+
  scale_x_datetime(limits = c(as.POSIXct("2022-03-15 00:00:00"), 
                              as.POSIXct("2022-11-01 00:00:00")),
                   date_breaks = "1 month",
                   date_labels = "%b",
                   name="")+
  geom_text(x = as.POSIXct("2022-10-28 00:00:00"), y = Inf, label = "PTR - TR", 
            data = f_labels, size = 4, vjust = 1.5, color = "black")

# For the secondary data, later we use the `proj` function from the helper
p <- p + geom_point(aes(y = sec$proj(rdo_concen)), colour = "black", size = 0.5)

# We feed the scale the secondary axis
ppt <- p + scale_y_continuous(sec.axis = sec) 
ppt


# 6 #
# PTR W #

df_troll %>% 
  filter(datetime>"2022-03-21 11:00:00", 
         datetime<"2022-11-01 11:00:00" ,
         flag_out_of_water=="FALSE",
         specific_conductivity>30,
         site=="PTR", location=="W")->dfpw

# Run the secondary axis helper
sec <- help_secondary(dfpw, primary = wl_below_surface_m, secondary = c(0,0.1), name = "DO (mg/L)")

# Making primary plot
p <- ggplot(dfpw, aes(datetime)) +
  geom_point(aes(y = wl_below_surface_m, colour = Eh), size = 3, alpha = 0.5) +
  scale_color_gradientn(colours = viridis(7), limits = color_limits, breaks = color_breaks) +
  guides(color = guide_colorbar(reverse = FALSE, nbin = 10, raster = TRUE, barheight = 10)) +
  labs(title = "", color = "Eh (mV)", y = "Groundwater level (m)") +
  theme(text = element_text(size = 12)) +
  scale_x_datetime(limits = c(as.POSIXct("2022-03-15 00:00:00"), 
                              as.POSIXct("2022-11-01 00:00:00")),
                   date_breaks = "1 month",
                   date_labels = "%b",
                   name="")+
  geom_text(x = as.POSIXct("2022-10-28 00:00:00"), y = Inf, label = "PTR - W", 
            data = f_labels, size = 4, vjust = 1.5, color = "black")

# For the secondary data, later we use the `proj` function from the helper
p <- p + geom_point(aes(y = sec$proj(rdo_concen)), colour = "black", size = 1)

# We feed the scale to the secondary axis
ppw <- p + scale_y_continuous(sec.axis = sec) 
ppw


# 7 #
# OWC TR #

df_troll %>% 
  filter(datetime>"2022-03-21 11:00:00", 
         datetime<"2022-11-01 11:00:00" ,
         flag_out_of_water=="FALSE",
         specific_conductivity>30,
         site=="OWC", location=="TR")->dfot

# Run the secondary axis helper
sec <- help_secondary(dfot, primary = wl_below_surface_m, secondary = c(0,20), name="DO (mg/L)")

# Making primary plot
p <- ggplot(dfot, aes(datetime)) +
  geom_point(aes(y = wl_below_surface_m, colour = Eh), size = 3, alpha = 0.5) +
  scale_color_gradientn(colours = viridis(7), limits = color_limits, breaks = color_breaks) +
  guides(color = guide_colorbar(reverse = FALSE, nbin = 10, raster = TRUE, barheight = 10)) +
  labs(title = "", color = "Eh (mV)", y = "Groundwater level (m)") +
  theme(text = element_text(size = 12)) +
  scale_x_datetime(limits = c(as.POSIXct("2022-03-15 00:00:00"), 
                              as.POSIXct("2022-11-01 00:00:00")),
                   date_breaks = "1 month",
                   date_labels = "%b",
                   name="")+
  geom_text(x = as.POSIXct("2022-10-28 00:00:00"), y = Inf, label = "OWC - TR", 
            data = f_labels, size = 4, vjust = 1.5, color = "black")

# For the secondary data, later we use the `proj` function from the helper
p <- p + geom_point(aes(y = sec$proj(rdo_concen)), colour = "black", size = 1)

# We feed the scale to the secondary axis
pot <- p + scale_y_continuous(sec.axis = sec) 
pot


# 8 #
# OWC WTE #

df_troll %>% 
  filter(datetime>"2022-03-21 11:00:00", 
         datetime<"2022-11-01 11:00:00" ,
         flag_out_of_water=="FALSE",
         specific_conductivity>30,
         site=="OWC", location=="WTE")->dfwte

# Run the secondary axis helper
sec <- help_secondary(dfwte, primary = wl_below_surface_m, secondary = c(0,30), name="DO (mg/L)")

# Making primary plot
p <- ggplot(dfwte, aes(datetime)) +
  geom_point(aes(y = wl_below_surface_m, colour = Eh), size = 3, alpha = 0.5) +
  scale_color_gradientn(colours = viridis(7), limits = color_limits, breaks = color_breaks) +
  guides(color = guide_colorbar(reverse = FALSE, nbin = 10, raster = TRUE, barheight = 10)) +
  labs(title = "", color = "Eh (mV)", y = "Groundwater level (m)") +
  theme(text = element_text(size = 12)) +
  scale_x_datetime(limits = c(as.POSIXct("2022-03-15 00:00:00"), 
                              as.POSIXct("2022-11-01 00:00:00")),
                   date_breaks = "1 month",
                   date_labels = "%b",
                   name="")+
  geom_text(x = as.POSIXct("2022-10-28 00:00:00"), y = Inf, label = "OWC - WTE", 
            data = f_labels, size = 4, vjust = 1.5, color = "black")

# For the secondary data, later we use the `proj` function from the helper
p <- p + geom_point(aes(y = sec$proj(rdo_concen)), colour = "black", size = 1, alpha = 0.5)

# We feed the scale to the secondary axis
pwte <- p + scale_y_continuous(sec.axis = sec) 
pwte


# 9 #
# OWC W #

df_troll %>% 
  filter(datetime>"2022-03-21 11:00:00", 
         datetime<"2022-11-01 11:00:00" ,
         flag_out_of_water=="FALSE",
         specific_conductivity>30,
         site=="OWC", location=="W")->dfow

# Run the secondary axis helper
sec <- help_secondary(dfow, primary = wl_below_surface_m, secondary = rdo_concen, name="DO (mg/L)")

# Making primary plot
p <- ggplot(dfow, aes(datetime)) +
  geom_point(aes(y = wl_below_surface_m, colour = Eh), size = 3, alpha = 0.5) +
  scale_color_gradientn(colours = viridis(7), limits = color_limits, breaks = color_breaks) +
  guides(color = guide_colorbar(reverse = FALSE, nbin = 10, raster = TRUE, barheight = 10)) +
  labs(title = "", color = "Eh (mV)", y = "Groundwater level (m)") +
  theme(text = element_text(size = 12)) +
  scale_x_datetime(limits = c(as.POSIXct("2022-03-15 00:00:00"), 
                              as.POSIXct("2022-11-01 00:00:00")),
                   date_breaks = "1 month",
                   date_labels = "%b",
                   name="")+
  geom_text(x = as.POSIXct("2022-10-28 00:00:00"), y = Inf, label = "OWC - W", 
            data = f_labels, size = 4, vjust = 1.5, color = "black")

# For the secondary data, later we use the `proj` function from the helper
p <- p + geom_point(aes(y = sec$proj(rdo_concen)), colour = "black", size = 1, alpha = 0.5)

# We feed the scale to the secondary axis
pow <- p + scale_y_continuous(sec.axis = sec) 
pow


######################################

library(gridExtra)
library(cowplot)

# Create a list to store the ggplot objects
plots <- list(pcu,
              pct,
              pcw,
              ppu,
              ppt,
              ppw,
              pot,
              pwte,
              pow)

# Combine all plots into a single plot grid
plot_grid(plotlist = plots, ncol = 1)

# Extract the legend from one of the plots
legend_plot <- get_legend(pcu)  # Replace pcu with the plot containing the desired legend

# Create a new empty plot with the same theme as the original plot
empty_plot <- ggplot() +
  theme_void() +
  theme(plot.margin = margin())

# Arrange the legend with the empty plot
combined_plot <- plot_grid(legend_plot, empty_plot, ncol = 1, rel_widths = c(1, 0.1))

# Combine the plot grid and the legend
final_plot <- plot_grid(combined_plot, plot_grid(plotlist = plots, ncol = 1), nrow = 2, rel_heights = c(0.1, 0.9))

# Display the final plot
final_plot









# combine the plots with a single leg
arranged_plots <- grid.arrange(pcu,
                               pct, 
                               pcw, 
                               ppu, 
                               ppt, 
                               ppw, 
                               pot,
                               pwte,
                               pow,
                               row = 9, col=1)

# Save the plot in PDF format with the current date included in the file name
ggsave(file_name, plot = arranged_plots, device = "pdf")




df%>%
ggplot(aes(x = wl_below_surface_m,
           y = Eh,
           color = datetime))+
  geom_point()

str(df_clima)
df_clima<-read_csv("df_clima.csv")
df_clima %>% 
  filter(datetime>"2022-07-10 11:00:00", 
         datetime<"2022-07-22 11:00:00" ,
         site=="CRC") %>%
  ggplot(aes(x = datetime, y = rain_mm_tot)) + 
  geom_point() + 
  facet_wrap(~site, 
             #scales="free"
  )  







df_clima %>%
  filter(datetime > "2022-03-10 11:00:00", 
         datetime < "2022-11-03 11:00:00",
         site == "CRC") %>%
  mutate(accumulated_rain = rollapplyr(rain_mm_tot, width = 3, FUN = sum, align = "right", fill = NA)) %>%
  ggplot(aes(x = datetime, y = accumulated_rain)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  labs(x = "Datetime", y = "Accumulated Rain (mm)") +
  theme_minimal()



str(df_troll)
str(df_clima)
