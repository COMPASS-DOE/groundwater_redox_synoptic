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




# 1 #
#  CRC UP

df_troll %>% 
  filter(datetime>"2022-03-21 11:00:00", 
         datetime<"2022-11-01 11:00:00" ,
         flag_out_of_water=="FALSE",
         specific_conductivity>30,
         site=="CRC", location=="UP")->df

# Run the secondary axis helper
sec <- help_secondary(df, primary = wl_below_surface_m, secondary = rdo_concen, name="")

# Making primary plot
p <- ggplot(df, aes(datetime)) +
  geom_point(aes(y = wl_below_surface_m, colour = Eh), size = 3, alpha = 0.5) +
  scale_color_gradientn(colours = viridis(7), limits = color_limits, breaks = color_breaks) +
  guides(color = guide_colorbar(reverse = FALSE, nbin = 10, raster = TRUE, barheight = 10)) +
  labs(title = "", color = "", y = "") +
  theme(text = element_text(size = 12)) +
  scale_x_datetime(limits = c(as.POSIXct("2022-03-15 00:00:00"), 
                              as.POSIXct("2022-11-01 00:00:00")),
                   date_breaks = "1 month",
                   date_labels = "%b",
                   name="")+
  geom_text(x = as.POSIXct("2022-10-28 00:00:00"), y = Inf, label = "CRC - UP", 
            size = 4, vjust = 1.5, color = "black")+
  theme(legend.position = "none")

# For the secondary data, later we use the `proj` function from the helper
p <- p + geom_point(aes(y = sec$proj(rdo_concen)), colour = "black", size = 1)

# We feed the scale the secondary axis
pcu <- p + scale_y_continuous(sec.axis = sec)
pcu
ggsave("CRCUP_GWL_Eh_DO2.png", plot = pcu, dpi = 300)

# 2 #
# CRC TR #

df_troll %>% 
  filter(datetime>"2022-03-21 11:00:00", 
         datetime<"2022-11-01 11:00:00" ,
         flag_out_of_water=="FALSE",
         specific_conductivity>30,
         site=="CRC", location=="TR")->dfct

# Run the secondary axis helper
sec <- help_secondary(dfct, primary = wl_below_surface_m, secondary = rdo_concen, name="")

# Making primary plot
p <- ggplot(dfct, aes(datetime)) +
  geom_point(aes(y = wl_below_surface_m, colour = Eh), size = 3, alpha = 0.5) +
  scale_color_gradientn(colours = viridis(7), limits = color_limits, breaks = color_breaks) +
  guides(color = guide_colorbar(reverse = FALSE, nbin = 10, raster = TRUE, barheight = 10)) +
  labs(title = "", color = "", y = "") +
  theme(text = element_text(size = 12)) +
  scale_x_datetime(limits = c(as.POSIXct("2022-03-15 00:00:00"), 
                              as.POSIXct("2022-11-01 00:00:00")),
                   date_breaks = "1 month",
                   date_labels = "%b",
                   name="")+
  geom_text(x = as.POSIXct("2022-10-28 00:00:00"), y = Inf, label = "CRC - TR", 
            size = 4, vjust = 1.5, color = "black")+
  theme(legend.position = "none")

# For the secondary data, later we use the `proj` function from the helper
p <- p + geom_point(aes(y = sec$proj(rdo_concen)), colour = "black", size = 1)

# We feed the scale the secondary axis
pct <- p + scale_y_continuous(sec.axis = sec) 
pct
ggsave("CRCTR_GWL_Eh_DO2.png", plot = pct, dpi = 300)

# 3 #
# CRC W #

df_troll %>% 
  filter(datetime>"2022-03-21 11:00:00", 
         datetime<"2022-11-01 11:00:00" ,
         flag_out_of_water=="FALSE",
         specific_conductivity>30,
         site=="CRC", location=="W")->dfcw

# Run the secondary axis helper
sec <- help_secondary(dfcw, primary = wl_below_surface_m, secondary = c(0,30), name="")

# Making primary plot
p <- ggplot(dfcw, aes(datetime)) +
  geom_point(aes(y = wl_below_surface_m, colour = Eh), size = 3, alpha = 0.5) +
  scale_color_gradientn(colours = viridis(7), limits = color_limits, breaks = color_breaks) +
  guides(color = guide_colorbar(reverse = FALSE, nbin = 10, raster = TRUE, barheight = 10)) +
  labs(title="", color = "", y="") +
  theme(text = element_text(size = 16))+
  scale_x_datetime(limits = c(as.POSIXct("2022-03-15 00:00:00"), 
                              as.POSIXct("2022-11-01 00:00:00")),
                   date_breaks = "1 month",
                   date_labels = "%b",
                   name="")+
  #geom_text(x = as.POSIXct("2022-10-28 00:00:00"), y = Inf, label = "CRC - W", 
  #          size = 4, vjust = 1.5, color = "black")+
  theme(legend.position = "none")

# For the secondary data, later we use the `proj` function from the helper
p <- p + geom_point(aes(y = sec$proj(rdo_concen)), colour = "black", size = 1)

# We feed the scale the secondary axis
pcw <- p + scale_y_continuous(sec.axis = sec) 
pcw
ggsave("CRCW_GWL_Eh_DO3.png", plot = pcw, dpi = 300)

# 4 #
# PTR UP #

df_troll %>% 
  filter(datetime>"2022-03-21 11:00:00", 
         datetime<"2022-11-01 11:00:00" ,
         flag_out_of_water=="FALSE",
         specific_conductivity>30,
         site=="PTR", location=="UP")->dfpu

# Run the secondary axis helper
sec <- help_secondary(dfpu, primary = wl_below_surface_m, secondary = rdo_concen, name="")

# Making primary plot
p <- ggplot(dfpu, aes(datetime)) +
  geom_point(aes(y = wl_below_surface_m, colour = Eh), size = 3, alpha = 0.5) +
  scale_color_gradientn(colours = viridis(7), limits = color_limits, breaks = color_breaks) +
  guides(color = guide_colorbar(reverse = FALSE, nbin = 10, raster = TRUE, barheight = 10)) +
  labs(title="", color = "", y="") +
  theme(text = element_text(size = 12))+
  scale_x_datetime(limits = c(as.POSIXct("2022-03-15 00:00:00"), 
                              as.POSIXct("2022-11-01 00:00:00")),
                   date_breaks = "1 month",
                   date_labels = "%b",
                   name="")+
  geom_text(x = as.POSIXct("2022-10-28 00:00:00"), y = Inf, label = "PTR - UP", 
            size = 4, vjust = 1.5, color = "black")+
  theme(legend.position = "none")

# For the secondary data, later we use the `proj` function from the helper
p <- p + geom_point(aes(y = sec$proj(rdo_concen)), colour = "black", size = 1)

# We feed the scale the secondary axis
ppu <- p + scale_y_continuous(sec.axis = sec) 
ppu
ggsave("PTRUP_GWL_Eh_DO2.png", plot = ppu, dpi = 300)


# 5 #
# PTR TR #

df_troll %>% 
  filter(datetime>"2022-03-21 11:00:00", 
         datetime<"2022-11-01 11:00:00" ,
         flag_out_of_water=="FALSE",
         specific_conductivity>30,
         site=="PTR", location=="TR")->dfpt

# Run the secondary axis helper
sec <- help_secondary(dfpt, primary = wl_below_surface_m, secondary = rdo_concen, name="")

# Making primary plot
p <- ggplot(dfpt, aes(datetime)) +
  geom_point(aes(y = wl_below_surface_m, colour = Eh), size = 3, alpha = 0.5) +
  scale_color_gradientn(colours = viridis(7), limits = color_limits, breaks = color_breaks) +
  guides(color = guide_colorbar(reverse = FALSE, nbin = 10, raster = TRUE, barheight = 10)) +
  labs(title="", color = "", y="") +
  theme(text = element_text(size = 12))+
  scale_x_datetime(limits = c(as.POSIXct("2022-03-15 00:00:00"), 
                              as.POSIXct("2022-11-01 00:00:00")),
                   date_breaks = "1 month",
                   date_labels = "%b",
                   name="")+
  geom_text(x = as.POSIXct("2022-10-28 00:00:00"), y = Inf, label = "PTR - TR", 
            size = 4, vjust = 1.5, color = "black")+
  theme(legend.position = "none")

# For the secondary data, later we use the `proj` function from the helper
p <- p + geom_point(aes(y = sec$proj(rdo_concen)), colour = "black", size = 0.5)

# We feed the scale the secondary axis
ppt <- p + scale_y_continuous(sec.axis = sec) 
ppt
ggsave("PTRTR_GWL_Eh_DO2.png", plot = ppt, dpi = 300)

# 6 #
# PTR W #

df_troll %>% 
  filter(datetime>"2022-03-21 11:00:00", 
         datetime<"2022-11-01 11:00:00" ,
         flag_out_of_water=="FALSE",
         specific_conductivity>30,
         site=="PTR", location=="W")->dfpw

# Run the secondary axis helper
sec <- help_secondary(dfpw, primary = wl_below_surface_m, secondary = c(0,0.1), name = "")

# Making primary plot
p <- ggplot(dfpw, aes(datetime)) +
  geom_point(aes(y = wl_below_surface_m, colour = Eh), size = 3, alpha = 0.5) +
  scale_color_gradientn(colours = viridis(7), limits = color_limits, breaks = color_breaks) +
  guides(color = guide_colorbar(reverse = FALSE, nbin = 10, raster = TRUE, barheight = 7)) +
  labs(title = "", color = "", y = "") +
  theme(text = element_text(size = 12)) +
  scale_x_datetime(limits = c(as.POSIXct("2022-03-15 00:00:00"), 
                              as.POSIXct("2022-11-01 00:00:00")),
                   date_breaks = "1 month",
                   date_labels = "%b",
                   name="")+
  geom_text(x = as.POSIXct("2022-10-28 00:00:00"), y = Inf, label = "PTR - W", 
            size = 4, vjust = 1.5, color = "black")+
  theme(legend.position = "none")

# For the secondary data, later we use the `proj` function from the helper
p <- p + geom_point(aes(y = sec$proj(rdo_concen)), colour = "black", size = 1)

# We feed the scale to the secondary axis
ppw <- p + scale_y_continuous(sec.axis = sec) + theme(axis.title.y = element_text(size = 14))
ppw
ggsave("PTRW_GWL_Eh_DO2.png", plot = ppw, dpi = 300)

# 7 #
# OWC TR #

df_troll %>% 
  filter(datetime>"2022-03-21 11:00:00", 
         datetime<"2022-11-01 11:00:00" ,
         flag_out_of_water=="FALSE",
         specific_conductivity>30,
         site=="OWC", location=="TR")->dfot

# Run the secondary axis helper
sec <- help_secondary(dfot, primary = wl_below_surface_m, secondary = c(0,20), name="")

# Making primary plot
p <- ggplot(dfot, aes(datetime)) +
  geom_point(aes(y = wl_below_surface_m, colour = Eh), size = 3, alpha = 0.5) +
  scale_color_gradientn(colours = viridis(7), limits = color_limits, breaks = color_breaks) +
  guides(color = guide_colorbar(reverse = FALSE, nbin = 10, raster = TRUE, barheight = 10)) +
  labs(title = "", color = "", y = "") +
  theme(text = element_text(size = 12)) +
  scale_x_datetime(limits = c(as.POSIXct("2022-03-15 00:00:00"), 
                              as.POSIXct("2022-11-01 00:00:00")),
                   date_breaks = "1 month",
                   date_labels = "%b",
                   name="")+
  geom_text(x = as.POSIXct("2022-10-28 00:00:00"), y = Inf, label = "OWC - TR", 
            size = 4, vjust = 1.5, color = "black")+
  theme(legend.position = "none")

# For the secondary data, later we use the `proj` function from the helper
p <- p + geom_point(aes(y = sec$proj(rdo_concen)), colour = "black", size = 1)

# We feed the scale to the secondary axis
pot <- p + scale_y_continuous(sec.axis = sec) 
pot
ggsave("OWCTR_GWL_Eh_DO2.png", plot = pot, dpi = 300)

# 8 #
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
                   name="")#+
 # geom_text(x = as.POSIXct("2022-10-28 00:00:00"), y = Inf, label = "OWC - WTE", 
          #  size = 4, vjust = 1.5, color = "black")

# For the secondary data, later we use the `proj` function from the helper
p <- p + geom_point(aes(y = sec$proj(rdo_concen)), colour = "black", size = 1, alpha = 0.5)

# We feed the scale to the secondary axis
pwte <- p + scale_y_continuous(sec.axis = sec) 
pwte
ggsave("OWCWTE_GWL_Eh_DO2.png", plot = pwte, dpi = 300)

# 9 #
# OWC W #

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
  facet_grid(site~location, scale="free_y")
  #geom_text(x = as.POSIXct("2022-10-28 00:00:00"), y = Inf, label = "OWC - W", 
    #        size = 4, vjust = 1.5, color = "black")+
  

# For the secondary data, later we use the `proj` function from the helper
p <- p + geom_point(aes(y = sec$proj(rdo_concen)), colour = "black", size = 1, alpha = 0.5)

# We feed the scale to the secondary axis
px3 <- p + scale_y_continuous(sec.axis = sec) 
px3
ggsave("OWCW_GWL_Eh_DO_todos_3.png", plot = px3, dpi = 300)


####################################

## 10 #
# OWC W - legend #

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
  labs(title = "", color = "Eh (mV)", y = "Groundwater Level (m)") +
  theme(text = element_text(size = 16)) +
  scale_x_datetime(limits = c(as.POSIXct("2022-03-15 00:00:00"), 
                              as.POSIXct("2022-11-01 00:00:00")),
                   date_breaks = "1 month",
                   date_labels = "%b",
                   name="")+
  geom_text(x = as.POSIXct("2022-10-28 00:00:00"), y = Inf, label = "OWC - W", 
            size = 4, vjust = 1.5, color = "black")

# For the secondary data, later we use the `proj` function from the helper
p <- p + geom_point(aes(y = sec$proj(rdo_concen)), colour = "black", size = 1, alpha = 0.5)

# We feed the scale to the secondary axis
powleg <- p + scale_y_continuous(sec.axis = sec) 
powleg
ggsave("leg_GWL_Eh_DO2.png", plot = powleg, dpi = 300)


##############


ggplot(dfx, aes(datetime)) +
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
            size = 4, vjust = 1.5, color = "black")+
  facet_grid(site~location)






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
