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

window_size <- 4
df_troll <- df_troll %>%
  mutate(smoothed_rdo_concen = rollmeanr(rdo_concen, k = window_size, fill = NA))

df_troll <- df_troll %>%
  mutate(smoothed_Eh = rollmeanr(Eh, k = window_size, fill = NA))







#######################################################
#  1 
#  CRC UP
################################

df_troll %>% 
  filter(datetime>"2022-03-21 00:00:00", 
         datetime<"2022-05-01 11:00:00" ,
         flag_out_of_water=="FALSE",
         specific_conductivity>30,
         #rdo_concen>0.01,
         site=="CRC", location=="UP")->df_CRCUP

# Run the secondary axis helper
sec <- help_secondary(df_CRCUP, primary = smoothed_Eh, secondary = smoothed_rdo_concen, name="DO (mg/L)")

# Making primary plot
p <- ggplot(df_CRCUP, aes(datetime)) +
  geom_point(aes(y = smoothed_Eh, color="Eh"), size = 1.5, alpha = 0.3) +
  labs(title = "", color = "", y = "Eh (mV)", x="") +
  theme(text = element_text(size = 12)) +
  scale_color_manual(values = c("darkblue", "#008080"))+
  theme(legend.position = "top")+
  labs(color = "")
  
# For the secondary data, later we use the `proj` function from the helper
p <- p + geom_point(aes(y = sec$proj(smoothed_rdo_concen), color="DO"), size = 1.5, alpha = 0.3)

# We feed the scale the secondary axis
CRCUP_DO_Eh <- p + scale_y_continuous(sec.axis = sec) 
  
CRCUP_DO_Eh

df$Eh
df$rdo_concen

model<-lm(Eh~rdo_concen, data=df_CRCUP)
summary(model)


ggsave("CRC UP Eh vs DO 2.png", plot = CRCUP_DO_Eh, dpi = 300)


df_CRCUP %>%
  filter(datetime>"2022-04-01 00:00:00", 
         datetime<"2022-05-01 11:00:00",
          Eh > 330) %>%
  ggplot(aes(x = smoothed_rdo_concen, y = smoothed_Eh, color = datetime)) +
  geom_point() +
  scale_color_gradientn(colours = viridis(7),
                        breaks = seq(as.POSIXct("2022-04-01 00:00:00"),
                                     as.POSIXct("2022-05-01 11:00:00"),
                                     "5 days"
                                     )
                        )+
  guides(
    color = guide_colorbar(reverse = FALSE, nbin = 10, raster = TRUE, barheight = 5)
  ) +
  labs(title= "CRC UP", color = "Date", x= "DO (mg/L)", y= "Eh (mV)") +
  theme(text = element_text(size = 15))->CRCUP_DO_Eh_Hysteresis 


  ggsave("CRCUP_DO_Eh_Hysteresis.png", plot = CRCUP_DO_Eh_Hysteresis, dpi = 300)


####################################################################################
#  2 
#  CRC TR
###################################################
  
df_troll %>% 
  filter(datetime>"2022-03-18 11:00:00", 
         datetime<"2022-03-22 11:00:00" ,
         flag_out_of_water=="FALSE",
         specific_conductivity>30,
         #rdo_concen>0.01,
         site=="CRC", location=="TR")->df_CRCTR

# Run the secondary axis helper
sec <- help_secondary(df_CRCTR, primary = smoothed_Eh, secondary = smoothed_rdo_concen, name="DO (mg/L)")

# Making primary plot
p <- ggplot(df_CRCTR, aes(datetime)) +
  geom_point(aes(y = smoothed_Eh, color="Eh"), size = 2, alpha = 0.5) +
  labs(title = "", color = "", y = "Eh (mV)", x="") +
  theme(text = element_text(size = 12)) +
  scale_color_manual(values = c("darkblue", "#008080"))+
  theme(legend.position = "top")+
  labs(color = "")

# For the secondary data, later we use the `proj` function from the helper
p <- p + geom_point(aes(y = sec$proj(smoothed_rdo_concen), color="DO"), size = 2)

# We feed the scale the secondary axis
CRCTR_DO_Eh <- p + scale_y_continuous(sec.axis = sec) 
CRCTR_DO_Eh

model<-lm(Eh~rdo_concen, data=df_CRCTR)
summary(model)

ggsave("CRC TR Eh vs DO.png", plot = CRCTR_DO_Eh, dpi = 300)

df %>%
  ggplot(aes(x = smoothed_rdo_concen, y = smoothed_Eh, color = datetime)) +
  geom_point()


####################################################################################
# 3 #
#  CRC W
##################################################
df_troll %>% 
  filter(datetime>"2022-08-01 11:00:00", 
         datetime<"2022-11-01 11:00:00" ,
         flag_out_of_water=="FALSE",
         specific_conductivity>30,
         #rdo_concen>0.01,
         site=="CRC", location=="W")->df_CRCW

# Run the secondary axis helper
sec <- help_secondary(df_CRCW, primary = smoothed_Eh, secondary = smoothed_rdo_concen, name="DO (mg/L)")

# Making primary plot
p <- ggplot(df_CRCW, aes(datetime)) +
  geom_point(aes(y = smoothed_Eh, color="Eh"), size = 2, alpha = 0.5) +
  labs(title = "", color = "", y = "Eh (mV)", x="") +
  theme(text = element_text(size = 12)) +
  scale_color_manual(values = c("darkblue", "#008080"))+
  theme(legend.position = "top")+
  labs(color = "")

# For the secondary data, later we use the `proj` function from the helper
p <- p + geom_point(aes(y = sec$proj(smoothed_rdo_concen), color="DO"), size = 2)

# We feed the scale the secondary axis
CRCW_DO_Eh <- p + scale_y_continuous(sec.axis = sec)

CRCW_DO_Eh

model<-lm(Eh~rdo_concen, data=df_CRCW)
summary(model)

ggsave("CRC W Eh vs DO.png", plot = CRCW_DO_Eh, dpi = 300)


df_CRCW %>%
  filter(datetime>"2022-08-26 11:00:00", 
         datetime<"2022-09-01 00:00:00"#,
         #Eh > 330
         ) %>%
  ggplot(aes(x = smoothed_rdo_concen, y = smoothed_Eh, color = datetime)) +
  geom_point() +
  scale_color_gradientn(colours = viridis(7),
                        breaks = seq(as.POSIXct("2022-08-01 11:00:00"),
                                     as.POSIXct("2022-09-01 11:00:00"),
                                     "1 day",
                                     ),
                        labels = seq(as.Date(as.POSIXct("2022-08-01")),
                                     as.Date(as.POSIXct("2022-09-01")),
                                     "1 day")
  )+
  guides(
    color = guide_colorbar(reverse = FALSE, nbin = 10, raster = TRUE, barheight = 5)
  ) +
  labs(title= "CRC W", color = "Date", x= "DO (mg/L)", y= "Eh (mV)") +
  theme(text = element_text(size = 15))->CRCW_DO_Eh_Hysteresis 
CRCW_DO_Eh_Hysteresis

ggsave("CRCW_DO_Eh_Hysteresis.png", plot = CRCW_DO_Eh_Hysteresis, dpi = 300)


###########################################################
# 4 #
#  PTR UP
###################################
df_troll %>% 
  filter(datetime>"2022-03-01 11:00:00", 
         datetime<"2022-04-01 11:00:00" ,
         flag_out_of_water=="FALSE",
         specific_conductivity>30,
         #rdo_concen>0.01,
         site=="PTR", location=="UP")->df_PTRUP

# Run the secondary axis helper
sec <- help_secondary(df_PTRUP, primary = smoothed_Eh, secondary = smoothed_rdo_concen, name="DO (mg/L)")

# Making primary plot
p <- ggplot(df_PTRUP, aes(datetime)) +
  geom_point(aes(y = smoothed_Eh, color="Eh"), size = 2, alpha = 0.5) +
  labs(title = "", color = "", y = "Eh (mV)", x="") +
  theme(text = element_text(size = 12)) +
  scale_color_manual(values = c("darkblue", "#008080"))+
  theme(legend.position = "top")+
  labs(color = "")

# For the secondary data, later we use the `proj` function from the helper
p <- p + geom_point(aes(y = sec$proj(smoothed_rdo_concen), color="DO"), size = 2)

# We feed the scale the secondary axis
PTRUP_DO_Eh <- p + scale_y_continuous(sec.axis = sec) 
PTRUP_DO_Eh

model<-lm(Eh~rdo_concen, data=df_PTRUP)
summary(model)

ggsave("PTRUP_DO_Eh.png", plot = PTRUP_DO_Eh, dpi = 300)

#######################################################################
# 5 #
#  PTR TR
####################################
df_troll %>% 
  filter(datetime>"2022-03-18 00:00:00", 
         datetime<"2022-04-01 11:00:00" ,
         flag_out_of_water=="FALSE",
         specific_conductivity>30,
         #rdo_concen>0.01,
         site=="PTR", location=="TR")->df_PTRTR

# Run the secondary axis helper
sec <- help_secondary(df_PTRTR, primary = smoothed_Eh, secondary = smoothed_rdo_concen, name="DO (mg/L)")

# Making primary plot
p <- ggplot(df_PTRTR, aes(datetime)) +
  geom_point(aes(y = smoothed_Eh, color="Eh"), size = 2, alpha = 0.5) +
  labs(title = "", color = "", y = "Eh (mV)", x="") +
  theme(text = element_text(size = 12)) +
  scale_color_manual(values = c("darkblue", "#008080"))+
  theme(legend.position = "top")+
  labs(color = "")

# For the secondary data, later we use the `proj` function from the helper
p <- p + geom_point(aes(y = sec$proj(smoothed_rdo_concen), color="DO"), size = 2)

# We feed the scale the secondary axis
PTRTR_DO_Eh <- p + scale_y_continuous(sec.axis = sec) 

PTRTR_DO_Eh

model<-lm(Eh~rdo_concen, data=df_PTRTR)
summary(model)


ggsave("PTRTR_DO_Eh.png", plot = PTRTR_DO_Eh, dpi = 300)

df_PTRTR %>%
  filter(datetime>"2022-03-18 00:00:00", 
         datetime<"2022-04-01 11:00:00")%>%
  ggplot(aes(x = smoothed_rdo_concen, y = smoothed_Eh, color = datetime)) +
  geom_point()+
  scale_color_gradientn(colours = viridis(7),
                        breaks = seq(as.POSIXct("2022-03-18 00:00:00"),
                                     as.POSIXct("2022-04-01 11:00:00"),
                                     "6 days"))+
  guides(color = guide_colorbar(reverse = FALSE, nbin = 10, raster = TRUE, barheight = 5)) +
  labs(title= "PTR TR", color = "Date", x= "DO (mg/L)", y= "Eh (mV)") +
  theme(text = element_text(size = 15))->PTRTR_DO_Eh_Hysteresis 


ggsave("PTRTR_DO_Eh_Hysteresis.png", plot = PTRTR_DO_Eh_Hysteresis, dpi = 300)

####################################
# 6 No DO in PTR
####################################
# 7 #
#################################
#  OWC TR

df_troll %>% 
  filter(datetime>"2022-04-10 00:00:00", 
         datetime<"2022-05-03 11:00:00" ,
         flag_out_of_water=="FALSE",
         specific_conductivity>30,
         #rdo_concen>0.000001,
         site=="OWC", location=="TR")->df_OWCTR

# Run the secondary axis helper
sec <- help_secondary(df_OWCTR, primary = smoothed_Eh, secondary = smoothed_rdo_concen, name="DO (mg/L)")

# Making primary plot
p <- ggplot(df_OWCTR, aes(datetime)) +
  geom_point(aes(y = smoothed_Eh, color="Eh"), size = 2, alpha = 0.5) +
  labs(title = "", color = "", y = "Eh (mV)", x="") +
  theme(text = element_text(size = 12)) +
  scale_color_manual(values = c("darkblue", "#008080"))+
  theme(legend.position = "top")+
  labs(color = "")

# For the secondary data, later we use the `proj` function from the helper
p <- p + geom_point(aes(y = sec$proj(smoothed_rdo_concen), color="DO"), size = 2)

# We feed the scale the secondary axis
OWCTR_DO_Eh <- p + scale_y_continuous(sec.axis = sec) 

OWCTR_DO_Eh

model<-lm(Eh~rdo_concen, data=df_OWCTR)
summary(model)


ggsave("OWCTR_DO_Eh.png", plot = OWCTR_DO_Eh, dpi = 300)


df_OWCTR %>%
  filter(datetime>"2022-04-10 00:00:00", 
         datetime<"2022-05-03 11:00:00")%>%
  ggplot(aes(x = smoothed_rdo_concen, y = smoothed_Eh, color = datetime)) +
  geom_point() +
  scale_color_gradientn(colours = viridis(7),
                        breaks = seq(as.POSIXct("2022-04-10 00:00:00"),
                                     as.POSIXct("2022-05-03 11:00:00"),
                                     "6 days"))+
  guides(color = guide_colorbar(reverse = FALSE, nbin = 10, raster = TRUE, barheight = 5)) +
  labs(title= "OWC TR", color = "Date", x= "DO (mg/L)", y= "Eh (mV)") +
  theme(text = element_text(size = 15))->OWCTR_DO_Eh_Hysteresis 


ggsave("OWCTR_DO_Eh_Hysteresis.png", plot = OWCTR_DO_Eh_Hysteresis, dpi = 300)
#####
########################################
#  8
#  OWC W
#########################################
df_troll %>% 
  filter(datetime>"2022-05-05 11:00:00", 
         datetime<"2022-05-09 11:00:00" ,
         flag_out_of_water=="FALSE",
         specific_conductivity>30,
         #rdo_concen>0.000001,
         site=="OWC", location=="W")->df_OWCW

# Run the secondary axis helper
sec <- help_secondary(df_OWCW, primary = smoothed_Eh, secondary = smoothed_rdo_concen, name="DO (mg/L)")

# Making primary plot
p <- ggplot(df_OWCW, aes(datetime)) +
  geom_point(aes(y = smoothed_Eh, color="Eh"), size = 2, alpha = 0.5) +
  labs(title = "", color = "", y = "Eh (mV)", x="") +
  theme(text = element_text(size = 12)) +
  scale_color_manual(values = c("darkblue", "#008080"))+
  theme(legend.position = "top")+
  labs(color = "")

# For the secondary data, later we use the `proj` function from the helper
p <- p + geom_point(aes(y = sec$proj(smoothed_rdo_concen), color="DO"), size = 2)

# We feed the scale the secondary axis
OWCW_DO_Eh <- p + scale_y_continuous(sec.axis = sec)
OWCW_DO_Eh

model<-lm(Eh~rdo_concen, data=df_OWCW)
summary(model)


ggsave("OWCW_DO_Eh.png", plot = OWCW_DO_Eh, dpi = 300)


df_OWCW %>%
  filter(datetime>"2022-05-05 11:00:00", 
         datetime<"2022-05-09 11:00:00" )%>%
  ggplot(aes(x = smoothed_rdo_concen, y = smoothed_Eh, color = datetime)) +
  geom_point() +
  scale_color_gradientn(colours = viridis(7),
                        breaks = seq(as.POSIXct("2022-05-05 11:00:00"),
                                     as.POSIXct("2022-05-09 11:00:00"),
                                     "1 day"),
                         labels = seq(as.POSIXct("2022-05-05"),
                                     as.POSIXct("2022-05-09"),
                                     "1 day")
                        )+
  guides(color = guide_colorbar(reverse = FALSE, nbin = 10, raster = TRUE, barheight = 5)) +
  labs(title= "OWC W", color = "Date", x= "DO (mg/L)", y= "Eh (mV)") +
  theme(text = element_text(size = 15))->OWCW_DO_Eh_Hysteresis 
OWCW_DO_Eh_Hysteresis

ggsave("OWCW_DO_Eh_Hysteresis.png", plot = OWCW_DO_Eh_Hysteresis, dpi = 300)


##############################################################################
#  9
#  OWC WTE
###############################################
df_troll %>% 
  filter(datetime>"2022-03-01 11:00:00", 
         datetime<"2022-11-01 11:00:00" ,
         flag_out_of_water=="FALSE",
         specific_conductivity>30,
         #rdo_concen>0.000001,
         site=="OWC", location=="WTE")->df_WTE

# Run the secondary axis helper
sec <- help_secondary(df_WTE, primary = smoothed_Eh, secondary = smoothed_rdo_concen, name="DO (mg/L)")

# Making primary plot
p <- ggplot(df_WTE, aes(datetime)) +
  geom_point(aes(y = smoothed_Eh, color="Eh"), size = 1, alpha = 0.5) +
  labs(title = "", color = "", y = "Eh (mV)", x="") +
  theme(text = element_text(size = 12)) +
  scale_color_manual(values = c("darkblue", "#008080"))+
  theme(legend.position = "top")+
  labs(color = "")+
  scale_x_datetime(
    date_breaks = "1 month",
    date_labels = "%b",
    limits = as.POSIXct(c("2022-03-15", "2022-11-15"))
  )

  
# For the secondary data, later we use the `proj` function from the helper
p <- p + geom_point(aes(y = sec$proj(smoothed_rdo_concen), color="DO"), size = 1)

# We feed the scale the secondary axis
OWCWTE_DO_Eh <- p + scale_y_continuous(sec.axis = sec)

OWCWTE_DO_Eh

model<-lm(Eh~rdo_concen, data=df_WTE)
summary(model)


ggsave("OWCWTE_DO_Eh.png", plot = OWCWTE_DO_Eh, dpi = 300)


df_WTE %>% 
  filter(datetime>"2022-03-01 11:00:00", 
         datetime<"2022-11-01 11:00:00" )%>%
  ggplot(aes(x = smoothed_rdo_concen, y = smoothed_Eh, color = datetime)) +
  geom_point() +
  scale_color_gradientn(colours = viridis(7),
                        breaks = seq(as.POSIXct("2022-03-01 11:00:00"),
                                     as.POSIXct("2022-11-01 11:00:00"),
                                     "6 weeks"),
                        labels = seq(as.Date(as.POSIXct("2022-03-01")),
                                     as.Date(as.POSIXct("2022-11-01")),
                                     "6 weeks")
  )+
  guides(color = guide_colorbar(reverse = FALSE, nbin = 10, raster = TRUE, barheight = 5)) +
  labs(title= "OWC WTE", color = "Date", x= "DO (mg/L)", y= "Eh (mV)") +
  theme(text = element_text(size = 15))->OWCWTE_DO_Eh_Hysteresis 
OWCWTE_DO_Eh_Hysteresis 

ggsave("OWCWTE_DO_Eh_Hysteresis.png", plot = OWCWTE_DO_Eh_Hysteresis, dpi = 300)


#########################################################
# Arrange
######################################################

plot_grid(
  CRCUP_DO_Eh_Hysteresis,
  CRCW_DO_Eh_Hysteresis,
  PTRTR_DO_Eh_Hysteresis,
  OWCTR_DO_Eh_Hysteresis,
  OWCW_DO_Eh_Hysteresis,
  OWCWTE_DO_Eh_Hysteresis,
  ncol = 2)->grid_hysteresis

ggsave("grid_hysteresis.png", plot = grid_hysteresis, dpi = 300)


plot_grid(  
  CRCUP_DO_Eh,
  CRCTR_DO_Eh,
  CRCW_DO_Eh,
  PTRUP_DO_Eh,
  PTRTR_DO_Eh,
  OWCTR_DO_Eh,
  OWCW_DO_Eh,
  OWCWTE_DO_Eh, 
  ncol=2)

CRCUP_DO_Eh->p1
CRCTR_DO_Eh->p2
CRCW_DO_Eh->p3
PTRUP_DO_Eh->p4
PTRTR_DO_Eh->p5
OWCTR_DO_Eh->p6
OWCW_DO_Eh->p7
OWCWTE_DO_Eh->p8

prow <- plot_grid(
  p1 + theme(legend.position="none"),
  p2 + theme(legend.position="none"),
  p3 + theme(legend.position="none"),
  p4 + theme(legend.position="none"),
  p5 + theme(legend.position="none"),
  p6 + theme(legend.position="none"),
  p7 + theme(legend.position="none"),
  p8 + theme(legend.position="none"),
  align = 'vh',
  labels = c("A", "B", "C", "D", "E", "F", "G", "H"),
  hjust = -1,
  nrow = 4
)

legend_b <- get_legend(
  p1 + 
    guides(color = guide_legend(nrow = 1)) +
    theme(legend.position = "bottom")
)

plot_grid(prow, legend_b, ncol = 1, rel_heights = c(1, .1))
          




###############

# Other codes:

+
  geom_text(
    data = data.frame(
      x = as.POSIXct("2022-04-17 00:00:00"),
      y = 395,
      label = "Eh=DO*125+309 r2=0.18***"
    ),
    aes(x = x, y = y, label = label),
    parse = FALSE, size = 4, vjust = 1.5, color = "black"
  )



  geom_text(
    data = data.frame(
      x = as.POSIXct("2022-04-29 00:00:00"),
      y = 407,
      label = "paste(italic(r) ^ 2, \" = 0.18***\")"
    ),
    aes(x = x, y = y, label = label),
    parse = TRUE, size = 4, vjust = 1.5, color = "black"
  )

  
  geom_text(
    data = data.frame(
      x = as.POSIXct("2022-04-17 00:00:00"),
      y = 395,
      label = "Eh=DO*125+309 r2=0.18***"
    ),
    aes(x = x, y = y, label = label),
    parse = FALSE, size = 4, vjust = 1.5, color = "black"
  )
  
  "Results regression:
  
  CRC UP Eh=DO*125+309 r2=0.18***
  
  CRCTR Eh=DO*341+330 r2=0.47***  
      
  CRCW_DO_Eh Eh=DO*91-2 r2=0.29***
      
  PTR UP Eh=DO*76+331 r2=0.70***
  
  PTRTR_DO_Eh Eh=DO*27+368 r2=0.34***
        
  OWCTR Eh=DO*56+172 r2=0.24***
        
  OWC W Eh=DO*47-95 r2=0.41***
  
  OWCWTE Eh=DO*111+83 r2=0.25***"
        