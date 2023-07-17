
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

#smooth
window_size <- 50
df_troll <- df_troll %>%
  mutate(smoothed_rdo_concen = rollmeanr(rdo_concen, k = window_size, align = "center", fill = NA))

df_troll <- df_troll %>%
  mutate(smoothed_Eh = rollmeanr(Eh, k = window_size, align = "center", fill = NA))


#######################################################
#  1A 
#  CRC UP
################################

df_troll %>% 
  filter(datetime>"2022-03-21 00:00:00", 
         datetime<"2022-05-01 11:00:00" ,
         flag_out_of_water=="FALSE",
         specific_conductivity>30,
         rdo_concen>0.01,
         site=="CRC", location=="UP")->df_CRCUP

model<-lm(formula=Eh~rdo_concen, data=df_CRCUP)
summary(model)

# Run the secondary axis helper
sec1 <- help_secondary(df_CRCUP, primary = smoothed_Eh, secondary = c(0,1), name="DO (mg/L)")

# Making primary plot
p1 <- ggplot(df_CRCUP, aes(datetime)) +
  geom_point(aes(y = smoothed_Eh, color="Eh"), size = 1.5, alpha = 0.3) +
  labs(title = "A. CRC UP", color = "", y = "Eh (mV)", x="") +
  theme(text = element_text(size = 12)) +
  scale_color_manual(values = c("darkblue", "#008080"))+
  theme(legend.position = "top")+
  labs(color = "")+ 
  theme(legend.position = "none")+
  geom_text(
    data = data.frame(
      x = as.POSIXct("2022-04-16 00:00:00"),
      y = 370,
      label = "Eh=DO*52+343 r2=0.74***"
    ),
    aes(x = x, y = y, label = label),
    parse = FALSE, size = 4, vjust = 1.5, color = "black"
  )

# For the secondary data, later we use the `proj` function from the helper
p1 <- p1 + geom_point(aes(y = sec1$proj(smoothed_rdo_concen), color="DO"), size = 1.5, alpha = 0.3)

# We feed the scale the secondary axis
CRCUP_DO_Eh1 <- p1 + scale_y_continuous(sec.axis = sec1) 
CRCUP_DO_Eh1
ggsave("CRCUP_DO_Eh1.png", plot = CRCUP_DO_Eh1, dpi = 300)

####################################################################################
#  2 B
#  CRC TR
###################################################

df_troll %>% 
  filter(datetime>"2022-03-18 11:00:00", 
         datetime<"2022-03-22 11:00:00" ,
         flag_out_of_water=="FALSE",
         specific_conductivity>30,
         rdo_concen>0.01,
         site=="CRC", location=="TR")->df_CRCTR

model<-lm(formula=Eh~rdo_concen, data=df_CRCTR)
summary(model)

# Run the secondary axis helper
sec2 <- help_secondary(df_CRCTR, primary = smoothed_Eh, secondary = c(0,0.25), name="DO (mg/L)")

# Making primary plot
p2 <- ggplot(df_CRCTR, aes(datetime)) +
  geom_point(aes(y = smoothed_Eh, color="Eh"), size = 2, alpha = 0.5) +
  labs(title = "B. CRC TR", color = "", y = "Eh (mV)", x="") +
  theme(text = element_text(size = 12)) +
  scale_color_manual(values = c("darkblue", "#008080"))+
  theme(legend.position = "top")+
  labs(color = "")+ 
  theme(legend.position = "none")+
  geom_text(
    data = data.frame(
      x = as.POSIXct("2022-03-21 00:00:00"),
      y = 380,
      label = "Eh=DO*346+303 r2=0.47***"
    ),
    aes(x = x, y = y, label = label),
    parse = FALSE, size = 4, vjust = 1.5, color = "black"
  )


# For the secondary data, later we use the `proj` function from the helper
p2 <- p2 + geom_point(aes(y = sec2$proj(smoothed_rdo_concen), color="DO"), size = 2)

# We feed the scale the secondary axis
CRCTR_DO_Eh1 <- p2 + scale_y_continuous(sec.axis = sec2) 
CRCTR_DO_Eh1
ggsave("CRCTR_DO_Eh1.png", plot = CRCTR_DO_Eh1, dpi = 300)


####################################################################################
# 3 C
#  CRC W
##################################################
df_troll %>% 
  filter(datetime>"2022-08-01 11:00:00", 
         datetime<"2022-11-01 11:00:00" ,
         flag_out_of_water=="FALSE",
         specific_conductivity>30,
         rdo_concen>0.01,
         site=="CRC", location=="W")->df_CRCW

model<-lm(formula=Eh~rdo_concen, data=df_CRCW)
summary(model)

# Run the secondary axis helper
sec3 <- help_secondary(df_CRCW, primary = smoothed_Eh, secondary = c(0,10), name="DO (mg/L)")

# Making primary plot
p3 <- ggplot(df_CRCW, aes(datetime)) +
  geom_point(aes(y = smoothed_Eh, color="Eh"), size = 2, alpha = 0.5) +
  labs(title = "C. CRC W", color = "", y = "Eh (mV)", x="") +
  theme(text = element_text(size = 12)) +
  scale_color_manual(values = c("darkblue", "#008080"))+
  theme(legend.position = "top")+
  labs(color = "")+ 
  theme(legend.position = "none")+
  geom_text(
    data = data.frame(
      x = as.POSIXct("2022-09-05 00:00:00"),
      y = 450,
      label = "N.S."
    ),
    aes(x = x, y = y, label = label),
    parse = FALSE, size = 4, vjust = 1.5, color = "black"
  )

# For the secondary data, later we use the `proj` function from the helper
p3 <- p3 + geom_point(aes(y = sec3$proj(smoothed_rdo_concen), color="DO"), size = 2)

# We feed the scale the secondary axis
CRCW_DO_Eh1 <- p3 + scale_y_continuous(sec.axis = sec3)
CRCW_DO_Eh1
ggplotly(CRCW_DO_Eh1)
ggsave("CRCW_DO_Eh1.png", plot = CRCW_DO_Eh1, dpi = 300)

###########################################################################
# 4 D
# PTR UP
###########

df_troll %>% 
  filter(datetime>"2022-03-01 11:00:00", 
         datetime<"2022-04-01 11:00:00" ,
         flag_out_of_water=="FALSE",
         specific_conductivity>30,
         rdo_concen>0.01,
         site=="PTR", location=="UP")->df_PTRUP

model<-lm(formula=Eh~rdo_concen, data=df_PTRUP)
summary(model)

# Run the secondary axis helper
sec4 <- help_secondary(df_PTRUP, primary = smoothed_Eh, secondary = c(0,2), name="DO (mg/L)")

# Making primary plot
p4 <- ggplot(df_PTRUP, aes(datetime)) +
  geom_point(aes(y = smoothed_Eh, color="Eh"), size = 2, alpha = 0.5) +
  labs(title = "D. PTR UP", color = "", y = "Eh (mV)", x="") +
  theme(text = element_text(size = 12)) +
  scale_color_manual(values = c("darkblue", "#008080"))+
  theme(legend.position = "top")+
  labs(color = "")+ 
  theme(legend.position = "none")+
  geom_text(
  data = data.frame(
    x = as.POSIXct("2022-03-20 00:00:00"),
    y = 430,
    label = "Eh=DO*38+353 r2=0.91***"
  ),
  aes(x = x, y = y, label = label),
  parse = FALSE, size = 4, vjust = 1.5, color = "black"
)

# For the secondary data, later we use the `proj` function from the helper
p4 <- p4 + geom_point(aes(y = sec4$proj(smoothed_rdo_concen), color="DO"), size = 2)

# We feed the scale the secondary axis
PTRUP_DO_Eh1 <- p4 + scale_y_continuous(sec.axis = sec) 
PTRUP_DO_Eh1
ggsave("PTRUP_DO_Eh1.png", plot = PTRUP_DO_Eh1, dpi = 300)

#######################################################################
# 5 #
#  PTR TR
####################################
df_troll %>% 
  filter(datetime>"2022-03-18 00:00:00", 
         datetime<"2022-04-01 11:00:00" ,
         flag_out_of_water=="FALSE",
         specific_conductivity>30,
         rdo_concen>0.01,
         site=="PTR", location=="TR")->df_PTRTR

model<-lm(formula=Eh~rdo_concen, data=df_PTRTR)
summary(model)

# Run the secondary axis helper
sec5 <- help_secondary(df_PTRTR, primary = smoothed_Eh, secondary = c(0,5), name="DO (mg/L)")

# Making primary plot
p5 <- ggplot(df_PTRTR, aes(datetime)) +
  geom_point(aes(y = smoothed_Eh, color="Eh"), size = 2, alpha = 0.5) +
  labs(title = "E. PTR TR", color = "", y = "Eh (mV)", x="") +
  theme(text = element_text(size = 12)) +
  scale_color_manual(values = c("darkblue", "#008080"))+
  theme(legend.position = "top")+
  labs(color = "")+ 
  theme(legend.position = "none")+
  geom_text(
    data = data.frame(
      x = as.POSIXct("2022-03-25 00:00:00"),
      y = 450,
      label = "Eh=DO*22+378 r2=0.26***"
    ),
    aes(x = x, y = y, label = label),
    parse = FALSE, size = 4, vjust = 1.5, color = "black"
  )

# For the secondary data, later we use the `proj` function from the helper
p5 <- p5 + geom_point(aes(y = sec5$proj(smoothed_rdo_concen), color="DO"), size = 2)

# We feed the scale the secondary axis
PTRTR_DO_Eh1 <- p5 + scale_y_continuous(sec.axis = sec5) 
PTRTR_DO_Eh1
ggsave("PTRTR_DO_Eh1.png", plot = PTRTR_DO_Eh1, dpi = 300)

####################################
# 6 #
#################################
#  OWC TR

df_troll %>% 
  filter(datetime>"2022-04-10 00:00:00", 
         datetime<"2022-05-03 11:00:00" ,
         flag_out_of_water=="FALSE",
         specific_conductivity>30,
         rdo_concen>0.01,
         site=="OWC", location=="TR")->df_OWCTR

model<-lm(formula=smoothed_Eh~smoothed_rdo_concen, data=df_OWCTR)
summary(model)

# Run the secondary axis helper
sec6 <- help_secondary(df_OWCTR, primary = smoothed_Eh, secondary = c(0,9), name="DO (mg/L)")

# Making primary plot
p6 <- ggplot(df_OWCTR, aes(datetime)) +
  geom_point(aes(y = smoothed_Eh, color="Eh"), size = 2, alpha = 0.5) +
  labs(title = "F. OWC TR", color = "", y = "Eh (mV)", x="") +
  theme(text = element_text(size = 12)) +
  scale_color_manual(values = c("darkblue", "#008080"))+
  theme(legend.position = "top")+
  labs(color = "")+ 
  theme(legend.position = "none")+
  geom_text(
    data = data.frame(
      x = as.POSIXct("2022-04-22 00:00:00"),
      y = 600,
      label = "Eh=DO*8+409 r2<1***"
    ),
    aes(x = x, y = y, label = label),
    parse = FALSE, size = 4, vjust = 1.5, color = "black"
  )

# For the secondary data, later we use the `proj` function from the helper
p6 <- p6 + geom_point(aes(y = sec6$proj(smoothed_rdo_concen), color="DO"), size = 2)

# We feed the scale the secondary axis
OWCTR_DO_Eh1 <- p6 + scale_y_continuous(sec.axis = sec6) 
OWCTR_DO_Eh1
ggsave("OWCTR_DO_Eh1.png", plot = OWCTR_DO_Eh1, dpi = 300)


########################################
#  7
#  OWC W
#########################################
df_troll %>% 
  filter(datetime>"2022-05-05 00:00:00", 
         datetime<"2022-05-07 00:00:00" ,
         flag_out_of_water=="FALSE",
         specific_conductivity>30,
         rdo_concen>0.01,
         site=="OWC", location=="W")->df_OWCW

model<-lm(formula=Eh~rdo_concen, data=df_OWCW)
summary(model)

# Run the secondary axis helper
sec7 <- help_secondary(df_OWCW, primary = smoothed_Eh, secondary = c(0,2), name="DO (mg/L)")

# Making primary plot
p7 <- ggplot(df_OWCW, aes(datetime)) +
  geom_point(aes(y = smoothed_Eh, color="Eh"), size = 2, alpha = 0.5) +
  labs(title = "H. OWC W", color = "", y = "Eh (mV)", x="") +
  theme(text = element_text(size = 12)) +
  scale_color_manual(values = c("darkblue", "#008080"))+
  theme(legend.position = "right")+
  labs(color = "")+
  geom_text(
    data = data.frame(
      x = as.POSIXct("2022-05-06 08:00:00"),
      y = 120,
      label = "Eh=DO*(-5)+95 r2=0.17*"
    ),
    aes(x = x, y = y, label = label),
    parse = FALSE, size = 4, vjust = 1.5, color = "black"
  )+ theme(legend.position = "none")+
  scale_x_datetime(limits = c(as.POSIXct("2022-05-05 23:00:00"), 
                              as.POSIXct("2022-05-06 10:00:00")),
                   date_breaks = "3 hour",
                   date_labels = "%d %b %H:%M",
                   name="")
  

# For the secondary data, later we use the `proj` function from the helper
p7 <- p7 + geom_point(aes(y = sec7$proj(smoothed_rdo_concen), color="DO"), size = 2)

# We feed the scale the secondary axis
OWCW_DO_Eh1 <- p7 + scale_y_continuous(sec.axis = sec7)
OWCW_DO_Eh1
ggsave("OWCW_DO_Eh1.png", plot = OWCW_DO_Eh1, dpi = 300)

##############################################################################
#  8
#  OWC WTE
###############################################
df_troll %>% 
  filter(datetime>"2022-03-01 11:00:00", 
         datetime<"2022-11-01 11:00:00" ,
         flag_out_of_water=="FALSE",
         specific_conductivity>30,
         rdo_concen>0.01,
         site=="OWC", location=="WTE")->df_WTE

model<-lm(formula=Eh~rdo_concen, data=df_WTE)
summary(model)

# Run the secondary axis helper
sec8 <- help_secondary(df_WTE, primary = smoothed_Eh, secondary = c(0,10), name="DO (mg/L)")

# Making primary plot
p8 <- ggplot(df_WTE, aes(datetime)) +
  geom_point(aes(y = smoothed_Eh, color="Eh"), size = 1, alpha = 0.5) +
  labs(title = "G. OWC WTE", color = "", y = "Eh (mV)", x="") +
  theme(text = element_text(size = 12)) +
  scale_color_manual(values = c("darkblue", "#008080"))+
  theme(legend.position = "top")+
  labs(color = "")+
  scale_x_datetime(
    date_breaks = "1 month",
    date_labels = "%b",
    limits = as.POSIXct(c("2022-03-15", "2022-11-15"))
  )+ theme(legend.position = "none")+
  
  geom_text(
    data = data.frame(
      x = as.POSIXct("2022-07-17 00:00:00"),
      y = 650,
      label = "Eh=DO*3.2+519 r2<1***"
    ),
    aes(x = x, y = y, label = label),
    parse = FALSE, size = 4, vjust = 1.5, color = "black"
  )

# For the secondary data, later we use the `proj` function from the helper
p8 <- p8 + geom_point(aes(y = sec8$proj(smoothed_rdo_concen), color="DO"), size = 1)

# We feed the scale the secondary axis
OWCWTE_DO_Eh1 <- p8 + scale_y_continuous(sec.axis = sec8)
OWCWTE_DO_Eh1
ggsave("OWCWTE_DO_Eh1.png", plot = OWCWTE_DO_Eh1, dpi = 300)



#########################################

# 10 legend
#########################################

df_troll %>% 
  filter(datetime>"2022-05-05 11:00:00", 
         datetime<"2022-05-09 11:00:00" ,
         flag_out_of_water=="FALSE",
         specific_conductivity>30,
         #rdo_concen>0.000001,
         site=="OWC", location=="W")->df_L

# Run the secondary axis helper
sec10 <- help_secondary(df_OWCW, primary = smoothed_Eh, secondary = c(0,2), name="DO (mg/L)")

# Making primary plot
p10 <- ggplot(df_OWCW, aes(datetime)) +
  geom_point(aes(y = smoothed_Eh, color="Eh"), size = 2, alpha = 0.5) +
  labs(title = "H. OWC W", color = "", y = "Eh (mV)", x="") +
  theme(text = element_text(size = 12)) +
  scale_color_manual(values = c("darkblue", "#008080"))+
  theme(legend.position = "bottom")+
  labs(color = "")+
  geom_text(
    data = data.frame(
      x = as.POSIXct("2022-05-08 00:00:00"),
      y = 100,
      label = "Eh=DO*76-101 r2=0.54***"
    ),
    aes(x = x, y = y, label = label),
    parse = FALSE, size = 4, vjust = 1.5, color = "black"
  )

# For the secondary data, later we use the `proj` function from the helper
p10 <- p10 + geom_point(aes(y = sec10$proj(smoothed_rdo_concen), color="DO"), size = 2)

# We feed the scale the secondary axis
legend_DO_EH <- p10 + scale_y_continuous(sec.axis = sec7)
legend_DO_EH


#########################################
# Extract the legend from the ggplot
legend <- get_legend(legend_DO_EH)
# Create a grid of plots
grid <- plot_grid(
  CRCUP_DO_Eh1, CRCTR_DO_Eh1, CRCW_DO_Eh1,
  PTRUP_DO_Eh1, PTRTR_DO_Eh1,
  OWCTR_DO_Eh1, OWCWTE_DO_Eh1, OWCW_DO_Eh1,
  ncol = 2
)
# Arrange the grid and legend vertically
plot_grid(grid, legend, ncol = 1, rel_heights = c(9, 1))->plotfinal

ggsave("Figure6_DO_Eh1.png", plot = plotfinal, dpi = 300)

##################################

# 4 figures arranged in 2 rows and 2 columns
attach(mtcars)
par(mfrow=c(2,4))
CRCUP_DO_Eh1
CRCTR_DO_Eh1
CRCW_DO_Eh1
PTRUP_DO_Eh1
PTRTR_DO_Eh1
OWCTR_DO_Eh1
OWCWTE_DO_Eh1
OWCW_DO_Eh1

a<-layout(matrix(c(1,2,3,3)))

library(gridExtra)

# Create a list of grobs from the variable names
plots <- list(CRCUP_DO_Eh1,
              CRCTR_DO_Eh1,
              CRCW_DO_Eh1,
              PTRUP_DO_Eh1,
              PTRTR_DO_Eh1,
              OWCTR_DO_Eh1,
              OWCWTE_DO_Eh1,
              OWCW_DO_Eh1)

# Save the combined plots using ggsave
ggsave("Plots_Combined%03d.png", width = 8.5, height = 11, 
       arrangeGrob(grobs = plots, nrow = 4, ncol = 2, top = NULL))

############################################################################################################################################

library(gridExtra)
install.packages("png")
library(png)

# Specify the paths to your PNG files
file_paths <- c("C:/Users/fsilva/OneDrive - University of Toledo/Documents/Fausto UT/COMPASS/Manuscript 1 Redox/Figures/Codes/CRCUP_DO_Eh1.png", 
                "C:/Users/fsilva/OneDrive - University of Toledo/Documents/Fausto UT/COMPASS/Manuscript 1 Redox/Figures/Codes/CRCTR_DO_Eh1.png", 
                "C:/Users/fsilva/OneDrive - University of Toledo/Documents/Fausto UT/COMPASS/Manuscript 1 Redox/Figures/Codes/CRCW_DO_Eh1.png", 
                "C:/Users/fsilva/OneDrive - University of Toledo/Documents/Fausto UT/COMPASS/Manuscript 1 Redox/Figures/Codes/PTRUP_DO_Eh1.png",
                "C:/Users/fsilva/OneDrive - University of Toledo/Documents/Fausto UT/COMPASS/Manuscript 1 Redox/Figures/Codes/PTRTR_DO_Eh1.png", 
                "C:/Users/fsilva/OneDrive - University of Toledo/Documents/Fausto UT/COMPASS/Manuscript 1 Redox/Figures/Codes/OWCTR_DO_Eh1.png", 
                "C:/Users/fsilva/OneDrive - University of Toledo/Documents/Fausto UT/COMPASS/Manuscript 1 Redox/Figures/Codes/OWCW_DO_Eh1.png", 
                "C:/Users/fsilva/OneDrive - University of Toledo/Documents/Fausto UT/COMPASS/Manuscript 1 Redox/Figures/Codes/OWCWTE_DO_Eh1.png")

# Read the PNG files as grobs
grobs <- lapply(file_paths, readPNG)

# Arrange the grobs into a 2x4 grid
grid <- arrangeGrob(grobs = grobs, ncol = 2, nrow = 4)

# Save the combined image as a PNG file
ggsave("combined_image.png", grid, width = 8, height = 6, dpi = 300)

# Read the PNG files as raster grobs
grobs <- lapply(file_paths, function(path) rasterGrob(readPNG(path)))


plots <- lapply(ll <- list.files(file_paths),function(x){
  img <- as.raster(readPNG(x))
  rasterGrob(img, interpolate = FALSE)
})
ggsave("Plots_Combined%03d.png",width=8.5, height=11, 
       marrangeGrob(grobs = plots, nrow=2, ncol=1,top=NULL))

ggdraw()+draw_image(file_paths)
