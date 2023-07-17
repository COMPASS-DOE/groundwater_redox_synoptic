
install.packages("devtools") 
devtools::install_github("kaizadp/soilpalettes")

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
       cowplot)

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
                                    (site=="CRC" & datetime>"2022-07-19 00:00:00" & datetime<"2022-07-28 00:00:00")| 
                                    (site=="OWC" & datetime>"2022-07-21 08:00:00" & datetime<"2022-07-31 08:00:00")| 
                                    (site=="PTR" & location =="UP" & datetime>"2022-08-02 08:00:00" & datetime<"2022-08-12 08:00:00")|
                                    (site=="PTR" & location !="UP" & datetime>"2022-07-19 08:00:00" & datetime<"2022-07-29 08:00:00")
                                     ),"TRUE","FALSE"
                                    )
         )

df_troll$month <- month(df_troll$datetime)

df_troll$month2 <- factor(df_troll$month, levels = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12), 
                          labels = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"))
                          

str(df_troll)

##########################################################################################

# (1) eh vs time 

df_troll %>% 
  filter(datetime>"2022-09-01 00:00:00", 
         datetime<"2022-09-30 00:00:00" ,
         flag_out_of_water=="FALSE",
         site=="PTR",
         #location=="UP"
         ) %>%
  ggplot(aes(x = datetime, y = p_h_orp, color=location))+
  geom_point(size = 2, alpha=1)+
  labs(title="PTR", y="Eh (mV)", x="Time")+
  theme(text = element_text(size = 18))+
  guides(colour = guide_legend(override.aes = list(alpha = 1)))->p1

df_troll %>% 
  filter(datetime>"2022-09-01 00:00:00", 
         datetime<"2022-09-30 00:00:00" ,
         flag_out_of_water=="FALSE",
         site=="PTR",
         location=="UP") %>%
  ggplot(aes(x = datetime, y = p_h, 
             #color=location
  ))+
  geom_point(size = 3, alpha=0.1, color="grey20")+
  labs(title="PTR", y="Eh (mV)", x="Time")+
  #scale_color_manual(values=c("burlywood3",
  #                           "brown4", 
  #                          "blue4", 
  #                          ))+
  theme(text = element_text(size = 18))+
  guides(colour = guide_legend(override.aes = list(alpha = 1)))->p2

# (2) eh vs time 

# PTR UP

df_troll %>% 
  filter(datetime>"2022-09-01 00:00:00", 
         datetime<"2022-09-30 00:00:00" ,
         flag_out_of_water=="FALSE",
         site=="PTR", location!="W") %>%
  ggplot(aes(x = datetime, y = p_h_orp, color=location))+
  geom_point(size = 3, alpha=0.1)+
  labs(title="PTR", y="Eh (mV)", x="Time")+
  scale_color_manual(values=c("forestgreen","maroon"))+
  theme(text = element_text(size = 18))+
  guides(colour = guide_legend(override.aes = list(alpha = 1)))+ 
  theme(legend.position = "none")->p1


##########################################################################################

# (3) GWL vs time 3 zones

df_troll %>% 
  filter(datetime>"2022-09-01 00:00:00", 
         datetime<"2022-09-30 00:00:00" ,
         flag_out_of_water=="FALSE",
         site=="PTR",location!="W"
         ) %>%
  ggplot(aes(x = datetime, y = wl_below_surface_m, color=location))+
  geom_point(size = 3, alpha=0.05)+
  labs(title="PTR", y="Groundwater level below surface (m)", x="Time")+
  scale_color_manual(values=c("forestgreen",
                              "maroon" 
                              #"purple1", 
                              #"cornflowerblue"
                              ))+
  theme(text = element_text(size = 18))+
  guides(colour = guide_legend(override.aes = list(alpha = 1)))+ 
  theme(legend.position = "none")->pz

df_troll %>% 
  filter(datetime>"2022-09-01 00:00:00", 
         datetime<"2022-09-30 00:00:00" ,
         flag_out_of_water=="FALSE",
         site=="PTR",location!="W"
  ) %>%
  ggplot(aes(x = datetime, y = specific_conductivity, color=location))+
  geom_point(size = 3, alpha=0.05)+
  labs(title="PTR", y="SPC (uS/cm)", x="Time")+
  scale_color_manual(values=c("forestgreen",
                                           "maroon" 
                                           #"purple1", 
                                           #"cornflowerblue"
  ))+
  theme(text = element_text(size = 18))+
  guides(colour = guide_legend(override.aes = list(alpha = 1)))+ 
  theme(legend.position = "none")->pr

df_troll %>% 
  filter(datetime>"2022-09-01 00:00:00", 
         datetime<"2022-09-30 00:00:00" ,
         flag_out_of_water=="FALSE",
         site=="PTR",location!="W"
  ) %>%
  ggplot(aes(x = datetime, y = p_h, color=location))+
  geom_point(size = 3, alpha=0.05)+
  labs(title="PTR", y="pH", x="Time")+
  scale_color_manual(values=c("forestgreen",
                                           "maroon" 
                                           #"purple1", 
                                           #"cornflowerblue"
  ))+
  theme(text = element_text(size = 18))+
  guides(colour = guide_legend(override.aes = list(alpha = 1)))+ 
  theme(legend.position = "none")->pt

##########################################################################################

# (4) Calculating detrend wl_bellow surface for september

# df

df_troll %>% 
  filter(datetime>"2022-09-01 00:00:00", 
         datetime<"2022-09-30 00:00:00" ,
         flag_out_of_water=="FALSE",
         site=="PTR",
         location=="UP")->df_PTR_UP_sep

df<-df_PTR_UP_sep

y<-df$wl_below_surface_m
x<-df$datetime

# Fit the linear regression model
model <- lm(y ~ as.POSIXct(x))

# Extract the fitted values from the model
fit <- fitted(model)

# Detrend the data by subtracting the fitted values
detrended_data <- y - fit

# Plot the detrended data
plot(x, detrended_data)

# Plot undetrended data
plot(x, y)

df_troll$detrended_data<-df_troll$detrended_data


# ggplot detrended
df %>% 
  filter(datetime>"2022-09-01 00:00:00", 
         datetime<"2022-09-30 00:00:00" ,
         flag_out_of_water=="FALSE",
         site=="PTR",location=="TR") %>%
  ggplot(aes(x = datetime, y = y))+
  geom_point(size = 3, alpha=0.05, color="blue")+
  labs(title="", y="GWL (detrended)", x="Time")+
  theme(text = element_text(size = 18))+
  guides(colour = guide_legend(override.aes = list(alpha = 1)))->p32

########################################################################################

#detrend wl_bellow surface for september
df_troll %>% 
  filter(datetime>"2022-09-01 00:00:00", 
         datetime<"2022-09-30 00:00:00" ,
         flag_out_of_water=="FALSE",
         site=="PTR",
         location=="TR")->df_PTR_TR_sep

df2<-df_PTR_TR_sep

y2<-df2$wl_below_surface_m
x2<-df2$datetime

# Fit the linear regression model
model2 <- lm(y2 ~ as.POSIXct(x2))

# Extract the fitted values from the model
fit2 <- fitted(model2)

# Detrend the data by subtracting the fitted values
detrended_data2 <- y2 - fit2

# Plot the detrended data
plot(x2, detrended_data2)

# Plot undetrended data
plot(x2, y2)

df2 %>% 
  filter(datetime>"2022-09-01 00:00:00", 
         datetime<"2022-09-30 00:00:00" ,
         flag_out_of_water=="FALSE",
         site=="PTR",location=="TR") %>%
  ggplot(aes(x = datetime, y = detrended_data2))+
  geom_point(size = 3, alpha=0.05, color="maroon")+
  labs(title="", y="GWL (detrended)", x="Time")+
  theme(text = element_text(size = 18))+
  guides(colour = guide_legend(override.aes = list(alpha = 1)))->p4


########################################################################################

df_clima<-read.csv("df_clima.csv")
str(df_clima)

#x <- as.POSIXct(df_clima$datetime, format="%Y-%m-%dT%H:%M:%OSZ", tz="EST")
#df_clima$datetime <- as.POSIXct(gsub("EST", "", x))

df_clima$site<-as.factor(df_clima$site)
df_clima$datetime<-as.POSIXct(df_clima$datetime)

df_clima %>% 
  filter(datetime>"2022-09-01 00:00:00", 
         datetime<"2022-09-30 00:00:00" ,
         site=="PTR") %>%
  ggplot(aes(x = datetime, y = rain_mm_tot))+
  geom_point(size = 3, alpha=0.8, color="blue")+
  labs(title="", y="Rain (mm)", x="Time")+
  theme(text = element_text(size = 18))+
  guides(colour = guide_legend(override.aes = list(alpha = 1)))->p5

pp <- list(p1, pr, pt)
plot_grid(plotlist=pp, nrow = 3, ncol=1, align='hv',
          rel_widths= c(1,1,1,1),rel_heights = c(1.2,1.2,1.2))

