# Analysis Redox (Eh)

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
       nortest,
       coin,
       PMCMR,
       multcomp,
       lme4,
       gridExtra,
       patchwork,
       cowplot,
       ggforce)

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

# (0) To transform a dataframe with 24 variables measured each 15 minutes to a 
# new dataframe with all data per hour in R, you can use the dplyr and tidyr packages.

# Create a new column with the hourly interval using floor_date from the lubridate package.
# This code creates a new column hourly_interval that rounds the datetime column to the nearest hour.
df_troll$day_interval <- floor_date(df_troll$datetime, unit = "day")

# Use group_by and summarise functions from the dplyr package 
# to aggregate the data by site, location, month2, and hourly_interval, 
# and compute the mean for each variable. The code would be:

## This code groups the data by site, location, month2, and hourly_interval, 
## and then calculates the mean for each variable. 
## The argument across(c(temperature:wl_below_surface_m), mean, na.rm = TRUE) 
## applies the mean function to columns temperature through wl_below_surface_m.

df_troll_day <- df_troll %>%
  group_by(site, location, day_interval, flag_out_of_water) %>%
  summarise(across(c(temperature:wl_below_surface_m), mean, na.rm = TRUE))

# check structure
head(df_troll_day)
str(df_troll_day)

# Change column name from "hourly_interval" to "datetime"
names(df_troll_day)[names(df_troll_day) == "day_interval"] <- "datetime"

#check timezone
tz <- attr(df_troll_day$datetime, "tzone")
print(tz)

write.csv(df_troll_day, "df_troll_day.csv", row.names = FALSE)

##########################################################################################

# bring df trolls per hour
df_troll_day<-data.frame(fread("df_troll_day.csv"))

str(df_troll_day)


#Check timezone
tz <- attr(df_troll_day$datetime, "tzone")
print(tz)

# Datetime column is in "America/New_York" but not setted up
#then, you need to specify "datetime UTC" is "America/New_York"in your dataframe
#df_troll2$datetime <- as.POSIXct(format(df_troll2$datetime, tz = "UTC"), tz = "America/New_York")



#####################################

# (1) graph Eh by factors Site and location

df_troll_day %>% 
  filter(datetime>"2022-03-21 11:00:00", 
         datetime<"2022-11-01 11:00:00" ,
         flag_out_of_water=="FALSE",
         specific_conductivity>30,
         site!="GWI", site!="MSM")%>%
  ggplot(aes(x = factor(site), 
             y = p_h_orp,
             fill=location,
             color=location
             )
         )+
  geom_violin()+
  scale_fill_manual(values=c("#EFC00099", 
                             "#3B3B3B99", 
                             "#56B4E9",
                             "#0072B2"))+
  scale_x_discrete(breaks = c("CRC", "PTR", "OWC"),
                   labels = c("CRC","PTR","OWC"))+
  labs(y="Eh (mV)", x="")+
  ggtitle("A") +
  guides(fill = guide_legend(title="TAI Zone", position="top"))+
  theme(text = element_text(size = 16),
        legend.position = "none",
        legend.margin = margin(t = -0.1, b = 0, unit = "cm"))->plot1

##################################################################################

# (2) GWL box plot site*location

df_troll %>% 
  filter(datetime>"2022-03-21 11:00:00", 
         datetime<"2022-11-01 11:00:00" ,
         flag_out_of_water=="FALSE",
         specific_conductivity>30,
         site!="GWI", site!="MSM") %>%
  ggplot(aes(x = factor(site), 
             y = wl_below_surface_m,
             fill=location
  )
  )+
  geom_violin()+
  scale_fill_manual(values=c("#EFC00099", 
                             "#3B3B3B99", 
                             "#56B4E9",
                             "#0072B2"))+
  scale_x_discrete(breaks = c("CRC", "PTR", "OWC"),
                   labels = c("CRC","PTR","OWC"))+
  labs(y="Groundwaterlevel\nbelow surface (m)", x="")+
  ggtitle("B") +
  guides(fill = guide_legend(title="TAI Zone", position="top"))+
  theme(text = element_text(size = 16),
        legend.position = "none",
        legend.margin = margin(t = -0.1, b = 0, unit = "cm"))->plot2

##################################################################################

# (3) pH box plot site*location

df_troll %>% 
  filter(datetime>"2022-03-21 11:00:00", 
         datetime<"2022-11-01 11:00:00" ,
         flag_out_of_water=="FALSE",
         specific_conductivity>30,
         site!="GWI", site!="MSM") %>%
  ggplot(aes(x = factor(site), 
             y = p_h,
             fill=location
  )
  )+
  geom_violin()+
  scale_fill_manual(values=c("#EFC00099", 
                             "#3B3B3B99", 
                             "#56B4E9",
                             "#0072B2"))+
  scale_x_discrete(breaks = c("CRC", "PTR", "OWC"),
                   labels = c("CRC","PTR","OWC"))+
  ylim(5, 9) +
  labs(y="pH", x="")+
  ggtitle("C") +
  guides(fill = guide_legend(title="TAI Zone", position="top"))+
  theme(text = element_text(size = 16),
        legend.position = "none",
        legend.margin = margin(t = -0.1, b = 0, unit = "cm"))->plot3


# (4) O2 box plot site*location

df_troll %>% 
  filter(datetime>"2022-03-21 11:00:00", 
         datetime<"2022-11-01 11:00:00" ,
         flag_out_of_water=="FALSE",
         specific_conductivity>30,
         site!="GWI", site!="MSM") %>%
  ggplot(aes(x = factor(site), 
             y = rdo_concen,
             fill=location
  )
  )+
  geom_violin()+
  scale_fill_manual(values=c("#EFC00099", 
                                        "#3B3B3B99", 
                                        "#56B4E9",
                                        "#0072B2"))+
  scale_x_discrete(breaks = c("CRC", "PTR", "OWC"),
                   labels = c("CRC","PTR","OWC"))+
  #ylim(5, 9) +
  labs(y="DO (mg/L)", x="")+
  ggtitle("D") +
  guides(fill = guide_legend(title="TAI Zone", position="top"))+
  theme(text = element_text(size = 16),
        legend.position = "none",
        legend.margin = margin(t = -0.1, b = 0, unit = "cm"))->plot4
  #facet_zoom(ylim = c(0, 0.1))


# (5) SPC box plot site*location

df_troll %>% 
  filter(datetime>"2022-03-21 11:00:00", 
         datetime<"2022-11-01 11:00:00" ,
         flag_out_of_water=="FALSE",
         specific_conductivity>30,
         site!="GWI", site!="MSM") %>%
  ggplot(aes(x = factor(site), 
             y = specific_conductivity,
             fill=location
  )
  )+
  geom_violin()+
  scale_fill_manual(values=c("#EFC00099", 
                             "#3B3B3B99", 
                             "#56B4E9",
                             "#0072B2"))+
  scale_x_discrete(breaks = c("CRC", "PTR", "OWC"),
                   labels = c("CRC","PTR","OWC"))+
  #ylim(5, 9) +
  labs(y="SPC (uS/cm)", x="")+
  ggtitle("E") +
  guides(fill = guide_legend(title="TAI Zone", position="top"))+
  theme(text = element_text(size = 16),
        legend.position = "none",
        legend.margin = margin(t = -0.1, b = 0, unit = "cm"))->plot5



# (6) SPC box plot site*location

df_troll %>% 
  filter(datetime>"2022-03-21 11:00:00", 
         datetime<"2022-11-01 11:00:00" ,
         flag_out_of_water=="FALSE",
         specific_conductivity>30,
         site!="GWI", site!="MSM") %>%
  ggplot(aes(x = factor(site), 
             y = resistivity,
             fill=location
  )
  )+
  geom_violin()+
  scale_fill_manual(values=c("#EFC00099", 
                             "#3B3B3B99", 
                             "#56B4E9",
                             "#0072B2"))+
   scale_x_discrete(breaks = c("CRC", "PTR", "OWC"),
                    labels = c("CRC","PTR","OWC"))+
  #ylim(5, 9) +
  labs(y="R (ohm*cm)", x="")+
  ggtitle("F") +
  guides(fill = guide_legend(title="TAI Zone", position="top"))+
  theme(text = element_text(size = 16),
        legend.position = "none",
        legend.margin = margin(t = -0.1, b = 0, unit = "cm"))->plot6



# combine the plots with a single legend
#grid.arrange(plot1, plot2, ncol = 1)


pplot1 <- plot1+theme(plot.margin = unit(c(0.1, 1, 0.1, 0.5), "cm"))
pplot2 <- plot2+theme(plot.margin = unit(c(0.1, 1, 0.1, 0.5), "cm"))
pplot3 <- plot3+theme(plot.margin = unit(c(0.1, 1, 0.1, 0.5), "cm"))
pplot4 <- plot4+theme(plot.margin = unit(c(0.1, 1, 0.1, 0.5), "cm"))
pplot5 <- plot5+theme(plot.margin = unit(c(0.1, 1, 0.1, 0.5), "cm"))
pplot6 <- plot6+theme(plot.margin = unit(c(0.1, 1, 0.1, 0.5), "cm"))

pp <- list(pplot1, pplot2, pplot3, pplot4, pplot5, pplot6)
#plot_grid(plotlist = pp, nrow = 3, ncol = 2, align = 'hv', axis = "tb", legend = ggplot2::"top")


# create a legend
legend <- get_legend(plot1 + theme(legend.position = "bottom"))

# decrease the space of the legend
legend$heights[2] <- unit(0.1, "cm")

# add the legend to the bottom of the grid
grid.arrange(grobs = list(plot_grid(plotlist = pp, nrow = 3, ncol = 2, align = 'hv', axis = "tb"), 
                          arrangeGrob(legend)), ncol = 1, heights = c(4, 0.5))


#####################################################################

#  Statistics

df_troll %>% 
  filter(datetime>"2022-03-21 11:00:00", 
         datetime<"2022-11-01 11:00:00" ,
         flag_out_of_water=="FALSE",
         specific_conductivity>30,
         site!="GWI", site!="MSM", #site!="OWC",
         #location=="W"
  )->df

######################################


#######################################################

  # (1) Statistcs Eh
str(df)
    # Normality test:
    # Eh factors Site and location
    # Anderson-Darling test (n>5000)
    df_troll %>%
    group_by(site, location) %>% 
    summarize(p_value = ad.test(p_h_orp)$p.value)
  
  #################
  
  # Create a Q-Q plot with facets
  ggplot(df, aes(sample = p_h_orp)) +
    geom_qq() +
    geom_qq_line() +
    facet_grid(site ~ location) +
    labs(x = "Theoretical Quantiles", y = "Sample Quantiles") +
    ggtitle("Q-Q Plot by Site and Location")
  
  # Create a density plot
  ggplot(data, aes(x = response)) +
    geom_density() +
    labs(x = "Response Variable", y = "Density") +
    ggtitle("Density Plot")
  
  
  library(mgcv)
  # Define the GAMM formula
  # Fit GAMM
  gamm_model <- gamm(p_h_orp ~ site + location, data = df, random = list(subject = ~1), method = "REML")
  
  # View summary of model
  summary(gamm_model)
  
  # Test significance of fixed effects
  summary(gamm_model$gam)$s.table
  
  #`summarise()` has grouped output by 'site'. You can override using the `.groups` argument.
  # A tibble: 15 × 3
  # Groups:   site [5]
  #site  location p_value
  #<fct> <fct>      <dbl>
  #1  CRC   UP       3.7e-24
  #2  CRC   TR       3.7e-24
  #3  CRC   W        3.7e-24
  #4  PTR   UP       3.7e-24
  #5  PTR   TR       3.7e-24
  #6  PTR   W        3.7e-24
  #7  OWC   TR       3.7e-24
  #8  OWC   WTE      3.7e-24
  #9  OWC   W        3.7e-24
  #10 MSM   UP       3.7e-24
  #11 MSM   TR       3.7e-24
  #12 MSM   W        3.7e-24
  #13 GWI   UP       3.7e-24
  #14 GWI   TR       3.7e-24
  #15 GWI   W        3.7e-24
  
  df_troll <- df_troll %>% 
    group_by(site, location) %>%
    mutate(subject = as.factor(rep(1:n(), each = 1)))
  
  model <- lmer(p_h_orp ~ datetime + location + site + (1|subject), data = df_troll)
  summary(model)
  
  library(multcomp)
  model_glht <- glht(model, linfct = mcp(site = "Tukey", location = "Tukey"))
  summary(model_glht)
  
  df_troll %>% 
    group_by(site, location) %>% 
    summarize(mean = mean(p_h_orp), 
              median = median(p_h_orp), 
              Q1 = quantile(p_h_orp, 0.25),
              Q3 = quantile(p_h_orp, 0.75),
              max = max(p_h_orp), 
              min = min(p_h_orp))
  
  
  
  
  # Eh factor month grid site~location
  df_troll %>% 
    filter(datetime>"2022-03-21 11:00:00", 
           datetime<"2022-11-01 11:00:00" ,
           flag_out_of_water=="FALSE",
           specific_conductivity>30,
           site!="GWI", site!="MSM", #site!="OWC",
           #location=="W"
    ) %>%
    ggplot(aes(x = factor(month2), 
               y = p_h_orp))+
    geom_boxplot()+
    facet_grid(site~location)+
    labs(x="Months", y="Eh (mV)")+
    #ggtitle("Groundwater Redox Potential") +
    scale_y_continuous(limits = c(-500, 500))+
    theme(text = element_text(size = 17),
          axis.text.x = element_text(angle = 45, hjust = 1))
  
  
  df_troll %>% 
    group_by(site, location, month) %>% 
    summarize(mean = mean(p_h_orp), 
              median = median(p_h_orp), 
              Q1 = quantile(p_h_orp, 0.25),
              Q3 = quantile(p_h_orp, 0.75),
              max = max(p_h_orp), 
              min = min(p_h_orp))
  

############################################################################
  
  # (2) statistics GWL
  
# Normality test:
# Eh factors Site and location
# Anderson-Darling test (n>5000)
str(df_troll)

df_troll %>%
  group_by(site, location) %>% 
  summarize(p_value = ad.test(wl_below_surface_m)$p.value)

#`summarise()` has grouped output by 'site'. You can override using the `.groups` argument.
# A tibble: 15 × 3
# Groups:   site [5]
#site  location p_value
#<fct> <fct>      <dbl>
#1  CRC   UP       3.7e-24
#2  CRC   TR       3.7e-24
#3  CRC   W        3.7e-24
#4  PTR   UP       3.7e-24
#5  PTR   TR       3.7e-24
#6  PTR   W        3.7e-24
#7  OWC   TR       3.7e-24
#8  OWC   WTE      3.7e-24
#9  OWC   W        3.7e-24
#10 MSM   UP       3.7e-24
#11 MSM   TR       3.7e-24
#12 MSM   W        3.7e-24
#13 GWI   UP       3.7e-24
#14 GWI   TR       3.7e-24
#15 GWI   W        3.7e-24

df_troll <- df_troll %>% 
  group_by(site, location) %>%
  mutate(subject = as.factor(rep(1:n(), each = 1)))

model <- lmer(wl_below_surface_m ~ datetime + location + site + (1|subject), data = df_troll)
summary(model)

library(multcomp)
model_glht <- glht(model, linfct = mcp(site = "Tukey", location = "Tukey"))
summary(model_glht)

df_troll %>% 
  group_by(site, location) %>% 
  summarize(mean = mean(wl_below_surface_m), 
            median = median(wl_below_surface_m), 
            Q1 = quantile(wl_below_surface_m, 0.25),
            Q3 = quantile(wl_below_surface_m, 0.75),
            max = max(wl_below_surface_m), 
            min = min(wl_below_surface_m))



################################################

############################################################################

# (3) statistics pH

# Normality test:
# Eh factors Site and location
# Anderson-Darling test (n>5000)
str(df_troll)

df_troll %>%
  group_by(site, location) %>% 
  summarize(p_value = ad.test(p_h)$p.value)

#`summarise()` has grouped output by 'site'. You can override using the `.groups` argument.
# A tibble: 15 × 3
# Groups:   site [5]
#site  location p_value
#<fct> <fct>      <dbl>
#1  CRC   UP       3.7e-24
#2  CRC   TR       3.7e-24
#3  CRC   W        3.7e-24
#4  PTR   UP       3.7e-24
#5  PTR   TR       3.7e-24
#6  PTR   W        3.7e-24
#7  OWC   TR       3.7e-24
#8  OWC   WTE      3.7e-24
#9  OWC   W        3.7e-24
#10 MSM   UP       3.7e-24
#11 MSM   TR       3.7e-24
#12 MSM   W        3.7e-24
#13 GWI   UP       3.7e-24
#14 GWI   TR       3.7e-24
#15 GWI   W        3.7e-24

df_troll <- df_troll %>% 
  group_by(site, location) %>%
  mutate(subject = as.factor(rep(1:n(), each = 1)))

model <- lmer(p_h ~ datetime + location + site + (1|subject), data = df_troll)
summary(model)

library(multcomp)
model_glht <- glht(model, linfct = mcp(site = "Tukey", location = "Tukey"))
summary(model_glht)

df_troll %>% 
  group_by(site, location) %>% 
  summarize(mean = mean(wl_below_surface_m), 
            median = median(wl_below_surface_m), 
            Q1 = quantile(wl_below_surface_m, 0.25),
            Q3 = quantile(wl_below_surface_m, 0.75),
            max = max(wl_below_surface_m), 
            min = min(wl_below_surface_m))


#################################################






# Eh factor month grid site~location
df_troll %>% 
  filter(datetime>"2022-03-21 11:00:00", 
         datetime<"2022-11-01 11:00:00" ,
         flag_out_of_water=="FALSE",
         specific_conductivity>30,
         site!="GWI", site!="MSM", #site!="OWC",
         #location=="W"
  ) %>%
  ggplot(aes(x = factor(month2), 
             y = p_h_orp))+
  geom_boxplot()+
  facet_grid(site~location)+
  labs(x="Months", y="Eh (mV)")+
  #ggtitle("Groundwater Redox Potential") +
  scale_y_continuous(limits = c(-500, 500))+
  theme(text = element_text(size = 17),
        axis.text.x = element_text(angle = 45, hjust = 1))


df_troll %>% 
  group_by(site, location, month) %>% 
  summarize(mean = mean(p_h_orp), 
            median = median(p_h_orp), 
            Q1 = quantile(p_h_orp, 0.25),
            Q3 = quantile(p_h_orp, 0.75),
            max = max(p_h_orp), 
            min = min(p_h_orp))
























# GWL factor month grid site~location
df_troll %>% 
  filter(datetime>"2022-03-21 11:00:00", 
         datetime<"2022-11-01 11:00:00" ,
         flag_out_of_water=="FALSE",
         specific_conductivity>30,
         site!="GWI", site!="MSM", #site!="OWC",
         #location=="W"
  ) %>%
  ggplot(aes(x = factor(month2), 
             y = wl_below_surface_m))+
  geom_boxplot(fill="cyan")+
  facet_grid(site~location, scale="free")+
  labs(x="Months", y="Groundwater level below the surface (m)")+
  ggtitle("Groundwater Monitoring Lake Erie TAI - Groundwater Level") +
  theme(text = element_text(size = 16),
        axis.text.x = element_text(angle = 45, hjust = 1))
