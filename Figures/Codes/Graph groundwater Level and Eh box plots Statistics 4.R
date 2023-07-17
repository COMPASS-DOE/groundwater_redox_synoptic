# Analysis Redox (Eh)

install.packages("BiocManager") 
install.packages("mgcv-package")
library(BiocManager)

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
       #mgcv-package,
       nortest,
       coin,
       PMCMR,
       multcomp,
       lme4,
       gridExtra,
       patchwork,
       cowplot,
       ggforce,
       rstatix,
       nortest, 
       multcomp,
       lme4,
       nlme,
       lsmeans,
       nlme,
       ggpubr,
       rstatix)

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
                          


df_troll <- df_troll %>%
  mutate(Eh = p_h_orp - (0.718 * temperature) + 224.41)


str(df_troll)

#####################################

# (1) basic graph Eh by factors Site and location

df_troll %>% 
  filter(datetime>"2022-03-21 11:00:00", 
         datetime<"2022-11-01 11:00:00" ,
         flag_out_of_water=="FALSE",
         specific_conductivity>30,
         site!="GWI", site!="MSM")%>%
  ggplot(aes(x = factor(site), 
             y = Eh,
             fill=location
             )
         )+
  geom_boxplot()+
  scale_fill_manual(values=c("#2C8774",
                             "#EFC00099",
                             "#B2BEB5",
                             "#56B4E9"))+
  scale_x_discrete(breaks = c("CRC", "PTR", "OWC"),
                   labels = c("CRC","PTR","OWC"))+
  labs(y="Eh (mV)", x="")+
  ggtitle("b") +
  guides(fill = guide_legend(title="TAI Zone", position="top"))+
  theme(text = element_text(size = 18),
        legend.position = "none",
        legend.margin = margin(t = -0.1, b = 0, unit = "cm"))+
  stat_summary(fun.data = mean_cl_normal, geom = "pointrange",
               size = 0.5, shape = 3, stroke = 2,
               col = "black", 
               position = position_dodge(width = 0.75))->plot1


#####################################################################

#  Statistics

# (1) Desciptive statistics Eh, wl_below...

df_troll %>% 
  filter(datetime>"2022-03-21 11:00:00", 
         datetime<"2022-11-01 11:00:00" ,
         flag_out_of_water=="FALSE",
         specific_conductivity>30,
         site!="GWI", site!="MSM", #site!="OWC",
         #location=="W"
  )%>%
  group_by(site, location) %>% 
  summarize(mean = mean(Eh), 
          #  min = min(p_h), 
          #  man = max(p_h),
            Q1 = quantile(Eh, 0.001),    #removes maximum values that are wrong
            Q99 = quantile(Eh, 0.999))
  
# 1.1 Eh  

"`summarise()` has grouped output by 'site'. You can override using the `.groups` argument.
# A tibble: 9 × 5
# Groups:   site [3]
  site  location    mean    Q1   Q99
  <fct> <fct>      <dbl> <dbl> <dbl>
1 CRC   UP        -23.2  -133. 380. 
2 CRC   TR        -35.8  -158. 427. 
3 CRC   W        -128.   -204. 518. 
4 PTR   UP        214.    148. 351. 
5 PTR   TR        128.   -120. 430. 
6 PTR   W        -167.   -243.  80.2
7 OWC   TR          9.33 -202. 479. 
8 OWC   WTE       152.   -204. 659. 
9 OWC   W        -177.   -241.  92.1"  
  
# 1.2. Wl_below_surface:
  
"`summarise()` has grouped output by 'site'. You can override using the `.groups` argument.
# A tibble: 9 × 5
# Groups:   site [3]
  site  location    mean     Q1     Q99
  <fct> <fct>      <dbl>  <dbl>   <dbl>
1 CRC   UP       -2.11   -4.34  -0.0439
2 CRC   TR       -0.472  -1.31   0.200 
3 CRC   W        -0.0354 -0.355  0.368 
4 PTR   UP       -2.38   -3.86  -0.0957
5 PTR   TR       -1.10   -2.04  -0.0580
6 PTR   W        -0.329  -0.937  0.160 
7 OWC   TR       -0.0731 -0.493  0.154 
8 OWC   WTE      -0.276  -0.570  0.243 
9 OWC   W         0.0444 -0.276  0.670 "



############################    Test Site and zones: Friedman  #####################################

# 2. Normality test:
# Eh factors Site and location
# Anderson-Darling test (n>5000)

str(df_troll)

df_troll2 %>%
   group_by(site, location) %>%
   summarize(ad_stat = ad.test(Eh)$statistic, p_value = ad.test(Eh)$p.value)

# 2.1. Eh

"`summarise()` has grouped output by 'site'. You can override using the `.groups` argument.
# A tibble: 9 × 4
# Groups:   site [3]
  site  location ad_stat p_value
  <fct> <fct>      <dbl>   <dbl>
1 CRC   UP         2767. 3.7e-24
2 CRC   TR         1414. 3.7e-24
3 CRC   W          2271. 3.7e-24
4 PTR   UP          491. 3.7e-24
5 PTR   TR          409. 3.7e-24
6 PTR   W           559. 3.7e-24
7 OWC   TR          543. 3.7e-24
8 OWC   WTE         614. 3.7e-24
9 OWC   W          1591. 3.7e-24"


# 2.2. Wl_below

df_troll2 %>%
     group_by(site, location) %>%
     summarize(ad_stat = ad.test(wl_below_surface_m)$statistic, p_value = ad.test(wl_below_surface_m)$p.value)

"`summarise()` has grouped output by 'site'. You can override using the `.groups` argument.
# A tibble: 9 × 4
# Groups:   site [3]
  site  location ad_stat p_value
  <fct> <fct>      <dbl>   <dbl>
1 CRC   UP          582. 3.7e-24
2 CRC   TR          692. 3.7e-24
3 CRC   W           638. 3.7e-24
4 PTR   UP          308. 3.7e-24
5 PTR   TR          141. 3.7e-24
6 PTR   W           709. 3.7e-24
7 OWC   TR          512. 3.7e-24
8 OWC   WTE         167. 3.7e-24
9 OWC   W           622. 3.7e-24"

#data are not normal!


#############################################################################

# friedman test test sgnificant differences between fixed factor site_location
# repeated measurements per datetime as a unique ID
# paired per datetime

# then we need to filter to get data sampled simultanously in all zones:
# df_ filtered has data sampled in all zones (location) at the same time
# to test across site locations:

# creating de_filterd:

str(df_troll2)

df_troll2 <- df_troll %>% 
  filter(datetime>"2022-03-21 11:00:00", 
         datetime<"2022-11-01 11:00:00" ,
         flag_out_of_water=="FALSE",
         specific_conductivity>30,
         site!="GWI", site!="MSM", #site!="OWC",
         #location=="W"
  ) %>%
 #select(datetime, Eh, location, site)%>%
  mutate(site_location = factor(paste(site, location, sep = "-")))

# Group data by datetime and count distinct site_location values
group_counts <- df_troll2 %>%
  group_by(datetime) %>%
  summarize(n_groups = n_distinct(site_location))

# Find datetimes with observations for all groups
all_groups <- group_counts %>%
  filter(n_groups == 9) %>%
  pull(datetime)

# Filter data to only include datetime with observations for all groups
df_filtered <- df_troll2 %>%
  filter(datetime %in% all_groups)

str(df_filtered)
head(df_filtered)

#data as ID (as factor)
df_filtered$datetime2 <- as.factor(df_filtered$datetime)


##################################################################################

# Perform the Friedman test
library(rstatix)

# site_location as a fixed factor paired per ID datetime and repeated measurements:

res.fried <- friedman_test(Eh ~ site_location|datetime2, data=df_filtered)
res.fried

# effect size:

df_filtered%>%friedman_effsize(Eh ~ site_location|datetime2)

# pairwise compareison

pwc<-df_filtered%>%wilcox_test(Eh ~ site_location, paired = TRUE, p.adjust.method = "bonferroni")
print(pwc, n=36)

#pwc2<-df_filtered%>%sign_test(Eh ~ site_location, p.adjust.method = "bonferroni")
#print(pwc2, n=36)

### Result Redox ~ site_location repeated measurements and paired per datatime with Friedman test:

"> # Perform the Friedman test
> res.fried <- friedman_test(Eh ~ site_location|datetime2, data=df_filtered)
> res.fried
# A tibble: 1 × 6
  .y.       n statistic    df     p method       
* <chr> <int>     <dbl> <dbl> <dbl> <chr>        
1 Eh     3227    18480.     8     0 Friedman test
> df_filtered%>%friedman_effsize(Eh ~ site_location|datetime2)
# A tibble: 1 × 5
  .y.       n effsize method    magnitude
* <chr> <int>   <dbl> <chr>     <ord>    
1 Eh     3227   0.716 Kendall W large    
> pwc<-df_filtered%>%wilcox_test(Eh ~ site_location, paired = TRUE, p.adjust.method = "bonferroni")
> print(pwc, n=36)
# A tibble: 36 × 9
   .y.   group1  group2     n1    n2 statistic         p     p.adj p.adj.signif
 * <chr> <chr>   <chr>   <int> <int>     <dbl>     <dbl>     <dbl> <chr>       
 1 Eh    CRC-TR  CRC-UP   3227  3227  2467054  1   e-  2 3.45e-  1 ns          
 2 Eh    CRC-TR  CRC-W    3227  3227  5208068  0         0         ****        
 3 Eh    CRC-TR  OWC-TR   3227  3227  3327897  1.48e- 42 5.33e- 41 ****        
 4 Eh    CRC-TR  OWC-W    3227  3227  5197725  0         0         ****        
 5 Eh    CRC-TR  OWC-WTE  3227  3227  2895776  3.61e-  8 1.3 e-  6 ****        
 6 Eh    CRC-TR  PTR-TR   3227  3227   693828  3.07e-285 1.11e-283 ****        
 7 Eh    CRC-TR  PTR-UP   3227  3227        0  0         0         ****        
 8 Eh    CRC-TR  PTR-W    3227  3227  5208378  0         0         ****        
 9 Eh    CRC-UP  CRC-W    3227  3227  5208378  0         0         ****        
10 Eh    CRC-UP  OWC-TR   3227  3227  3454214  4.93e- 58 1.77e- 56 ****        
11 Eh    CRC-UP  OWC-W    3227  3227  5200517  0         0         ****        
12 Eh    CRC-UP  OWC-WTE  3227  3227  3221473  1.99e- 31 7.16e- 30 ****        
13 Eh    CRC-UP  PTR-TR   3227  3227  1037650. 1.68e-192 6.05e-191 ****        
14 Eh    CRC-UP  PTR-UP   3227  3227        0  0         0         ****        
15 Eh    CRC-UP  PTR-W    3227  3227  5208378  0         0         ****        
16 Eh    CRC-W   OWC-TR   3227  3227   117303  0         0         ****        
17 Eh    CRC-W   OWC-W    3227  3227  4123109  4.24e-181 1.53e-179 ****        
18 Eh    CRC-W   OWC-WTE  3227  3227   554287  0         0         ****        
19 Eh    CRC-W   PTR-TR   3227  3227        0  0         0         ****        
20 Eh    CRC-W   PTR-UP   3227  3227        0  0         0         ****        
21 Eh    CRC-W   PTR-W    3227  3227   267269  0         0         ****        
22 Eh    OWC-TR  OWC-W    3227  3227  4860725  0         0         ****        
23 Eh    OWC-TR  OWC-WTE  3227  3227  2461715  7   e-  3 2.56e-  1 ns          
24 Eh    OWC-TR  PTR-TR   3227  3227  1338961  2.82e-126 1.02e-124 ****        
25 Eh    OWC-TR  PTR-UP   3227  3227    49321  0         0         ****        
26 Eh    OWC-TR  PTR-W    3227  3227  4153120  3.04e-188 1.09e-186 ****        
27 Eh    OWC-W   OWC-WTE  3227  3227    68339  0         0         ****        
28 Eh    OWC-W   PTR-TR   3227  3227       19  0         0         ****        
29 Eh    OWC-W   PTR-UP   3227  3227        0  0         0         ****        
30 Eh    OWC-W   PTR-W    3227  3227   457973  0         0         ****        
31 Eh    OWC-WTE PTR-TR   3227  3227  1199428  3.39e-155 1.22e-153 ****        
32 Eh    OWC-WTE PTR-UP   3227  3227   108140  0         0         ****        
33 Eh    OWC-WTE PTR-W    3227  3227  4337944  2.57e-235 9.25e-234 ****        
34 Eh    PTR-TR  PTR-UP   3227  3227     8754  0         0         ****        
35 Eh    PTR-TR  PTR-W    3227  3227  5208378  0         0         ****        
36 Eh    PTR-UP  PTR-W    3227  3227  5208378  0         0         **** "

##############################################################################

# Graph

# creating Post comparisons letters as df_filterd$PC for the friedman test :Eh~site-zone, paired/repeated: datetime

str(df_filtered)

df_filtered <- df_filtered %>% 
  mutate(PC_Eh = case_when(
    site_location == "CRC-UP" ~ "a",
    site_location == "CRC-TR" ~ "a",
    site_location == "CRC-W" ~ "b",
    site_location == "PTR-UP" ~ "c",
    site_location == "PTR-TR" ~ "d",
    site_location == "PTR-W" ~ "e",
    site_location == "OWC-TR" ~ "f",
    site_location == "OWC-WTE" ~ "f",
    site_location == "OWC-W" ~ "g",
    TRUE ~ NA_character_
  ))

# graph as the basic graph:

df_filtered %>% 
  filter(datetime>"2022-03-21 11:00:00", 
         datetime<"2022-11-01 11:00:00" ,
         flag_out_of_water=="FALSE",
         specific_conductivity>30,
         site!="GWI", site!="MSM")%>%
  ggplot(aes(x = factor(site), 
             y = Eh,
             fill=location
  )
  )+
  geom_boxplot()+
  scale_fill_manual(values=c("#2C8774",
                             "#EFC00099",
                             "#B2BEB5",
                             "#56B4E9"))+
  scale_x_discrete(breaks = c("CRC", "PTR", "OWC"),
                   labels = c("CRC","PTR","OWC"))+
  labs(y="Eh (mV)", x="")+
  ggtitle("B") +
  guides(fill = guide_legend(title="TAI Zone", position="top"))+
  theme(text = element_text(size = 18),
        legend.position = "none",
        legend.margin = margin(t = -0.1, b = 0, unit = "cm"))+
  stat_summary(fun.data = mean_cl_normal, geom = "pointrange",
               size = 0.5, shape = 3, stroke = 2,
               col = "black", 
               position = position_dodge(width = 0.75))+
  stat_summary(fun.y = max, 
               geom = "text", 
               aes(label = PC_Eh), 
               vjust = 0, 
               hjust = -0.5,
               size=5,
               position = position_dodge(width = 0.75)
  )->plotf1

plotf1


###############################################################################

# (2) test GW level:

# normality

library(nortest)

df_troll2 %>%
  group_by(site, location) %>%
  summarize(ad_stat = ad.test(wl_below_surface_m)$statistic, p_value = ad.test(wl_below_surface_m)$p.value)


# Perform the Friedman test
library(rstatix)
str(df_filtered)
# site_location as a fixed factor paired per ID datetime and repeated measurements:

res.fried <- friedman_test(wl_below_surface_m ~ site_location|datetime2, data=df_filtered)
res.fried

# effect size:

df_filtered%>%friedman_effsize(wl_below_surface_m ~ site_location|datetime2)

# pairwise compareison

pwc<-df_filtered%>%wilcox_test(wl_below_surface_m ~ site_location, paired = TRUE, p.adjust.method = "bonferroni")
print(pwc, n=36)

pwc2<-df_filtered%>%sign_test(Eh ~ site_location, p.adjust.method = "bonferroni")
print(pwc2, n=36)

pwc <- pwc %>% add_xy_position(x = "Site-Zone")
ggboxplot(df_filtered, x = "Site-Zone", y = "Eh (mV)", add = "point") +
  stat_pvalue_manual(pwc, hide.ns = TRUE) +
  labs(
    subtitle = get_test_label(res.fried,  detailed = TRUE),
    caption = get_pwc_label(pwc)
  )


df_filtered <- df_filtered %>% 
  mutate(PC_WL = case_when(
    site_location == "CRC-UP" ~ "a",
    site_location == "CRC-TR" ~ "b",
    site_location == "CRC-W" ~ "c",
    site_location == "PTR-UP" ~ "d",
    site_location == "PTR-TR" ~ "e",
    site_location == "PTR-W" ~ "f",
    site_location == "OWC-TR" ~ "g",
    site_location == "OWC-WTE" ~ "h",
    site_location == "OWC-W" ~ "i",
    TRUE ~ NA_character_
  ))

df_filtered %>% 
  filter(datetime>"2022-03-21 11:00:00", 
         datetime<"2022-11-01 11:00:00" ,
         flag_out_of_water=="FALSE",
         specific_conductivity>30,
         site!="GWI", site!="MSM")%>%
  ggplot(aes(x = factor(site), 
             y = wl_below_surface_m,
             fill=location
  )
  )+
  geom_boxplot()+
  scale_fill_manual(values=c("#2C8774",
                             "#EFC00099",
                             "#B2BEB5",
                             "#56B4E9"))+
  scale_x_discrete(breaks = c("CRC", "PTR", "OWC"),
                   labels = c("CRC","PTR","OWC"))+
  labs(y="Groundwater Level (m)", x="")+
  ylim(-2, 0.5)+
  ggtitle("A") +
  guides(fill = guide_legend(title="TAI Zone", position="top"))+
  theme(text = element_text(size = 18),
        legend.position = "none",
        legend.margin = margin(t = -0.1, b = 0, unit = "cm"))+
  stat_summary(fun.data = mean_cl_normal, geom = "pointrange",
               size = 0.5, shape = 3, stroke = 2,
               col = "black", 
               position = position_dodge(width = 0.75))+
  stat_summary(fun.y = max, 
               geom = "text", 
               aes(label = PC_WL), 
               vjust = 0, 
               hjust = -0.5,
               size=5,
               position = position_dodge(width = 0.75)
  )->plotf2

plotf2


####### Statistical results:





###############################################################################

###############################################################################
# combine the plots with a single legend
#grid.arrange(plot1, plot2, ncol = 1)


pplot1 <- plot1+theme(plot.margin = unit(c(0.1, 1, 0.1, 0.5), "cm"))
pplot2 <- plot2+theme(plot.margin = unit(c(0.1, 1, 0.1, 0.5), "cm"))
pplot3 <- plot3+theme(plot.margin = unit(c(0.1, 1, 0.1, 0.5), "cm"))
pplot4 <- plot4+theme(plot.margin = unit(c(0.1, 1, 0.1, 0.5), "cm"))
pplot5 <- plot5+theme(plot.margin = unit(c(0.1, 1, 0.1, 0.5), "cm"))
pplot6 <- plot6+theme(plot.margin = unit(c(0.1, 1, 0.1, 0.5), "cm"))

pp <- list(pplot2, pplot1)
#plot_grid(plotlist = pp, nrow = 3, ncol = 2, align = 'hv', axis = "tb", legend = ggplot2::"top")


# create a legend
legend <- get_legend(plot1 + theme(legend.position = "bottom"))

# decrease the space of the legend
legend$heights[2] <- unit(0.1, "cm")

# add the legend to the bottom of the grid
grid.arrange(grobs = list(plot_grid(plotlist = pp, nrow = 1, ncol = 2, align = 'hv', axis = "tb"), 
                          arrangeGrob(legend)), ncol = 1, heights = c(4, 0.5))


###############################################################################

#######################################
############ OLD ######################
#######################################


df_lme <- df %>% 
  filter(site=="OWC", zone=="W")%>% 
  select(date, Eh, depth_cm) %>% 
  mutate(depth_cm = as.character(depth_cm))

lme_model <- lmer(Eh ~ site + location + (1|datetime), data = df_filtered)
summary(lme_model)

# Obtain the lsmeans and their pairwise comparisons
lsmeans_model <- lsmeans(lme_model, "depth_cat")
pairwise_comparisons <- pairs(lsmeans_model)

# Print the pairwise comparisons table
print(pairwise_comparisons)




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