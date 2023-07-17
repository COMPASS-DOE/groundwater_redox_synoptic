########################
### Graph REDOX MOnMOn
########################

############################     Set up     #####################################

#set packages
require(pacman)
p_load(dplyr, tidyverse, tidypaleo, tidyr, rstatix, MASS, afex, mvnormtest, userfriendlyscience, emmeans)

# set directory
setwd("C:/Users/fsilva/OneDrive - University of Toledo/Documents/Fausto UT/COMPASS/Manuscript 1 Redox/Figures/Codes")

# bring df
Redox<-read_csv("Redox_SWAP2.csv")
Redox<-read_csv("df_redox_monmon.csv")
head(Redox)

# prepare df: 
# clean names set chracter as factor
data<-Redox
data<-clean_names(Redox)
head(data)

# remove NANs
data_complete <- data[complete.cases(data$ehf_m_v), ]      # Remove incomplete rows where Eh == NA
data_complete -> df  
head(df)
str(df)

# set chracter as factor:
df$depth_cm<-as.factor(df$depth_cm)
df$zone<-as.factor(df$zone)
df$site<-as.factor(df$site)

# set dates
df$date <- as.Date(df$date, "%m/%d/%Y")

# order zone
df$zone <- factor(df$zone, levels=c('UP', 'TR', 'W'))

# correct Eh based on field ORP and T
df <- df %>%
  mutate(Eh = ehf_m_v - (0.718 * tmean) + 224.41)


str(df)
# create a case 15 to make distance equal  
df <- df %>%
  mutate(depth_cm = factor(depth_cm, levels = c("20", "15", "10",  "5")))

new_row <- data.frame(date = as.Date("2023-05-02"),
                      year = 2023,
                      month = "05",
                      day = 2,
                      campaing = "Camp",
                      site = "OWC",
                      depth_cm = "15",
                      zone = "W",
                      ehf_m_v = NA,
                      time = as.POSIXct("12:00:00", format="%H:%M:%S"),
                      tsoil = NA,
                      tmean = NA,
                      notes = NA,
                      x14 = NA,
                      x15 = NA,
                      x16 = NA,
                      Eh = NA
) # Change "depth_" to "depth_cm1" to match column names in df

colnames(new_row) <- colnames(df) # Rename columns in new_row to match those in df
df <- rbind(df, new_row) # Append new_row to df



print(df, n=10)
str(df)

############################     Graph soil profile  #####################################

# calculate mean and sd for the graph
df %>%
  group_by(site, zone) %>%
  summarize(median = median(Eh),
            lower_quartile = quantile(Eh, 0.25),
            upper_quartile = quantile(Eh, 0.75),
            mean = mean(Eh),
            sd = sd(Eh)
  )-> df_s

print(df_s, n=27)


# calculate mean and sd for the graph
df %>%
  group_by(site, zone, depth_cm) %>%
  summarise(mEh = mean(Eh),
            n = n(),
            sd = sd(Eh),
            se = sd(Eh)/sqrt(n),
            .groups="drop"
  )-> df_su

print(df_su, n=27)


# graph Eh profile (point): 
df_su %>%
  ggplot(aes(x=as.numeric(depth_cm), y=mEh, color=zone)) +
  geom_point(size=5, alpha=1)+
  geom_smooth(method="loess")+
  geom_errorbar(aes(ymin=mEh-sd,
                    ymax=mEh+sd), width=0.5, size=0.2)+
  facet_wrap(~site,#scales = "free_x",
             strip.position = "bottom")+
  scale_y_continuous(position="right")+
  labs(title="LE - Soil mean (SE) Redox potential profile", y="Eh (mV)", x = "Depth (cm)")+
  scale_color_manual(values=c("forestgreen",
                              "#EFC00099",
                              "#0072B2"))+
  coord_flip()+ #flip x and y defined ggplot(aes())
  theme(strip.background = element_rect(fill = "transparent", color="transparent"),
        text = element_text(size = 18),
        legend.margin = margin(t = -0.1, b = 0, unit = "cm"),
        legend.key = element_rect(colour = "transparent", fill = "white"))



#######################################################################################



df%>%
ggplot(aes(x=depth_cm, y=Eh, fill=zone)) + 
  geom_boxplot() +
  scale_y_continuous(position="right")+
  coord_flip()+
  facet_wrap(~ site,#scales = "free_x",
             strip.position = "bottom")+
  labs(title="", y="Eh (mV)", x = "Depth (cm)")+
  scale_fill_manual(values=c("forestgreen",
                             "#d7ac08",
                             "#0072B2"))+
  theme(strip.background = element_rect(fill = "transparent", color="transparent"),
        text = element_text(size = 18),
        legend.margin = margin(t = -0.1, b = 0, unit = "cm"),
        legend.key = element_rect(colour = "transparent", fill = "white"))+
  stat_summary(fun.y="mean",
               size = 0.5, shape = 18, 
               col = "black", 
               position = position_dodge(width = 0.75))


################################################################################

#######################      G  R  A  P  H  ##############

# Plot the boxplot with labels

df %>%
  ggplot(aes(x=depth_cm , y=Eh, fill=zone)) + 
  geom_boxplot() +
  scale_y_continuous(position="right", 
                     limits=c(-300, 800),
                     breaks=c(-250, 0, 250, 500, 750),
                     labels=c("-250", "0", "+250", "500", "750"))+
  stat_summary(fun.y="mean",
               size = 0.5, shape = 18, 
               col = "black", 
               position = position_dodge(width = 0.75))+
  stat_summary(fun.y = max, 
               geom = "text", 
               aes(label = PC), 
               vjust = 0, 
               hjust = -0.5
               #position = position_dodge(width = 0.75)
  ) +
  coord_flip()+
  facet_grid(site~zone, switch="x")+
  labs(title="", y="Eh (mV)", x = "Depth (cm)")+
  scale_fill_manual(values=c("forestgreen",
                             "#d7ac08",
                             "#0072B2")
  )+
  theme(strip.background = element_blank())

###############

print(df)
str(df)
names(df)
############################     Desciptive Statistics    #####################################

df_su %>% 
  filter(#date>"2022-03-21", 
         #date<"2022-12-01" )%>%#,
         #site == "OWC",
         zone == "W"
         )  %>% summarize(mean_mEh = mean(mEh),
                          min_mEh = min(mEh),
                          max_mEh = max(mEh),
                          sd = sd(mEh))
                  
df_su %>%
  filter(site == "OWC", zone=="TR") %>%
  slice(which.min(mEh), which.max(mEh)) %>%
  pull(depth_cm, mEh) 
 

head(df_su)

df_su <- df_su %>%
  mutate(Depth = case_when(depth_cm == "5" ~ 5,
                           depth_cm == "10" ~ 10,
                           depth_cm == "20" ~ 20,
                           TRUE ~ NA_real_))



print(df_su, n=27)



df_wide <- df %>%
  select(site, date, depth_cm, Eh) %>%
  group_by(site, date, depth_cm) %>%
  summarize(Eh = mean(Eh, na.rm = TRUE)) %>%
  pivot_wider(names_from = depth_cm, values_from = Eh)


############################      Statistics    #####################################

df_shapiro <- df %>%
  group_by(site, zone, depth_cm) %>%
  shapiro_test(Eh)
 
#data are not normal!


install.packages("lme4") 
install.packages("lsmeans")
library(lme4)
library(lsmeans)

df_lme <- df %>% 
  filter(site=="OWC", zone=="W")%>% 
  select(date, Eh, depth_cm) %>% 
  mutate(depth_cm = as.character(depth_cm))

lme_model <- lmer(Eh ~ depth_cm + (1|date), data = df_lme)
summary(lme_model)

# Obtain the lsmeans and their pairwise comparisons
lsmeans_model <- lsmeans(lme_model, "depth_cm")
pairwise_comparisons <- pairs(lsmeans_model)

# Print the pairwise comparisons table
print(pairwise_comparisons)



#################  CRC W ###################### 

"df_lme <- df %>% 
filter(site=="CRC", zone=="W")%>% 
  select(date, Eh, depth_cm) %>% 
  mutate(depth_cm = as.character(depth_cm))
lme_model <- lmer(Eh ~ depth_cm + (1|date), data = df_lme)
summary(lme_model)
Linear mixed model fit by REML. t-tests use Satterthwaite's method ['lmerModLmerTest']
Formula: Eh ~ depth_cm + (1 | date)
   Data: df_lme

REML criterion at convergence: 876.4

Scaled residuals: 
     Min       1Q   Median       3Q      Max 
-2.61171 -0.74410  0.08405  0.54003  2.82103 

Random effects:
 Groups   Name        Variance Std.Dev.
 date     (Intercept) 12481    111.72  
 Residual              2623     51.22  
Number of obs: 82, groups:  date, 6

Fixed effects:
            Estimate Std. Error      df t value Pr(>|t|)    
(Intercept)  438.693     46.622   5.176   9.410  0.00019 ***
depth_cm20    44.134     13.450  73.922   3.281  0.00158 ** 
depth_cm5    -91.710     14.658  74.195  -6.257 2.29e-08 ***
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

Correlation of Fixed Effects:
           (Intr) dpt_20
depth_cm20 -0.144       
depth_cm5  -0.126  0.459
> # Obtain the lsmeans and their pairwise comparisons
> lsmeans_model <- lsmeans(lme_model, "depth_cm")
> pairwise_comparisons <- pairs(lsmeans_model)
> # Print the pairwise comparisons table
> print(pairwise_comparisons)
 contrast                estimate   SE   df t.ratio p.value
 depth_cm10 - depth_cm20    -44.1 13.5 74.0  -3.281  0.0044
 depth_cm10 - depth_cm5      91.7 14.7 74.3   6.252  <.0001
 depth_cm20 - depth_cm5     135.8 14.7 74.3   9.261  <.0001

Degrees-of-freedom method: kenward-roger 
P value adjustment: tukey method for comparing a family of 3 estimates" 

#################  CRC TR ##################### 

"> df_lme <- df %>% 
+   filter(site=="CRC", zone=="TR")%>% 
+   select(date, Eh, depth_cm) %>% 
+   mutate(depth_cm = as.character(depth_cm))
> lme_model <- lmer(Eh ~ depth_cm + (1|date), data = df_lme)
> summary(lme_model)
Linear mixed model fit by REML. t-tests use Satterthwaite's method ['lmerModLmerTest']
Formula: Eh ~ depth_cm + (1 | date)
   Data: df_lme

REML criterion at convergence: 948.1

Scaled residuals: 
    Min      1Q  Median      3Q     Max 
-4.1001 -0.4835 -0.0322  0.5393  2.6834 

Random effects:
 Groups   Name        Variance Std.Dev.
 date     (Intercept) 36976    192.29  
 Residual              5512     74.25  
Number of obs: 83, groups:  date, 6

Fixed effects:
            Estimate Std. Error      df t value Pr(>|t|)   
(Intercept)  364.204     79.762   5.096   4.566  0.00575 **
depth_cm20    53.550     19.343  74.918   2.768  0.00709 **
depth_cm5     18.429     21.079  75.089   0.874  0.38474   
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

Correlation of Fixed Effects:
           (Intr) dpt_20
depth_cm20 -0.123       
depth_cm5  -0.104  0.469
> # Obtain the lsmeans and their pairwise comparisons
> lsmeans_model <- lsmeans(lme_model, "depth_cm")
> pairwise_comparisons <- pairs(lsmeans_model)
> # Print the pairwise comparisons table
> print(pairwise_comparisons)
 contrast                estimate   SE   df t.ratio p.value
 depth_cm10 - depth_cm20    -53.6 19.3 75.0  -2.768  0.0192
 depth_cm10 - depth_cm5     -18.4 21.1 75.2  -0.874  0.6584
 depth_cm20 - depth_cm5      35.1 20.9 75.2   1.681  0.2191

Degrees-of-freedom method: kenward-roger 
P value adjustment: tukey method for comparing a family of 3 estimates "


#################  CRC UP #####################

"> df_lme <- df %>% 
+   filter(site=="CRC", zone=="UP")%>% 
+   select(date, Eh, depth_cm) %>% 
+   mutate(depth_cm = as.character(depth_cm))
> lme_model <- lmer(Eh ~ depth_cm + (1|date), data = df_lme)
> summary(lme_model)
Linear mixed model fit by REML. t-tests use Satterthwaite's method ['lmerModLmerTest']
Formula: Eh ~ depth_cm + (1 | date)
   Data: df_lme

REML criterion at convergence: 690.2

Scaled residuals: 
    Min      1Q  Median      3Q     Max 
-2.3138 -0.6700  0.2091  0.6753  2.5186 

Random effects:
 Groups   Name        Variance Std.Dev.
 date     (Intercept) 398.7    19.97   
 Residual             662.4    25.74   
Number of obs: 75, groups:  date, 5

Fixed effects:
            Estimate Std. Error      df t value Pr(>|t|)    
(Intercept)  567.509     10.275   5.553  55.232 7.96e-09 ***
depth_cm20    28.248      7.361  69.993   3.838 0.000269 ***
depth_cm5    -21.664      7.361  69.993  -2.943 0.004404 ** 
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

Correlation of Fixed Effects:
           (Intr) dpt_20
depth_cm20 -0.303       
depth_cm5  -0.303  0.491
> # Obtain the lsmeans and their pairwise comparisons
> lsmeans_model <- lsmeans(lme_model, "depth_cm")
> pairwise_comparisons <- pairs(lsmeans_model)
> # Print the pairwise comparisons table
> print(pairwise_comparisons)
 contrast                estimate   SE   df t.ratio p.value
 depth_cm10 - depth_cm20    -28.2 7.40 69.8  -3.815  0.0008
 depth_cm10 - depth_cm5      21.7 7.40 69.8   2.926  0.0127
 depth_cm20 - depth_cm5      49.9 7.43 68.1   6.718  <.0001

Degrees-of-freedom method: kenward-roger 
P value adjustment: tukey method for comparing a family of 3 estimates "


########################## PTR UP ########################################

"> df_lme <- df %>% 
+   filter(site=="PTR", zone=="UP")%>% 
+   select(date, Eh, depth_cm) %>% 
+   mutate(depth_cm = as.character(depth_cm))
> lme_model <- lmer(Eh ~ depth_cm + (1|date), data = df_lme)
> summary(lme_model)
Linear mixed model fit by REML. t-tests use Satterthwaite's method ['lmerModLmerTest']
Formula: Eh ~ depth_cm + (1 | date)
   Data: df_lme

REML criterion at convergence: 532.8

Scaled residuals: 
    Min      1Q  Median      3Q     Max 
-3.6543 -0.2002  0.0562  0.4860  3.2746 

Random effects:
 Groups   Name        Variance Std.Dev.
 date     (Intercept) 1803     42.47   
 Residual              855     29.24   
Number of obs: 57, groups:  date, 3

Fixed effects:
            Estimate Std. Error      df t value Pr(>|t|)    
(Intercept)  555.939     25.564   1.917  21.747  0.00258 ** 
depth_cm20    44.064      9.708  52.613   4.539 3.32e-05 ***
depth_cm5    -19.930      9.708  52.613  -2.053  0.04507 *  
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

Correlation of Fixed Effects:
           (Intr) dpt_20
depth_cm20 -0.131       
depth_cm5  -0.131  0.496
> # Obtain the lsmeans and their pairwise comparisons
> lsmeans_model <- lsmeans(lme_model, "depth_cm")
> pairwise_comparisons <- pairs(lsmeans_model)
> # Print the pairwise comparisons table
> print(pairwise_comparisons)
 contrast                estimate   SE   df t.ratio p.value
 depth_cm10 - depth_cm20    -44.1 9.78 52.7  -4.506  0.0001
 depth_cm10 - depth_cm5      19.9 9.78 52.7   2.038  0.1132
 depth_cm20 - depth_cm5      64.0 9.75 52.0   6.566  <.0001

Degrees-of-freedom method: kenward-roger 
P value adjustment: tukey method for comparing a family of 3 estimates"


############################ PTR TR ##################

"> df_lme <- df %>% 
+   filter(site=="PTR", zone=="TR")%>% 
+   select(date, Eh, depth_cm) %>% 
+   mutate(depth_cm = as.character(depth_cm))
> lme_model <- lmer(Eh ~ depth_cm + (1|date), data = df_lme)
> summary(lme_model)
Linear mixed model fit by REML. t-tests use Satterthwaite's method ['lmerModLmerTest']
Formula: Eh ~ depth_cm + (1 | date)
   Data: df_lme

REML criterion at convergence: 529.7

Scaled residuals: 
    Min      1Q  Median      3Q     Max 
-2.1142 -0.4515  0.1631  0.5343  2.6453 

Random effects:
 Groups   Name        Variance Std.Dev.
 date     (Intercept) 6626     81.40   
 Residual             1079     32.85   
Number of obs: 55, groups:  date, 4

Fixed effects:
            Estimate Std. Error      df t value Pr(>|t|)    
(Intercept)  516.727     42.597   3.339  12.131 0.000708 ***
depth_cm20    39.224     11.317  49.141   3.466 0.001106 ** 
depth_cm5     13.125     11.089  49.569   1.184 0.242215    
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

Correlation of Fixed Effects:
           (Intr) dpt_20
depth_cm20 -0.123       
depth_cm5  -0.190  0.480
> # Obtain the lsmeans and their pairwise comparisons
> lsmeans_model <- lsmeans(lme_model, "depth_cm")
> pairwise_comparisons <- pairs(lsmeans_model)
> # Print the pairwise comparisons table
> print(pairwise_comparisons)
 contrast                estimate   SE   df t.ratio p.value
 depth_cm10 - depth_cm20    -39.2 11.3 49.0  -3.466  0.0031
 depth_cm10 - depth_cm5     -13.1 11.1 49.5  -1.180  0.4708
 depth_cm20 - depth_cm5      26.1 11.5 49.4   2.278  0.0685

Degrees-of-freedom method: kenward-roger 
P value adjustment: tukey method for comparing a family of 3 estimates "


##########################  PTR W   ###############################

"> df_lme <- df %>% 
+   filter(site=="PTR", zone=="W")%>% 
+   select(date, Eh, depth_cm) %>% 
+   mutate(depth_cm = as.character(depth_cm))
> lme_model <- lmer(Eh ~ depth_cm + (1|date), data = df_lme)
> summary(lme_model)
Linear mixed model fit by REML. t-tests use Satterthwaite's method ['lmerModLmerTest']
Formula: Eh ~ depth_cm + (1 | date)
   Data: df_lme

REML criterion at convergence: 518.4

Scaled residuals: 
    Min      1Q  Median      3Q     Max 
-2.0347 -0.4124  0.1620  0.5895  1.6792 

Random effects:
 Groups   Name        Variance Std.Dev.
 date     (Intercept) 14318    119.7   
 Residual              2007     44.8   
Number of obs: 51, groups:  date, 3

Fixed effects:
            Estimate Std. Error      df t value Pr(>|t|)  
(Intercept)  502.498     70.046   2.028   7.174   0.0182 *
depth_cm20   -23.367     14.932  45.976  -1.565   0.1245  
depth_cm5     -4.410     16.007  46.049  -0.275   0.7842  
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

Correlation of Fixed Effects:
           (Intr) dpt_20
depth_cm20 -0.107       
depth_cm5  -0.090  0.466
> # Obtain the lsmeans and their pairwise comparisons
> lsmeans_model <- lsmeans(lme_model, "depth_cm")
> pairwise_comparisons <- pairs(lsmeans_model)
> # Print the pairwise comparisons table
> print(pairwise_comparisons)
 contrast                estimate   SE   df t.ratio p.value
 depth_cm10 - depth_cm20    23.37 14.9 46.0   1.565  0.2710
 depth_cm10 - depth_cm5      4.41 16.0 46.1   0.275  0.9591
 depth_cm20 - depth_cm5    -18.96 16.0 46.1  -1.183  0.4690

Degrees-of-freedom method: kenward-roger 
P value adjustment: tukey method for comparing a family of 3 estimates"


############################### OWC W ###################

# OWC TR: NS, OWC UP: 5 diff 20, W 5 diff from 10 and 20 (10=20)


"> df_lme <- df %>% 
+   filter(site=="OWC", zone=="TR")%>% 
+   select(date, Eh, depth_cm) %>% 
+   mutate(depth_cm = as.character(depth_cm))
> lme_model <- lmer(Eh ~ depth_cm + (1|date), data = df_lme)
> summary(lme_model)
Linear mixed model fit by REML. t-tests use Satterthwaite's method ['lmerModLmerTest']
Formula: Eh ~ depth_cm + (1 | date)
   Data: df_lme

REML criterion at convergence: 716.7

Scaled residuals: 
    Min      1Q  Median      3Q     Max 
-3.6858 -0.5060  0.1931  0.5569  2.7021 

Random effects:
 Groups   Name        Variance Std.Dev.
 date     (Intercept)  753.8   27.46   
 Residual             1264.8   35.56   
Number of obs: 73, groups:  date, 5

Fixed effects:
            Estimate Std. Error      df t value Pr(>|t|)    
(Intercept)  521.441     14.172   5.671  36.794 5.74e-08 ***
depth_cm20    -1.050      9.404  66.339  -0.112    0.911    
depth_cm5     -3.513     12.113  67.810  -0.290    0.773    
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

Correlation of Fixed Effects:
           (Intr) dpt_20
depth_cm20 -0.356       
depth_cm5  -0.281  0.416
> # Obtain the lsmeans and their pairwise comparisons
> lsmeans_model <- lsmeans(lme_model, "depth_cm")
> pairwise_comparisons <- pairs(lsmeans_model)
> # Print the pairwise comparisons table
> print(pairwise_comparisons)
 contrast                estimate    SE   df t.ratio p.value
 depth_cm10 - depth_cm20     1.05  9.41 66.2   0.112  0.9932
 depth_cm10 - depth_cm5      3.51 12.20 67.7   0.288  0.9554
 depth_cm20 - depth_cm5      2.46 11.94 67.7   0.206  0.9768

Degrees-of-freedom method: kenward-roger 
P value adjustment: tukey method for comparing a family of 3 estimates 
> df_lme <- df %>% 
+   filter(site=="OWC", zone=="UP")%>% 
+   select(date, Eh, depth_cm) %>% 
+   mutate(depth_cm = as.character(depth_cm))
> lme_model <- lmer(Eh ~ depth_cm + (1|date), data = df_lme)
> summary(lme_model)
Linear mixed model fit by REML. t-tests use Satterthwaite's method ['lmerModLmerTest']
Formula: Eh ~ depth_cm + (1 | date)
   Data: df_lme

REML criterion at convergence: 562.4

Scaled residuals: 
    Min      1Q  Median      3Q     Max 
-2.2970 -0.4368  0.2057  0.7093  1.8341 

Random effects:
 Groups   Name        Variance Std.Dev.
 date     (Intercept)  734     27.09   
 Residual             1454     38.13   
Number of obs: 57, groups:  date, 5

Fixed effects:
            Estimate Std. Error      df t value Pr(>|t|)    
(Intercept)  559.533     14.797   5.813  37.814 3.53e-08 ***
depth_cm20    25.924     11.767  49.928   2.203   0.0322 *  
depth_cm5    -23.500     13.305  51.431  -1.766   0.0833 .  
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

Correlation of Fixed Effects:
           (Intr) dpt_20
depth_cm20 -0.398       
depth_cm5  -0.329  0.442
> # Obtain the lsmeans and their pairwise comparisons
> lsmeans_model <- lsmeans(lme_model, "depth_cm")
> pairwise_comparisons <- pairs(lsmeans_model)
> # Print the pairwise comparisons table
> print(pairwise_comparisons)
 contrast                estimate   SE   df t.ratio p.value
 depth_cm10 - depth_cm20    -25.9 11.8 50.0  -2.203  0.0804
 depth_cm10 - depth_cm5      23.5 13.4 51.5   1.752  0.1961
 depth_cm20 - depth_cm5      49.4 13.4 51.5   3.684  0.0016

Degrees-of-freedom method: kenward-roger 
P value adjustment: tukey method for comparing a family of 3 estimates 
> df_lme <- df %>% 
+   filter(site=="OWC", zone=="W")%>% 
+   select(date, Eh, depth_cm) %>% 
+   mutate(depth_cm = as.character(depth_cm))
> lme_model <- lmer(Eh ~ depth_cm + (1|date), data = df_lme)
> summary(lme_model)
Linear mixed model fit by REML. t-tests use Satterthwaite's method ['lmerModLmerTest']
Formula: Eh ~ depth_cm + (1 | date)
   Data: df_lme

REML criterion at convergence: 711.4

Scaled residuals: 
     Min       1Q   Median       3Q      Max 
-3.06369 -0.63234 -0.00509  0.60132  2.38927 

Random effects:
 Groups   Name        Variance Std.Dev.
 date     (Intercept) 4335     65.84   
 Residual             3309     57.52   
Number of obs: 66, groups:  date, 6

Fixed effects:
            Estimate Std. Error      df t value Pr(>|t|)    
(Intercept)   81.166     29.201   5.787   2.780   0.0333 *  
depth_cm20   -13.812     17.266  58.528  -0.800   0.4269    
depth_cm5    101.188     18.361  59.044   5.511 8.22e-07 ***
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

Correlation of Fixed Effects:
           (Intr) dpt_20
depth_cm20 -0.255       
depth_cm5  -0.200  0.348
> # Obtain the lsmeans and their pairwise comparisons
> lsmeans_model <- lsmeans(lme_model, "depth_cm")
> pairwise_comparisons <- pairs(lsmeans_model)
> # Print the pairwise comparisons table
> print(pairwise_comparisons)
 contrast                estimate   SE   df t.ratio p.value
 depth_cm10 - depth_cm20     13.8 17.3 58.6   0.798  0.7056
 depth_cm10 - depth_cm5    -101.2 18.4 59.1  -5.488  <.0001
 depth_cm20 - depth_cm5    -115.0 20.5 60.0  -5.606  <.0001

Degrees-of-freedom method: kenward-roger 
P value adjustment: tukey method for comparing a family of 3 estimates"




######################### OLD codes ###########################################


df <- df %>% 
  mutate(PC = case_when(
    site == "CRC" & zone == "UP" & depth_cm == "20" ~ "c",
    site == "CRC" & zone == "UP" & depth_cm == "15" ~ "",
    site == "CRC" & zone == "UP" & depth_cm == "10" ~ "b",
    site == "CRC" & zone == "UP" & depth_cm == "5" ~ "a",
    site == "CRC" & zone == "TR" & depth_cm == "20" ~ "b",
    site == "CRC" & zone == "TR" & depth_cm == "15" ~ "",
    site == "CRC" & zone == "TR" & depth_cm == "10" ~ "b",
    site == "CRC" & zone == "TR" & depth_cm == "5" ~ "a",
    site == "CRC" & zone == "W" & depth_cm == "20" ~ "c",
    site == "CRC" & zone == "W" & depth_cm == "15" ~ "",
    site == "CRC" & zone == "W" & depth_cm == "10" ~ "b",
    site == "CRC" & zone == "W" & depth_cm == "5" ~ "a",
    site == "PTR" & zone == "UP" & depth_cm == "20" ~ "b",
    site == "PTR" & zone == "UP" & depth_cm == "15" ~ "",
    site == "PTR" & zone == "UP" & depth_cm == "10" ~ "a",
    site == "PTR" & zone == "UP" & depth_cm == "5" ~ "ab",
    site == "PTR" & zone == "TR" & depth_cm == "20" ~ "b",
    site == "PTR" & zone == "TR" & depth_cm == "15" ~ "",
    site == "PTR" & zone == "TR" & depth_cm == "10" ~ "a",
    site == "PTR" & zone == "TR" & depth_cm == "5" ~ "a",
    site == "PTR" & zone == "W" & depth_cm == "20" ~ "b",
    site == "PTR" & zone == "W" & depth_cm == "15" ~ "",
    site == "PTR" & zone == "W" & depth_cm == "10" ~ "",
    site == "PTR" & zone == "W" & depth_cm == "5" ~ "",
    site == "OWC" & zone == "UP" & depth_cm == "20" ~ "b",
    site == "OWC" & zone == "UP" & depth_cm == "15" ~ "",
    site == "OWC" & zone == "UP" & depth_cm == "10" ~ "ab",
    site == "OWC" & zone == "UP" & depth_cm == "5" ~ "a",
    site == "OWC" & zone == "TR" & depth_cm == "20" ~ "",
    site == "OWC" & zone == "TR" & depth_cm == "15" ~ "",
    site == "OWC" & zone == "TR" & depth_cm == "10" ~ "",
    site == "OWC" & zone == "TR" & depth_cm == "5" ~ "",
    site == "OWC" & zone == "W" & depth_cm == "20" ~ "b",
    site == "OWC" & zone == "W" & depth_cm == "15" ~ "",
    site == "OWC" & zone == "W" & depth_cm == "10" ~ "b",
    site == "OWC" & zone == "W" & depth_cm == "5" ~ "a",
    TRUE ~ NA_character_
  ))


