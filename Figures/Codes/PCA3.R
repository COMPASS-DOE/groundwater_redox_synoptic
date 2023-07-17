# (0) references:
#https://bayesbaes.github.io/2021/01/28/PCA-tutorial.html


################################################################################
## (1) Set up

## packages
require(pacman)
p_load(tidyverse,dplyr, palmerpenguins, ggfortify, corrplot, paletteer, corrplot, psych, caret)

## working directory
setwd("C:/Users/fsilva/OneDrive - University of Toledo/Documents/Fausto UT/COMPASS/Manuscript 1 Redox/Figures/Codes")

################################################################################

## (2) Data frame preparation

## bring df
df_troll<-read_csv("trolls.csv")
str(df_troll)

## QAQC
df_troll<-df_troll %>% 
  mutate(flag_out_of_water = ifelse((wl_below_surface_m < ((ground_to_sensor_cm/100)* -0.98)|
                                       (site=="CRC" & datetime>"2022-07-19 00:00:00" & datetime<"2022-07-28 00:00:00")| 
                                       (site=="OWC" & datetime>"2022-07-21 08:00:00" & datetime<"2022-07-31 08:00:00")| 
                                       (site=="PTR" & location =="UP" & datetime>"2022-08-02 08:00:00" & datetime<"2022-08-12 08:00:00")|
                                       (site=="PTR" & location !="UP" & datetime>"2022-07-19 08:00:00" & datetime<"2022-07-29 08:00:00")
  ),"TRUE","FALSE"
  )
  )

#Change OWC UP to WTE, order sites and locations
df_troll <- df_troll %>% 
  mutate(location = ifelse(site == "OWC" & 
                             location == "UP", 
                           "WTE", location))

# create a new factor variable that combines site and location
df_troll$site_location <- interaction(df_troll$site, df_troll$location, sep = "_")

# Identify the character variables in the data frame
char_cols <- sapply(df_troll, is.character)

# Convert character variables to factors
df_troll[char_cols] <- lapply(df_troll[char_cols], factor)

# reorder
df_troll_new <- df_troll %>% 
  select(where(is.factor), everything())

## selecting data time range df_troll to df
df_troll_new%>%
  filter(datetime>"2022-03-01 11:00:00", 
         datetime<"2022-11-01 11:00:00" ,
         flag_out_of_water=="FALSE",
         # specific_conductivity>30,
         site!="MSM", site!="GWI",
  )->df

str(df)

##################################################################################

# create a correlation matrix by groups
library(corrplot)
corr_mat <- cor(df[,4:22])
corrplot(corr_mat, method = "circle", type = "lower", order = "hclust",
         tl.col = "black", tl.srt = 45, addCoef.col = "black",
         cl.cex = 0.6, cl.pos = "n", number.cex = 0.5, 
         groups = df$site_location, group.cex = 0.8, 
         col = colorRampPalette(c("#BB4444", "#EE9988", "#FFFFFF", "#77AADD", "#4477AA"))(200))


################################################################################

## (3) Standartize data

## create df_std standartizing numerical data from df

#Select only the numerical columns
df_num <- df[, sapply(df, is.numeric)]

#Standardize the numerical columns
df_num_std <- scale(df_num)

#Create a new data frame with standardized data
df_std <- cbind(df[, !sapply(df, is.numeric)], as.data.frame(df_num_std))

#Rename the columns to the original names
#colnames(df_num_std) <- paste0(colnames(df_num_std), "_std")
colnames(df_std) <- c(colnames(df[, !sapply(df, is.numeric)]), colnames(df_num_std))

# View the new data frame
head(df_std)


################################################################################

## (3) Check for correlation: Correlation matrix All

#need to choose variables to use in the PCA, based on corr matrix
ncol(df_std)
str(df_std)
df_num <- df_std[, 5:23] 

#df structure
str(df_num)

# Calculate correlation matrix
corr_matrix1 <- cor(df_num)

# Visualize correlation matrix
corrplot(corr_matrix1, method = "ellipse", type = "upper", 
         sig.level = 0.001, insig = "blank",
         tl.col="black")

# Find highly correlated variables and Eliminate highly correlated variables
#highly_correlated <- findCorrelation(corr_matrix, cutoff = 0.8)
#df_num_reduced <- df_num[,-highly_correlated]
#corr_matrix <- cor(df_num_reduced)
#corrplot(corr_matrix, method = "ellipse", type = "upper", 
  #       sig.level = 0.001, insig = "blank",
  #       tl.col="black")


#choose variables for PCA

newdata <- df[c(
  "datetime",
  "site",
  #"season",
  "location",
  #"water",
  #"actual_conductivity",   
  #"density_gcm3_cor",      
  #"depth",                
  #"flag_out_of_water",
  #"ground_to_sensor_cm",
  #"location",
  "p_h",
  "p_h_orp",
  #"pressure_mbar",
  #"pressure_psi",
  #"pressurehead_m",
  "rdo_concen",
  #"rdo_part_pressure",
  #"rdo_perc_sat",
  "resistivity",
  #"salinity",
  "specific_conductivity",
  #"tds",
  "temperature",
  #"water",
  #"water_density",
  "wl_below_surface_m")]

#newdata <- read_csv("newdata.csv")
#newdata<-newdata %>% mutate(paste = paste(site,"-",location))
#newdata$paste <- as.factor(newdata$paste)

str(newdata)
#newdata$water <- as.factor(newdata$water)

ncol(newdata)

pca_values <- prcomp(newdata[, c(3:9)], center = TRUE, scale = TRUE)

autoplot(pca_values, loadings = TRUE, loadings.label = TRUE)

autoplot(pca_values, loadings = TRUE, loadings.label = TRUE,
         data = newdata, colour = 'location')

pca_points <- 
  # first convert the pca results to a tibble
  as_tibble(pca_values$x) %>% 
  # now we'll add the penguins data
  bind_cols(newdata)

head(pca_points)

pc1_mod <- 
  lm(PC1 ~ location, pca_points)

summary(pc1_mod)

newdata$location

#write_csv(newdata, "newdata.csv")


  ggplot(pca_points, aes(x = PC1, y = PC2)) +
  geom_point(aes(colour = location, size=2)) +
  theme_light() +
  scale_color_manual(values = c("forestgreen", 
                                "maroon", 
                                #"darkorchid4", 
                                "cornflowerblue"))->basic_plot

basic_plot 
  

# first create a dataframe to extract the convex hull points
pca_hull <- 
  pca_points %>% 
  group_by(location) %>% 
  slice(chull(PC1, PC2))

# now, we'll just continue to build on our ggplot object
chull_plot <- 
  basic_plot +
  geom_polygon(data = pca_hull,
               aes(fill = location,
                   colour = location),
               alpha = 0.3,
               show.legend = FALSE)


pca_load <- 
  as_tibble(pca_values$rotation, rownames = 'variable') %>% 
  # we can rename the variables so they look nicer on the figure
  mutate(variable = dplyr::recode(variable,
                                  'p_h' = 'pH',
                                  'p_h_orp' = 'Eh',
                                  'rdo_concen' = 'DO',
                                  'resistivity' = 'R',
                                  'specific_conductivity' = 'SPC',
                                  'temperature' = 'T',
                                  'wl_below_surface_m' = 'GWL'))







basic_plot +
  geom_segment(data = pca_load, 
               aes(x = 0, y = 0, 
                   xend = PC1*7,
                   yend = PC2*7),
               arrow = arrow(length = unit(1/2, 'picas'))) +
  annotate('text', x = (pca_load$PC1*8), y = (pca_load$PC2*8),
           label = pca_load$variable,
           size = 5) +
  ggtitle("CRC") +
  xlab("PC1 (46.01%)") + ylab("PC2(21.3%)")+
  theme(text = element_text(size = 20))+
  guides(color = guide_legend(override.aes = list(size = 10)))






#PTR
######################################################
df_troll%>%
  filter(datetime>"2022-03-01 11:00:00", 
         datetime<"2022-11-01 11:00:00" ,
         flag_out_of_water=="FALSE",
         # specific_conductivity>30,
         site=="PTR")->dfPTR


newdata <- dfPTR[c(
  "datetime",
  #"site",
  #"season",
  "location",
  #"water",
  #"actual_conductivity",   
  #"density_gcm3_cor",      
  #"depth",                
  #"flag_out_of_water",
  #"ground_to_sensor_cm",
  #"location",
  "p_h",
  "p_h_orp",
  #"pressure_mbar",
  #"pressure_psi",
  #"pressurehead_m",
  "rdo_concen",
  #"rdo_part_pressure",
  #"rdo_perc_sat",
  "resistivity",
  #"salinity",
  "specific_conductivity",
  #"tds",
  "temperature",
  #"water",
  #"water_density",
  "wl_below_surface_m")]

#newdata <- read_csv("newdata.csv")
#newdata<-newdata %>% mutate(paste = paste(site,"-",location))
#newdata$paste <- as.factor(newdata$paste)

str(newdata)
#newdata$water <- as.factor(newdata$water)

ncol(newdata)

pca_values <- prcomp(newdata[, c(3:9)], center = TRUE, scale = TRUE)

autoplot(pca_values, loadings = TRUE, loadings.label = TRUE)

autoplot(pca_values, loadings = TRUE, loadings.label = TRUE,
         data = newdata, colour = 'location')

pca_points <- 
  # first convert the pca results to a tibble
  as_tibble(pca_values$x) %>% 
  # now we'll add the penguins data
  bind_cols(newdata)

head(pca_points)

pc1_mod <- 
  lm(PC1 ~ location, pca_points)

summary(pc1_mod)

newdata$location

#write_csv(newdata, "newdata.csv")


ggplot(pca_points, aes(x = PC1, y = PC2)) +
  geom_point(aes(colour = location, size=2)) +
  theme_light() +
  scale_color_manual(values = c("forestgreen", 
                                             "maroon", 
                                             #"darkorchid4", 
                                             "cornflowerblue"))->basic_plot
                                             
basic_plot 


# first create a dataframe to extract the convex hull points
pca_hull <- 
  pca_points %>% 
  group_by(location) %>% 
  slice(chull(PC1, PC2))

# now, we'll just continue to build on our ggplot object
chull_plot <- 
  basic_plot +
  geom_polygon(data = pca_hull,
               aes(fill = location,
                   colour = location),
               alpha = 0.3,
               show.legend = FALSE)


pca_load <- 
  as_tibble(pca_values$rotation, rownames = 'variable') %>% 
  # we can rename the variables so they look nicer on the figure
  mutate(variable = dplyr::recode(variable,
                                  'p_h' = 'pH',
                                  'p_h_orp' = 'Eh',
                                  'rdo_concen' = 'DO',
                                  'resistivity' = 'R',
                                  'specific_conductivity' = 'SPC',
                                  'temperature' = 'T',
                                  'wl_below_surface_m' = 'GWL'))







basic_plot +
  geom_segment(data = pca_load, 
               aes(x = 0, y = 0, 
                   xend = PC1*7,
                   yend = PC2*7),
               arrow = arrow(length = unit(1/2, 'picas'))) +
  annotate('text', x = (pca_load$PC1*8), y = (pca_load$PC2*8),
           label = pca_load$variable,
           size = 5) +
  ggtitle("PTR") +
  xlab("PC1 (46.56%)") + ylab("PC2(17.47%)")+
  theme(text = element_text(size = 20))+
  guides(color = guide_legend(override.aes = list(size = 10)))




#OWC
######################################################
df_troll%>%
  filter(datetime>"2022-03-01 11:00:00", 
         datetime<"2022-11-01 11:00:00" ,
         flag_out_of_water=="FALSE",
         # specific_conductivity>30,
         site=="OWC")->dfOWC


newdata <- dfOWC[c(
  "datetime",
  #"site",
  #"season",
  "location",
  #"water",
  #"actual_conductivity",   
  #"density_gcm3_cor",      
  #"depth",                
  #"flag_out_of_water",
  #"ground_to_sensor_cm",
  #"location",
  "p_h",
  "p_h_orp",
  #"pressure_mbar",
  #"pressure_psi",
  #"pressurehead_m",
  "rdo_concen",
  #"rdo_part_pressure",
  #"rdo_perc_sat",
  "resistivity",
  #"salinity",
  "specific_conductivity",
  #"tds",
  "temperature",
  #"water",
  #"water_density",
  "wl_below_surface_m")]

#newdata <- read_csv("newdata.csv")
#newdata<-newdata %>% mutate(paste = paste(site,"-",location))
#newdata$paste <- as.factor(newdata$paste)

str(newdata)
#newdata$water <- as.factor(newdata$water)

ncol(newdata)

pca_values <- prcomp(newdata[, c(3:9)], center = TRUE, scale = TRUE)

autoplot(pca_values, loadings = TRUE, loadings.label = TRUE)

autoplot(pca_values, loadings = TRUE, loadings.label = TRUE,
         data = newdata, colour = 'location')

pca_points <- 
  # first convert the pca results to a tibble
  as_tibble(pca_values$x) %>% 
  # now we'll add the penguins data
  bind_cols(newdata)

head(pca_points)

pc1_mod <- 
  lm(PC1 ~ location, pca_points)

summary(pc1_mod)

newdata$location

#write_csv(newdata, "newdata.csv")


ggplot(pca_points, aes(x = PC1, y = PC2)) +
  geom_point(aes(colour = location, size=2)) +
  theme_light() +
  scale_color_manual(values = c(#"forestgreen", 
                                             "maroon", 
                                             "darkorchid4", 
                                             "cornflowerblue"))->basic_plot
                                             
basic_plot 


# first create a dataframe to extract the convex hull points
pca_hull <- 
  pca_points %>% 
  group_by(location) %>% 
  slice(chull(PC1, PC2))

# now, we'll just continue to build on our ggplot object
chull_plot <- 
  basic_plot +
  geom_polygon(data = pca_hull,
               aes(fill = location,
                   colour = location),
               alpha = 0.3,
               show.legend = FALSE)


pca_load <- 
  as_tibble(pca_values$rotation, rownames = 'variable') %>% 
  # we can rename the variables so they look nicer on the figure
  mutate(variable = dplyr::recode(variable,
                                  'p_h' = 'pH',
                                  'p_h_orp' = 'Eh',
                                  'rdo_concen' = 'DO',
                                  'resistivity' = 'R',
                                  'specific_conductivity' = 'SPC',
                                  'temperature' = 'T',
                                  'wl_below_surface_m' = 'GWL'))







basic_plot +
  geom_segment(data = pca_load, 
               aes(x = 0, y = 0, 
                   xend = PC1*7,
                   yend = PC2*7),
               arrow = arrow(length = unit(1/2, 'picas'))) +
  annotate('text', x = (pca_load$PC1*8), y = (pca_load$PC2*8),
           label = pca_load$variable,
           size = 5) +
  ggtitle("OWC") +
  xlab("PC1 (48.39%)") + ylab("PC2(25.28%)")+
  theme(text = element_text(size = 20))+
  guides(color = guide_legend(override.aes = list(size = 10)))



###################################################################

#UP
######################################################
df_troll%>%
  filter(datetime>"2022-03-01 11:00:00", 
         datetime<"2022-11-01 11:00:00" ,
         flag_out_of_water=="FALSE",
         # specific_conductivity>30,
         site!="GWI", site!="MSM",
         location=="UP")->df_UP

df_UP$site

newdata <- df_UP[c(
  "datetime",
  "site",
  #"season",
  "location",
  #"water",
  #"actual_conductivity",   
  #"density_gcm3_cor",      
  #"depth",                
  #"flag_out_of_water",
  #"ground_to_sensor_cm",
  #"location",
  "p_h",
  "p_h_orp",
  #"pressure_mbar",
  #"pressure_psi",
  #"pressurehead_m",
  "rdo_concen",
  #"rdo_part_pressure",
  #"rdo_perc_sat",
  "resistivity",
  #"salinity",
  "specific_conductivity",
  #"tds",
  "temperature",
  #"water",
  #"water_density",
  "wl_below_surface_m")]

#newdata <- read_csv("newdata.csv")
#newdata<-newdata %>% mutate(paste = paste(site,"-",location))
#newdata$paste <- as.factor(newdata$paste)

str(newdata)
#newdata$water <- as.factor(newdata$water)
head(newdata)
ncol(newdata)

pca_values <- prcomp(newdata[, c(4:10)], center = TRUE, scale = TRUE)

autoplot(pca_values, loadings = TRUE, loadings.label = TRUE)

autoplot(pca_values, loadings = TRUE, loadings.label = TRUE,
         data = newdata, colour = 'site')

pca_points <- 
  # first convert the pca results to a tibble
  as_tibble(pca_values$x) %>% 
  # now we'll add the penguins data
  bind_cols(newdata)

head(pca_points)

pc1_mod <- 
  lm(PC1 ~ site, pca_points)

summary(pc1_mod)

newdata$location

#write_csv(newdata, "newdata.csv")


ggplot(pca_points, aes(x = PC1, y = PC2)) +
  geom_point(aes(colour = site, size=0.5),alpha=0.1) +
  theme_light() +
  scale_color_manual(values = c("forestgreen", 
                                             #"maroon", 
                                             #"darkorchid4", 
                                             "cornflowerblue"))->basic_plot
                                             
basic_plot 


# first create a dataframe to extract the convex hull points
pca_hull <- 
  pca_points %>% 
  group_by(location) %>% 
  slice(chull(PC1, PC2))

# now, we'll just continue to build on our ggplot object
chull_plot <- 
  basic_plot +
  geom_polygon(data = pca_hull,
               aes(fill = location,
                   colour = location),
               alpha = 0.3,
               show.legend = FALSE)


pca_load <- 
  as_tibble(pca_values$rotation, rownames = 'variable') %>% 
  # we can rename the variables so they look nicer on the figure
  mutate(variable = dplyr::recode(variable,
                                  'p_h' = 'pH',
                                  'p_h_orp' = 'Eh',
                                  'rdo_concen' = 'DO',
                                  'resistivity' = 'R',
                                  'specific_conductivity' = 'SPC',
                                  'temperature' = 'T',
                                  'wl_below_surface_m' = 'GWL'))







basic_plot +
  geom_segment(data = pca_load, 
               aes(x = 0, y = 0, 
                   xend = PC1*7,
                   yend = PC2*7),
               arrow = arrow(length = unit(1/2, 'picas'))) +
  annotate('text', x = (pca_load$PC1*8), y = (pca_load$PC2*8),
           label = pca_load$variable,
           size = 5) +
  ggtitle("Upland") +
  xlab("PC1 (46.79%)") + ylab("PC2(25.02%)")+
  theme(text = element_text(size = 20))+
  guides(color = guide_legend(override.aes = list(size = 10)))+
  guides(colour = guide_legend(override.aes = list(alpha = 1)))
  


###################################################################

#TR
######################################################
df_troll%>%
  filter(datetime>"2022-03-01 11:00:00", 
         datetime<"2022-11-01 11:00:00" ,
         flag_out_of_water=="FALSE",
         # specific_conductivity>30,
         site!="GWI", site!="MSM",
         location=="TR")->df_TR

df_TR$site

newdata <- df_TR[c(
  "datetime",
  "site",
  #"season",
  "location",
  #"water",
  #"actual_conductivity",   
  #"density_gcm3_cor",      
  #"depth",                
  #"flag_out_of_water",
  #"ground_to_sensor_cm",
  #"location",
  "p_h",
  "p_h_orp",
  #"pressure_mbar",
  #"pressure_psi",
  #"pressurehead_m",
  "rdo_concen",
  #"rdo_part_pressure",
  #"rdo_perc_sat",
  "resistivity",
  #"salinity",
  "specific_conductivity",
  #"tds",
  "temperature",
  #"water",
  #"water_density",
  "wl_below_surface_m")]

#newdata <- read_csv("newdata.csv")
#newdata<-newdata %>% mutate(paste = paste(site,"-",location))
#newdata$paste <- as.factor(newdata$paste)

str(newdata)
#newdata$water <- as.factor(newdata$water)
head(newdata)
ncol(newdata)

pca_values <- prcomp(newdata[, c(4:10)], center = TRUE, scale = TRUE)

autoplot(pca_values, loadings = TRUE, loadings.label = TRUE)

autoplot(pca_values, loadings = TRUE, loadings.label = TRUE,
         data = newdata, colour = 'site')

pca_points <- 
  # first convert the pca results to a tibble
  as_tibble(pca_values$x) %>% 
  # now we'll add the penguins data
  bind_cols(newdata)

head(pca_points)

pc1_mod <- 
  lm(PC1 ~ site, pca_points)

summary(pc1_mod)

newdata$location

#write_csv(newdata, "newdata.csv")


ggplot(pca_points, aes(x = PC1, y = PC2)) +
  geom_point(aes(colour = site, size=0.5),alpha=0.1) +
  theme_light() +
  scale_color_manual(values = c("forestgreen", 
                                             "maroon", 
                                             #"darkorchid4", 
                                             "cornflowerblue"))->basic_plot
                                             
basic_plot 


# first create a dataframe to extract the convex hull points
pca_hull <- 
  pca_points %>% 
  group_by(location) %>% 
  slice(chull(PC1, PC2))

# now, we'll just continue to build on our ggplot object
chull_plot <- 
  basic_plot +
  geom_polygon(data = pca_hull,
               aes(fill = location,
                   colour = location),
               alpha = 0.3,
               show.legend = FALSE)


pca_load <- 
  as_tibble(pca_values$rotation, rownames = 'variable') %>% 
  # we can rename the variables so they look nicer on the figure
  mutate(variable = dplyr::recode(variable,
                                  'p_h' = 'pH',
                                  'p_h_orp' = 'Eh',
                                  'rdo_concen' = 'DO',
                                  'resistivity' = 'R',
                                  'specific_conductivity' = 'SPC',
                                  'temperature' = 'T',
                                  'wl_below_surface_m' = 'GWL'))







basic_plot +
  geom_segment(data = pca_load, 
               aes(x = 0, y = 0, 
                   xend = PC1*7,
                   yend = PC2*7),
               arrow = arrow(length = unit(1/2, 'picas'))) +
  annotate('text', x = (pca_load$PC1*8), y = (pca_load$PC2*8),
           label = pca_load$variable,
           size = 5) +
  ggtitle("Transition") +
  xlab("PC1 (45.2%)") + ylab("PC2(24.22%)")+
  theme(text = element_text(size = 20))+
  guides(color = guide_legend(override.aes = list(size = 10)))+
  guides(colour = guide_legend(override.aes = list(alpha = 1)))


##############################


#W
######################################################
df_troll%>%
  filter(datetime>"2022-03-01 11:00:00", 
         datetime<"2022-11-01 11:00:00" ,
         flag_out_of_water=="FALSE",
         # specific_conductivity>30,
         site!="GWI", site!="MSM",
         location=="W")->df_W



newdata <- df_W[c(
  "datetime",
  "site",
  #"season",
  "location",
  #"water",
  #"actual_conductivity",   
  #"density_gcm3_cor",      
  #"depth",                
  #"flag_out_of_water",
  #"ground_to_sensor_cm",
  #"location",
  "p_h",
  "p_h_orp",
  #"pressure_mbar",
  #"pressure_psi",
  #"pressurehead_m",
  "rdo_concen",
  #"rdo_part_pressure",
  #"rdo_perc_sat",
  "resistivity",
  #"salinity",
  "specific_conductivity",
  #"tds",
  "temperature",
  #"water",
  #"water_density",
  "wl_below_surface_m")]

#newdata <- read_csv("newdata.csv")
#newdata<-newdata %>% mutate(paste = paste(site,"-",location))
#newdata$paste <- as.factor(newdata$paste)

str(newdata)
#newdata$water <- as.factor(newdata$water)
head(newdata)
ncol(newdata)

pca_values <- prcomp(newdata[, c(4:10)], center = TRUE, scale = TRUE)

autoplot(pca_values, loadings = TRUE, loadings.label = TRUE)

autoplot(pca_values, loadings = TRUE, loadings.label = TRUE,
         data = newdata, colour = 'site')

pca_points <- 
  # first convert the pca results to a tibble
  as_tibble(pca_values$x) %>% 
  # now we'll add the penguins data
  bind_cols(newdata)

head(pca_points)

pc1_mod <- 
  lm(PC1 ~ site, pca_points)

summary(pc1_mod)

newdata$location

#write_csv(newdata, "newdata.csv")


ggplot(pca_points, aes(x = PC1, y = PC2)) +
  geom_point(aes(colour = site, size=0.1),alpha=0.1) +
  theme_light() +
  scale_color_manual(values = c("forestgreen", 
                                "maroon", 
                                #"darkorchid4", 
                                "cornflowerblue"))->basic_plot
                                             
basic_plot 


# first create a dataframe to extract the convex hull points
pca_hull <- 
  pca_points %>% 
  group_by(location) %>% 
  slice(chull(PC1, PC2))

# now, we'll just continue to build on our ggplot object
chull_plot <- 
  basic_plot +
  geom_polygon(data = pca_hull,
               aes(fill = location,
                   colour = location),
               alpha = 0.3,
               show.legend = FALSE)


pca_load <- 
  as_tibble(pca_values$rotation, rownames = 'variable') %>% 
  # we can rename the variables so they look nicer on the figure
  mutate(variable = dplyr::recode(variable,
                                  'p_h' = 'pH',
                                  'p_h_orp' = 'Eh',
                                  'rdo_concen' = 'DO',
                                  'resistivity' = 'R',
                                  'specific_conductivity' = 'SPC',
                                  'temperature' = 'T',
                                  'wl_below_surface_m' = 'GWL'))







basic_plot +
  geom_segment(data = pca_load, 
               aes(x = 0, y = 0, 
                   xend = PC1*7,
                   yend = PC2*7),
               arrow = arrow(length = unit(1/2, 'picas'))) +
  annotate('text', x = (pca_load$PC1*8), y = (pca_load$PC2*8),
           label = pca_load$variable,
           size = 5) +
  ggtitle("Wetland") +
  xlab("PC1 (35.68%)") + ylab("PC2(21.21%)")+
  theme(text = element_text(size = 20))+
  guides(color = guide_legend(override.aes = list(size = 3)))+
  guides(colour = guide_legend(override.aes = list(alpha = 1)))



#WTE
######################################################
df_troll%>%
  filter(datetime>"2022-03-21 11:00:00", 
         datetime<"2022-11-01 11:00:00" ,
         flag_out_of_water=="FALSE",
         # specific_conductivity>30,
         site!="GWI", site!="MSM",
         location=="WTE")->df_WTE



newdata <- df_WTE[c(
  "datetime",
  "site",
  #"season",
  "location",
  #"water",
  #"actual_conductivity",   
  #"density_gcm3_cor",      
  #"depth",                
  #"flag_out_of_water",
  #"ground_to_sensor_cm",
  #"location",
  "p_h",
  "p_h_orp",
  #"pressure_mbar",
  #"pressure_psi",
  #"pressurehead_m",
  "rdo_concen",
  #"rdo_part_pressure",
  #"rdo_perc_sat",
  "resistivity",
  #"salinity",
  "specific_conductivity",
  #"tds",
  "temperature",
  #"water",
  #"water_density",
  "wl_below_surface_m")]

#newdata <- read_csv("newdata.csv")
#newdata<-newdata %>% mutate(paste = paste(site,"-",location))
#newdata$paste <- as.factor(newdata$paste)

str(newdata)
#newdata$water <- as.factor(newdata$water)
head(newdata)
ncol(newdata)

pca_values <- prcomp(newdata[, c(4:10)], center = TRUE, scale = TRUE)

autoplot(pca_values, loadings = TRUE, loadings.label = TRUE)

autoplot(pca_values, loadings = TRUE, loadings.label = TRUE,
         data = newdata, colour = 'site')

pca_points <- 
  # first convert the pca results to a tibble
  as_tibble(pca_values$x) %>% 
  # now we'll add the penguins data
  bind_cols(newdata)

head(pca_points)

pc1_mod <- 
  lm(PC1 ~ site, pca_points)

summary(pc1_mod)

newdata$location

#write_csv(newdata, "newdata.csv")


ggplot(pca_points, aes(x = PC1, y = PC2)) +
  geom_point(aes(colour = site, 
                 size=0.1),alpha=0.1) +
  theme_light() +
  scale_color_manual(values = c("forestgreen", 
                                             "maroon", 
                                             #"darkorchid4", 
                                             "cornflowerblue"))->basic_plot
                                             
basic_plot 


# first create a dataframe to extract the convex hull points
pca_hull <- 
  pca_points %>% 
  group_by(location) %>% 
  slice(chull(PC1, PC2))

# now, we'll just continue to build on our ggplot object
chull_plot <- 
  basic_plot +
  geom_polygon(data = pca_hull,
               aes(fill = location,
                   colour = 'blue'),
               alpha = 0.3,
               show.legend = FALSE)


pca_load <- 
  as_tibble(pca_values$rotation, rownames = 'variable') %>% 
  # we can rename the variables so they look nicer on the figure
  mutate(variable = dplyr::recode(variable,
                                  'p_h' = 'pH',
                                  'p_h_orp' = 'Eh',
                                  'rdo_concen' = 'DO',
                                  'resistivity' = 'R',
                                  'specific_conductivity' = 'SPC',
                                  'temperature' = 'T',
                                  'wl_below_surface_m' = 'GWL'))







basic_plot +
  geom_segment(data = pca_load, 
               aes(x = 0, y = 0, 
                   xend = PC1*7,
                   yend = PC2*7),
               arrow = arrow(length = unit(1/2, 'picas'))) +
  annotate('text', x = (pca_load$PC1*8), y = (pca_load$PC2*8),
           label = pca_load$variable,
           size = 5) +
  ggtitle("WTE") +
  xlab("PC1 (50.8%)") + ylab("PC2(24.97%)")+
  theme(text = element_text(size = 20))+
  guides(color = guide_legend(override.aes = list(size = 3)))+
  guides(colour = guide_legend(override.aes = list(alpha = 1)))


