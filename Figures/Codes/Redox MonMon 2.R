########################
### Graph REDOX MOnMOn
#######
setwd("C:/Users/fsilva/OneDrive - University of Toledo/Documents/Fausto UT/COMPASS/Manuscript 1 Redox/Figures/Codes")
require(pacman)
p_load(dplyr, tidyverse,tidypaleo)

Redox<-read_csv("Redox_SWAP2.csv")
head(Redox)
data<-clean_names(Redox)
head(data)

data_complete <- data[complete.cases(data$ehf_m_v), ]      # Remove incomplete rows where Eh == NA
data_complete -> df  

head(df)
str(df)
df$Depth2<-as.factor(df$depth_cm)
df$zone<-as.factor(df$zone)
df$Site<-as.factor(df$site)

df$date <- as.Date(df$date, "%m/%d/%Y")


####precisa arrumar locations? graphs location order
df$zone <- factor(df$zone, levels=c('Up', 'Tr', 'We'))

df <- df %>%
  mutate(Eh = ehf_m_v - (0.718 * tmean) + 224.41)




df %>% 
  filter(date>"2022-03-21", 
         date<"2022-12-01" ,
         site == "OWC",
         zone == "We") %>% 
                  summarize(mean = mean(Eh), 
                  median = median(Eh), 
                  Q1 = quantile(Eh, 0.25),
                  Q3 = quantile(Eh, 0.75),
                  max = max(Eh), 
                  min = min(Eh))

head(df_su)
df_su$Depth<-as.numeric(df_su$Depth)

df %>%
  ggplot(aes(x=Depth, y=Eh, group=zone, color=zone)) +
  geom_line() +
  geom_point(size=3, alpha=1)+
  geom_errorbar(aes(ymin=Eh-df_su$se,
                    ymax=Eh+df_su$se), width=.3,
  )+
  facet_wrap(~ Site,
             #scales = "free_x",
             strip.position = "bottom" # "top" or "bottom"
  )+
  scale_y_continuous(position="right")+
  labs(title="LE - Soil mean (SE) Redox potential profile", y="Eh (mV)", x = "Depth (cm)")+
  scale_color_manual(values=c('orange', 'grey60','black'))+
  coord_flip()+ #flip x and y defined ggplot(aes())
  scale_x_reverse()+
  theme(strip.background = element_rect(fill = "white"))
