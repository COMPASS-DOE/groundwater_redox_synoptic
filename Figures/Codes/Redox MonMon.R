########################
### Graph REDOX MOnMOn
#######

require(pacman)
p_load(dplyr, tidyverse,tidypaleo)

#Redox<-read_csv("Redox.csv")
head(Redox)

data<-Redox
data_complete <- data[complete.cases(data$Eh), ]      # Remove incomplete rows where Eh == NA
data_complete -> df  

head(df)
str(df)
df$Depth2<-as.factor(df$Depth)
df$zone<-as.factor(df$zone)
df$Site<-as.factor(df$Site)

####precisa arrumar locations? graphs location order
df$zone <- factor( df$zone, levels=c('Up', 'Tr', 'We'))

df %>%
  group_by(Site, zone, Depth) %>%
  summarise(mEh = mean(Eh),
            n = n(),
            sd = sd(Eh),
            se = sd(Eh)/sqrt(n),
            .groups="drop"
  )-> df_su

head(df_su)
df_su$Depth<-as.numeric(df_su$Depth)

df_su %>%
  ggplot(aes(x=Depth, y=mEh+202, group=zone, color=zone)) +
  geom_line() +
  geom_point(size=3, alpha=1)+
  geom_errorbar(aes(ymin=mEh+202-df_su$se,
                    ymax=mEh+202+df_su$se), width=.3,
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
