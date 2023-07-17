

Samples_Current <- read.csv("C:/Users/fsilva/OneDrive - University of Toledo/ESE Lab Stuff/People/Post Docs/Fausto/H2S/CSVs/Samples_Current.csv")

head(Samples_Current)
str(Samples_Current)
Samples_Current<-clean_names(Samples_Current)

data_complete <- Samples_Current[complete.cases(Samples_Current$s_conc_mmol_s_l_1), ]      # Remove incomplete rows where Eh == NA
data_complete -> df  
head(df)
str(df)

# set chracter as factor:
df$depth2<-as.factor(df$depth)
df$zone<-as.factor(df$zone)
df$site<-as.factor(df$site)

# set dates
df$date <- as.Date(df$date, "%m/%d/%Y")

# order zone
df$zone <- factor(df$zone, levels=c('UP', 'TR', 'W'))

# create a case 15 to make distance equal  
df <- df %>%
  mutate(depth3 = factor(depth2, levels = c("45","40","35","30","25","20","15","10","5")))

df$H2S<-as.numeric(df$s_conc_mmol_s_l_1)
  
    
# calculate mean and sd for the graph
  df %>%
    filter(campaign=="MonMon")%>%
    group_by(site, zone, depth3) %>%
    summarise(mEh = mean(H2S),
              n = n(),
              sd = sd(H2S),
              se = sd(H2S)/sqrt(n),
              .groups="drop"
    )-> df_su
  
  df_su %>%
    ggplot(aes(x=depth3, y=mEh, color=zone)) +
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
  
  
  
  df%>%
    filter(site!="MSM",
           H2S<20,
           depth3!="NA",
           depth!="5",
           zone!="UP")%>%
    ggplot(aes(x=depth3, y=H2S, fill=zone)) + 
    geom_boxplot() +
    scale_y_continuous(position="right")+
    coord_flip()+
    facet_wrap(~ site,#scales = "free_x",
               strip.position = "bottom")+
    labs(title="LE Pore Water", y="H2S (µmol-S/L)", x = "Depth (cm)")+
    scale_fill_manual(values=c("#d7ac08",
                               "#0072B2"))+
    theme(strip.background = element_rect(fill = "transparent", color="transparent"),
          text = element_text(size = 18),
          legend.margin = margin(t = -0.1, b = 0, unit = "cm"),
          legend.key = element_rect(colour = "transparent", fill = "white"))
  