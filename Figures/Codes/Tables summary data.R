
df_troll %>%
  filter(datetime > "2022-03-01 11:00:00",
         datetime < "2022-11-01 11:00:00",
         flag_out_of_water == "FALSE",
         specific_conductivity > 30,
         site != "GWI", site != "MSM") %>%
  group_by(site, location) %>% 
  summarize(median = median(Eh),
            lower_quartile = quantile(Eh, 0.25),
            upper_quartile = quantile(Eh, 0.75),
            mean = mean(Eh),
            sd = sd(Eh))->Eh

df_troll %>%
  filter(datetime > "2022-03-01 11:00:00",
         datetime < "2022-11-01 11:00:00",
         flag_out_of_water == "FALSE",
         specific_conductivity > 30,
         site != "GWI", site != "MSM") %>%
  group_by(site, location) %>% 
  summarize(median = median(wl_below_surface_m),
            lower_quartile = quantile(wl_below_surface_m, 0.25),
            upper_quartile = quantile(wl_below_surface_m, 0.75),
            mean = mean(wl_below_surface_m),
            sd = sd(wl_below_surface_m))->GWL

df_troll %>%
  filter(datetime > "2022-03-01 11:00:00",
         datetime < "2022-11-01 11:00:00",
         flag_out_of_water == "FALSE",
         specific_conductivity > 30,
         site != "GWI", site != "MSM") %>%
  group_by(site, location) %>% 
  summarize(median = median(p_h),
            lower_quartile = quantile(p_h, 0.25),
            upper_quartile = quantile(p_h, 0.75),
            mean = mean(p_h),
            sd = sd(p_h))->pH

df_troll %>%
  filter(datetime > "2022-03-01 11:00:00",
         datetime < "2022-11-01 11:00:00",
         flag_out_of_water == "FALSE",
         specific_conductivity > 30,
         site != "GWI", site != "MSM") %>%
  group_by(site, location) %>% 
  summarize(median = median(specific_conductivity),
            lower_quartile = quantile(specific_conductivity, 0.25),
            upper_quartile = quantile(specific_conductivity, 0.75),
            mean = mean(specific_conductivity),
            sd = sd(specific_conductivity))->SPC

df_troll %>%
  filter(datetime > "2022-03-01 11:00:00",
         datetime < "2022-11-01 11:00:00",
         flag_out_of_water == "FALSE",
         specific_conductivity > 30,
         site != "GWI", site != "MSM") %>%
  group_by(site, location) %>% 
  summarize(median = median(rdo_concen),
            lower_quartile = quantile(rdo_concen, 0.25),
            upper_quartile = quantile(rdo_concen, 0.75),
            mean = mean(rdo_concen),
            sd = sd(rdo_concen))->DO

df_troll %>%
  filter(datetime > "2022-03-01 11:00:00",
         datetime < "2022-11-01 11:00:00",
         flag_out_of_water == "FALSE",
         specific_conductivity > 30,
         site != "GWI", site != "MSM") %>%
  group_by(site, location) %>% 
  summarize(median = median(temperature),
            lower_quartile = quantile(temperature, 0.25),
            upper_quartile = quantile(temperature, 0.75),
            mean = mean(temperature),
            sd = sd(temperature))->Tem

print(Eh, n=100)
print(GWL, n=100)
print(DO, n=100)
print(pH, n=100)
print(SPC, n=100)
print(Tem, n=100)




df_troll %>%
  filter(datetime > "2022-03-01 11:00:00",
         datetime < "2022-11-01 11:00:00",
         flag_out_of_water == "FALSE",
         specific_conductivity > 30,
         site != "GWI", site != "MSM") %>%
  group_by(site, location, month2) %>% 
  summarize(median = median(Eh),
            lower_quartile = quantile(Eh, 0.25),
            upper_quartile = quantile(Eh, 0.75),
            mean = mean(Eh),
            sd = sd(Eh))->Eh

df_troll %>%
  filter(datetime > "2022-03-01 11:00:00",
         datetime < "2022-11-01 11:00:00",
         flag_out_of_water == "FALSE",
         specific_conductivity > 30,
         site != "GWI", site != "MSM") %>%
  group_by(site, location, month2) %>% 
  summarize(median = median(wl_below_surface_m),
            lower_quartile = quantile(wl_below_surface_m, 0.25),
            upper_quartile = quantile(wl_below_surface_m, 0.75),
            mean = mean(wl_below_surface_m),
            sd = sd(wl_below_surface_m))->GWL

df_troll %>%
  filter(datetime > "2022-03-01 11:00:00",
         datetime < "2022-11-01 11:00:00",
         flag_out_of_water == "FALSE",
         specific_conductivity > 30,
         site != "GWI", site != "MSM") %>%
  group_by(site, location, month2) %>% 
  summarize(median = median(p_h),
            lower_quartile = quantile(p_h, 0.25),
            upper_quartile = quantile(p_h, 0.75),
            mean = mean(p_h),
            sd = sd(p_h))->pH

df_troll %>%
  filter(datetime > "2022-03-01 11:00:00",
         datetime < "2022-11-01 11:00:00",
         flag_out_of_water == "FALSE",
         specific_conductivity > 30,
         site != "GWI", site != "MSM") %>%
  group_by(site, location, month2) %>% 
  summarize(median = median(specific_conductivity),
            lower_quartile = quantile(specific_conductivity, 0.25),
            upper_quartile = quantile(specific_conductivity, 0.75),
            mean = mean(specific_conductivity),
            sd = sd(specific_conductivity))->SPC

df_troll %>%
  filter(datetime > "2022-03-01 11:00:00",
         datetime < "2022-11-01 11:00:00",
         flag_out_of_water == "FALSE",
         specific_conductivity > 30,
         site != "GWI", site != "MSM") %>%
  group_by(site, location, month2) %>% 
  summarize(median = median(rdo_concen),
            lower_quartile = quantile(rdo_concen, 0.25),
            upper_quartile = quantile(rdo_concen, 0.75),
            mean = mean(rdo_concen),
            sd = sd(rdo_concen))->DO

print(Eh, n=100)
print(GWL, n=100)
print(DO, n=100)
print(pH, n=100)
print(SPC, n=100)
