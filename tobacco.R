library(tidyverse)
library(ggplot2)
library(readr)
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)
library(owidR)
library(maps)
library(viridis)
library(rgdal)
library(fs)
library(readr)
library(purrr)
library(ggpubr)

options(scipen = 999)

f1 <- read_csv('csv_files/adults-smoking-2007-2018.csv')
f2 <- read_csv('csv_files/age-dependency-ratio-of-working-age-population.csv')
f3 <- read_csv('csv_files/cancer-death-rates-by-age.csv')
f4 <- read_csv('csv_files/comparing-the-share-of-men-and-women-who-are-smoking.csv')
f5 <- read_csv('csv_files/hourly-earnings-male-vs-female.csv')
f6 <- read_csv('csv_files/share-deaths-smoking.csv')
f7 <- read_csv('csv_files/share-of-adults-who-smoke.csv')
f8 <- read_csv('csv_files/share-of-cancer-deaths-attributed-to-tobacco.csv')
f9 <- read_csv('csv_files/tobbaco_prod.csv')
f10 <- read_csv('csv_files/total-cancer-deaths-by-type.csv')
f11 <- read_csv('csv_files/smoking-deaths-1990-2017.csv')
f12 <- read_csv('csv_files/consumption-per-smoker-per-day (1).csv')

f1 %>% filter(Year...3 %in% c(1990:2019)) %>% arrange(Year...3) -> f1
f2 %>% filter(Year %in% c(1990:2019)) %>% arrange(Year) -> f2
f3 %>% filter(Year %in% c(1990:2019)) %>% arrange(Year) -> f3
f4 %>% filter(Year %in% c(1990:2019)) %>% arrange(Year) -> f4
f5 %>% filter(Year %in% c(1990:2019)) %>% arrange(Year) -> f5
f6 %>% filter(Year %in% c(1990:2019)) %>% arrange(Year) -> f6
f7 %>% filter(Year %in% c(1990:2019)) %>% arrange(Year) -> f7
f8 %>% filter(Year %in% c(1990:2019)) %>% arrange(Year) -> f8
f9 %>% filter(Year %in% c(1990:2019)) %>% arrange(Year) -> f9
f10 %>% filter(Year %in% c(1990:2019)) %>% arrange(Year) -> f10
f11 %>% filter(Year...3 %in% c(1990:2019)) %>% arrange(Year...3) -> f11
f12 %>% filter(Year %in% c(1990:2019)) %>% arrange(Year) -> f12

# coordinates of country
world_map <- map_data("world" , region = cc_data$region)

# world_map %>% write.csv("country_coordinates.csv")

# csv for continents and countries present in it
cc_data <- ne_countries(scale = "medium", continent = "asia", returnclass = "sf")

cc_data %>% 
  as.data.frame() %>% 
  select("name","iso_a3","continent") -> cc_data


names(cc_data)[1] <- 'region'
# cc_data %>% write.csv("continent_country_names.csv")

location_data <-  merge(x = world_map, y = cc_data, by = 'region') 
# location_data %>% write.csv("asia_coordinates.csv")

# download.file("http://thematicmapping.org/downloads/TM_WORLD_BORDERS_SIMPL-0.3.zip" ,
#               destfile="world_shape_file.zip")
# unzip("shape_file/world_shape_file.zip")

my_spdf <- readOGR( 
  dsn= paste0("shape_file/") , 
  layer="TM_WORLD_BORDERS_SIMPL-0.3",
  verbose=FALSE
)

names(my_spdf)[3] <- "iso_a3"
merge(x = location_data, y = my_spdf, by = 'iso_a3') -> locate_data

# names(world_map)[5] <- "Entity"
# map_plt <- merge(x = f4, y = f2, by = 'Entity')
# world_map_data <- merge(x = world_map, y = f6, by = 'Entity')
# names(world_map_data)

names(f6)[1] <- 'region'

world_map1 <- left_join(world_map,f6,by = 'region')
View(world_map1)
world_map1 <- world_map1 %>% filter(!is.na(world_map1$`Deaths - Cause: All causes - Risk: Smoking - OWID - Sex: Both - Age: Age-standardized (Percent)`))

names(f9)[1] <- 'region'
f9.1 <- f9 %>% select(region, Year, `Production (t)`)
left_join(world_map,f9.1,by = 'region') -> tobacco
tobacco1 <- tobacco %>% filter(!is.na(tobacco$`Production (t)`))

names(world_map1)[9] <- 'Deaths'

world_map1 %>% ggplot(aes(x = long, y = lat, group = group))+
  scale_fill_gradient(name = '% deaths due to smoking ', low = 'yellow', high = 'red', na.value = 'grey50')+
  geom_polygon(aes(fill = `Deaths - Cause: All causes - Risk: Smoking - OWID - Sex: Both - Age: Age-standardized (Percent)`), color = 'black')+
  theme(axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        rect = element_blank(),
        panel.border = element_rect(colour = "black", fill=NA, size=1))+
  ggtitle("Global Distribution of Deaths due to Smoke")
################################################ 
region.lab.data <- world_map1 %>%
  group_by(region) %>%
  summarise(long = mean(long), lat = mean(lat))

 
ggplot(world_map1, aes(x = long, y = lat)) +
  geom_polygon(aes( group = group, fill = Deaths))+
  geom_text(aes(label = region), data = region.lab.data,  size = 3, hjust = 0.5)+
  scale_fill_gradient(name = '% deaths due to smoking ', low = 'light grey', high = '#d46053', na.value = 'grey50')+
  ggtitle("Asian Distribution of Deaths due to Smoking")+
  theme_void()+
  theme(legend.position = "bottom") -> world_map_plot1
  
region.lab.data1 <- tobacco1 %>%
  group_by(region) %>%
  summarise(long = mean(long), lat = mean(lat))


ggplot(tobacco1, aes(x = long, y = lat)) +
  geom_polygon(aes( group = group, fill = `Production (t)`))+
  geom_text(aes(label = region), data = region.lab.data1,  size = 3, hjust = 0.5)+
  scale_fill_gradient(name = 'Tobacco Production (in tonnes)', low = 'light grey', high = '#68d453', na.value = 'grey50')+
  ggtitle("Asian Distribution of Tobacco Production")+
  theme_void()+
  theme(legend.position = "right") -> world_map_plot2

#######################################################
ggsave("map1.png", world_map_plot1, bg ='white', width = 10, height = 8, dpi = 150, units = "in", device='png')  
#####################################################

left_join(world_map1,tobacco1,by = 'region') -> dual_data
tobacco1 <- tobacco %>% filter(!is.na(tobacco$`Production (t)`))


tobacco1 -> tob1
world_map1 -> wmp1

tob1 <- tob1 %>% select(7,8)
wmp1 <- wmp1 %>% select(8,9)

wmp1 %>%
  group_by(Year) %>%
  summarise(deaths_due_to_smoke = mean(Deaths)) -> wmp1

tob1 %>%
  group_by(Year) %>%
  summarise(production = mean(`Production (t)`)) -> tob1

left_join(wmp1,tob1,by = 'Year') -> dual_dt

max(dual_dt$production) / max(dual_dt$deaths_due_to_smoke) -> scale
max(dual_dt$deaths_due_to_smoke) / max(dual_dt$production) -> scale1

dual_dt$smoke_update <- dual_dt$deaths_due_to_smoke * scale 

dual_dt$Year <- as.Date()
x_axis_labels <- min(dual_dt$Year):max(dual_dt$Year)


dual_dt %>% 
  ggplot(aes(x = Year, y = smoke_update))+
  geom_line(aes(color = "Smoke Deaths"), lwd = 1.5)+
  geom_line(aes(y = production,color = "Tobacco Production"), lwd = 1.5)+
  scale_color_manual(values=c('#d46053','#68d453'))+
  scale_y_continuous('Tobacco Production (in tonnes)', sec.axis = sec_axis(~ . *scale1, name = "Deaths due to Smoke (per 100,000)")) +
  ggtitle("Tobacco Production and Smoking Death during 1990 to 2019")+
  theme_classic()+
  theme(axis.text.x = element_text(angle = 45, vjust = 0.5), 
        axis.title.y.right = element_text(color = "#d46053"),
        axis.text.y.right = element_text(colour = "#d46053"),
        axis.title.y.left = element_text(color = "#68d453"),
        axis.text.y.left = element_text(colour = "#68d453"),
        rect = element_blank())+
  scale_x_continuous(labels = x_axis_labels, breaks = x_axis_labels)+
  guides(fill=guide_legend(title=" ")) -> dual_y_axis_plot

ggsave("dual_axis_y.png", dual_y_axis_plot, bg ='white', width = 10, height = 8, dpi = 150, units = "in", device='png')  


f4 %>% filter(Entity %in% cc_data$region) -> smoke_share
f11 %>% filter(Entity %in% cc_data$region) -> smoke_death
f12 %>% filter(Entity %in% cc_data$region) -> cigratte

smoke_death %>% filter(!is.na(smoke_death$`Income classifications (World Bank (2017))`)) -> smoke_death

left_join(smoke_share,smoke_death, by = "Year") -> smoke_data


names(smoke_death)[4] <- "death_4"
names(smoke_death)[3] <- "Year"

smoke_share %>%
  arrange(desc(`Population (historical estimates)`)) %>%
  mutate(country = factor(Entity)) %>%
  ggplot(aes(x= `Prevalence of current tobacco use, males (% of male adults)`, 
             y= `Prevalence of current tobacco use, females (% of female adults)`, size=`Population (historical estimates)`, color=country)) +
  geom_point(alpha=0.5) +
  scale_size(range = c(.1, 24), name="Population (M)")


smoke_data %>%
  arrange(desc(`Population (historical estimates)`)) %>%
  mutate(country = factor(Entity)) %>%
  ggplot(aes(x= `Prevalence of current tobacco use, males (% of male adults)`, 
             y= `Prevalence of current tobacco use, females (% of female adults)`, size=`Population (historical estimates)`, color=country)) +
  geom_point(alpha=0.5) +
  scale_size(range = c(.1, 24), name="Population (M)")


smoke_death %>%
  arrange(desc(death_4)) %>%
  mutate(country = factor(Entity)) %>%
  ggplot(aes(x= year_3, 
             y= death_4, color=factor(`Income classifications (World Bank (2017))`))) +
  geom_point(alpha=0.5) 

####################################################################################

bar_data %>% 
  na.omit() %>% filter(`Income classifications (World Bank (2017))`%in% c('High income','Low income',
                                                                          'Lower-middle income', 'Upper-middle income')) %>%  
  ggplot( aes(x=Year, y=death_4, fill = `Income classifications (World Bank (2017))`)) + 
  geom_bar(stat = 'identity') + 
  facet_wrap(~`Income classifications (World Bank (2017))`, scales="free")+
  theme_bw() +
  xlab("Year")+
  ylab("Smoking Prevalence")+
  guides(fill=guide_legend(title="Income Classification"))+
  theme(
    axis.text.x = element_text(angle = 60, vjust = 0.5),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank()
  )+
  ggtitle('Smoking Prevalence in Asian Population by Income Levels')+
  scale_x_continuous(labels = x_axis_labels, breaks = x_axis_labels) -> bar_facet_plot

library(treemap)

cigratte %>% 
  group_by(Entity) %>% 
  summarise(consup = mean(`Cigarette consumption per smoker per day (IHME, GHDx (2012))`)) -> map_dt

treemap(cigratte,
        index = c('Entity'),
        vSize = "Cigarette consumption per smoker per day (IHME, GHDx (2012))",
        type = 'index')

cigratte %>% 
  group_by(Entity) %>%
  summarise(consump = round(mean(`Cigarette consumption per smoker per day (IHME, GHDx (2012))`),2))%>%
  mutate(cig.Index=paste(Entity, paste0(consump,'%'), sep ="\n"))%>%
  treemap(index="cig.Index", vSize="consump", 
          inflate.labels=F, 
          title="Daily Cigarette Consumption per Smoker",
          palette = "Blues",border.col = "white")

###############################################################

ggsave("bar_facet_plot.png", bar_facet_plot, bg ='white', width = 8, height = 8, dpi = 150, units = "in", device='png')  
#####################################################################


f7 %>% filter(Entity %in% cc_data$region) -> line_data

ggplot(line_data, aes(x = Year, y = `Prevalence of current tobacco use (% of adults)`, group = Entity, color = Entity)) +
  geom_line(lwd = 1.5) +
  ggtitle("Asian Countries Line Chart") +
  xlab("Year") +
  ylab("Value")+
  geom_text(data = line_data, aes(x = Year, y = `Prevalence of current tobacco use (% of adults)`, label = Entity), hjust = 1.5, vjust = 1.5, size = 3)

  
  
