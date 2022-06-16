
libs <- c("leaflet", "tidyverse", "ggmap", "leaflet.extras", "readr", "htmltools", "ggplot2", "maps", "mapproj", "summarytools", "mapdata", "stringr")
lapply(libs, require, character.only = TRUE)


# Upload csv

food_access <- read_csv("F:/Apps/Education/Summer school/Dushko-personal-website/food_access.csv")



#US map
us <- map_data('state')
county <- map_data('county')

ggplot(us, aes(x = long, y = lat, group = group, fill = region)) +
  geom_polygon(color = 'black') +
  coord_map('polyconic')+
  guides(fill = F)



#Merge data

food_access1 <- food_access %>% select(State,`Vehicle Access.1 Mile`, County) 

food_access1 <- food_access1 %>% group_by(State,`Vehicle Access.1 Mile`,County) 

us <- us %>% group_by(region,group, long, lat)      

food_access1$County <- tolower(food_access$County)

food_access1 <- food_access1 %>%
  mutate_at("County", str_replace, "county", "")


comb1 <- inner_join(food_access1, county, by=c("County"= "subregion"))



ggplot(comb1, aes(x = long, y = lat, group = group, fill = `Vehicle Access.1 Mile`)) +
  geom_polygon(color = 'gray') +
  coord_map('polyconic')+
  scale_fill_gradient2(low = 'blue', high = 'red')+
  theme_void()+
  ggtitle('Vehicle Access 1 Mile')



mycolor <- colorNumeric(palette = 'RdBu',
                        domain = c(1:4000))

comb %>% 
  leaflet() %>% 
  addProviderTiles('CartoDB') %>% 
  addCircleMarkers(radius = 0.1*`Vehicle Access.1 Mile`,
                   color = ~mycolor`Vehicle Access.1 Mile`),
popup = ~paste0(County,
                "<br/>",
                `Vehicle Access.1 Mile`)) %>% 
  addLegend(pal = mycolor, values = c(1:1000),
            opacity = 0.75,
            title = 'Vehicle Access.1 Mile',
            position = 'topleft')





```

Mapping by State
```{r}

combined_ds$Vehicle_Access.1_Mile <- combined_ds$`Vehicle Access.1 Mile`

combined_ds %>% summarize(County,`Vehicle Access.1 Mile`)

combined_ds1 <- combined_ds %>% group_by(County, long, lat, region,`Vehicle Access.1 Mile`) %>% 
  summarise(count = n()) %>% 
  arrange(desc(count))


ggplot(combined_ds1, aes(x = long, y = lat, group = group, fill = combined_ds1$`Vehicle Access.1 Mile`)) +
  geom_polygon(color = 'gray') +
  coord_map('polyconic')+
  scale_fill_gradient2(low = 'blue', mid = 'white', high = 'red', midpoint = 711)+
  theme_void()+
  ggtitle('Vehicle Access 1 Mile')






summary(combined_ds$`Vehicle Access.1 Mile`)  



```



```{r}
#US map for each county

combined_ds1 <- combined_ds %>% filter(combined_ds$`Vehicle Access.1 Mile`)
combined_ds1 <- combined_ds %>% group_by(County, long, lat, region, `Vehicle Access.1 Mile`) %>% 
  summarise(count = n()) %>% 
  arrange(desc(count))


combined_ds1 <- combined_ds %>% group_by(County, long, lat, region,`Vehicle Access.1 Mile`) %>% 
  summarise(count = n()) %>% 
  arrange(desc(count))


mycolor <- colorNumeric(palette = 'RdBu',
                        domain = c(1:4000))

combined_ds %>% 
  leaflet() %>% 
  addProviderTiles('CartoDB') %>% 
  addCircleMarkers(radius = ~0.01*combined_ds1$count,
                   color = ~mycolor(combined_ds1$count),
                   popup = ~paste0(County,
                                   "<br/>",
                                   count)) %>% 
  addLegend(pal = mycolor, values = c(1:1000),
            opacity = 0.75,
            title = 'Vehicle Access.1 Mile',
            position = 'topleft')






```





```{r}

# COVID data - USA


c <- read.csv(file.choose(), header = T)
usa <- c %>% filter(country == 'United States')


usa <- usa %>% group_by(province) %>% 
  summarise(count = n()) %>% 
  arrange(desc(count))


count(usa$province)

```
```{r}



```


# World map
w <- map_data('world')
icj <- map_data('world',
                region = c('India', 'China', 'Japan'))
ggplot(icj, aes(x = long, y = lat, group = group)) +
  geom_polygon(color = 'black') 

# COVID data - USA
c <- read.csv(file.choose(), header = T)
usa <- c %>% filter(country == 'United States')
usa <- usa %>% group_by(province) %>% 
  summarise(count = n()) %>% 
  arrange(desc(count))

# Merge data
usa$province <- tolower(usa$province)
data <- merge(s, usa,
              by.x = 'region',
              by.y = 'province')

ggplot(data, aes(x = long, y = lat, 
                 group = group,
                 fill = count)) +
  geom_polygon(color = 'gray') 

# Leaflet
leaflet() %>% addTiles()
# Allahabad, India
leaflet() %>% addProviderTiles('CartoDB') %>% 
  setView(lng = 81.878357, lat = 25.473034,
          zoom = 10) 

# COVID data - leaflet map of US
usa <- c %>% filter(country == 'United States')
usa <- usa %>% group_by(city, province, longitude, latitude) %>% 
  summarise(count = n()) %>% 
  arrange(desc(count))

# US map for each county
mycolor <- colorNumeric(palette = 'RdBu',
                        domain = c(1:4000))

usa %>% 
  leaflet() %>% 
  addProviderTiles('CartoDB') %>% 
  addCircleMarkers(radius = 2,
                   color = ~mycolor(count),
                   popup = ~paste0(city)) 

# Map for MA & NY
usa <- c %>% filter(country == 'United States',
                    province == 'Massachusetts' | province == 'New York')
usa <- usa %>% group_by(city, province, longitude, latitude) %>% 
  summarise(count = n()) %>% 
  arrange(desc(count))

map <- usa  %>% leaflet()  %>% 
  addProviderTiles('OpenStreetMap') %>% 
  addCircleMarkers(radius = 2,
                   color = 'red',
                   popup = ~city)
map %>% addMarkers()  

# Color factor variable
mycolor <- colorFactor(palette = c('red', 'blue'),
                       levels = c('Massachusetts', 'New York'))
map %>% 
  addCircleMarkers(data = usa,
                   radius = 2,
                   color = ~mycolor(province))

# Toggle between states
MA <- filter(usa, province == 'Massachusetts')
m <- leaflet() %>% 
  addProviderTiles('CartoDB') %>% 
  addCircleMarkers(data = MA,
                   radius = 5,
                   label = ~htmlEscape(city),
                   color = 'blue',
                   group = 'Massachusetts')
NY <- filter(usa, province == 'New York')
m <- m %>% 
  addCircleMarkers(data = NY,
                   radius = 5,
                   label = ~htmlEscape(city),
                   color = 'red',
                   group = 'New York') %>% 
  addLayersControl(overlayGroups = 
                     c('Massachusetts', 'New York'))
m

# Cluster counties
usa %>% 
  leaflet() %>% 
  addTiles() %>% 
  addCircleMarkers(radius = 10,
                   label = ~htmlEscape(city),
                   color = 'red',
                   clusterOptions = markerClusterOptions())



