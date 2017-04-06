library(ggplot2)
library(ggmap)
# Load packages and data
packages <- c("jsonlite", "dplyr", "purrr", "ggplot2", "scales")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)

data <- fromJSON("train.json")

# unlist every variable except `photos` and `features` and convert to tibble
vars <- setdiff(names(data), c("photos", "features"))
data <- map_at(data, vars, unlist) %>% tibble::as_tibble(.)
vars



#extracting mainly the numeric variables, the list_id might be useful later if I have to join with another dataframe
rentals <- select(data, c(listing_id, bedrooms, bathrooms, created, price, street_address, interest_level, longitude, latitude))
head(rentals, 5)



#creating a new column that counts how many photos each listing has
photos_count <- map(data[['photos']], function(x) length(unlist(x)))
photos_count <- do.call('rbind', photos_count)
rentals$photos_count <- photos_count[,1]

#doing the same with features
features_count <- map(data[['features']], function(x) length(unlist(x)))
features_count <- do.call('rbind', features_count)
rentals$features_count <- features_count[,1]

#interest_level is a categorical variable so it makes sense for it to be a factor    
rentals['interest_level_factor'] <- factor(rentals[['interest_level']], levels = c("low", "medium", "high"))

#having a total room count might come in handy
rentals <- mutate(rentals, total_rooms = bedrooms + bathrooms, beds_to_baths = bedrooms/bathrooms)



nyc_toner <- get_map(location = c(-73.94, 40.76), zoom = 12, maptype = 'toner')

nyc_road <- get_map(location = c(-73.94, 40.76), zoom = 12, maptype = 'roadmap')


brooklyn_road <- get_map(location = c(-73.92, 40.68), zoom = 12, maptype = 'roadmap')


nyc_stamen <- get_map(location = c(-73.94, 40.75), zoom = 11, maptype = 'watercolor')

nyc_osm <- get_map(location = c(-73.94, 40.8), zoom = 12, source = 'osm')

ggmap(nyc) +
  stat_density2d(data = rentals, aes(x = longitude, y = latitude, fill =..level.., alpha = 0.2),  
                 geom = 'polygon') + 
  #facet_grid(~interest_level_factor)
  ggtitle("Locations") 
  
ggmap(nyc_stamen) +
  geom_point(data = rentals, aes(x = longitude, y = latitude, color =interest_level_factor), alpha = 0.2) + 
  #facet_grid(~interest_level_factor)
  ggtitle("Rental Locations") +
  guides(alpha = FALSE)

ggmap(nyc_road) +
  geom_point(data = rentals, aes(x = longitude, y = latitude, color =interest_level_factor), alpha = 0.2) + 
  #facet_grid(~interest_level_factor)
  ggtitle("Rental Locations") +
  guides(alpha = FALSE)

ggmap(brooklyn_road) +
  geom_point(data = rentals, aes(x = longitude, y = latitude, color =interest_level_factor), alpha = 0.2) + 
  #facet_grid(~interest_level_factor)
  ggtitle("Rental Locations") +
  guides(alpha = FALSE)

#faceted stamen
ggmap(nyc_stamen) +
  geom_point(data = rentals, aes(x = longitude, y = latitude, color =interest_level_factor), alpha = 0.2) + 
  facet_grid(~interest_level_factor) +
  ggtitle("Rental Locations") +
  guides(alpha = FALSE) +
  theme(plot.title = element_text(hjust = 0.5))

#low interest
ggmap(nyc_road) +
  geom_point(data = filter(rentals, interest_level_factor == 'low'), aes(x = longitude, y = latitude, color =interest_level_factor), alpha = 0.2) + 
  #facet_grid(~interest_level_factor)
  ggtitle("Rental Locations") +
  guides(alpha = FALSE)


#overall map satmen, faceted by interest
ggmap(nyc_road) +
  stat_density2d(data = rentals, aes(x = longitude, y = latitude, fill = ..level.., alpha = 0.2),  
                 geom = 'polygon') + 
  facet_grid(~interest_level_factor) +
  ggtitle("Listing Density") +
  guides(alpha = FALSE) +
  theme(plot.title = element_text(hjust = 0.5, face = 'bold')) +
  scale_fill_gradient(low = 'gray', high = 'red') +
  list()


ggplot(data = rentals) +
  geom_point(aes(x = longitude, y = latitude, color = interest_level_factor), alpha = 0.2) +
  #stat_density2d(data = rentals, aes(x = longitude, y = latitude), geom = 'polygon') + 
  #facet_grid(~interest_level_factor) +
  ggtitle("Rental Locations Scatterplot") +
  guides(alpha = FALSE) +
  theme(plot.title = element_text(hjust = 0.5, face = 'bold')) +
  #scale_fill_gradient(low = 'gray', high = 'red') +
  xlim(-74.05, -73.8) +
  ylim(40.6, 40.9) + 
  list()
  

ggplot() +
  #geom_point(aes(x = longitude, y = latitude, color = interest_level_factor), alpha = 0.2) +
  stat_density2d(data = rentals, aes(x = longitude, y = latitude, fill = ..level..), geom = 'polygon', alpha = 0.4) + 
  facet_grid(~interest_level_factor) +
  ggtitle("Density") +
  guides(alpha = FALSE) +
  theme(plot.title = element_text(hjust = 0.5, face = 'bold')) +
  scale_fill_gradient(low = 'blue', high = 'red') +
  xlim(-74.05, -73.9) +
  ylim(40.65, 40.86) + 
  list()

#brooklyn density
ggplot() +
  #geom_point(aes(x = longitude, y = latitude, color = interest_level_factor), alpha = 0.2) +
  stat_density2d(data = rentals, aes(x = longitude, y = latitude, alpha = ..density..), geom = 'tile') + 
  #facet_grid(~interest_level_factor) +
  ggtitle("Density") +
  guides(alpha = FALSE) +
  theme(plot.title = element_text(hjust = 0.5, face = 'bold')) +
  scale_fill_gradient(low = muted('blue'), high = 'red') +
  xlim(-74.05, -73.9) +
  ylim(40.6, 40.7) + 
  list()


  
#kmeans
cluster_model_1 <- select(rentals, longitude, latitude) %>% kmeans(., centers = 20)



#looking at the centers without map
centers <- as.data.frame(cluster_model_1$centers)
centers$size <- cluster_model_1$size
centers_cleaned <- filter(centers, longitude < 0 & longitude >- 75)

ggplot(centers_cleaned, aes(x = longitude, y = latitude)) + 
  geom_point()

#overlaying a map
ggmap(nyc_toner) + 
  geom_point(data = centers_cleaned, 
            aes(x = longitude, y = latitude, size = size), alpha = 0.5)+
  scale_size(range = c(1, 10))

#creating proportion
counts_by_variable <- group_by(rentals, interest_level_factor, bathrooms) %>% 
  summarize(counts = n())
counts_by_group <- group_by(rentals, interest_level_factor) %>% 
  summarize(totals = n()) 
proportions <- left_join(counts_by_variable, counts_by_group, by = "interest_level_factor") %>% 
  mutate(p = counts/totals)

#bathrooms barchart
ggplot(proportions, aes(x = bathrooms, y = p)) +
  geom_bar(aes(fill = interest_level_factor), stat = "identity") +
  facet_grid(~interest_level_factor) +
  scale_x_continuous(limits = c(-1, 5)) +
  #coord_cartesian(xlim = c(0,6)) +
  list()