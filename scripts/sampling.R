#rentals df with 99th percentile on price
rentals.99 <- filter(rentals, price < quantile(rentals$price, .99), longitude > -74.05 & longitude < -73.8, latitude > 40.55 & latitude < 40.95) 

#sampling
lows <- filter(rentals.99, interest_level_factor == 'low')
meds <- filter(rentals.99, interest_level_factor == 'medium')
highs <- filter(rentals.99, interest_level_factor == 'high')

idx_lows <- sample(1:nrow(lows), 10000)
idx_meds <- sample(1:nrow(meds), 10000)
idx_highs <- sample(1:nrow(highs), 10000, replace = TRUE)


rentals.99_sampled <- bind_rows(list(lows[idx_lows,], meds[idx_meds,], highs[idx_highs,]))

locations <- select(rentals.99_sampled, listing_id, longitude, latitude)

ggplot(locations, aes(x = longitude, y = latitude)) +
  geom_point()

location_data <- locations[,2:3]

#kmeans
set.seed(42)
withins <- seq(0, 30)
for (i in 1:25) {
  withins[i] = kmeans(location_data, centers = i , nstart = 10)$tot.withinss
}

plot(x = 1:(length(withins)), y = withins, type = "b", xlim = c(1, 30), ylim = c(0, 100))

kmeans_model <- kmeans(location_data, centers = 10, nstart = 10)

kmeans_model$tot.withinss  
rentals.99_sampled$cluster <- kmeans_model$cluster

prob_chart <- group_by(rentals.99_sampled, interest_level_factor, cluster) %>% 
  summarize(count = n()) %>% 
  mutate(total = sum(count), p = count/total)

ggplot(prob_chart, aes(x = cluster, y = p)) + 
  geom_bar(aes(fill = interest_level_factor), stat = "identity") +
  #facet_grid(~interest_level_factor)
  list()

#hclust