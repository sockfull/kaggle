#create bootstrap function
select_sample <- function(data_vec, sample_size = 1000) {
  
  idx = sample(1:dim(array(data_vec))[1], sample_size)
  return(data_vec[idx])

}

bootstrap <- function(data_vec, sample_size = 1000, replications = 1000, func) {
  
  reps <- 1:replications
  for (i in reps) {
    
    reps[i] = func(select_sample(data_vec, sample_size))
    
  }
  return(reps)
}

#example

price_means <- bootstrap(rentals$price, sample_size = 30000, replications = 10000, func = mean)

qplot(price_means, 
      xlab="Price means", 
      geom="histogram", 
      ylab="Count", 
      binwidth=10, 
      fill=I("#ADD8E6"), 
      col=I("black"))