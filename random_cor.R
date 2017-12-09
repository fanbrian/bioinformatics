random_cor = function(set = all2_bac_25504_noCALCA, n_iterations = 3000){
  cors = NULL
  for(iteration in 1:n_iterations){
    correlation_result = cor(sample(set, size = 2, replace = FALSE, prob = NULL))
    cors[[paste0('iteration ', iteration)]] = correlation_result[2] #only get relevant correlation coefficient, since others are 1 or the same number
    cat(iteration, 'of', n_iterations, 'done...\n ')}
  cors = as.data.frame(cors)
  p <- ggplot(data = cors) 
  p <-  p + geom_histogram(mapping = aes(x = cors), binwidth = 0.01)
  print(p)
  return(cors)
}

p + geom_vline(xintercept = -0.37, col = "red")
