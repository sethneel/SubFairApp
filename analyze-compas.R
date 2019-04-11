# returns pairs indexed as elements of distances
sampler = function(distances, d_min, d_max, n_pairs){
  inds = which(distances < d_max & distances > d_min, arr.ind = TRUE)
  s = dim(inds)[1]
  rows = sample(1:s, n_pairs, replace=FALSE)
  selected_pairs = inds[rows, ]
  return(selected_pairs)
}


plot_pair = function(pair, predictor_df){
  person_1 = predictor_df[pair[1,1],]
  person_2 = predictor_df[pair[1,2],]
  print(person_1)
  print(person_2)

  plot_df = data.frame(cbind(c(colnames(person_1), colnames(person_2)), c(as.character(person_1), as.character(person_2)), c(rep('person 1',9), rep('person 2', 9))))
  colnames(plot_df) = c('key', 'value', 'person')
  plot <- ggplot(data=plot_df, aes(x=key, y=value, col=person)) + geom_point() + ggtitle('Person 1 vs. Person 2')
  return(plot)
}
  
