
get_movie_name <- function(movie_id_list) {
  
  movie_id_name <- data.table::fread('datasets/movie.metadata.tsv', header = FALSE, quote = '')
  movies <- movie_id_name[movie_id_name$V1 %in% movie_id_list$V1, c('V3', 'V4')]
  names(movies) <- c('name', 'year')
  return(movies)
}