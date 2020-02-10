library(doParallel)
library(foreach)


extract_text_features_g <- function(file_address, keywords) {
  #' gets the keywords as input and returns the dataframe including those keywords along with the number of keywords
  #' 
  #' @param keyword: list of keywords needed for searching the movies 
  #' @return file_address: address of the file includes the plot summaries of the movies
  #' @return results: a dataframe including movies titles and the year it is built
    
  # read in the plot summaries from txt file
  plot_summary <- data.table::fread(file_address, header = FALSE, quote = '')
  
  # setting up the infrastructure for parallel computing
  summaries <- plot_summary$V2
  num_cores <- parallel::detectCores() 
  doParallel::registerDoParallel(num_cores)
  # parallel search for keyword existance and word counts
  parallel_combined <- foreach::foreach (i = 1:length(summaries), .combine = rbind) %dopar% {
    c(all(sapply(keywords, grepl, summaries[i])),
      sapply(strsplit(tolower(summaries[i]), " "), length))
  }
  plot_summary[, c('keyword', 'word_counts')] <- as.data.frame(parallel_combined)
  # filter out and return the rows that has the keyword 
  results <- plot_summary[plot_summary$keyword == 1, c('V1', 'keyword', 'word_counts')]
  movies <- get_movie_name(results)
  return(movies)
}

# profvis(tst <- extract_text_features_g(file_address = 'datasets/plot_summaries.txt',
#                                      keywords = c('love', 'world war')))
