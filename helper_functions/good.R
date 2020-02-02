library(doParallel)
library(foreach)


extract_text_features_g <- function(file_address, keywords) {
  #' gets the keywords as input and returns the dataframe including those keywords along with the number of keywords
  #' 
  #' @param keyword: a list of keywords
  #' @return results: a dataframe including movies with the keyword and number of words in the summary
  
  # read in the plot summaries from txt file
  plot_summary <- data.table::fread(file_address, header = FALSE, quote = '')
  summaries <- plot_summary$V2
  num_cores <- parallel::detectCores() 
  doParallel::registerDoParallel(num_cores)
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

# profvis(tst <- extract_text_features_g(file_address = 'data/plot_summaries.txt',
#                                      keywords = c('love', 'world war')))
