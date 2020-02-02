
extract_text_features_b <- function(file_address, keywords) {
  #' gets the keywords as input and returns the dataframe including those keywords along with the number of keywords
  #' 
  #' @param keyword: a list of keywords
  #' @return results: a dataframe including movies with the keyword and number of words in the summary
  
  # read in the plot summaries from txt file
  plot_summary <- read.table(file_address, sep = '\t', quote = '', stringsAsFactors = FALSE)
  text_list <- as.list(plot_summary$V2)
  plot_summary[c('keyword', 'word_counts')] <- 
    do.call(rbind, 
            lapply(text_list, 
                   function(x) {
                     word_exists <- all(sapply(keywords, grepl, x))
                     word_count <- sapply(strsplit(tolower(x), " "), length)
                     return(c(word_exists, word_count))
                   }))
  
  # filter out and return the rows that has the keyword 
  results <- plot_summary[plot_summary$keyword == 1, c('V1', 'keyword', 'word_counts')]
  movies <- get_movie_name(as.data.frame(results))
  return(movies)
}

# profvis(tst <- extract_text_features_b(file_address = 'data/plot_summaries.txt',
#                                      keywords = c('love', 'world war')))
