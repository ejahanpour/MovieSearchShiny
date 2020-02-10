
extract_text_features_b <- function(file_address, keywords) {
  #' gets the keywords as input and returns the dataframe including those keywords along with the number of keywords
  #' 
  #' @param keyword: list of keywords needed for searching the movies 
  #' @return file_address: address of the file includes the plot summaries of the movies
  #' @return results: a dataframe including movies titles and the year it is built
  
  # read in the plot summaries from txt file
  plot_summary <- read.table(file_address, sep = '\t', quote = '', stringsAsFactors = FALSE)
  
  plot_summary <- as.list(plot_summary)
  
  for (i in 1:length(plot_summary[[1]])) {
    text <- plot_summary[[2]][i]
    # search if the keyword exists in plot_summaries
    word_exists <- all(sapply(keywords, grepl, text))
    # count number of words in the text
    word_count <- sapply(strsplit(tolower(text), " "), length)
    # insert the keyword existence and word counts into associated columns
    plot_summary[['keyword']][i] <- word_exists
    plot_summary[['word_count']][i] <- word_count
  }
  # filter out and return the rows that has the keyword 
  results <- plot_summary[plot_summary$keywords == 1, c('V1', 'keyword', 'word_counts')]
  movies <- get_movie_name(as.data.frame(results))
  return(movies)
  
}

# profvis(tst <- extract_text_features_b(file_address = 'datasets/plot_summaries.txt',
#                                      keywords = c('love', 'world war')))
