
extract_text_features_u <- function(file_address, keywords) {
  #' gets the keywords as input and returns the dataframe including those keywords along with the number of keywords
  #' 
  #' @param keyword: list of keywords needed for searching the movies 
  #' @return file_address: address of the file includes the plot summaries of the movies

  # read in the plot summaries from txt file
  plot_summary <- read.table(file_address, sep = '\t')

  for (i in 1:nrow(plot_summary)) {
    text = plot_summary[i, 2]
    # search if the keyword exists in plot_summaries
    word_exists <- all(sapply(keywords, grepl, text))
    # count number of words in the text
    word_count <- sapply(strsplit(tolower(text), " "), length)
    # insert the keyword existence and word counts into associated columns
    plot_summary[i, c('keyword', 'word_counts')] <- c(word_exists, word_count)
  }
  # filter out and return the rows that has the keyword 
  results <- plot_summary[plot_summary$keyword == 1, c('V1', 'keyword', 'word_counts')]
  movies <- get_movie_name(as.data.frame(results))
  return(movies)
}

# profvis(extract_text_features_u(file_address = 'data/plot_summaries.txt', 
#                              keywords = c('love', 'world war')))
