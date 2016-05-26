require(qdap)

analyseReadability <- function(x){
  # Computes the readablity score of some textual input, using the flesch_kincaid measure.
  # Args:
  #   x: the textual input. Can be a single character vector or a list of character vectors.
  #
  # Returns:
  #   A data frame contraining the corresponding FK readability scores for the given text
  #   inputs.
  
  fk_output <- lapply(x, function(y) cleanTextAndGetFKScore(y))
  scores.df <- data.frame(readability=unlist(fk_output))
  return(scores.df)     
 
}

cleanTextAndGetFKScore <- function(x){
  # Takes some character vector input, splits it into sentences and 
  # then calculates the Flesch-Kincaid readability measure.
  #
  # Args:
  #   x: A character vector.
  # 
  # Returns:
  #   The Flesch-Kincaid readability score for the input.


   text <- unlist(strsplit(x, "\\."))
   fk <- flesch_kincaid(text)
   return(fk$Readability$FK_read.ease)
}