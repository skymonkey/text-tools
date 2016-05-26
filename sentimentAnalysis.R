require(tm)
require(stringr)

analyseSentiment <- function(x, lang, wordlist.dir="./wordlists"){
  # Performs a very simple sentiment analysis for textual input. 
  
  x <- tolower(x)
  stopwords <- stopwords(lang)
  wordlists <- read_scores(wordlist.dir)
  lang.specific.list <- wordlists[which(wordlists[,3] == lang),]
  pos <- lang.specific.list[which(lang.specific.list$score > 0),]
  neg <- lang.specific.list[which(lang.specific.list$score <=0),]


  tokens <- str_match_all(x, "\\S+")
  content.words <- list(unlist(tokens)[!(unlist(tokens) %in% stopwords)])
  

  pos.term.frequency <- sum(str_count(content.words, as.vector(pos$word)))
  neg.term.frequency <- sum(str_count(content.words, as.vector(neg$word)))

  text.length <-length(content.words[[1]])
  df <- data.frame(pos.term.frequency, neg.term.frequency, score=((pos.term.frequency-neg.term.frequency) /text.length))
  return(df)
  

}


read_scores <- function(wordlistDir){
  # Reads sentiment wordlists from local storage.
  #
  # Args:
  #   wordlistDir: the location of the wordlists (local)
  # 
  # Returns:
  #   A dataframe combining all the wordlists found in the input directory.
  
  wordlists <- list.files(wordlistDir, pattern=".tsv", full.names=T)
  combined.lists.df <- do.call(rbind.data.frame, lapply(wordlists, function(x) read_wordlist(x)))
  names(combined.lists.df) <- c("words", "score", "lang")
  return(combined.lists.df) 
}


read_wordlist <- function(x){
  # Reads a tab separated wordlist with the expected name .*?([a-z]{2})\.tsv
  # where $1 is a 2-char language code (e.g. filename: wordlist-en.tsv)
  #
  # Args:
  #   x: the name of the wordlist to be read. 
  #   
  # Return:
  #   A dataframe containing the wordlist, with a new extra column for the language
  #   whose value is extracted from the filename (e.g. filename wordlist-en.tsv
  #   will yield language 'en'.


  lang <- substr(x, nchar(x)-5, nchar(x)-4)
  words <- read.csv(x, header=F, sep="\t", stringsAsFactors=F)
  words[,3] <- lang
  return(words)
}