# prepare text to analyze by stripping leading and trailing punctuation
# preserves contractions and all emoticons found in vader dictionary

strip_punc <- function(wpe) {
  for(i in 1:length(wpe)){
    #checks if word is emoticon found in vader lexicon
    if(!(tolower(wpe[i]) %in% vaderLexicon$V1)) {
      #if not, strip punctuation
      leadingAndTrailing <- "(^\\W+)|(\\W+$)"
      wpe[i] <- gsub(leadingAndTrailing, "", wpe[i])
    }
  }
  return(wpe)
}

wordsPlusEmo <- function(text) {
  #splits text into vector of words
  wpe <- unlist(strsplit(text, "\\s+"))
  #strips words of punctuation (unless the word is an emoticon)
  stripped <- strip_punc(wpe)
  return(stripped)
}
