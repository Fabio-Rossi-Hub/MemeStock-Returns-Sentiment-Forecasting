#' Get a dataframe of vader results for multiple text documents
#'
#' Use vader_df() to calculate the valence of multiple texts contained within a vector or column in a dataframe.
#'
#' @param text to be analyzed; for vader_df(), the text should be a single vector (e.g. 1 column)
#' @param incl_nt defaults to T, indicates whether you wish to incl UNUSUAL n't contractions (e.g., yesn't) in negation analysis
#' @param neu_set defaults to T, indicates whether you wish to count neutral words in calculations
#' @param rm_qm defaults to T, indicates whether you wish to clean quotation marks from text (setting to F may result in errors)
#' @importFrom tm stopwords
#' @return A dataframe containing the valence score for each word; an overall, compound valence score for the text; the weighted percentage of positive, negative, and neutral words in the text; and the frequency of the word "but".
#' @examples
#' vader_df(c("I'm happy", "I'm yesn't happy"))
#' vader_df(c("I'm happy", "I'm yesn't happy"), incl_nt = FALSE)
#' vader_df(c("I'm happy", "I'm yesn't happy"), neu_set = FALSE)
#' vader_df(c("I said \"I'm not happy\", "I said \" I'm not happy \" "), rm_qm = FALSE)
#'
#' @export
#'
#' @section N.B.:
#' In the examples below, "yesn't" is an internet neologism meaning "no", "maybe yes, maybe no", "didn't", etc.
#'
#' @seealso \code{\link{get_vader}} to get vader results for a single text document

vader_df <- function(text, incl_nt = T, neu_set = T, rm_qm = F){
  # Unlisting in case tibble
  text <- unlist(text)
  df <- lapply(text, function(x) data.frame(as.list(get_vader(x, incl_nt, neu_set, rm_qm))))
  df <- do.call("rbind", df)
  df <- cbind(text, df)

  # Changing Factors to Numeric (Must Convert to Character 1st)
  df[ , -c(1:2)] <- sapply(df[ , -c(1:2)], as.character)
  df[ , -c(1:2)] <- sapply(df[ , -c(1:2)], as.numeric)

  # Warnings
  err <- length(which(df$word_scores == "ERROR"))
  if(err == 1 ) warning("1 row contains an error. Filter word_scores for 'ERROR' to identify the problematic text.")
  if(err > 1 ) warning(paste(err, "rows contain an error. Filter word_scores for 'ERROR' to identify the problematic text."))

  return(df)
}
