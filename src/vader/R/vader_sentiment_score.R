#calculate sentiment score

# check for modification in sentiment due to contrastive conjunction "but"
but_check <- function(wpe, sentiments){
  but_index <- NULL
  if("but" %in% tolower(wpe)){
    but_index <- which(tolower(wpe) == "but")
    for (s in seq_along(sentiments)) {
      if (s < but_index[1]) {sentiments[s] <- sentiments[s] * 0.5}
      # else if (s > but_index[1] && s < but_index[length(but_index)]) {sentiments[s] <- sentiments[s] * 0.75}
      else if (s > but_index[1]) {sentiments[s] <- sentiments[s] * 1.5}
    }
  }
  results <- list(sentiments = sentiments, but_count = length(but_index))
  return(results)
}

# Normalize the score to be between -1 and 1 using an alpha that approximates the max expected value
normalize <- function(score, alpha=15) {
  norm_score = score / sqrt((score * score) + alpha)
  if(norm_score < (-1.0)) {return(-1.0)}
  else if(norm_score > 1.0) {return(1.0)}
  else {return(norm_score)}
}

# count frequency of character in text
count_char <- function(my_char, text){
  char_count <- 0
  char_look <- unlist(gregexpr(my_char, text))
  if(any(char_look != -1)){
    char_count <- length(char_look)
  }
  return(char_count)
}

# count frequency of ! in text and assign appropriate scaling
p_amp <- function(text){
  p_count <- count_char("!", text)
  if(p_count > 4) {p_count <- 4}
  amp_p <- p_count * 0.292
  return(amp_p)
}

# count frequency of ? in text and assign appropriate scaling
qm_amp <- function(text){
  qm_count <- count_char("\\?", text)
  amp_qm <- 0
  if(qm_count > 1) {
    if(qm_count <= 3) {amp_qm <- qm_count * 0.18}
    else(amp_qm <- 0.96)
  }
  return(amp_qm)
}

# add emphasis from exclamation points and question marks
pq_amp <- function(text) {
  amp_p <- p_amp(text)
  amp_qm <- qm_amp(text)
  amp_pqm <- amp_p + amp_qm
  return(amp_pqm)
}

# create positive, negative, and neutral sentiment scores
sift_senti_scores <- function(score){
  pos_sum <- 0
  neu_sum <- 0
  neg_sum <- 0

  for(s in score){
    # 1 is added/subtracted based on Vader Python Script
    if(s > 0) {pos_sum <- pos_sum + s + 1}
    if(s < 0) {neg_sum <- neg_sum + s - 1}
    if(s == 0) {neu_sum <- neu_sum + 1}
  }

  return(c(pos_sum = pos_sum, neu_sum = neu_sum, neg_sum = neg_sum))
}

# calculate (adjusted) percentage of text that are positive, negative, or neutral
total_senti_scores <- function(pos_sum, neu_sum, neg_sum){
  total <- pos_sum + abs(neg_sum) + neu_sum
  pos <- pos_sum / total
  neg <- abs(neg_sum) / total
  neu <- neu_sum / total

  return(c(pos_total = pos, neu_total = neu, neg_total = neg))
}

# calculate compound score after factoring in added emphasis for exclamation points and question marks
compound_calc <- function(sentiments, text, punc_amp){
  sum_s <- sum(sentiments, na.rm = T)
  if(sum_s > 0) {sum_s <- sum_s + punc_amp}
  else if(sum_s < 0) {sum_s <- sum_s - punc_amp}

  compound <- normalize(sum_s)
  return(compound)
}

score_val <- function(sentiments, text){
  punc_amp <- 0
  punc_amp <- pq_amp(text)

  compound <- 0
  scores_total <- c(pos_total = 0, neu_total = 0, neg_total = 0)

  if(any(!is.na(sentiments))) {
    compound <- compound_calc(sentiments, text, punc_amp)

    # if text contains more positive words, increase positive score depending on punctuation marks
    # if text contains more negative words, decrease negative score depending on punctuation marks
    scores_dif <- sift_senti_scores(sentiments[!is.na(sentiments)])
    if(scores_dif["pos_sum"] > abs(scores_dif["neg_sum"])) {scores_dif["pos_sum"] <- scores_dif["pos_sum"] + punc_amp}
    else if(scores_dif["pos_sum"] < abs(scores_dif["neg_sum"])) {scores_dif["neg_sum"] <- scores_dif["neg_sum"] - punc_amp}

    # create vector positive, negative, and neutral words
    scores_total <- total_senti_scores(scores_dif["pos_sum"], scores_dif["neu_sum"], scores_dif["neg_sum"])
    names(scores_total) <- c("pos", "neu", "neg")
  }

  # return compound and indivdiual (positive, negative, neutral) sentiment scores
  results <- c(compound = compound, scores_total)
  results <- round(results, digits = 3)
  return(results)
}
