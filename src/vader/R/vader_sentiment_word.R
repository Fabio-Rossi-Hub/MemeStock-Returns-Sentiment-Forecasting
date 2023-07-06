# Return a float for sentiment strength based on the input text for each word.
# Positive values are positive valence, negative value are negative valence.

##################################
# senti_valence helper functions #
##################################

#raw vader score with modification for ALL CAPS in mixed string
get_vader_score <- function(item){
  # get raw vader score
  vrow <- which(vaderLexicon$V1==item)
  valence <- vaderLexicon[vrow,2]
  return(valence)
}

##################################

#check for "no" as modifier
no_check <- function(wpe, i, item, valence){
  if(item == "no" && (i < length(wpe) && tolower(wpe[i+1]) %in% vaderLexicon$V1) ||
     item == "no" && (i < length(wpe)-1 && tolower(wpe[i+2]) %in% vaderLexicon$V1) ||
     item == "no" && (i < length(wpe)-2 && tolower(wpe[i+3]) %in% vaderLexicon$V1 && tolower(wpe[i+2]) %in% c("or", "nor")))
  {
    valence <- 0
  }
  if((i > 1 && tolower(wpe[i-1]) == "no") ||
     (i > 2 && tolower(wpe[i-2]) == "no") ||
     (i > 3 && tolower(wpe[i-3]) == "no" && tolower(wpe[i-1]) == "or")) {                                 # nor is already a negation modifier
    valence <- get_vader_score(item) * N_SCALAR
  }
  return(valence)
}

##################################

#idiom dictionary check
dic_check <- function(idiomLength, checkMe, checkIdiom){
  dicCheck <- paste0(idiomLength, collapse = '$|^')
  dicCheck <- paste0('^', dicCheck, '$') #add word boundary to start and end of vector
  dicCheck <- grep(dicCheck, vaderLexicon$V1)
  dicCheck <- vaderLexicon[dicCheck, 1]

  for(d in 1:length(dicCheck)) {names(dicCheck)[d] <- grep(dicCheck[d], idiomLength)}
  if(min(names(dicCheck))==names(dicCheck[grep(checkMe, dicCheck)])) {valence <- IDIOMS[grep(checkIdiom, names(IDIOMS))]}
  else {valence <- NA}
  return(valence)
}

dic_check_nd <- function(idiomLength, checkIdiom) {
  div <- tm::removeWords(idiomLength, stopwords("en"))
  div <- length(div[div != ""])
  valence <- IDIOMS[grep(checkIdiom, names(IDIOMS))] / div
  return(valence)
}

#check idiom words
idioms_check <- function(wpe, i, valence, non_dic = F){

  myStrSplit <- unlist(strsplit(tolower(wpe), " "))
  # if text is only 1 word, then we shouldn't check idioms
  if(length(myStrSplit) > 1){
    if(length(grep("[[:punct:]]", myStrSplit)) == 0){

      checkMe <- paste0("\\b", myStrSplit[i], "\\b")
      if(length(grep(checkMe, names(IDIOMS))) > 0) {

        checkIdiom <- NULL
        strIndex <- i
        found <- grep(checkMe, names(IDIOMS))

        for(f in 1:length(found)){
          idiomLength <- unlist(strsplit(names(IDIOMS)[found[f]], " "))

          if(length(idiomLength) < 3) {
            checkIdiom <- paste0(paste(myStrSplit[strIndex], myStrSplit[strIndex+1]),"|",paste(myStrSplit[strIndex-1], myStrSplit[strIndex]))
          } else {
            checkIdiom <- paste0(paste(myStrSplit[strIndex], myStrSplit[strIndex+1], myStrSplit[strIndex+2]), "|",
                                 paste(myStrSplit[strIndex-1], myStrSplit[strIndex], myStrSplit[strIndex+1]))
            if(i > 1) {checkIdiom <- paste0(checkIdiom, "|", paste(myStrSplit[strIndex-2], myStrSplit[strIndex-1], myStrSplit[strIndex]))}
          }
          if(length(grep(checkIdiom, names(IDIOMS))) > 0) {
            if(non_dic == F) {valence <- dic_check(idiomLength, checkMe, checkIdiom)}
            else if(!any(idiomLength %in% vaderLexicon$V1)) {valence <- dic_check_nd(idiomLength, checkIdiom)}
            break
          }
        }
      }
    }
  }
  return(valence)
}

##################################

# Check whether ALL words or just SOME words in the input are ALL CAPS
# n.b. ^ = start of str, +$ = until end of str (otherwise checks if ANY letter is cap, and not whole word)
allcap_diff <- function(words){
  is_upper <- 0
  incl_abc <- 0                                                        ### ADDED this b/c "I WANT MY 100" would:
  for(i in 1:length(words)){                                           ### be mixed if comparing  is_upper to length(words) BUT
    if(grepl("[[:alpha:]]", words[i])) {                               ### be all caps if comparing is_upper to length(words with letters only)
      incl_abc <- incl_abc + 1
      if(grepl("^[^a-z]+$", words[i])) {is_upper <- is_upper + 1}
    }
  }
  if(is_upper > 0 && is_upper < incl_abc) {TRUE} else {FALSE}
}

# check if sentiment laden word is in ALL CAPS (while others aren't)
# CAN'T use item because item has been lowered
all_caps <- function(wpe, i, is_cap_diff, valence){
  if(is_cap_diff && grepl("^[[:upper:]]+$", wpe[i])){
    if(valence > 0) {valence <- valence + C_INCR}
    else{valence <- valence - C_INCR}
  }
  return(valence)
}

##################################

# Check if the preceding words increase, decrease, or negate/nullify the valence
# helper function modifies the direction of the scalar if valence < 0
# and modifies intensity of scalar if word is all caps in mixed case str
scalar_helper <- function(word, valence, is_cap_diff, scalar){
  if (valence < 0){scalar <- (-scalar)}
  # check if booster/dampener word is in ALLCAPS (while some words aren't)
  if(is_cap_diff && grepl("^[^a-z]+$", word)) {
    if(valence > 0) {scalar <- scalar + C_INCR}
    else {scalar <- scalar - C_INCR}
  }
  return(scalar)
}

# function checks if scalar word is in dictionary and calls helper function
scalar_inc_dec <- function(wpe, i, start_i, is_cap_diff, valence){
  scalar <- 0.0
  w <- wpe[i-start_i]
  w_lower <- tolower(wpe[i-start_i])
  is_bigram <- "n"
  #check if item is in booster diction (single words)
  if(w_lower %in% names(BOOSTER_DICT)){
    scalar <- BOOSTER_DICT[w_lower]
    scalar <- scalar_helper(w, valence, is_cap_diff, scalar)
  }
  # check if item is in booster diction (compound words)
  else if(start_i < length(wpe) && w_lower %in% c("kind", "sort", "just")) {
    bigram <- paste(w, wpe[i-start_i+1])
    if(tolower(bigram) %in% names(BOOSTER_DICT)) {
      scalar <- BOOSTER_DICT[tolower(bigram)]
      scalar <- scalar_helper(bigram, valence, is_cap_diff, scalar)
      is_bigram <- "y"
    }
  }
  return(c(scalar, is_bigram))
}

#get scalar
get_scalar <- function(wpe, i, start_i, is_cap_diff, valence){
  booster_results <- scalar_inc_dec(wpe, i, start_i, is_cap_diff, valence)
  s <- as.numeric(booster_results[1])
  is_bigram <- booster_results[2]
  if(is_bigram == "n"){
    if(start_i==2 && s!=0) {s <- s*0.95}
    if(start_i==3 && s!=0) {s <- s*0.9}
    if(start_i==4 && s!=0) {s <- s*0}
  } else {
    if(start_i==3 && s!=0) {s <- s*0.95}
    if(start_i==4 && s!=0) {s <- s*0.9}
  }
  return(s)
}

# Determine if input contains negation words
negated <- function(input_words) {                           ### incl_nt set in POLARITY_SCORES fx (set to TRUE unless changed by user)
  w <- tolower(input_words)
  if(any(w %in% NEGATE)) {TRUE}                              ### probably don't need (any) since only 1 word passed at a time
  else if (isTRUE(incl_nt) && any(grepl("n't", w))) {TRUE}
  else {FALSE}
  # else if("least" %in% w) {                                ### removing this code section because
  #   i <- which(w == "least")                               ### a.) won't work since only 1 word passed at a time in code below
  #   if(i > 1 && w[i-1] != "at") {TRUE}                     ### b.) separate least check fx written
  # }
}

#check negation words
negation_check <- function(wpe, start_i, i, valence){
  wpe_lower <- tolower(wpe)
  if(start_i==1){
    if(negated(wpe_lower[i-1])) {valence <- valence * N_SCALAR}
  }
  if(start_i==2){
    if(wpe_lower[i-2]=="never" && wpe_lower[i-1] %in% c("so", "this")) {valence <- valence*1.25}
    else if(wpe_lower[i-2]=="without" && wpe_lower[i-1] =="doubt") {valence <- valence}  #WITHOUT in NEGATE dict, so cancelling effect
    else if(negated(wpe_lower[i-2])) {valence <- valence * N_SCALAR}
  }
  if(start_i==3){
    if(wpe_lower[i-3]=="never" && (wpe_lower[i-2] %in% c("so", "this") || wpe_lower[i-1] %in% c("so", "this"))) {valence <- valence*1.25}
    else if(wpe_lower[i-3]=="without" && (wpe_lower[i-2]=="doubt" || wpe_lower[i-1]=="doubt")) {valence <- valence}
    else if(negated(wpe_lower[i-3])) {valence <- valence * N_SCALAR}
  }
  return(valence)
}

# dampen/amplify the scalar modifier of preceding words and emoticons (excluding the ones that immediately preceed the item)
# dampening/amplifying is based on their distance from the current item
modify <- function(wpe, i, is_cap_diff, valence){
  for(start_i in c(1:4)){
    # booster
    if(i > start_i && (tolower(wpe[i-start_i]) == "kind" || !(tolower(wpe[i-start_i]) %in% vaderLexicon$V1))){
      s <- get_scalar(wpe, i, start_i, is_cap_diff, valence)
      valence <- valence + s
    }
  }
  for(start_i in c(1:3)) {
    if(i > start_i) {
      # negation
      valence <- negation_check(wpe, start_i, i, valence)
      # idiom
      # if(start_i == 3) {valence <- idioms_check(wpe, i, valence)} #this is from the original script, but not implementing idioms here
    }
  }
  return(valence)
}

##################################

#check least modifier
least_check <- function(wpe, i, valence){
  if(i > 2 && tolower(wpe[i-1]) == "least"){
    if(!(tolower(wpe[i-2]) %in% c("at", "very"))) {valence <- valence * N_SCALAR}
  } else if(i > 1 && tolower(wpe[i-1]) == "least") {valence <- valence * N_SCALAR}
  return(valence)
}

#############################################################
# main function to calculate sentiment scores for each word #
#############################################################

senti_valence <- function(wpe, i, item){

  is_cap_diff <- allcap_diff(wpe)
  valence <- mean(get_vader_score(item))                                                            ### getting mean of score because sometimes word is duped in dictionary
  valence <- no_check(wpe, i, item, valence)
  valence <- idioms_check(wpe, i, valence)                                                          ### moved idioms above modify (and all_caps)
  valence <- all_caps(wpe, i, is_cap_diff, valence)
  valence <- modify(wpe, i, is_cap_diff, valence)
  valence <- least_check(wpe, i, valence)
  return(valence)
}
