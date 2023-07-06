#import vader lexicon
# vaderLexicon <- read.delim("vaderLexicon.txt", header = F, stringsAsFactors = F, quote = "")

#create vader constants

# empirically derived mean sentiment intensity rating increase/decrease for booster words
B_INCR = 0.293
B_DECR = -0.293

# empirically derived mean sentiment intensity rating increase for using ALLCAPs to emphasize a word
C_INCR = 0.733
N_SCALAR = -0.74

######################################################################################################################

NEGATE = c("aint",
           "arent",
           "cannot",
           "cant",
           "couldnt",
           "darent",
           "didnt",
           "doesnt",
           "ain't",
           "aren't",
           "can't",
           "couldn't",
           "daren't",
           "didn't",
           "doesn't",
           "dont",
           "hadnt",
           "hasnt",
           "havent",
           "isnt",
           "mightnt",
           "mustnt",
           "neither",
           "don't",
           "hadn't",
           "hasn't",
           "haven't",
           "isn't",
           "mightn't",
           "mustn't",
           "neednt",
           "needn't",
           "never",
           "none",
           "nope",
           "nor",
           "not",
           "nothing",
           "nowhere",
           "oughtnt",
           "shant",
           "shouldnt",
           "uhuh",
           "wasnt",
           "werent",
           "oughtn't",
           "shan't",
           "shouldn't",
           "uh-uh",
           "wasn't",
           "weren't",
           "without",
           "wont",
           "wouldnt",
           "won't",
           "wouldn't",
           "rarely",
           "seldom",
           "despite")


# booster/dampener 'intensifiers' or 'degree adverbs'
# http://en.wiktionary.org/wiki/Category:English_degree_adverbs

BOOSTER_POS_NAMES = c("absolutely",
                      "amazingly",
                      "awfully",
                      "completely",
                      "considerable",
                      "considerably",
                      "decidedly",
                      "deeply",
                      "effing",
                      "enormous",
                      "enormously",
                      "entirely",
                      "especially",
                      "exceptional",
                      "exceptionally",
                      "extreme",
                      "extremely",
                      "fabulously",
                      "flipping",
                      "flippin",
                      "frackin",
                      "fracking",
                      "fricking",
                      "frickin",
                      "frigging",
                      "friggin",
                      "fully",
                      "fuckin",
                      "fucking",
                      "fuggin",
                      "fugging",
                      "greatly",
                      "hella",
                      "highly",
                      "hugely",
                      "incredible",
                      "incredibly",
                      "intensely",
                      "major",
                      "majorly",
                      "more",
                      "most",
                      "particularly",
                      "purely",
                      "quite",
                      "really",
                      "remarkably",
                      "so",
                      "substantially",
                      "thoroughly",
                      "total",
                      "totally",
                      "tremendous",
                      "tremendously",
                      "uber",
                      "unbelievably",
                      "unusually",
                      "utter",
                      "utterly",
                      "very")

BOOSTER_POS <- rep(B_INCR, length(BOOSTER_POS_NAMES))
names(BOOSTER_POS) <- BOOSTER_POS_NAMES

BOOSTER_NEG_NAMES = c("almost",
                      "barely",
                      "hardly",
                      "just enough",
                      "kind of",
                      "kinda",
                      "kindof",
                      "kind-of",
                      "less",
                      "little",
                      "marginal",
                      "marginally",
                      "ocassional",
                      "occasionally",
                      "partly",
                      "scarce",
                      "scarcely",
                      "slight",
                      "slightly",
                      "somewhat",
                      "sort of",
                      "sorta",
                      "sortof",
                      "sort-of")

BOOSTER_NEG<- rep(B_DECR, length(BOOSTER_NEG_NAMES))
names(BOOSTER_NEG) <- BOOSTER_NEG_NAMES

BOOSTER_DICT = c(BOOSTER_POS, BOOSTER_NEG)

# check for special case idioms using a sentiment-laden keyword known to SAGE
IDIOMS = c("the shit" = 3,
           "the bomb" = 3,
           "bad ass" = 1.5,                                  ### BOTH WORDS IN DICT
           # "badass" = 1.5,                                 ### removing because in dictionary as single word
           "yeah right" = -2,                                ### right not in dic
           "kiss of death" = -1.5,                           ### BOTH WORDS IN DICT
           "to die for" = 3,
           "cut the mustard" = 2,                            ### mustard not in dic
           "hand to mouth" = -2,                             ### mouth not in dic
           "upper hand" = 1,                                 ### upper not in dic
           ################################################# IDIOMNS NOT IN VADER LEXICION
           "back handed" = -2,
           "blow smoke" = -2,
           "blowing smoke" = -2,
           "break a leg" = 2,
           "cooking with gas" = 2,
           "in the black" = 2,
           "in the red" = -2,
           "on the ball" = 2,
           "under the weather" = -2)
