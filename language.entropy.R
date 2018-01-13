# Date: January 10, 2018
# Procedure: language efficiency analysi: english vs german
# Purpose: to use infomration theory to determine which language is more 
#   efficient, english or german

################# SETUP ################# 
library(tidyverse)
################# FUNCTIONS #################
to.string <- function(str.vector) {
    # Converts a character vector to a string
    # Args:
    #   chr.vector: a vector of strings
    # Returns:
    #   a large string of connected words
    paste(str.vector, collapse = '')
}
#test <- c("hi", "bye")
#paste(test, collapse = '')
find.entropy <- function(word, letter.freq, per.letter = TRUE) {
    # Calculates the shannon entropy of a given string 
    # Args:
    #   word: a string
    #   letter.freq: the frequncy of given letters
    #   per.letter: boolean indicating whether per-letter entropy 
    #       should be returned. Otherwise total entropy will be returned
    # Returns:
    #   the shannon entropy of the word
    letters <- regmatches(word, gregexpr(".", word))[[1]]
    ent <- sapply(letters, function(i) {
        letter.freq[grep(i, names(letter.freq))]
    }) %>% -log2(.)
    
    if(per.letter == TRUE) {
        ent <- mean(ent)
    } else if(per.letter == FALSE) {
        ent <- sum(ent)
    } else {
        stop("Please select a boolean value for per.letter")
    }
    
    return(ent)
}
#test <- find.entropy("blue", english.freq)
is.letter <- function(x) grepl("[[:alpha:]]", x)
# Letter filter
letter.filter <- function(word) {
    # Takes in a word as input, and removes all non-letters
    # Args:
    #   word: a string of interest
    # Returns:
    #   result: the same string minus non-letters
    
    letters <- regmatches(word, gregexpr(".", word))[[1]]
    result <- sapply(letters, function(i) {
        if(!is.letter(i)) {
            return(NA)
        }
            return(i)
    })
    result <- result[complete.cases(result)]
    result <- paste(result, collapse = '') %>% tolower()
    return(result)
}
#test <- letter.filter("hi*m//o<f4>  ( ` sß`)m")

################# FREQS ################# 

# Entropy with prior knowledge of letter frequencies (no text)

# English letter freq: http://www.oxfordmathcenter.com/drupal7/node/353
english.freq <- c(a = 0.08167, 
                  b = 0.01492,
                  c = 0.02782,
                  d = 0.04253,
                  e = 0.12702,
                  f = 0.02228, 
                  g = 0.02015, 
                  h = 0.06094,
                  i = 0.06966, 
                  j = 0.00153, 
                  k = 0.00772,
                  l = 0.04025,
                  m = 0.02406,
                  n = 0.06749,
                  o = 0.07507,
                  p = 0.01929,
                  q = 0.00095,
                  r = 0.05987,
                  s = 0.06327,
                  t = 0.09056,
                  u = 0.02758,
                  v = 0.00978,
                  w = 0.02360,
                  x = 0.01974,
                  y = 0.01974,
                  z = 0.00074)

# German letter freq: https://www.sttmedia.com/characterfrequency-german
german.freq <- c(a = 0.0558,
                 b = 0.0196,
                 c = 0.0316,
                 d = 0.0498,
                 e = 0.1693,
                 f = 0.0149,
                 g = 0.0302,
                 h = 0.0498,
                 i = 0.0802,
                 j = 0.0024,
                 k = 0.0132,
                 l = 0.0360,
                 m = 0.0255, 
                 n = 0.1053,
                 o = 0.0224,
                 p = 0.0067,
                 q = 0.0002,
                 r = 0.0689,
                 s = 0.0642,
                 t = 0.0579,
                 u = 0.0383,
                 v = 0.0084,
                 w = 0.0178,
                 x = 0.0005,
                 y = 0.0005,
                 z = 0.0121,
                 ä = 0.0054,
                 ö = 0.003,
                 ü = 0.0065,
                 ß = 0.0037)

################# PIPELINE #################

# Entropy vectors, all letters 
# Average entropy for the given alphabet. It's around 5 bits / character.
eng.entropy <- -log2(english.freq) %>% mean()
ger.entropy <- -log2(german.freq) %>% mean()

# Entropy for a random sample of German or English text
eng.txt <- "This is an example of text in this written language"
ger.txt <- "dies ist ein Beispiel für einen text in dieser Shriftsprache"

# Collapse to long char
eng.txt <- to.string(eng.txt) %>% letter.filter(.) %>% find.entropy(., english.freq, per.letter = FALSE)
ger.txt <- to.string(ger.txt) %>% letter.filter(.) %>% find.entropy(., german.freq, per.letter = FALSE)



    