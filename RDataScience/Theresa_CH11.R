#####String Basics#####

#1.
(paste("a", "b", sep = "_")) #concatinates strings with sep between them
(paste0("a", "b")) #concatinates strings with no sep between them

#they are like str_c(), with and without sep
#str_c() treats NA as a missing value, whereas paste and paste0 treat NA as a String

#2. 
?str_c
#sep determines what character to put in between Strings
#collapse determines whether to make the items of the vector into a single string

#3.
my_string = "the quic"
str_length(my_string)

str_sub(my_string, start = (str_length(my_string)/2 + 1), end = (str_length(my_string)/2 + 1)) #for even numbers, returns the second "middle" character

#4.
?str_wrap
#adds line breaks and indents to a string
#good for if you're formatting to print

#5.
?str_trim
#removes whitespace from beginning and end of string
?str_pad
#adds whitespace (opposite of trim)

#6.
list_maker = function(vector){
  if(length(vector) == 0)
    return("")
  
  else if(length(vector) == 1)
    return(str_c(vector))
  
  else if(length(vector) ==2)
    return(str_c(vector, collapse = " and "))
  
  else
    prefix <- vector[1:length(vector) - 1] %>% str_c(collapse = ", ")
    suffix <- str_c(", and", vector[length(vector)], sep = " ")
    return(str_c(prefix, suffix, sep = ""))
}

#####Matching Patterns with Regular Expressions#####

#Basic Matches
#1. \ doesn't work because that is the escape character so R sees that as ""
# \\ doesn't work because R sees that as \, which is the escape character in strings, so matches to ""
# \\\ doesn't work because it results in a backslash in the String, which escapes the next character
#(this makes sense in my head, but it's really hard to type out)

#2. 
str_view("\"'\\", "\"'\\\\")

#3. 
# a period and then any character
".c.a.t"

#Anchors
#1.
str_view("$^$", "\\$\\^\\$")

#2. 
str_view(words, "^y", match = T)
str_view(words, "x$", match = T)
str_view(words, "^...$", match = T)
str_view(words, "^.......", match = T)

#Character Classes and Alternatives
#1.
str_view(words, "^[aeiouy]", match = T)
str_view(words, "^[^aeiouy]$", match = T)
str_view(words, "[^e]ed$", match = T)
str_view(words, "ing$|ize$", match =T)
#alternatively:
str_view(words, "i(ng|ze)$", match=T)

#2. 
str_view(words, "(cei|[^c]ie)", match = T) #follow rule
str_view(words, "cie|[^c]ei", match = T) #don't follow rule

#3. 
str_view(words, "q[^u]", match = T) #yes

#4.
str_view(words, "ou|ae", match = T)

#5.
str_view("(650)275-3887", "\\(\\d\\d\\d\\)\\d\\d\\d\\-\\d\\d\\d\\d")

#Repitition
#1.? = {0,1}
# + = {1,}
#* = {0, }

#2. 
#a. any word
#b. any word (1 or more char) surrounded by curly braces
#c. four digits - two digits - four digits
#d.four backslashes

#3. 
str_view(words, "^[^aeiou]{3}", match = T)
str_view(words, "[aeiou]{3,}", match = T)
str_view(words, "([aeiou][^aeiou]){2,}", match = T)

#Grouping and Backreferences
#1.
#a. any string with a letter repeated 3 times
#b. any string with an "ABBA" pattern ("ANNA", for example)
#c. any string with a two letter combination that is repeated
#d. a letter repeated 3 times, separated by any letters ("AKANAI")
#e. three letter string, any number of letters, the same 3 letter string repeated backwards
#e.g. "ABCCBA" or "ABCXYXYHSCBA"

#2. 
str_view(words, "^(.).*\\1$", match = T)
str_view(words, "(..).*\\1", match = T)
str_view(words, "(.).*\\1.*\\1", match = T)

#####Tools#####

#Detect Matches
#a. 
str_view(words, "x$", match = T)
words[str_detect(words, "x$")]

#b.
str_view(words, "^[aeiou].*[^aeiou]$", match = T)
start_w_vowel <- str_detect(words, "^[aeiou]")
end_w_consonant <- str_detect(words, "[^aeiou]$")
words[start_w_vowel & end_w_consonant]

#c.
cont_a <- str_detect(words, "a")
cont_e <- str_detect(words, "e")
cont_i <- str_detect(words, "i")
cont_o <- str_detect(words, "o")
cont_u <- str_detect(words, "u")

words[cont_a & cont_e & cont_i & cont_o & cont_u] #no words contain all vowels

#d.
words1 <- words %>%
  as.tibble() %>%
  mutate(
    vowels = str_count(words, "[aeiou]"),
    prop_vowels = vowels / str_count(words, ".")
  ) %>%
  arrange(desc(vowels))
#the largest number of vowels is 5 (appropriate, associate, avaliable, etc)

words2 <- words1 %>%
  arrange(desc(prop_vowels))
# the largest proportion of vowels is 1 ("a")

#Extract Matches

#1.
colors <- c(
" red ", " orange ", " yellow ", " green ", " blue ", " purple "
)
colors_end <- colors %>% str_trim(side = "right") %>% str_c(".")
colors_begin <- colors %>% str_trim(side = "left") %>% str_to_title()
color_match <- str_c(colors, colors_end, colors_begin, sep = "|", collapse = "|")
color_match

color_sentences <- sentences[str_detect(sentences, color_match)]
str_view_all(sentences, color_match, match =T)

#2.
#a.
first_word <- str_extract(sentences, "[a-zA-Z]*")

#b.
action_verbs <- str_extract_all(sentences, "[a-zA-Z]*ing", simplify = TRUE)

#c. 
action_verbs <- str_extract_all(sentences, "[a-zA-Z]{3,}s ", simplify = TRUE)

#Grouped Matches
#1.
numbers <- " (one|two|three|four) ([^ ]+)"

after_num <- str_extract(sentences, numbers)
 
tibble(sentence = sentences) %>%
  tidyr::extract(
    sentence, c("number", "word"), numbers,
    remove = FALSE
  ) %>%
  filter(!is.na(number))

#2.
contractions <- "([a-zA-Z]+)'([a-zA-Z]+)" #i don't exclude s for possessives because of words like it's

str_extract(sentences, contractions)

tibble(sentence = sentences) %>%
  tidyr::extract(
    sentence, c("before_apostrophe", "after_apostrophe"), contractions,
    remove = FALSE
  ) %>%
  filter(!is.na(before_apostrophe))

#Replacing Matches
#1. 
slashes <-  c("he/him", "she/her", "they/them")

str_replace(slashes, "/", "\\\\")

#2. 
rep_upper <- c(
  "A" = "a", "B" = "b", "C" = "c" #etc etc. Typing out all the letters would be a waste of time
               )
test <- c("HI", "sPoNgEbOB")

str_replace_all(test, rep_upper)

#3. 
str_replace(words, "^(.)(.*)(.)$", "\\3\\2\\1")
#the strings with the same first and last letter (e.g. yesterday) are still words

#Splitting
#1. 
example <- "apples, pears, and bananas"
str_split(example, ", +(and)? *")

#2. Because using " " will sometimes include punctuation

#3. 
str_split(example, "")
#it extracts each character
# equal to boundary("character")

#####Other Types of Patterns#####
#1. 
example = c("no slashes", "yes\\ slashes\\")

reg_expression_vers <- regex("\\\\")
fixed_vers <- fixed("\\")

str_view_all(example, fixed_vers)

#2.
words_from_sent <- str_extract_all(sentences, boundary("word")) %>%
  unlist() %>%
  as.tibble() %>%
  rename("word" = "value") %>%
  mutate("word" = str_to_lower(word)) %>%
  count(word) %>%
  arrange(desc(n))

#they, a, of, to, and

#####stringi#####

#1.
#a. stri_count (and 5 associated functions)
#b. stri_duplicated and stri_duplicated_any
#c. stri_rand_strings (and stri_rand_lipsum, which isn't truly random, but produced "nonsense" text)

#2. use locale = "___"

