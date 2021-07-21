library(tidyverse)

# Download a dictionary of all words
dict_dir <- tempdir()
dict_url <- 'http://downloads.sourceforge.net/wordlist/scowl-2016.01.19.zip'
dict_local_zip <- file.path(dict_dir, basename(dict_url))
if (! file.exists(dict_local_zip)) {
  download.file(dict_url, dict_local_zip)
  unzip(dict_local_zip, exdir=dict_dir)
}

#filter to english words
dict_files <- list.files(file.path(dict_dir, 'final'), full.names=TRUE)
dict_files_match <- as.numeric(tools::file_ext(dict_files)) <= 60 & grepl("english-", dict_files, fixed = TRUE)
dict_files <- dict_files[ dict_files_match ]

#produce a series of all ~120k words
words <- unlist(sapply(dict_files, readLines, USE.NAMES=FALSE)) %>% 
  as.data.frame() %>%
  mutate(word = words) %>%
  select(-c(1)) %>%
  filter(nchar(word)>3) %>%
  filter(!grepl("'",word)) %>%
  filter(!grepl("[A-Z]",word))

##Create Functions

#Function counting the number of distinct letters in words
numlet <- function(word1){
  numlets <- 0
  for (i in letters){
    if(grepl(i,word1)==T){
      numlets <- numlets + 1
    }
  }
  return(numlets)
}

#List out ordered letters in each word
listletts <- function(word2){
  listlet <- c()
  for (i in letters){
    if(grepl(i,word2)==T){
      listlet <- c(listlet,i)}
  }
  list(listlet)
}

#Produce list of all solutions for a given letter combo
solutions <- function(letts1){
  
  letts2 <- letts1 %>% 
    as.data.frame()
  
  words %>%
    filter(!grepl(paste(unlist(
      as.list(letters %>%
                as.data.frame() %>%
                anti_join(.,letts2))),
      collapse = "|"),.))
}

#Produce filtered list based on center letter
iterate <- function(let,num,ans){
  ans <- as.data.frame(ans)
  ans %>% 
  filter(.,grepl(as.data.frame(let)[[1]][num],.[[1]])==T) 
}

#Produce total score of each iteration
score <- function(entry){
  score1 <- function(entry1){
    ifelse(nchar(entry1)>4,
       ifelse(nchar(listletts(entry1))==36,
                    14,nchar(entry1)),1)
  }
  enter <- as.data.frame(entry) %>% 
    mutate(score = sapply(.[[1]],score1)) 
  sum(enter$score)
}

#Count the number of words in each cell
numberWords <- function(list){
  nrow(as.data.frame(list))
}

#Unlist each row so it can be exported as a CSV
pasteUnlisted <- function(list){
  paste(unlist(list),collapse = ", ")
}


# Filter list of words to panagrams
reword <- words  %>% 
  # Filter to panagrams
    mutate(number = sapply(word,numlet)) %>% 
    filter(number == 7) 

#Add list of solutions for each panagram-creating letter combination
reword1 <- reword %>% 
  mutate(lets = sapply(word,listletts)) %>% 
  select(-c(1,2)) %>% 
  unique() %>% 
  mutate(lets = sapply(lets,as.data.frame)) %>% 
  #head() %>% 
  mutate(answers = sapply(lets,solutions))

#Produce word list for each potential center letter
reword2 <- reword1 %>% 
  #head() %>% 
  mutate(first = mapply(iterate,
                        let = lets,
                        num = 1,
                        ans = answers),
         second = mapply(iterate,
                        let = lets,
                        num = 2,
                        ans = answers),
         third = mapply(iterate,
                        let = lets,
                        num = 3,
                        ans = answers),
         fourth = mapply(iterate,
                        let = lets,
                        num = 4,
                        ans = answers),
         fifth = mapply(iterate,
                        let = lets,
                        num = 5,
                        ans = answers),
         sixth = mapply(iterate,
                        let = lets,
                        num = 6,
                        ans = answers),
         seventh = mapply(iterate,
                        let = lets,
                        num = 7,
                        ans = answers))
  
#Produce score for each potential combination
reword3 <- reword2 %>% 
  #head() %>% 
  mutate(
    firstscore = sapply(first,score),
    secondscore = sapply(second,score),
    thirdscore = sapply(third,score),
    fourthscore = sapply(fourth,score),
    fifthscore = sapply(fifth,score),
    sixthscore = sapply(sixth,score),
    seventhscore = sapply(seventh,score)
  )

reword4 <- reword3 %>% 
  #head() %>% 
  mutate(
    firstnumber = sapply(first,numberWords),
    secondnumber = sapply(second,numberWords),
    thirdnumber = sapply(third,numberWords),
    fourthnumber = sapply(fourth,numberWords),
    fifthnumber = sapply(fifth,numberWords),
    sixthnumber = sapply(sixth,numberWords),
    seventhnumber = sapply(seventh,numberWords)
  )

reworder <- reword4 %>% 
  #head() %>% 
  mutate(
    lets = sapply(lets,pasteUnlisted),
    first = sapply(first,pasteUnlisted),
    second = sapply(second,pasteUnlisted),
    third = sapply(third,pasteUnlisted),
    fourth = sapply(fourth,pasteUnlisted),
    fifth = sapply(fifth,pasteUnlisted),
    sixth = sapply(sixth,pasteUnlisted),
    seventh = sapply(seventh,pasteUnlisted)
  ) %>% 
  select(-c(2))

write.csv(reworder,"~rewrite.csv")
