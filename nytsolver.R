library(tidyverse)

# Enter letters for NYT Spelling Bee, putting the center letter first
nytimes  <- c("l",
              "o",
              "u",
              "r",
              "n",
              "i",
              "p") %>%
  as.data.frame() 

# Download a dictionary of all English words
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
words <- unlist(sapply(dict_files, readLines, USE.NAMES=FALSE))

# solve the puzzle!
reword <- words %>% 
  # Make data frame
  as.data.frame() %>%
  # Name the main column
  mutate(word = words) %>%
  # Remove the prior column
  select(-c(1)) %>%
  # Filter to words longer than 3 letters  
  filter(nchar(word)>3) %>%
  # Remove words with apostrophes
  filter(!grepl("'",word)) %>%
  # Remove words with capital letters
  filter(!grepl("[A-Z]",word)) %>%
  # Filter to words containing the first letter of the NYT prompt
  filter(grepl(nytimes[[1]][1],word)) %>%
  # Produce a list of letters not contained in the NYT prompt using the letters
  # dataset native to R. Then, filter out words that contain these letters.
  # This will leave you with words that work in the NYT puzzle.
  filter(!grepl(paste(unlist(
    as.list(letters %>%
            as.data.frame() %>%
            anti_join(.,nytimes))),
            collapse = "|"),
            word))
  