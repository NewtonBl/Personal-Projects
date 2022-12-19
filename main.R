library(tidyverse)
library(stringr)

#Import the .txt file containing the words to be analyzed
raw_document <- read_file("text.txt")

#Build a tibble of the words from the file
raw_tibble <- tibble(word = unlist(str_extract_all(raw_document, boundary("word"))))

#Filter the tibble to eliminate "words" containing or consisting of letters
no_numbers <- raw_tibble |> filter(str_detect(word, "[0123456789]", negate = TRUE))

#Convert all words to lowercase
to_lowercase <- no_numbers |> mutate(word = str_to_lower(word))

#Build list of common words to exclude from the count
excluded_words <- c("a", "the", "and", "to", "of", "in", "with", "or", "for", "experience", "work", "you", "as", "is", "on", "required", "skills", "team", "ability", "will", "be", "including", "are", "have", "this", "at", "that", "knowledge", "environment", "support", "working", "by", "our", "position", "time", "all", "an", "duties", "other", "must", "control", "include", "level", "degree", "maintain", "perform", "performing", "performs", "we", "job", "qualifications", "related", "variety", "years", "basic", "new", "training", "using", "within", "your", "e.g", "education", "health", "next", "not", "equivalent", "etc", "general", "if", "may", "participate", "preferred", "project", "responsibilities", "staff", "able", "full", "minimum", "practices", "role", "well", "but", "center", "community", "from", "requirements")

#Count and filter out the excluded words
word_count <- to_lowercase |> count(word, sort = TRUE) |> rename(count = n)
filtered_word_count <- word_count |> filter(!(word %in% excluded_words))

#Generate top 50 tibble
top_fifty <- filtered_word_count |> head(50)

#Export list to Excel spreadsheet
write_excel_csv(top_fifty, "top50.csv")

#Create factored top 25 tibble and visualize
top_25 <- filtered_word_count |> head(25) |> arrange(count) |>  mutate(word=factor(word, levels=word))

ggplot(top_25, aes(word, count, fill = count)) +
  geom_col() +
  coord_flip() +
  xlab("Word") +
  ylab("Number of Occurrences") +
  labs(title = "Top 25 Words", caption = "25 most frequently used words in job descriptions that I applied to. \nWords were filtered to exclude common and irrelevant words. \n Provides personal insight to positions I am inherently drawn to.") +
  theme(panel.grid.major.x = element_line(color = "black", size = 0.25), 
        panel.grid.minor.x = element_line(color = "black", linetype = "dotted"))

#Save Graphic
ggsave("Top_25_Graphic.pdf")
