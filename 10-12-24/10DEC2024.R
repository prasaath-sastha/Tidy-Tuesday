library(tidyverse)
library(tidytuesdayR)

tues_data <- tt_load(2024, week = 50)

perfume_data <- tues_data$parfumo_data_clean

view(perfume_data)

ggplot(perfume_data, aes(y = Rating_Value, x = Release_Year))+
  geom_histogram()


library(tm)
library(wordcloud)
library(RColorBrewer)


main_accords <- perfume_data$Main_Accords
main_accords <- na.omit(main_accords)


all_accords <- paste(main_accords, collapse = ", ")


corpus <- Corpus(VectorSource(all_accords))
corpus <- tm_map(corpus, content_transformer(tolower))  
corpus <- tm_map(corpus, removePunctuation)            
corpus <- tm_map(corpus, removeNumbers)                
corpus <- tm_map(corpus, removeWords, stopwords("en"))


tdm <- TermDocumentMatrix(corpus)
matrix <- as.matrix(tdm)


word_freq <- sort(rowSums(matrix), decreasing = TRUE)
word_freq_df <- data.frame(word = names(word_freq), freq = word_freq)


set.seed(123)  
wordcloud(words = word_freq_df$word,
          freq = word_freq_df$freq,
          min.freq = 2,            
          max.words = 100,        
          random.order = FALSE,
          colors = brewer.pal(8, "Dark2"))
title(main = "Main Accords", col.main = "#fb6f92", cex.main = 3)
mtext("Visualsing the source of parfumo", side = 1, line = 0, col = "#ffb3c6", cex = 1)


top_notes <- perfume_data$Top_Notes
top_notes <- na.omit(top_notes)


all_top_notes <- paste(top_notes, collapse = ", ")


corpus <- Corpus(VectorSource(all_top_notes))
corpus <- tm_map(corpus, content_transformer(tolower))  
corpus <- tm_map(corpus, removePunctuation)            
corpus <- tm_map(corpus, removeNumbers)                
corpus <- tm_map(corpus, removeWords, stopwords("en")) 


tdm <- TermDocumentMatrix(corpus)
matrix <- as.matrix(tdm)

word_freq <- sort(rowSums(matrix), decreasing = TRUE)
word_freq_df <- data.frame(word = names(word_freq), freq = word_freq)


set.seed(123)  
wordcloud(words = word_freq_df$word,
          freq = word_freq_df$freq,
          min.freq = 2,            
          max.words = 100,         
          random.order = FALSE,   
          colors = brewer.pal(8, "Dark2"))

title(main = "😶‍🌫️Top Notes in the air👃", col.main = "#84a59d", cex.main = 3)
mtext("Visualizing the most frequent fragrance top notes", side = 1, line = 0, col = "#f6bd60", cex = 1)



middle_notes <- perfume_data$Middle_Notes
middle_notes <- na.omit(middle_notes)


all_middle_notes <- paste(middle_notes, collapse = ", ")


corpus <- Corpus(VectorSource(all_middle_notes))
corpus <- tm_map(corpus, content_transformer(tolower))  
corpus <- tm_map(corpus, removePunctuation)            
corpus <- tm_map(corpus, removeNumbers)                
corpus <- tm_map(corpus, removeWords, stopwords("en")) 


tdm <- TermDocumentMatrix(corpus)
matrix <- as.matrix(tdm)

word_freq <- sort(rowSums(matrix), decreasing = TRUE)
word_freq_df <- data.frame(word = names(word_freq), freq = word_freq)


set.seed(123)  
wordcloud(words = word_freq_df$word,
          freq = word_freq_df$freq,
          min.freq = 2,            
          max.words = 100,         
          random.order = FALSE,   
          colors = brewer.pal(8, "Dark2"))

title(main = "Middle Notes that Pull🧲", col.main = "#9b5de5", cex.main = 3)
mtext("Visualizing the strength of gravity🧭", side = 1, line = 0, col = "#00bbf9", cex = 1)


base_notes <- perfume_data$Base_Notes
base_notes <- na.omit(base_notes)


all_base_notes <- paste(base_notes, collapse = ", ")


corpus <- Corpus(VectorSource(all_base_notes))
corpus <- tm_map(corpus, content_transformer(tolower))  
corpus <- tm_map(corpus, removePunctuation)            
corpus <- tm_map(corpus, removeNumbers)                
corpus <- tm_map(corpus, removeWords, stopwords("en")) 


tdm <- TermDocumentMatrix(corpus)
matrix <- as.matrix(tdm)

word_freq <- sort(rowSums(matrix), decreasing = TRUE)
word_freq_df <- data.frame(word = names(word_freq), freq = word_freq)


set.seed(123)  
wordcloud(words = word_freq_df$word,
          freq = word_freq_df$freq,
          min.freq = 2,            
          max.words = 100,         
          random.order = FALSE,   
          colors = brewer.pal(8, "Dark2"))

title(main = "Base Notes that Click💌", col.main = "#e55812", cex.main = 3)
mtext("The ONE💘", side = 1, line = 0, col = "#95c623", cex = 1)

