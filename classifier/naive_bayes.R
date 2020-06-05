#Import package
library(dplyr)
library(tidyverse)
library(tm)
library(e1071)
library(caret)

features_rds_path = "classifier/features.rds"
naive_bayes_rda_path = "classifier/naive_bayes.rda"

# Membersihkan data dan merubah data menjadi bentuk corpus
clean_data <- function(data) {
  corpus <- VCorpus(VectorSource(data))
  
  corpus_clean <- tm_map(corpus, content_transformer(tolower))
  corpus_clean <- tm_map(corpus_clean, removeNumbers)
  corpus_clean <- tm_map(corpus_clean, removeWords, stopwords())
  corpus_clean <- tm_map(corpus_clean, removePunctuation)
  corpus_clean <- tm_map(corpus_clean, stripWhitespace)
  
  return(corpus_clean)
}

# Menerapkan features dan mengubah data menjadi document term matrix
apply_feature <- function(corpus, features) {
  dtm <- DocumentTermMatrix(corpus, control = list(dictionary = features))
  return(apply(dtm, 2, convert_count))
}

# Mengubah jumlah kemunculan kata menjadi "Yes" dan "No"
convert_count <- function(x) {
  y <- ifelse(x > 0, 1,0)
  y <- factor(y, levels=c(0,1), labels=c("No", "Yes"))
  return(y)
}

# Traning naive bayes model
train_model <- function() {
  # Membaca training dataset
  file_path <- "dataset/tripadvisor-restauran-traning-dataset.txt"
  data.source <- read_delim(file_path, delim = "\t")
  
  # Menambahkan kolom kelas pada data frame
  data.source$sentiment <-  ifelse(data.source$score > 0, "Positive", "Negative")
  # Mengubah data menjadi factor
  data.source$sentiment <- as.factor(data.source$sentiment)
  
  # Mengacak data agar tidak berurutan
  set.seed(1)
  data.source <- data.source[sample(nrow(data.source)),]
  
  # Pembersihan data
  data.corpus <- clean_data(data.source$review)
  
  # Mengubah data corpus menjadi document term matrix
  data.dtm <- DocumentTermMatrix(data.corpus)
  
  # Rasio perbandingan antara data training dengan data testing
  training_ratio = 0.8
  
  # Memecah data menjadi data training dan data testing
  data.source.total <- nrow(data.source)
  data.source.train <- data.source[1 : round(training_ratio * data.source.total),]
  data.source.test <- data.source[(round(training_ratio * data.source.total) + 1) : data.source.total,]
  
  data.corpus.total <- length(data.corpus)
  data.corpus.train <- data.corpus[1 : round(training_ratio * data.corpus.total)]
  data.corpus.test <- data.corpus[(round(training_ratio * data.corpus.total) + 1) : data.corpus.total]
  
  data.dtm.total <- nrow(data.dtm)
  data.dtm.train <- data.dtm[1 : round(training_ratio * data.dtm.total),]
  data.dtm.test <- data.dtm[(round(training_ratio * data.dtm.total) + 1) : data.dtm.total,]
  
  # Mengambil kata yang sering muncul, minimal 3 kali 
  freq_terms <- findFreqTerms(data.dtm.train, 3)
  length(freq_terms)
  
  # Save features yang sudah dibuat
  saveRDS(freq_terms, file = features_rds_path)
  
  # Mengaplikasikan fungsi convert_count untuk mendapatkan hasil training dan testing DTM
  data.dtm.train <- apply_feature(data.corpus.train, freq_terms)
  data.dtm.test <- apply_feature(data.corpus.test, freq_terms)
  
  # Membuat model naive bayes
  model <- naiveBayes(data.dtm.train, data.source.train$sentiment, laplace = 1)
  
  # Save Model yang sudah dibuat agar bisa dipakai di Shiny
  save(model, file = naive_bayes_rda_path)
  
  # Membuat prediksi
  prediction <- predict(model, newdata = data.dtm.test) 
  
  # Mengecek akurasi dari model yang telah dibuat
  result <- confusionMatrix(table(Prediction = prediction, Actual = data.source.test$sentiment))
  result
}

# Prediksi sentimen
predict_sentiment <- function(review) {
  features <- readRDS(features_rds_path)
  model <- get(load(naive_bayes_rda_path))
  
  data.corpus <- clean_data(review)
  data.test <- apply_feature(data.corpus, features = features)
  prediction <- predict(model, newdata = data.test) 
  
  return(data.frame(review = review, sentiment = prediction))
}

# Hapus komentar untuk traning data
# train_model()
