#install.packages("ggthemes")
# install.packages('qdap')
# install.packages('dplyr')
# install.packages('tm')
# install.packages('wordcloud')
# install.packages('plotrix')
# install.packages('dendextend')
# install.packages('ggplot2')
#install.packages('RWeka')
# install.packages('reshape2')
# install.packages(quanteda)


#library(tokenizers)
#library(tm)
library(SnowballC)
library(tidytext)
library(broom)
library(qdap)
library(dplyr) 
library(wordcloud)
#library(plotrix)
#library(dendextend)
library(ggplot2)
#library(ggthemes)
#library(RWeka)
library(reshape2)
library(tidyr)
library(quanteda)
library(xlsx)


# ------------------ Importing Sentiment Analysis Scores ----------------

reviews_scores <- read.csv('C:/Users/shore/Desktop/UIC/Spring 2019/Healthcare/Project/Reviews_scores_steve_final.csv', stringsAsFactors = FALSE)


# Dividing sentences into positive and negative bucket
# Neutral sentences have been divided into the positive bag

reviews_positive  <- subset(reviews_scores, (reviews_scores$positive == 1)|((reviews_scores$negative == 0) & (reviews_scores$positive == 0)))

reviews_negative  <- subset(reviews_scores, reviews_scores$negative == 1)


# ----- Reading Bag of words and keeping only those tokenized words which are present in vocab, all words are included -----

#To do this correctly and ensuring any major word does not misses out, we reviewed the tf-idf matrix and added words to the bag of words file

# We created a file which has all the words that are included in the bag irrespective of the bucket they are in, these form our vocab or dictionary

vocab_file <- read.csv("C:/Users/shore/Desktop/UIC/Spring 2019/Healthcare/Project/bag_file.csv")

#as_tibble conversion for tidy/dplyr format
vocab  <- as_tibble(unique(vocab_file$Vocab))%>% 
           mutate(stemmed_word = wordStem(value))

# Dividing the vocab that is present in positive context(positive sentences)and vocab which is present in the negative context in the reviews

vocab_positive <- reviews_positive %>%
                    group_by(Name) %>% 
                    ungroup() %>%  
                    unnest_tokens(word, Review) %>%
                    mutate(word = wordStem(word)) %>%
                    inner_join(vocab, by= c("word" = "stemmed_word")) %>%
                    select(Name, word) 
# ungroup() above helps ungroup the data so further lines can be executed without corrupt_df error

vocab_negative <- reviews_negative %>%
                     group_by(Name) %>% 
                     ungroup() %>% 
                     unnest_tokens(word, Review) %>%
                     mutate(word = wordStem(word)) %>%
                     inner_join(vocab, by= c("word" = "stemmed_word")) %>%
                     select(Name, word) 

# Finding the unique words in positive set of vocab 

unique_words_positive <- vocab_positive %>%
                           count(Name, word) 

	              #count gets the count for each unique word in a table format, we select the 'word' column from table which has only unique words
				   
# Finding the unique words in negative set of vocab 
unique_words_negative <- vocab_negative %>%
                           count(Name, word) 


# Concatinating from 'unnest form' to one row for each doctor format
concat_positive  <- unique_words_positive %>% 
                      group_by(Name) %>% 
                      mutate(words = paste(word, collapse = " ")) %>% slice(1) %>% 
                      select(Name, words)
            #slice helps to keep only unique rows
					  
# Concatinating from 'unnest form' to one row for each doctor format
concat_negative <- unique_words_negative %>% 
                     group_by(Name) %>% 
                     mutate(words = paste(word, collapse = " ")) %>% 
                     slice(1) %>% 
                     select(Name, words)
					 
					 
# Calculating total positive and negative words per doctor
total_words_neg <- unique_words_negative %>% 
                    group_by(Name) %>% 
                    summarize(total = n_distinct(word)) 
# n_distinct function used to choose unique words only and sum them from the unnest format

total_words_pos <- unique_words_positive %>% 
                    group_by(Name) %>% 
                    summarize(total = n_distinct(word))
					
# Applying full_join to get final tables with set of positive and negative words respectively for each doctor and total count of words

final_positive_words <- full_join(concat_positive, total_words_pos)

final_negative_words <- full_join(concat_negative, total_words_neg)

#  ------------------------ Score Calculations -----------------------------

#Reads the bag of words file with words under each bucket
bucket_file <- read.csv("C:/Users/shore/Desktop/UIC/Spring 2019/Healthcare/Project/bag of words.csv")


# Storing words under different buckets in a new table so that matching is easy

communication <- as_tibble(unique(bucket_file$Communication_skills)) %>%
                   mutate(stemmed_word = wordStem(value)) %>% 
                   count(stemmed_word) %>% select(stemmed_word)

medical_expertise <- as_tibble(unique(bucket_file$Medical_expertise)) %>%
                     mutate(stemmed_word = wordStem(value))%>%
                     count(stemmed_word) %>% select(stemmed_word)

time <-  as_tibble(unique(bucket_file$Time_spent_schedule))  %>%
                mutate(stemmed_word = wordStem(value)) %>%
                count(stemmed_word) %>% select(stemmed_word)

bonding <- as_tibble(unique(bucket_file$Patient_bonding)) %>%
              mutate(stemmed_word = wordStem(value)) %>%
              count(stemmed_word) %>% select(stemmed_word)

bedside <- as_tibble(unique(bucket_file$Bedside_manner)) %>%
              mutate(stemmed_word = wordStem(value)) %>%
              count(stemmed_word) %>% select(stemmed_word)


office <- as_tibble(unique(bucket_file$Office_Staff)) %>%
            mutate(stemmed_word = wordStem(value))%>%
            count(stemmed_word) %>% select(stemmed_word)

cost <-  as_tibble(unique(bucket_file$Cost))  %>%
             mutate(stemmed_word = wordStem(value))%>%
             count(stemmed_word) %>% select(stemmed_word)


# --------------------To run the code below this line, run the 'function definitions' section (present at the bottom) first--------------- #

# The below code calculates the count of words present in the positive and negative set respectively that matches with the each bucket for each doctor and then calculates the score

# We try to get a positive and negative score for each bucket, so in our case we will have a total of 7*2 buckets (one positive, one negative)


# Calculating the bucket scores for each doctor

# positive bucket scores
final_pos_com_score <- data.frame(bucket_score(final_positive_words,communication, 'com_pos'))
final_pos_med_score <- data.frame(bucket_score(final_positive_words,medical_expertise,'med_pos'))
final_pos_time_score <- data.frame(bucket_score(final_positive_words,time,'time_pos'))
final_pos_bonding_score <- data.frame(bucket_score(final_positive_words,bonding,'bonding_pos'))
final_pos_bedside_score <- data.frame(bucket_score(final_positive_words,bedside,'bedside_pos'))
final_pos_office_score <- data.frame(bucket_score(final_positive_words ,office,'office_pos'))
final_pos_cost_score <- data.frame(bucket_score(final_positive_words,cost,'cost_pos'))

# negative bucket scores

final_neg_com_score <- data.frame(bucket_score(final_negative_words,communication,'com_neg'))
final_neg_med_score <- data.frame(bucket_score(final_negative_words,medical_expertise,'med_neg'))
final_neg_time_score <- data.frame(bucket_score(final_negative_words,time,'time_neg'))
final_neg_bonding_score <- data.frame(bucket_score(final_negative_words,bonding,'bonding_neg'))
final_neg_bedside_score <- data.frame(bucket_score(final_negative_words,bedside,'bedside_neg'))
final_neg_office_score <- data.frame(bucket_score(final_negative_words,office,'office_neg'))
final_neg_cost_score <- data.frame(bucket_score(final_negative_words,cost,'cost_neg'))


# Now we have to join all the 14 tables together to get a combined table of all the scores for a doctor

library(plyr)

# final Score Table
final_scores <-  join_all(list(final_pos_com_score,final_pos_med_score,final_pos_time_score,
                    final_pos_bonding_score,final_pos_bedside_score,final_pos_office_score,
                    final_pos_cost_score,final_neg_com_score,final_neg_med_score,final_neg_time_score,
                    final_neg_bonding_score,final_neg_bedside_score,final_neg_office_score,
                    final_neg_cost_score),by = "Name", type = 'full')


detach(package:plyr) # detach package after using above line to avoid dplyr;plyr problem


# Replacing all the NA's with 0 since NA means that there is no negative/positive word for that bucket
final_scores[is.na(final_scores)] <- 0

#--------------------Approach 2---------------------------

# Calculating total positive and negative words per doctor
total_words_neg_2 <- unique_words_negative %>% group_by(Name) %>% 
                      summarize(total_neg = n_distinct(word)) 

# n_distinct function used to choose unique words only and sum them from the unnest format

total_words_pos_2 <- unique_words_positive %>% group_by(Name) %>% 
                      summarize(total_pos = n_distinct(word))

total_all_words <- merge(total_words_pos_2,total_words_neg_2,by="Name",all = TRUE)
# Replacing all the NA's with 0 since NA means that there is no negative/positive word for that bucket
total_all_words[is.na(total_all_words)] <- 0

total_all_words <- total_all_words %>% mutate(total_count = (total_pos + total_neg))


# Adding total_count column to final set of positive and negative words
v1_final_positive_words <- left_join(concat_positive,total_all_words)
v1_final_negative_words <- left_join(concat_negative,total_all_words)


# Score Calculation

# Calculating the bucket scores for each doctor

# positive bucket scores
v1_final_pos_com_score <- data.frame(bucket_score_v1(v1_final_positive_words,communication, 'com_pos'))
v1_final_pos_med_score <- data.frame(bucket_score_v1(v1_final_positive_words,medical_expertise,'med_pos'))
v1_final_pos_time_score <- data.frame(bucket_score_v1(v1_final_positive_words,time,'time_pos'))
v1_final_pos_bonding_score <- data.frame(bucket_score_v1(v1_final_positive_words,bonding,'bonding_pos'))
v1_final_pos_bedside_score <- data.frame(bucket_score_v1(v1_final_positive_words,bedside,'bedside_pos'))
v1_final_pos_office_score <- data.frame(bucket_score_v1(v1_final_positive_words ,office,'office_pos'))
v1_final_pos_cost_score <- data.frame(bucket_score_v1(v1_final_positive_words,cost,'cost_pos'))

# negative bucket scores

v1_final_neg_com_score <- data.frame(bucket_score_v1(v1_final_negative_words,communication,'com_neg'))
v1_final_neg_med_score <- data.frame(bucket_score_v1(v1_final_negative_words,medical_expertise,'med_neg'))
v1_final_neg_time_score <- data.frame(bucket_score_v1(v1_final_negative_words,time,'time_neg'))
v1_final_neg_bonding_score <- data.frame(bucket_score_v1(v1_final_negative_words,bonding,'bonding_neg'))
v1_final_neg_bedside_score <- data.frame(bucket_score_v1(v1_final_negative_words,bedside,'bedside_neg'))
v1_final_neg_office_score <- data.frame(bucket_score_v1(v1_final_negative_words,office,'office_neg'))
v1_final_neg_cost_score <- data.frame(bucket_score_v1(v1_final_negative_words,cost,'cost_neg'))


library(plyr)

# final Score Table
v1_final_scores <-  join_all(list(v1_final_pos_com_score,v1_final_pos_med_score,
                                  v1_final_pos_time_score,v1_final_pos_bonding_score,
                                  v1_final_pos_bedside_score,v1_final_pos_office_score,
                                  v1_final_pos_cost_score,v1_final_neg_com_score,
                                  v1_final_neg_med_score,v1_final_neg_time_score,
                                  v1_final_neg_bonding_score,v1_final_neg_bedside_score,v1_final_neg_office_score,v1_final_neg_cost_score),
                                  by = "Name", type = 'full')


detach(package:plyr) # detach package after using above line to avoid dplyr;plyr problem


# Replacing all the NA's with 0 since NA means that there is no negative/positive word for that bucket
v1_final_scores[is.na(v1_final_scores)] <- 0


#------------------ Writing All Important Tables to Files -------------------

#Writing the final_scores approach_1 dataframe to excel

write.xlsx(final_scores, file = "Bucket_scores_v2.xlsx",
sheetName="Raw_Scores", append=TRUE)

#Writing the final_scores approach_2 dataframe to excel

write.xlsx(v1_final_scores, file = "Bucket_scores_approach2.xlsx",
           sheetName="Raw_Scores", append=TRUE)

#vocab 
write.xlsx(vocab, file = "vocab_stemmed.xlsx")

#positive set of words for each doctor

write.xlsx(data.frame(final_positive_words), file = "positive_reviews_tokenized.xlsx")

#negative set of words for each doctor
write.xlsx(data.frame(final_negative_words), file = "negative_reviews_tokenized.xlsx")

#Tip - without conversion of table_df to data.frame,it will throw an error

#----------- Plots and Wordclouds ----------------

final_positive_words %>% 
 unnest_tokens(word,words)%>% 
 count(word) %>%
 with(wordcloud(word, n, max.words = 20))

# Code for resizing the plot area

resize.win <- function(Width=6, Height=6)
{
  # works for windows
  dev.off(); # dev.new(width=6, height=6)
  windows(record=TRUE, width=Width, height=Height)
}
resize.win(5,5)
plot(rnorm(100))
resize.win(10,10)
plot(rnorm(100))


#------------------------------- Function definitions -------------------------------

# The bucket_count function counts the no. of words (positive or negative set) for each doctor that match with a particular bucket in bag of words and then calculate the bucket_score and dynamically creates a score_column for that bucket

bucket_score <- function(data, bucket_name,string_bucket_name){ 
                
    df =   data %>% group_by(Name) %>% 
              ungroup() %>% 
              unnest_tokens(word, words) %>%
              inner_join(bucket_name, by= c("word" = "stemmed_word")) %>%
              #select(Name,word) %>% # not necessary with summarize function
              group_by(Name,total) %>% 
              summarize(total_match = n_distinct(word))
    
    varname <- paste("score_",string_bucket_name, sep = "")
    df[[varname]] <- with(df,(total_match/total))
    df <-  subset(df, select = c('Name','total',varname))
    df
}

bucket_score_v1 <- function(data,bucket_name,string_bucket_name){ 
  
  df =   data %>% group_by(Name) %>% 
    ungroup() %>% 
    unnest_tokens(word, words) %>%
    inner_join(bucket_name, by= c("word" = "stemmed_word")) %>%
    #select(Name,word) %>% # not necessary with summarize function
    group_by(Name,total_count) %>% 
    summarize(total_match = n_distinct(word))
  
  varname <- paste("score_",string_bucket_name, sep = "")
  df[[varname]] <- with(df,(total_match/total_count))
  df <-  subset(df, select = c('Name','total_count',varname))
  df
}




