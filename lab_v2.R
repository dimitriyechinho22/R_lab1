library(tidytext)
library(readr)
library(dplyr)
library(ggplot2)
library(MLmetrics)

# read csv
train_path <- "train.csv"
test_path <- "test.csv"
train <- read.csv(file = train_path, stringsAsFactors = FALSE)
test <- read.csv(file = test_path, stringsAsFactors = FALSE)

# remove punctuation and stop words
stop_words_path <- "stop_words.txt"
stop_words <- read_file(stop_words_path)
splitted_stop_words <- strsplit(stop_words, split = '\n')
splitted_stop_words <- splitted_stop_words[[1]]

naiveBayes <- setRefClass("naiveBayes",
                          # here it would be wise to have some vars to store intermediate result
                          # frequency dict etc. Though pay attention to bag of wards!
                          fields = list(
                            data = "data.frame",                       # all data
                            labels = "vector",                         # array with fake and credible
                            fake_article_probability = "numeric",      # probability of fake
                            credible_article_probability = "numeric",  # probability of credible
                            all_vocabulary = "data.frame",             # dataframe with all occurences
                            fake_words_dictionary = "data.frame",      # dataframe with fake words occurences
                            credible_words_dictionary = "data.frame"   # dataframe with credible words occurences
                          ),

                          methods = list(
                            fit = function(X = data, Y = labels) {
                              tidy_text <- unnest_tokens(X, 'splitted', 'Body', token = "words") %>%
                                filter(!splitted %in% splitted_stop_words)

                              fake_article_probability <<- nrow(filter(train, Label == Y[2])) / nrow(X)
                              credible_article_probability <<- 1 - fake_article_probability

                              # all vocabulary
                              all_vocabulary <<- tidy_text %>% count(splitted, sort = TRUE)

                              # fake vocabulary
                              fake_words_dictionary <<- tidy_text[tidy_text$Label == Y[2],] %>% count(splitted, sort = TRUE) # creates {key : value}

                              # credible vocabulary
                              credible_words_dictionary <<- tidy_text[tidy_text$Label == Y[1],] %>% count(splitted, sort = TRUE)
                            },

                            probability_of_one_word = function(wordi, lable) {
                              lable_dictionary <- fake_words_dictionary
                              if (lable == "credible") {
                                lable_dictionary <- credible_words_dictionary
                              }
                              occurence <- lable_dictionary[lable_dictionary$splitted == wordi, "n"]  # amount of words
                              if (length(occurence) == 0) {
                                return(1 / (nrow(all_vocabulary) + nrow(lable_dictionary)))
                              }
                              else {
                                return((occurence + 1) / (nrow(all_vocabulary) + nrow(lable_dictionary)))
                              }
                            },

                            predict = function(message) {
                              print("ZALOOP")
                              result_for_fake <- fake_article_probability
                              result_for_credible <- credible_article_probability

                              multiplier <- 10000

                              message <- gsub('[[:punct:] ]+', ' ', message)
                              message <- casefold(message, upper = FALSE)
                              words <- strsplit(message, split = ' ')
                              words <- words[[1]]
                              words <- unlist(words)[!(unlist(words) %in% splitted_stop_words)]
                              for (word in words) {
                                result_for_fake <- result_for_fake *
                                  probability_of_one_word(word, lables[2]) * multiplier
                                result_for_credible <- result_for_credible *
                                  probability_of_one_word(word, lables[1]) * multiplier
                              }

                              if (result_for_fake > result_for_credible) {
                                return ("fake")
                              } else {
                                return ("credible")
                              }
                            },

                            score = function (X_test) {
                              X_test["prediction"] <- apply(X_test['Body'],1, FUN = model$predict)
                              return (X_test)
                            }

                          )
)


model_accuracy <- function(dataframe) {
  correct_predictions <- 0
  articles_amount <- nrow(dataframe)

  for (i in 1:articles_amount) {
    actual_value <- dataframe$Label[i]
    predicted_value <- dataframe$prediction[i]
    if (actual_value == predicted_value) {
      correct_predictions <- correct_predictions + 1
    }
  }

  accuracy_value <- correct_predictions / articles_amount
  return(accuracy_value)
}


F1_scoring <- function (dataframe){
  actual <- dataframe$Label
  predicted <- dataframe$prediction

  print(F1_Score(predicted,actual))
}


error_in_fake_analysis <- function(dataframe) {
  error_in_credible <- dataframe[dataframe$Label == "fake",]
  error_in_credible2 <- error_in_credible[error_in_credible$prediction == "credible",]

  return (error_in_credible2)
}


error_in_credible_analysis <- function(dataframe) {
  error_in_credible <- dataframe[dataframe$Label == "credible",]
  error_in_credible2 <- error_in_credible[error_in_credible$prediction == "fake",]

  return (error_in_credible2)
}


create_diagrams <- function(num_fake, num_credible, name_fake, name_credible, title) {
  # pie chart of true numbers of fake and credible articles

  first_column <- c(name_credible, name_fake)
  second_column <- c(num_credible, num_fake)
  df <- data.frame(first_column, second_column)

  # Compute the position of labels
  df <- df %>%
    arrange(desc(first_column)) %>%
    mutate(prop = second_column / sum(df$second_column) * 100) %>%
    mutate(ypos = cumsum(prop)- 0.5*prop )
  pie_chart <-ggplot(df, aes(x="", y=prop, fill=first_column)) +
    geom_bar(stat="identity", width=3, color="white") +
    coord_polar("y", start=0) +
    theme_void() +
    theme(legend.title=element_blank(), legend.position="right", legend.text = element_text(size=12))+
    geom_text(aes(y = ypos, label = as.character(second_column)), color = "white", size=5) +
    scale_fill_brewer(palette="Set5") +
    ggtitle(title, ) +
    scale_fill_hue(l=45)
  pie_chart
}


# call to class
lables <- unique(train$Label)
model <- naiveBayes(data = train, labels = unique(train$Label))
model$fit()
result <- model$score(test)

print("Model accuracy")
print(model_accuracy(result))  # ~81,8%
print(F1_scoring(result))      # ~85%

error_in_fake <- error_in_fake_analysis(result)          # dataframe with (credible - fake)
error_in_credible <- error_in_credible_analysis(result)  # dataframe with (fake - credible)

print(error_in_fake)     # dataframe with (fake - credible)
print(error_in_credible) # dataframe with (credible - fake)

num_fake_articles_test <- nrow(filter(test, Label == "fake"))
num_credible_articles_test <- nrow(filter(test, Label == "credible"))
names_initial_credible <- "number of credible articles in test.csv"
names_initial_fake <- "number of fake articles in test.csv"
initial_title <- "Initial number of fake and credible news in test.csv"
resulted_title <- "Number of fake and credible news according to Bayes Classifier"

fakes_amount <- nrow(result[result$prediction == "fake",])
credible_amount <- nrow(result[result$prediction == "credible",])

create_diagrams(num_fake_articles_test, num_credible_articles_test, names_initial_fake, names_initial_credible, initial_title)
create_diagrams(fakes_amount, credible_amount, "Predicted as fake", "Predicted as credible", resulted_title)
