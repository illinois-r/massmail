## ---- email-body-txt ----

## Obtain e-mail content corpus ----
massmail_corpus = VCorpus(VectorSource(massmail_data_covid$content))

## Clean Text Corpus ----
clean_abstract_corpus = function(corpus){
  corpus = tm_map(corpus, content_transformer(tolower))
  corpus = tm_map(corpus, removeWords, stopwords("en"))
  corpus = tm_map(corpus, removePunctuation)
  corpus = tm_map(corpus, removeNumbers)
  corpus = tm_map(corpus, stripWhitespace)
  corpus = tm_map(corpus, PlainTextDocument)
  corpus = tm_map(corpus, removeWords,
                  c("also", "use", "thus", "given", "well", "many",
                    "may", "via", "way", "paper", "can", "using", "used",
                    "shown", "apply", "provide", "will", "however",
                    "often",
                    "one", "two", "three", "four"))
  return(corpus)
}

massmail_corpus_cleaned = clean_abstract_corpus(massmail_corpus)

## Obtain Frequencies for Word Usage ----
dtm_massmail = TermDocumentMatrix(massmail_corpus_cleaned)
matrix_dtm = as.matrix(dtm_massmail)
sorted_matrix_dtm = sort(rowSums(matrix_dtm), decreasing = TRUE)

## Construct a popular words dataframe ----
most_popular_words = data.frame(word = names(sorted_matrix_dtm),
                                freq = sorted_matrix_dtm)

## ---- most-popular-words ----
head(most_popular_words, 10) %>%
  knitr::kable(row.names = FALSE)

## ---- word-cloud ----

## Create a wordcloud ---- 
set.seed(1830)
subset_popular_words = most_popular_words[most_popular_words$freq > 20, ]
ggplot(subset_popular_words,
       aes(
         label = word,
         size = freq,
         color = word
       )) +
  geom_text_wordcloud_area(
  ) +
  scale_size_area(max_size = 12) +
  theme_minimal() +
  theme(plot.background = element_rect(fill = "black"),
        plot.title = element_text(color = "white", hjust = 0.5, vjust = -5, size = 16)) +
  theme(plot.margin = unit(c(0, 0, 0, 0), "cm")) +
  labs(title = "Most Popular Words Used in Massmails During COVID-19")