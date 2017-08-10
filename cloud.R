library(tm)
library("SnowballC")
library("wordcloud")
library("RColorBrewer")
library(stringr)

library(rvest)

url <- "http://www.myjoyonline.com/news/2017/January-7th/full-text-president-akufo-addos-inaugural-speech.php"

speech <- read_html(url)

speech_data <- html_nodes(speech, css = "p") %>%
  html_text()

speech_data <- speech_data[-c(1:4)]


make_word_cloud <- function(documents) {
  corpus = Corpus(VectorSource(tolower(documents)))
  corpus = tm_map(corpus, removePunctuation)
  corpus = tm_map(corpus, removeNumbers)
  corpus = tm_map(corpus, removeWords, c(stopwords("english"), "will"))
  
  frequencies = DocumentTermMatrix(corpus)
  word_frequencies = as.data.frame(as.matrix(frequencies))
  
  words <- colnames(word_frequencies)
  freq <- colSums(word_frequencies)
  wordcloud(words, freq,
            min.freq=sort(freq, decreasing=TRUE)[[200]],
            colors=brewer.pal(8, "Dark2"),
            random.color=FALSE,
            random.order = FALSE,
            rot.per = .1)  
}

png("wordcloud.png")
make_word_cloud(speech_data)
dev.off()

new_post(title = "WordCloud on President Nana Akufo-Addo Inaugural Speech", rmd = TRUE)
