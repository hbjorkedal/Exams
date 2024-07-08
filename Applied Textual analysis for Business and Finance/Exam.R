### BAN 432
### EXAM
### CANDIATES NR.: 108, 118, 145, 154

# Data preprocessing libraries
require(readr)
require(dplyr)
require(rvest)
require(tidytext)
require(dplyr)
require(lubridate)
require(zoo)
require(stringr)
require(tm)

# Data analysis libraries
require(sentimentr)
require(wordcloud)
require(quanteda)
require(textrank)
require(udpipe)
require(tokenizers)
require(tidyr)
require(slam)
require(SentimentAnalysis)
require(widyr)

# Visualization libraries
library(knitr)
library(kableExtra)
require(ggplot2)
require(ggraph)
require(igraph)

# Inspect a single file
data <- read_rds("dataShareWithStudents/4459774.rds")
data

# Read the html content
html_text(read_html(data1$data$attributes$content))

# Inspect the structure of a file
str(data1)

# Function for reading in all the files to a data frame
read_in_data <- function() {
  # Gets all the earning calls
  file_list <- list.files("dataShareWithStudents/", full.names = TRUE)
  
  # Initialize an empty data frame
  final = data.frame()
  
  # Loop trough all the files
  for (i in file_list) {
    data <- read_rds(i)$data
    df <- data.frame(id = data$id,
                     published = data$attributes$publishOn,
                     sector = data$attributes$themes[[length(data$attributes$themes)]]$slug,
                     ticker = toupper(data$attributes$themes[[2]]$slug),
                     transcript = html_text(read_html(data$attributes$content)))
    final <- rbind(final, df)
  }
  # Divide data into quarters
  final$quarter <- as.yearqtr(substr(final$published, 1, 10), format = "%Y-%m-%d")
    
  return (final)
}

# Runs the read in data function
final <- read_in_data()

## Explorative Analysis

# Number per industry
final %>% 
  group_by(sector) %>% 
  summarize(count = n()) %>% 
  arrange(desc(count))

# Number of documents per sector over time
final %>%
  group_by(quarter, sector) %>% 
  summarise(count=n()) %>% 
  ggplot(aes(x=quarter, y=count, color=sector)) +
  geom_line() + 
  theme_minimal() +
  labs(title = "Number of Documents per Sector Over Time",
       x = "Quarter",
       y = "Number of Documents")
 
# Inspect the sudden peak in utilities 2022 Q3 
final %>% 
  group_by(sector, ticker) %>%
  filter(sector == "utilities" |
           sector == "electric-utilities") %>% 
  summarize(count = n()) %>% 
  arrange(desc(ticker)) %>% 
  print(n=50)

## Preproccesing
# List with sectors we need to replace
list_with_utilities <- c("electric-utilities","diversified-utilities", "gas-utilities", "water-utilities" )

# Replace *-utilities too utilities 
final$sector <- sapply(final$sector,                           
                       function(x) replace(x, x %in% list_with_utilities, "utilities"))

# Drop sectors who has low number of documents
final <- final %>% 
  filter(sector != "trucking" & 
         sector != "closed-end-fund-debt")

# Capitalize First Letter
final$sector <- sapply(final$sector, 
                       function(x) str_to_title(x))

# Function for splitting the the transcript into two columns
# One for the Management section of the earning call
# And one for the QA session of the earning call
# The QA session could be biased and thus affect or analysis
split_text <- function(text) {
  keyword <- "Conference Call Participants"
  keyword_position <- regexpr(keyword, text)
  if (keyword_position[1] != -1) {
    remaining_text <- substr(text, keyword_position[1], nchar(text))
  } else {
    remaining_text <- text
  }
  split_text <- strsplit(remaining_text, "Question-and-Answer Session")
  management <- split_text[[1]][1]
  q_and_a <- ifelse(length(split_text[[1]]) > 1, split_text[[1]][2], "No_q_and_a session")
  return(list(management = management, q_and_a = q_and_a))
}

split_results <- lapply(final$transcript, split_text)

final$management <- sapply(split_results, function(x) x$management)
final$q_and_a <- sapply(split_results, function(x) x$q_and_a)


### TASK 1
## Step 1: Find keywords related to chatGPT that's affecting a industry

## Approach:
## Find related terms to chatGPT
## Make a initial list with keywords we belive is affecting a industry
## Perform a KWIC analysis on each keyword to see if the keyword is affecting the industry
## Make a wordcloud of every extracted segments
## Conclude a final list of keywords who are affecting a industry
## Count the mean frequency of keywords per sector
## Create a linechart over the frequency per sector and quarter
## Perform a Wilcox Rang Sum Test
## Conclude

# Load the udpipe model once, outside the loop
# udpipe_download_model("english")
model <- udpipe_load_model("english-ewt-ud-2.5-191206.udpipe")

# Split the management text into sentence tokens
toks <- tokens(final$management, what = "sentence")

# KWIC analysis on chatGPT to find related terms
kwic_results <- kwic(toks,
                     pattern = "\\b(?i)chat[-]?gpt\\b",
                     valuetype = "regex",
                     window = 4,
                     case_insensitive = T)

# Combines the context into a single paragraph
kwic_results <- as.data.frame(kwic_results) %>% 
  mutate(par = paste(pre, keyword, post, sep = " ")) 

# Cleans the paragraph and returns lemmatized versions of the words
clean_document <- function(doc_text) {
  doc_text %>%
    removePunctuation() %>%
    removeNumbers() %>%
    removeWords(stopwords("en")) %>%
    tolower() %>%
    stripWhitespace() -> cleaned_text
  
  annotations <- udpipe_annotate(model, x=cleaned_text)
  data_frame <- as.data.frame(annotations)
  cleaned_text <- data_frame %>%
    pull(lemma) %>%
    paste(collapse = " ")
  
  return(cleaned_text)

}

kwic_results$par <- sapply(kwic_results$par, clean_document)

# Make bigrams to extract related terms to chatGPT
bigrams <- kwic_results %>% 
  unnest_tokens(bigram, par, token = "ngrams", n = 2) %>% 
  separate(bigram, c("word1", "word2"), sep = " ") %>% 
  filter(!word1 %in% stop_words$word,
         !word2 %in% stop_words$word) %>% 
  count(word1, word2, sort = TRUE) %>% 
  filter(n>5,
         !str_detect(word1, "\\d"),
         !str_detect(word1, "\\d"))

# Inspect the bigrams 
head(bigrams, n=12)

# Initial keyword list based on the bigrams list
keywords <- c("\\b(?i)machine learning(s)?\\b", "\\b(?i)gpt(-\\d+)?\\b",
              "\\b(?i)large language model(s)?\\b","\\b(?i)ml\\b",
              "\\b(?i)ai\\b", "\\b(?i)artificial intelligence\\b",
              "\\b(?i)chat[-]?gpt\\b", "\\b(?i)openai\\b",
              "\\b(?i)generative ai\\b", "\\b(?i)llm\\b")

# Loop trough each keyword
for (keyword in keywords) {
  # KWIC analysis
  kwic.results <- kwic(toks,
                       pattern = keyword, 
                       valuetype = "regex",
                       window = 1,
                       case_insensitive = T) %>% 
    as_tibble() %>% 
    mutate(par = paste(pre, keyword, post, sep = " "))
    
  
  # Annotate text using udpipe
  annotated.text <- udpipe_annotate(model, x = kwic.results$par)
  annotated.df <- as.data.frame(annotated.text)
  
  # Clean the data
  word.freqs <- annotated.df %>%
    select(lemma, upos) %>% 
    filter(!lemma %in% stopwords()) %>% 
    filter(!str_detect(lemma, keyword)) %>%
    filter(!lemma %in% str_split(gsub("\\\\b|\\(\\?i\\)|\\\\|\\[-]\\?|\\(-\\\\d\\+\\)\\?||\\(s)\\?", "", keyword), " ")[[1]]) %>% 
    filter(upos %in% c("NOUN", "ADJ", "VERB")) %>% 
    count(lemma, sort=T)
  
  # Create the word cloud with an adjusted scale
  filename <- paste0("wordcloud_", gsub("\\\\b|\\(\\?i\\)|\\\\|\\[-]\\?|\\(-\\\\d\\+\\)\\?|\\(s)\\?", "", keyword), ".png")
  png(filename, width = 2000, height = 2000)
  wordcloud(words = (word.freqs$lemma[1:30]), freq = word.freqs$n[1:30],
            scale = c(20, 3), colors = brewer.pal(8, "Dark2"),
            main = keyword)
  dev.off()
}



## Step 2: Frequency list over related keywords
keywords.final <- c("\\b(?i)machine learning(s)?\\b", "\\b(?i)gpt(-\\d+)?\\b",
              "\\b(?i)large language model(s)?\\b","\\b(?i)ml\\b",
              "\\b(?i)ai\\b", "\\b(?i)artificial intelligence\\b",
              "\\b(?i)chat[-]?gpt\\b", "\\b(?i)openai\\b",
              "\\b(?i)generative ai\\b", "\\b(?i)llm\\b")

pattern <- paste(keywords.final, collapse = "|")

# Extract all matched terms into a vector
final$matched_keywords <- str_extract_all(final$management, pattern)

# Calculate the number of keywords per text
final$n_keywords <- sapply(final$matched_keywords, length)

# Computing the mean frequency of keywords per document for each sector and quarter
mean_frequency_by_industry_time <- final %>%
  group_by(sector, quarter) %>%
  summarize(mean_frequency = mean(n_keywords, na.rm = TRUE),
            count = n())

# Create the plot with a modified scale_color_brewer to capitalize the first letter of each word
ggplot(mean_frequency_by_industry_time, aes(x = quarter, y = mean_frequency, group = sector, color = sector)) +
  geom_line(linewidth = 0.6) +  # Slightly thicker lines for better visibility
  theme_minimal(base_size = 14) +  # Clean theme with larger base font size
  labs(
    title = "Mean Frequency of Keywords per Document Over Time",
    x = "Quarter",
    y = "Mean Keyword Frequency",
    color = "Sector"  # Rename legend title
  ) +
  theme(
    plot.title = element_text(size = 16, face = "bold"),
    plot.subtitle = element_text(size = 14),
    axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1),
    axis.title = element_text(size = 14),
    legend.title = element_text(size = 14),
    legend.text = element_text(size = 12)
  ) 

## Step 4: Wilcxo Rang Sum test

final <- final %>%
  mutate(time_period = ifelse(quarter <= "2022 Q4", "Before 2022 Q4", "After 2022 Q4"))

# Wilcox Rang Sum test to see if the increase is significant
results <- final %>%
  group_by(sector) %>%
  do(broom::tidy(wilcox.test(n_keywords ~ time_period, data = .)))

# Print the results
print(results$p.value)

# Define the significance level
significance_level <- 0.05

# Filter out non-significant results
significant_results <- results %>%
  filter(p.value < significance_level)

# Check the significant results
print(significant_results)

# Calculate the mean frequency before and after the cutoff for each sector
# Calculate the increase
# Join the results from the Wilcox test
mean_before_and_after <- final %>%
  group_by(sector, time_period) %>%
  summarize(mean_freq_before = mean(n_keywords)) %>% 
  pivot_wider(names_from = time_period, values_from = mean_freq_before) %>% 
  mutate(diff = `After 2022 Q4`- `Before 2022 Q4`) %>% 
  left_join(results, by="sector") %>% 
  select(sector, `After 2022 Q4`, `Before 2022 Q4`, diff, p.value) %>% 
  arrange(p.value)

# Creating a knitr kable
kable(mean_before_and_after, format = "html", caption = "Keyword Frequency Analysis by Sector") %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive"), 
                full_width = F, 
                position = "center") %>%
  column_spec(1, bold = T) %>%  
  column_spec(5, color = "red")  


### TASK 2
## What are the risks firms face regarding AI?

## Approach:
## Filter out the affected sectors
## Extract segments where our keywords appear
## Filter out segments with negative context
## Calculate correlation of unigrams and bigrams 
## Plot network of words to find risk
## Conclude



# List with affected sectors from task 1
industries_affected <- c("Information-Technology", "Communication-Services",
                         "Financials", "Industrials", "Consumer-Discretionary")

# Creat a sub-data frame with affected sectors
ai_industries <- final %>% 
  filter(sector %in% industries_affected)

toks <- tokens(ai_industries$transcript, what = "sentence")

# Extract the context around our keyword.final from task 1
kwic_results <- kwic(toks,
                     pattern = pattern,
                     valuetype = "regex",
                     window = 1,
                     case_insensitive = T)

# Combines the context into a single paragraph
kwic_results <- as.data.frame(kwic_results) %>% 
  mutate(par = paste(pre, keyword, post, sep = " ")) 

# Filters out duplicates from sentences that have multiple keywords
kwic_results <- kwic_results %>%
  group_by(keyword) %>%
  slice(1) %>%
  ungroup()

# Cleans the paragraph and returns stemmed versions of the words
clean_document <- function(doc_text) {
  doc_text %>%
    removePunctuation() %>%
    removeNumbers() %>%
    removeWords(stopwords("en")) %>%
    tolower() %>%
    str_replace_all("\\b\\w{1,2}\\b|\\b\\w{21,}\\b", "") %>% 
    stripWhitespace() -> cleaned_text
  
  annotations <- udpipe_annotate(model, x=cleaned_text)
  data_frame <- as.data.frame(annotations)
  cleaned_text <- data_frame %>%
    pull(lemma) %>%
    paste(collapse = " ")
  
  return(cleaned_text)
}

# Preproccessing of data
kwic_results$par <- sapply(kwic_results$par, clean_document)

# Create a corpus and a DTM
corpus <- Corpus(VectorSource(kwic_results$par))
dtm <- DocumentTermMatrix(corpus)

# Calculate the frecunecy of each word
term.freq <- tibble(term = dtm$dimnames$Terms, 
                    freq = col_sums(dtm))

# Calculate how many docs who has a singel word
term.freq$doc.freq <- col_sums(dtm != 0)

# Most frequent words from LM. Question is not really a negative word in our case
# and ill is a result of removing ' from i'll.
# So we make a modified dictionary without these two. 
term.freq %>% 
  filter(term %in% DictionaryLM$negative) %>% 
  arrange(desc(freq))

term.freq %>% 
  filter(term %in% DictionaryLM$positive) %>% 
  arrange(desc(freq))

modified_dict <- setdiff(DictionaryLM$negative, c("question", "ill"))

# Calculates negativity score for each paragraph
kwic_results$neg <- row_sums(
  dtm[,dtm$dimnames$Terms %in% modified_dict]) /
  row_sums(dtm)

# Calculates positivity score for each paragraph
kwic_results$pos <- row_sums(
  dtm[,dtm$dimnames$Terms %in% DictionaryLM$positive]) /
  row_sums(dtm)

# TF-IDF score for negative words
term.freq$tfidf.score <- (col_sums(dtm) / 
                            sum(row_sums(dtm)) * 
                            log(nrow(dtm) /
                                  col_sums(dtm!=0)))

dtm$v <- dtm$v * term.freq$tfidf.score[dtm$j]

kwic_results$fin.neg.tfidf <- row_sums(
  dtm[,dtm$dimnames$Terms %in% modified_dict]) /
  row_sums(dtm)

# Display the negative words with the highest TD-IDF scores
term.freq <- term.freq %>% 
  arrange(desc(tfidf.score)) %>% 
  filter(term %in% modified_dict)
print(term.freq, n=40)

# We use the most important negative words and use intuition to choose words that
# can be used to describe risks
keywords_list <- c("\\brisk\\b", "\\bproblem\\b", "\\bconcern\\b", 
                   "\\bthreat\\b", "\\bchallenge\\b", "\\bcritical\\b", 
                   "\\bbreach\\b", "\\bvulnerability\\b", "\\bfraud\\b")

# Create a regular expression pattern from the list
keywords_pattern <- paste(keywords_list, collapse = "|")

# Filters the original results. We are left with paragraphs that includes one or more
# keywords and have a negativity score above 1
kwic_filtered <- kwic_results %>% 
  filter(grepl(keywords_pattern, par, ignore.case = TRUE)) %>% 
  filter(pos < neg)

# Grouping all paragraph from the same earning call
grouped_docs <- kwic_filtered %>% 
  group_by(docname) %>% 
  summarize(combined_text = paste(par, collapse = " "))

# Filters out nouns and adjectives to extract the words that gives the most
# Context
filter_upos <- function(doc_text) {
  annotations <- udpipe_annotate(model, x=doc_text)
  data_frame <- as.data.frame(annotations)
  filtered_data <- data_frame %>%
    filter(upos %in% c("NOUN", "ADJ")) %>%
    pull(lemma) %>%
    paste(collapse = " ")
  
  return(filtered_data)
}

grouped_docs_upos <- grouped_docs
grouped_docs_upos$combined_text <- sapply(grouped_docs_upos$combined_text, filter_upos)

# Restructure to words appearing in a section
sections <- as.data.frame(grouped_docs_upos) %>% 
  unnest_tokens(word, combined_text) %>% 
  filter(!word %in% stop_words$word)

# Filters out words that appear more than 7 times and calculates correlation 
# with the other words. High correlation means that if a word1 appears in a document
# it is likely that word2 also appears
word_correlation <- sections %>% 
  group_by(word) %>% 
  filter(n() >=7) %>% 
  pairwise_cor(word, docname, sort = TRUE)

set.seed(1234)

# Plots the word network for words having higher than a 35% correlation
word_correlation %>%
  filter(correlation > .35) %>%
  graph_from_data_frame() %>%
  ggraph(layout = "fr") +
  geom_edge_link(aes(edge_alpha = correlation), show.legend = FALSE) +
  geom_node_point(color = "lightblue", size = 5) +
  geom_node_text(aes(label = name), repel = TRUE) +
  theme_void()

#Creates bigrams from the text
bigrams <- as.data.frame(grouped_docs) %>%
  unnest_tokens(bigram, combined_text, token = "ngrams", n = 2)

# Filters out bigrams that appear more than 4 times and calculates correlation 
# with the other bigrams.
bigram_correlation <- bigrams %>% 
  group_by(bigram) %>% 
  filter(n() >=4) %>% 
  pairwise_cor(bigram, docname, sort = TRUE)

set.seed(1234)

# Plots the word network for bigrams having higher than a 20% correlation
bigram_correlation %>%
  filter(correlation > .2) %>%
  graph_from_data_frame() %>%
  ggraph(layout = "fr") +
  geom_edge_link(aes(edge_alpha = correlation), show.legend = FALSE) +
  geom_node_point(color = "lightblue", size = 5) +
  geom_node_text(aes(label = name), repel = TRUE) +
  theme_void()

# Unnest the vector containg matched keywords
final %>% 
  unnest(matched_keywords) -> t %>% 
  unique()
  
t <- t %>% 
  as_tibble()

# Count the frequency of chatGPT 
t %>% 
  filter(matched_keywords == "ChatGPT" | 
           matched_keywords == "chatGPT" |
           matched_keywords == "chatgpt" |
           matched_keywords == "GPT-3"|
           matched_keywords == "GPT-4") %>%
  count()
     