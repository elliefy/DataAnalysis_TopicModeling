Brief Introduction - LDA
----------------
LDA stands for Latent Dirichelet Allocation. LDA topic modeling is a computational text analysis method used to find “latent” thematic structure of a given textual dataset.

Preparation - Install packages
----------------
<pre class="r"><code>library(quanteda)
library(topicmodels)
library(ldatuning)
library(dplyr)
library(tidytext)
library(tidyverse)
library(ggplot2)
library(rmarkdown)
library(ggthemes)
library(scales)</code></pre>

Import Data
----------------
<pre class="r"><code>distinct = read.csv("total_post.csv")

distinct = distinct%>%
  mutate(fulltext = paste(title,post_text,sep = ""))%>%
  select(-X)#combine title and post_text

write.csv(distinct, "lda.csv",row.names = FALSE)

df <- read.csv("lda.csv", header = TRUE, sep = ",")
df$fulltext <- as.character(df$fulltext)
df$subreddit <- as.character(df$subreddit)

df_corpus <- corpus(df$fulltext)</code></pre>

pre-process-remove stopwords, stemming
----------------
<pre class="r"><code>token = tokens(df_corpus,remove_punct = TRUE,remove_symbols = TRUE,remove_numbers = TRUE)
df_dfm_raw <- dfm(token)
stem = dfm_wordstem(df_dfm_raw)</code></pre>

Decide the number of topics (K)
----------------
<pre class="r"><code>result <- FindTopicsNumber(df_dfm_trim, 
                           topics = seq(from = 10, to = 20, by = 1),
                           metrics = c("Griffiths2004", 
                                       "CaoJuan2009", "Arun2010", 
                                       "Deveaud2014"),
                           method = "Gibbs", 
                           control = list(seed = 123), 
                           mc.cores = 2L, 
                           verbose = TRUE)</code></pre>

<pre class="r"><code>r# Validation
n_topics <- c(5, 10, 15, 20)
lda_compare <- n_topics %>% 
  map(LDA, x = df_dfm_trim, control = list(seed = 123))
data_frame(k = n_topics, 
           perplex = map_dbl(lda_compare, perplexity)) %>% 
  ggplot(aes(k, perplex)) + 
  geom_point() + 
  geom_line() + 
  labs(title = "Evaluation for LDA topic modeling", 
       x = "Number of topics", 
       y = "Perplexity")</code></pre>

Run LDA topic modeling
----------------
<pre class="r"><code>k <- 20 # Number of topics
control_LDA <- list(alpha = 50/k, estimate.beta = TRUE, 
                    verbose = 0, prefix = tempfile(), 
                    save = 0, keep = 0, 
                    seed = 123, nstart = 1, 
                    best = TRUE, delta = 0.1, iter = 2000, 
                    burnin = 100, thin = 2000)
lda = LDA(df_dfm_trim, k = k, method = "Gibbs", 
          control = control_LDA)
terms(lda, 10)</code></pre>

Visualization
----------------
<pre class="r"><code># Terms within each topic
topics <- tidy(lda, matrix = "beta")
top_terms <- topics %>% group_by(topic) %>% 
  top_n(6, beta) %>% 
  ungroup() %>%
  arrange(topic, -beta)

top_terms %>% mutate(term = reorder(term, beta)) %>% 
  ggplot(aes(term, beta, fill = factor(topic))) + 
  geom_col(show.legend = FALSE) + 
  facet_wrap(~ topic, scales = "free") + 
  coord_flip()</code></pre>

