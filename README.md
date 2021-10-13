Brief Introduction - LDA
----------------
LDA stands for Latent Dirichelet Allocation. LDA topic modeling is a computational text analysis method used to find “latent” thematic structure of a given textual dataset.

Preparation - Install packages
----------------
<pre class="r"><code>library(dplyr)
library(quanteda)
library(topicmodels)
library(ldatuning)
library(dplyr)
library(tidytext)
library(tidyverse)
library(ggplot2)
library(rmarkdown)</code></pre>

Pre-Process data frames
----------------
<pre class="r"><code>df <- read.csv("BidenFoxNBC.csv", header = TRUE, sep = ",")
df$text <- as.character(df$text)
df$source <- as.character(df$text)
df_corpus <- corpus(df$text)
docvars(df_corpus, "source") <- df$source
metadoc(df_corpus, "language") <- "english"</code></pre>

Clean text data
----------------
<pre class="r"><code>df_dfm <- dfm(df_corpus, remove = stopwords("en"), remove_punct = TRUE)
#df_dfm
df_dfm_trim <- dfm_trim(df_dfm, min_termfreq = 2, max_docfreq = 100)
#df_dfm_trim</code></pre>

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

