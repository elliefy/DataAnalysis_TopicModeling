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
<p>1 - Load data and check the descriptions</p>
<pre class="r"><code>storyURL <- read.csv("TrumpCovidTheHill.csv", header = TRUE, sep = ",")</code></pre>
<p>2 - Get the necessary information: URLs</p>
<pre class="r"><code>storyChar <- matrix(as.character(storyURL[ , 4]))</code></pre>
<p>3 - Check the length of the vector</p>
<pre class="r"><code>num <- length(storyURL[[1]])
num</code></pre>
<p>4 - Read the html codes.</p>
<pre class="r"><code>xml <- try(apply(storyChar, 1, read_html))</code></pre>

Inspect the html code you need to pull
----------------
<p>To see what parts of html code you need next, you can open a page and “inspect” the html code in Chrome.

Some tools make it easier. Here are some extensions that allow us to “point and click” specific part(s) of a website and identify the html code parts:

For Chrome: "[SelectorGadget](https://selectorgadget.com/)"

For Firefox: "[Page Inspector](https://developer.mozilla.org/en-US/docs/Tools/Page_Inspector)"

For Safari: "[Web Inspector](https://www.technipages.com/macos-enable-web-inspector-in-safari)"</p>

Scrape text from the html code
----------------
<p>First, let’s create a function to scrape text from The Hill</p>
<pre class="r"><code>textScraper <- function(x) {
  as.character(html_text(html_nodes (x, ".content-wrapper") %>% html_nodes("p"))) %>%
    str_replace_all("[\n]", "") %>%
    str_replace_all("    ", "") %>%
    str_replace_all("[\t]", "") %>%
    paste(collapse = '')}</code></pre>
    
Apply this function to our html content
----------------
<pre class="r"><code>articleText <- lapply(xml, textScraper) #list of article text
articleText[[1]] # Or use head(articleText)</code></pre>

Scrape time
----------------
<p>Use similar methods to scrape the time for all the articles. We create another function timeScraper, and apply it to the html content.</p>
<pre class="r"><code>timeScraper <- function(x) {
  timestampHold <- as.character(html_text(html_nodes(x, ".submitted-date"))) %>% str_replace_all("[\n]", "")
  matrix(unlist(timestampHold))
  timestampHold[1]} 
timestamp <- lapply(xml, timeScraper) #list of timestamps
head(timestamp)</code></pre>

Output as a dataframe
----------------
<p>Create a dataframe for the text and time we scraped above</p>
<pre class="r"><code>articleDF <- data.frame(storyID = as.character(storyURL[,1]), 
                        headline = as.character(storyURL[,3]), 
                        matrix(unlist(articleText), nrow = num), 
                        matrix(unlist(timestamp), nrow = num), 
                        themes = as.character(storyURL[,7]))
names(articleDF)[3] <- 'text'
names(articleDF)[4] <- 'time'
#review the output
#articleDF[1: 2, ]
write.csv(articleDF, file = "TheHill_TrumpCovid_text.csv")</code></pre>

Further readings
----------------
The official documentation for the package [`rvest`](https://cran.r-project.org/web/packages/rvest/rvest.pdf)

[Simple web scraping for R in Github](https://github.com/tidyverse/rvest);

[RStudio Blog: rvest: easy web scraping with R] (https://blog.rstudio.com/2014/11/24/rvest-easy-web-scraping-with-r/);

Real-world applications: [Most popular films](https://www.analyticsvidhya.com/blog/2017/03/beginners-guide-on-web-scraping-in-r-using-rvest-with-hands-on-knowledge/), [Trip Advisor Reviews](https://www.johnlittle.info/project/custom/rfun-scrape/rvest_demo.nb.html), [IMDb pages](https://stat4701.github.io/edav/2015/04/02/rvest_tutorial/).





