# Playing with Google Scholar

# My user ID: Myvc78MAAAAJ&hl
#install.packages("scholar")
library(scholar)
#install.packages("tidyverse")
library(tidyverse)
#install.packages("gridExtra")
library(gridExtra)

# Insert your Scholar ID here from your profile URL 
# Erwin = https://scholar.google.co.id/citations?user=Myvc78MAAAAJ&hl=en&authuser=1
# Edy Soewono = https://scholar.google.co.id/citations?user=6U_YtvMAAAAJ&hl=en&authuser=1
# Khairurrijal = https://scholar.google.co.id/citations?user=4_asJ0MAAAAJ&hl=en&authuser=1

E <- "Myvc78MAAAAJ&hl"  #hydrogeology
ES <- "6U_YtvMAAAAJ&hl" #math
Kh <- "4_asJ0MAAAAJ&hl" #physics
JT <- "P7FvGMEAAAAJ&hl" #paleontology

# How many papers have I published?
E.num <- get_num_articles(E)
ES.num <- get_num_articles(ES)
Kh.num <- get_num_articles(Kh)
JT.num <- get_num_articles(JT)

num <- c(E.num, ES.num, Kh.num, JT.num)
num <- as.data.frame(num)
num
barplot(num,
        names.arg=c("E", "ES", "Kh", "JT")) 

# Compare career
## Join two dataframe
IDs <- c(ES, Kh, JT)
compare_scholar_careers(IDs, career = TRUE)

# Citation history
ES.cite.year <- get_citation_history(ES)  # success
Kh.cite.year <- get_citation_history(Kh)  # success
JT.cite.year <- get_citation_history(JT)  # success
E.cite.year <- get_citation_history(E)   # hmm, error, strange

# Plot citation history
ES.hist <- ggplot(ES.cite.year, 
                 aes(year,cites)) + 
  geom_bar(stat='identity',fill=colors()[128]) +
  scale_x_continuous(
    breaks = c(2009, 2010, 2011, 2012, 2013, 2014, 2015, 2016, 2017))

Kh.hist <- ggplot(Kh.cite.year, 
       aes(year,cites)) + 
  geom_bar(stat='identity',fill=colors()[120]) +
  scale_x_continuous(
    breaks = c(2009, 2010, 2011, 2012, 2013, 2014, 2015, 2016, 2017))

JT.hist <- ggplot(JT.cite.year, 
                aes(year,cites)) + 
  geom_bar(stat='identity',fill=colors()[118]) +
  scale_x_continuous(
    breaks = c(2009, 2010, 2011, 2012, 2013, 2014, 2015, 2016, 2017))

## My work around
E.cite.year <- get_publications(E)
E.cite.year <- as.data.frame(lapply(E.cite.year,
                                 function(x) if (is.factor(x)) as.character(x) else x))
w <-grep("Ultrafast", E.cite.year$title) 
E.cite.year$title[w]
E.cite.year$pubid[w] 
E.cite.year <- get_article_cite_history(E, E.cite.year$pubid[w])

E.hist <- ggplot(E.cite.year, aes(year, cites)) +
  geom_bar(stat='identity',fill=colors()[110]) +
  scale_x_continuous(
    breaks = c(2000, 2001, 2002, 2003, 2004, 2005, 2006, 2007, 2008, 2009, 2010, 2011, 2012, 2013, 2014, 2015, 2016, 2017))+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Number of unique journals
get_num_distinct_journals(E)
get_num_distinct_journals(ES)
get_num_distinct_journals(Kh)
get_num_distinct_journals(JT)

# Number of top journals
get_num_top_journals(E)
get_num_top_journals(ES)
get_num_top_journals(Kh)
get_num_top_journals(JT)

# But that’s only according to this paper so who cares
# DE Acuna, S Allesina, KP Kording (2012) Future impact: Predicting scientific success. Nature, 489, 201-202. http://dx.doi.org/10.1038/489201a.

# When did I become a scientist?
get_oldest_article(E)
get_oldest_article(ES)
get_oldest_article(Kh)
get_oldest_article(JT)

# Get profile information
E.profile <- get_profile(E)
ES.profile <- get_profile(ES)
Kh.profile <- get_profile(Kh)
JT.profile <- get_profile(JT)


# Get publications and write to a csv file
pubs<-get_publications(ID)
write.csv(pubs, file = "citations.csv")

# Predict my h-index
h.index<-predict_h_index(ID)