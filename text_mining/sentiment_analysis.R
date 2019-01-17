# install.packages(c("ggrepel",'kableExtra','formattable','circlize','memery','magick','yarrr','radarchart','igraph','ggraph'))


library(dplyr)
library(tidytext)
library(tidyr)
library(widyr)

library(ggplot2)
library(ggrepel)
library(gridExtra)
library(knitr)
library(kableExtra)
library(formattable)
library(circlize)
library(memery)
library(magick)
library(yarrr)
library(radarchart)
library(igraph)
library(ggraph)


my_colors <- c("#90EF73","#E69F00", "#56B4E9", "#009E73", "#CC79A7", "#D55E00", "#D65E00", "#83FA12")



theme_lyrics <- function(aticks = element_blank(),
                         pgminor = element_blank(),
                         lt = element_blank(),
                         lp = "none")
{
  theme(plot.title = element_text(hjust = 0.5), #Center the title
        axis.ticks = aticks, #Set axis ticks to on or off
        panel.grid.minor = pgminor, #Turn the minor grid lines on or off
        legend.title = lt, #Turn the legend title on or off
        legend.position = lp) #Turn the legend on or off
}

#Customize the text tables for consistency using HTML formatting
my_kable_styling <- function(dat, caption) {
  kable(dat, "html", escape = FALSE, caption = caption) %>%
    kable_styling(bootstrap_options = c("striped", "condensed", "bordered"),
                  full_width = FALSE)
}


prince_orig <- read.csv("prince_raw_data.csv", stringsAsFactors = FALSE)


prince <- prince_orig %>% 
  select(lyrics = text, song, year, album, peak, 
         us_pop = US.Pop, us_rnb = US.R.B)

fix.contractions <- function(doc){
  doc <- gsub("won't", "will not", doc)
  doc <- gsub("can't", "can not", doc)
  doc <- gsub("n't", " not", doc)
  doc <- gsub("'ll", " will", doc)
  doc <- gsub("'re", " are", doc)
  doc <- gsub("'ve", " have", doc)
  doc <- gsub("'m", " am", doc)
  doc <- gsub("'d", " would", doc)
  # 's could be 'is' or could be possessive: it has no expansion
  doc <- gsub("'s", "", doc)
  return(doc)
}

# fix (expand) contractions
prince$lyrics <- sapply(prince$lyrics, fix.contractions)

removeSpecialChars <- function(x) gsub("[^a-z0-9A-Z]", " ",x)

prince$lyrics <- sapply(prince$lyrics, removeSpecialChars)

prince$lyrics <- sapply(prince$lyrics, tolower)


prince <- prince %>%
  mutate(decade = 
           ifelse(prince$year %in% 1978:1979, "1970s", 
                  ifelse(prince$year %in% 1980:1989, "1980s", 
                         ifelse(prince$year %in% 1990:1999, "1990s", 
                                ifelse(prince$year %in% 2000:2009, "2000s", 
                                       ifelse(prince$year %in% 2010:2015, "2010s", 
                                              NA))))))


#create the chart level column
prince <- prince %>%
  mutate(chart_level = 
           ifelse(prince$peak %in% 1:10, "Top 10", 
                  ifelse(prince$peak %in% 11:100, "Top 100", "Uncharted")))


#create binary field called charted showing if a song hit the charts at all
prince <- prince %>%
  mutate(charted = 
           ifelse(prince$peak %in% 1:100, "Charted", "Uncharted"))

#save the new dataset to .csv for use in later tutorials
write.csv(prince, file = "prince_new.csv")

glimpse(prince)

undesirable_words <- c("prince", "chorus", "repeat", "lyrics",
                       "theres", "bridge", "fe0f", "yeah", "baby",
                       "alright", "wanna", "gonna", "chorus", "verse",
                       "whoa", "gotta", "make", "miscellaneous", "2",
                       "4", "ooh", "uurh", "pheromone", "poompoom", "3121",
                       "matic", " ai ", " ca ", " la ", "hey", " na ",
                       " da ", " uh ", " tin ", "  ll", "transcription",
                       "repeats", "la", "da", "uh", "ah")

prince_tidy <- prince %>% 
  unnest_tokens(word, lyrics) %>%
  anti_join(stop_words) %>%
  filter(!word %in% undesirable_words) %>%
  filter(!nchar(word)<3)

glimpse(prince_tidy)


word_summary <- prince_tidy %>%
  mutate(decade = ifelse(is.na(decade), "NONE", decade)) %>%
  group_by(song, decade) %>%
  mutate(word_count = n_distinct(word))%>%
  select(song, Release = decade, Charted = charted, word_count) %>%
  distinct() %>%
  ungroup()
  
pirateplot(word_count ~ Release + Charted, data = word_summary, pal = "google", point.o = 0.2, avg.line.o = 1, #Turn on the Average/Mean line
           theme = 0, #Theme
           point.pch = 16, #Point `pch` type
           point.cex = 1.5, #Point size
           jitter.val = .2 #Turn on jitter to see the songs better)
)

decade_chart <- prince %>%
  filter(!is.na(decade)) %>%
  count(decade, charted)

circos.clear()
grid.col = c("1970's" = my_colors[1], "1980s" = my_colors[2], "1990s" = my_colors[3], "2000s" = my_colors[4], "2010s" = my_colors[5], "Charted" = "grey", "Uncharted" = "grey")

a = unique(decade_chart$decade)
b = unique(decade_chart$charted)
ab=c(a,b)

grid.col = my_colors[1:length(ab)]
names(grid.col) = ab

circos.par(gap.after = c(rep(8, length(a)-1), 15, rep(8, length(b)-1),15))
chordDiagram(decade_chart, grid.col = grid.col, transparency = 0.3)


new_sentiments <- sentiments %>%
  filter(lexicon != "loughran" ) %>%
  mutate(sentiment = ifelse(lexicon == 'AFINN' & score>=0, 'positive', 
                            ifelse(lexicon == 'AFINN' & score <0, 'negative', sentiment))) %>%
  group_by(lexicon) %>%
  mutate(word_in_lexicon = n_distinct(word)) %>%
  ungroup()
  
new_sentiments %>% group_by(sentiment, lexicon, word_count) %>%
  summarise(distinct_word = n_distinct(word))%>%
  ungroup()%>%
  spread(sentiment, distinct_word) %>%
  mutate(lexicon = color_bar('red')(lexicon)) %>%
  kable('html',escape = F, align ='c') %>%
  kable_styling(bootstrap_option = c('condensed','stripped','bordered'), full_width = F)


prince_tidy %>% 
  inner_join(new_sentiments) %>%
  group_by(lexicon) %>%
  summarise(word_count = n_distinct(word))


prince_tidy %>% 
  mutate(words_in_lyrics = n_distinct(word)) %>%
  inner_join(new_sentiments) %>%
  group_by(lexicon, words_in_lyrics, word_in_lexicon) %>%
  summarise(lex_match_words = n_distinct(word)) %>%
  ungroup() %>%
  mutate(total_match_words = sum(lex_match_words), match_ratio = lex_match_words/words_in_lyrics) %>%
  mutate(lex_match_words = color_bar("lightpink")(lex_match_words), lexicon = color_tile("lightgreen","green")(lexicon)) %>%
  my_kable_styling("")


new_sentiments %>% 
  filter(word %in% c("happy","ecstatic")) %>%
  arrange(word) %>%
  select(-score) %>%
  mutate(word = color_tile("white","blue")(word)) %>%
  my_kable_styling("")



my_word_list = prince %>%
  unnest_tokens(word, lyrics) %>%
  filter(grepl("sex",word)) %>%
  count(word) %>%
  select(myword = word, n)%>%
  arrange(desc(n))

new_sentiments %>% 
  right_join(my_word_list, by = c("word"="myword")) %>%
  filter(word %in% my_word_list$myword) %>%
  mutate(word = color_tile("pink","pink")(word), instance = color_bar("pink")(n), lexicon = color_tile('blue','blue')(lexicon))%>%
  select(-score, -n) %>%
  my_kable_styling("")



prince_bing <- prince_tidy%>%
  inner_join(get_sentiments("bing"))

prince_nrc <- prince_tidy%>%
  inner_join(get_sentiments("nrc"))

prince_nrc_sub <- prince_tidy %>%
  inner_join(get_sentiments("nrc")) %>%
  filter(!sentiment %in% c("positive", "negative"))



nrc_plot <- prince_nrc_sub %>%
  count(sentiment) %>%
  mutate(sentiment = reorder(sentiment, n))%>%
  ggplot()+geom_col(aes(sentiment, n, fill = -n))+coord_flip()+guides(fill=F)

img <- 'download.jpg'
lab=""
meme(img, lab, "meme_nrc.jpg", inset = nrc_plot)
nrc_meme <- image_read('meme_nrc.jpg')
plot(nrc_meme)


prince_polarity_chat <- prince_bing%>%
  count(sentiment, chart_level) %>%
  spread(sentiment, n, fill = 0) %>%
  mutate(polarity = positive - negative, percent_positive = positive/(positive+negative)*100)

plot1 = prince_polarity_chat %>%
  ggplot()+geom_col(aes(chart_level, polarity, fill = chart_level))

plot2 <- prince_polarity_chat %>%
  ggplot( aes(chart_level, percent_positive, fill = chart_level)) +
  geom_col() 

grid.arrange(plot1, plot2, ncol = 2)

prince_bing%>%
  count(sentiment, chart_level) %>% 
  ggplot()+geom_col(aes(sentiment, n, fill=sentiment))+facet_wrap(~chart_level, scales = "free")



prince_polarity_year <- prince_bing %>%
  count(sentiment, year) %>%
  spread(sentiment, n, fill = 0) %>%
  mutate(polarity = positive - negative, percent_positive = positive/(positive+negative)*100)

prince_polarity_year %>% 
  ggplot(aes(year,polarity, color = ifelse(polarity>=0, my_colors[5], my_colors[4])))+geom_col()+guides(fill = F)+theme_lyrics() +
  geom_smooth(method = 'loess', se=F) + geom_smooth(method ='lm', se=F, aes(color = my_colors[1]))


mood_decade = prince_nrc_sub %>%
  filter(!is.na(decade))%>%
  count(sentiment, decade) 

circos.clear()

a = unique(mood_decade$sentiment)
b = unique(mood_decade$decade)

grid.col = c(my_colors[1:length(a)], rep('gray',length(b)))
names(grid.col) = c(a, b)

circos.par(gap.after = c(rep(5, length(a)-1),15, rep(5, length(b)-1), 15))

chordDiagram(mood_decade,grid.col = grid.col,transparency = 0.2)








library(jpeg)
library(circlize) #you'll use this package later
#read in the list of jpg files of album/book covers
files = list.files("jpg\\", full.names = TRUE)
#clean up the file names so we can use them in the diagram
removeSpecialChars <- function(x) gsub("[^a-zA-Z]", " ", x)
names <- lapply(files, removeSpecialChars)
names <- gsub("jpg","", names )

#read up on the circlize package for details
#but there will be comments in later sections
circos.clear()
circos.par("points.overflow.warning" = FALSE)
circos.initialize(names, xlim = c(0, 2))
circos.track(ylim = c(0, 1), panel.fun = function(x, y) {
  image = as.raster(readJPEG(files[CELL_META$sector.numeric.index]))
  circos.text(CELL_META$xcenter, CELL_META$cell.ylim[1] - uy(1.5, "mm"),
              CELL_META$sector.index,
              CELL_META$sector.index, facing = "clockwise", niceFacing = TRUE,
              adj = c(1, 0.5), cex = 0.9)
  circos.raster(image, CELL_META$xcenter, CELL_META$ycenter, width = "1.5cm",
                facing = "downward")
}, bg.border = 1, track.height = .4)


library(plotly)
library(topicmodels)

word_chart <- function(data, input, title) {
  data %>%
    #set y = 1 to just plot one variable and use word as the label
    ggplot(aes(as.factor(row), 1, label = input, fill = factor(topic) )) +
    #you want the words, not the points
    geom_point(color = "transparent") +
    #make sure the labels don't overlap
    geom_label_repel(nudge_x = .2,  
                     direction = "y",
                     box.padding = 0.1,
                     segment.color = "transparent",
                     size = 3) +
    facet_grid(~topic) +
    theme_lyrics() +
    theme(axis.text.y = element_blank(), axis.text.x = element_blank(),
          #axis.title.x = element_text(size = 9),
          panel.grid = element_blank(), panel.background = element_blank(),
          panel.border = element_rect("lightgray", fill = NA),
          strip.text.x = element_text(size = 9)) +
    labs(x = NULL, y = NULL, title = title) +
    #xlab(NULL) + ylab(NULL) +
    #ggtitle(title) +
    coord_flip()
}



#Get Tidy Prince Dataset and Balanced Tidy Dataset of All Sources and 3 Sources

three_sources_tidy_balanced <- read.csv("three_sources_tidy_balanced.csv",
                                        stringsAsFactors = FALSE)

all_sources_tidy_balanced <- read.csv("all_sources_tidy_balanced.csv",
                                      stringsAsFactors = FALSE)

prince_tidy <- read.csv("prince_tidy.csv",
                        stringsAsFactors = FALSE)



#group the dataset by writer (source) and count the words
three_sources_tidy_balanced %>%
  group_by(source) %>%
  mutate(word_count = n()) %>%
  select(source, genre, word_count) %>% #only need these fields
  distinct() %>%
  ungroup() %>%
  #assign color bar for word_count that varies according to size
  #create static color for source and genre
  mutate(word_count = color_bar("lightpink")(word_count),  
         source = color_tile("lightblue","lightblue")(source),
         genre = color_tile("lightgreen","lightgreen")(genre)) %>%
  my_kable_styling("Three Sources Stats")


all_sources_tidy_balanced %>%
  count(source, genre)%>%
  mutate(word_count = color_bar('pink')(n), 
         source = color_tile('lightblue','white')(source),
         genre = color_tile('white','lightblue')(genre)) %>%
  my_kable_styling(" ")



three_sources_dtm_balanced <- three_sources_tidy_balanced %>%
  #get word count per document to pass to cast_dtm
  count(document, word, sort = TRUE) %>%
  ungroup() %>%
  #create a DTM with docs as rows and words as columns
  cast_dtm(document, word, n)
#examine the structure of the DTM
three_sources_dtm_balanced
library(tm)
inspect(three_sources_dtm_balanced[1,1])


#assign the source dataset to generic var names
#so we can use a generic function per model
source_dtm <- three_sources_dtm_balanced
source_tidy <- three_sources_tidy_balanced

k <- 3 #number of topics
seed = 1234 #necessary for reproducibility
#fit the model passing the parameters discussed above
#you could have more control parameters but will just use seed here
lda <- LDA(source_dtm, k = k, method = "GIBBS", control = list(seed = seed))
#examine the class of the LDA object
class(lda)


#convert the LDA object into a tidy format
#passing "beta" shows the word probabilities
#filter on the word iceberg as an example
#results show probability of iceberg for each topic
tidy(lda, matrix = "beta") %>% filter(term == "rough")


num_words <- 10 #number of words to visualize

#create function that accepts the lda model and num word to display
top_terms_per_topic <- function(lda_model, num_words) {
  
  #tidy LDA object to get word, topic, and probability (beta)
  topics_tidy <- tidy(lda_model, matrix = "beta")
  
  
  top_terms <- topics_tidy %>%
    group_by(topic) %>%
    arrange(topic, desc(beta)) %>%
    #get the top num_words PER topic
    slice(seq_len(num_words)) %>%
    arrange(topic, beta) %>%
    #row is required for the word_chart() function
    mutate(row = row_number()) %>%
    ungroup() %>%
    #add the word Topic to the topic labels
    mutate(topic = paste("Topic", topic, sep = " "))
  #create a title to pass to word_chart
  title <- paste("LDA Top Terms for", k, "Topics")
  #call the word_chart function you built in prep work
  word_chart(top_terms, top_terms$term, title)
}
#call the function you just built!
top_terms_per_topic(lda, num_words)


#this time use gamma to look at the prob a doc is in a topic
#just look at the Prince song 1999 as an example
tidy(lda, matrix = "gamma") %>% filter(document == "1999")


#using tidy with gamma gets document probabilities into topic
#but you only have document, topic and gamma
source_topic_relationship <- tidy(lda, matrix = "gamma") %>%
  #join to orig tidy data by doc to get the source field
  inner_join(three_sources_tidy_balanced, by = "document") %>%
  select(source, topic, gamma) %>%
  group_by(source, topic) %>%
  #get the avg doc gamma value per source/topic
  mutate(mean = mean(gamma)) %>%
  #remove the gamma value as you only need the mean
  select(-gamma) %>%
  #removing gamma created duplicates so remove them
  distinct()


#relabel topics to include the word Topic
source_topic_relationship$topic = paste("Topic", source_topic_relationship$topic, sep = " ")

circos.clear() #very important! Reset the circular layout parameters
#assign colors to the outside bars around the circle
grid.col = c("prince" = my_colors[1],
             "icebergs" = my_colors[2],
             "machine_learning" = my_colors[3],
             "Topic 1" = "grey", "Topic 2" = "grey", "Topic 3" = "grey")

# set the global parameters for the circular layout. Specifically the gap size (15)
#this also determines that topic goes on top half and source on bottom half
circos.par(gap.after = c(rep(5, length(unique(source_topic_relationship[[1]])) - 1), 15,
                         rep(5, length(unique(source_topic_relationship[[2]])) - 1), 15))
#main function that draws the diagram. transparancy goes from 0-1
chordDiagram(source_topic_relationship, grid.col = grid.col, transparency = .2)
title("Relationship Between Topic and Source")


num_doc = 5

tidy(lda, matrix = 'gamma') %>% 
  group_by(topic) %>%
  arrange(topic, desc(gamma))%>%
  slice(seq_len(num_doc)) %>%
  mutate(row = row_number())



number_of_documents = 5 #number of top docs to view
title <- paste("LDA Top Documents for", k, "Topics")

#create tidy form showing topic, document and its gamma value
topics_tidy <- tidy(lda, matrix = "gamma")

#same process as used with the top words
top_documents <- topics_tidy %>%
  group_by(topic) %>%
  arrange(topic, desc(gamma)) %>%
  slice(seq_len(number_of_documents)) %>%
  arrange(topic, gamma) %>%
  mutate(row = row_number()) %>%
  ungroup() %>%
  #re-label topics
  mutate(topic = paste("Topic", topic, sep = " "))

title <- paste("LDA Top Documents for", k, "Topics")
word_chart(top_documents, top_documents$document, title)


#use the same three sources you started with
source_dtm <- three_sources_dtm_balanced
source_tidy <- three_sources_tidy_balanced

#Set a seed for replicable results
set.seed(1234)
k <- 3
kmeansResult <- kmeans(source_dtm, k)
str(kmeansResult)

head(kmeansResult$cluster["1999"])



num_words <- 8 #number of words to display
#get the top words from the kmeans centers
kmeans_topics <- lapply(1:k, function(i) {
  s <- sort(kmeansResult$centers[i, ], decreasing = T)
  names(s)[1:num_words]
})

#make sure it's a data frame
kmeans_topics_df <- as.data.frame(kmeans_topics)
#label the topics with the word Topic
names(kmeans_topics_df) <- paste("Topic", seq(1:k), sep = " ")
#create a sequential row id to use with gather()
kmeans_topics_df <- cbind(id = rownames(kmeans_topics_df),
                          kmeans_topics_df)
#transpose it into the format required for word_chart()
kmeans_top_terms <- kmeans_topics_df %>% gather(id, 1:k)
colnames(kmeans_top_terms) = c("topic", "term")

kmeans_top_terms <- kmeans_top_terms %>%
  group_by(topic) %>%
  mutate(row = row_number()) %>% #needed by word_chart()
  ungroup()

title <- paste("K-Means Top Terms for", k, "Topics")
word_chart(kmeans_top_terms, kmeans_top_terms$term, title)
