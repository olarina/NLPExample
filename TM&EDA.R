library(dplyr)
library(ggplot2)
library(gridExtra)
library(tidytext)
library(wordcloud2)

prince_orig = read.csv("prince_raw_data.csv", stringsAsFactors = F)
names(prince_orig)
prince = prince_orig %>%
    select(lyrics = text, song, year, album, peak,
           us_pop=US.Pop, us_rnb = US.R.B)
glimpse(prince[139,])
str(prince[139,]$lyrics, nchar.max = 300)
fix.contractions = function(doc)
{
    doc = gsub("won't","will not",doc)
    doc = gsub("can't","can not",doc)
    doc = gsub("n't"," not",doc)
    doc = gsub("'ll"," will",doc)
    doc = gsub("'re"," are",doc)
    doc = gsub("'ve"," have",doc)
    doc = gsub("'m"," am",doc)
    doc = gsub("'d"," would",doc)
    doc = gsub("'s","",doc)
    return(doc)
}
prince$lyrics = sapply(prince$lyrics, fix.contractions)
removeSpecialChars = function(x)
    gsub("[^a-zA-Z0-9 ]"," ",x)
prince$lyrics = sapply(prince$lyrics, removeSpecialChars)
prince$lyrics = sapply(prince$lyrics, tolower)
summary(prince)

prince = prince %>%
    mutate(decade =
               ifelse(year %in% 1978:1979, "1970s",
                      ifelse(year %in% 1980:1989, "1980s",
                             ifelse(year %in% 1990:1999, "1990s",
                                    ifelse(year %in% 2000:2009, "2000s",
                                           ifelse(year %in% 2010:2019, "2010s",
                                                  NA)))))
    )
prince = prince %>%
    mutate(chart_level =
               ifelse(peak %in% 1:10, "Top 10",
                      ifelse(peak %in% 11:100, "Top 100", "Uncharted"))
    )
prince = prince %>%
    mutate(charted =
               ifelse(peak %in% 1:100, "Charted", "Uncharted")
    )
write.csv(prince, "prince_new.csv")
my_colors = c("#E69F00","#56B4E9","#009E73","#CC79A7","#D55E00")
theme_lyrics = function()
{
    theme(plot.title = element_text(hjust = 0.5),
          axis.text.x = element_blank(),
          axis.ticks = element_blank(),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          legend.position = "none")    
}
prince %>%
    filter(!is.na(decade)) %>%
    group_by(decade, charted) %>%
    summarise(number_of_songs = n()) %>%
    ggplot(aes(x = decade, y = number_of_songs, fill = charted)) + 
    geom_bar(stat = "identity") + 
    ggtitle("Released songs") +
    labs(x = NULL, y = "Song count")

charted_songs_over_time = prince %>%
    filter(peak>0) %>%
    group_by(decade, chart_level) %>%
    summarise(number_of_songs = n())

charted_songs_over_time %>%
    ggplot(aes(x = decade, y = number_of_songs, fill = chart_level)) + 
    geom_bar(stat = "identity") +
    ggtitle("Released songs") +
    labs(x = NULL, y = "Song count")

prince %>%
    group_by(decade, charted) %>%
    summarise(number_of_songs = n()) %>%
    ggplot(aes(x = decade, y = number_of_songs, fill = charted)) + 
    geom_bar(stat = "identity") + 
    ggtitle("Released songs") +
    labs(x = NULL, y = "Song count")

library(knitr) # for dynamic reporting
library(kableExtra) # create a nicely formated HTML table
library(formattable) # for the color_tile function
prince %>%
    filter(peak == "1") %>%
    select(year, song, peak) %>%
    arrange(year) %>%
    mutate(year = color_tile("lightblue", "lightgreen")(year)) %>%
    mutate(peak = color_tile("lightgreen", "lightgreen")(peak)) %>%
    kable("html", escape = FALSE, align = "c", caption = "Prince's No. 1 Songs") %>%
    kable_styling(bootstrap_options = 
                      c("striped", "condensed", "bordered"), 
                  full_width = FALSE)

## Tokenization
undesirable_words <- c("prince", "chorus", "repeat", "lyrics", 
                       "theres", "bridge", "fe0f", "yeah", "baby", 
                       "alright", "wanna", "gonna", "chorus", "verse", 
                       "whoa", "gotta", "make", "miscellaneous", "2", 
                       "4", "ooh", "uurh", "pheromone", "poompoom", "3121", 
                       "matic", " ai ", " ca ", " la ", "hey", " na ", 
                       " da ", " uh ", " tin ", "  ll", "transcription",
                       "repeats")
sample(stop_words$word, 15)
prince_words_filtered = prince %>%
    unnest_tokens(word, lyrics) %>%
    anti_join(stop_words) %>%
    distinct() %>%
    filter(!word %in% undesirable_words) %>%
    filter(nchar(word)>3)

prince_words_filtered %>% 
    filter(word == "race") %>%
    select(word, song, year, peak, decade, chart_level, charted) %>%
    arrange() %>%
    top_n(10,song) %>%
    mutate(song = color_tile("lightblue","lightblue")(song)) %>%
    mutate(word = color_tile("lightgreen","lightgreen")(word)) %>%
    kable("html", escape = FALSE, align = "c", caption = "Tokenized Format Example") %>%
    kable_styling(bootstrap_options = 
                      c("striped", "condensed", "bordered"), 
                  full_width = FALSE)
## word frequency
full_word_count = prince %>%
    unnest_tokens(word, lyrics) %>%
    group_by(song, chart_level) %>%
    summarise(num_words = n()) %>%
    arrange(desc(num_words))

full_word_count[1:10,] %>%
    ungroup(num_words, song) %>%
    mutate(num_words = color_bar("lightblue")(num_words)) %>%
    mutate(song = color_tile("lightpink","white")(song)) %>%
    kable("html", escape = FALSE, align = "c", caption = "Songs With Highest Word Count") %>%
    kable_styling(bootstrap_options = 
                      c("striped", "condensed", "bordered"), 
                  full_width = FALSE)
full_word_count %>%
    ggplot() +
    geom_histogram(aes(x = num_words, fill = chart_level)) +
    ylab("Song count") + 
    xlab("Word count per song") +
    ggtitle("Word count distribution")

full_word_count %>%
    filter((num_words > 800)&(chart_level == "Top 10")) %>%
    left_join(prince_orig) %>%
    select(Song = song, 
           "Word Count" = num_words, 
           "Peak Position" = peak, 
           "US Pop" = US.Pop, 
           "US R&B" = US.R.B, 
           Canada = CA, 
           Ireland = IR) %>%
    kable("html", escape = FALSE) %>%
    kable_styling(bootstrap_options = c("striped", "condensed", "bordered"))

prince_words_filtered %>%
    count(word, sort = TRUE) %>%
    top_n(10) %>%
    ungroup() %>%
    mutate(word = reorder(word,n)) %>%
    ggplot() +
    geom_col(aes(word, n), fill = my_colors[4]) +
    xlab("") +
    ylab("Song Count") +
    ggtitle("Most Frequently Used Words in Prince Lyrics") +
    coord_flip()

## word clouds
prince_words_counts = prince_words_filtered %>%
    count(word, sort = TRUE)
library(wordcloud2)
wordcloud2(prince_words_counts[1:300,], size = 0.5)
## doesn't work
## https://github.com/Lchiffon/wordcloud2/issues/12
wordcloud2(prince_words_counts[1:300, ], figPath = "guitar_icon.jpg", 
           color = "random-dark", size = 1.5)
letterCloud(prince_words_counts[1:300,], word = "PRINCE", size = 2)

popular_words = prince_words_filtered %>%
    group_by(chart_level) %>%
    count(word, chart_level, sort = TRUE) %>%
    slice(seq_len(8)) %>%
    ungroup() %>%
    arrange(chart_level, n) %>%
    mutate(row = row_number())
    
popular_words %>%
    ggplot(aes(row, n, fill = chart_level)) +
    geom_col(show.legend = F) +
    labs(x = NULL, y = "Song Count") +
    ggtitle("Popular Words by Chart Level") + 
    theme_lyrics() +  
    facet_wrap(~chart_level, scales = "free") +
    scale_x_continuous(  # This handles replacement of row 
        breaks = popular_words$row, # notice need to reuse data frame
        labels = popular_words$word) +
    coord_flip()  
    
timeless_words = prince_words_filtered %>%
    filter(!is.na(decade)) %>%
    group_by(decade) %>%
    count(word, decade, sort = TRUE) %>%
    slice(seq_len(8)) %>%
    ungroup() %>%
    arrange(decade, n) %>%
    mutate(row = row_number())

timeless_words %>%
    ggplot(aes(row, n, fill = decade)) + 
    geom_col(show.legend = F) +
    labs(x = NULL, y = "Song Count") +
    ggtitle("Timeless Words by decade") + 
    theme_lyrics() +  
    facet_wrap(~decade, scales = "free", ncol = 5) +
    scale_x_continuous(
        breaks = timeless_words$row,
        labels = timeless_words$word) +
    coord_flip()

prince_word_lengths = prince %>%
    unnest_tokens(word, lyrics) %>%
    group_by(song, decade) %>%
    distinct() %>%
    filter(!word %in% undesirable_words) %>%
    mutate(word_length= nchar(word)) 
prince_word_lengths = prince_word_length %>%
    count(word_lengths, sort = TRUE)
    
    
    
    
    
    
    
    
    
    


    
    
    
    
    
    
    
    
    






    






