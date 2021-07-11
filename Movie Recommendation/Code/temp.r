library(ggeasy)
library(ggrepel)
library(DescTools)
library(data.table)
library(DT)
library(dplyr)
library(forcats)
library(ggplot2)
library(tidyverse)
library(purrr)
library(dqshiny)
library(stringi)
library(rvest)
library(reshape2)
library(tm)
library(plotrix)
library(plotly)


View(Netflix)
View(TV2)

Netflix1 <- read.csv("D:\\DurvACEE\\Study\\MCS\\Project\\DataScience\\DataSets\\movie_metadata.csv")
poster <- read.csv("D:\\DurvACEE\\Study\\MCS\\Project\\DataScience\\DataSets\\MovieGenre1.csv")
posterx <- read.csv("D:\\DurvACEE\\Study\\MCS\\Project\\DataScience\\DataSets\\MovieGenre.csv")

colnames(Netflix)

TV<-read.csv("D:\\DurvACEE\\Study\\MCS\\Project\\DataScience\\DataSets\\TvSeries.csv")

TV2<-select(TV , X , Title , totalSeasons , Genre  , imdbRating ,Plot , Year , Actors , Country , Language , Director , Poster , Rated , Released , Runtime.in.minutes , imdbVotes , Writer , Type , Awards)
TV2$Genre<-gsub(", ","|",TV2$Genre)
TV3<-data.frame(TV2)

Netflix <- data.frame(Netflix1)
Poster<-data.frame(poster)
Poster2<-data.frame(Poster)
Posterx<-data.frame(posterx)
TVS<-data.frame(TV3)


tv4<- TV3 %>% separate(Genre, c("g1", "g2", "g3", "g4","g5", "g6", "g7", "g8"), "\\|")
tv4[is.na(tv4)]<-0

#removing Â & na from movie_title

Netflix <- Netflix %>% mutate(movie_title = gsub("Â", "", movie_title))
Netflix$movie_imdb_link <- paste0("<a href='",Netflix$movie_imdb_link,"'>",Netflix$movie_imdb_link,"</a>")

Netflix <- Netflix %>% na.omit()
#sum(is.na(Netflix))
Netflix[is.na(Netflix)] <- 0 
	
##
Netflix[] <- lapply(Netflix, function(x){ 
  x[is.na(x)] <- mean(x, na.rm = TRUE)
  x
})

#top 10 Movies by imdb score
sorted_m<-Netflix[order(Netflix$imdb_score),]
top10Movies <- tail(sorted_m,n=10)


#top 10 most liked movies
sorted_m2 <- Netflix[order(Netflix$movie_facebook_likes),]
top10Movies2<-tail(sorted_m2,n=10)



data1 <- data.frame(
  name=top10Movies$movie_title,
  value=top10Movies$imdb_score
)

data1<-unique(data1)

data2 <- data.frame(
  name=top10Movies2$movie_title,
  value=top10Movies2$movie_facebook_likes
)

Netflix2<-select(Netflix , movie_title , genres , content_rating , imdb_score , duration , title_year , movie_imdb_link , language , director_name , actor_1_name , actor_2_name , actor_3_name , gross ,movie_facebook_likes , plot_keywords , num_voted_users )
#Netflix2
Netflix$movie_imdb_link <- paste0("<a href='",Netflix$movie_imdb_link,"'>",Netflix$movie_imdb_link,"</a>")

data4 <- data.frame(
  Rating=Netflix$content_rating,
  Duration=Netflix$duration
)

#Netflix Genre Separation 

dataForPie<-Netflix %>% separate_rows(genres) %>% count(genres) 

dataForPie<-data.frame(dataForPie)

dataForPie$n<-(dataForPie$n*100)/sum(dataForPie$n)

dataForPie<-data.frame(Genre=dataForPie$genres,count=dataForPie$n)
dataForPie$count<-format(round(dataForPie$count, 2), nsmall = 2)

dataForPie$count<-as.numeric(dataForPie$count)