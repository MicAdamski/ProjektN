library(RSelenium)
library(seleniumPipes)
library(dplyr)
library(stringr)
library(gtools)
library(xml2)
library(rvest)
library(tidyverse)
#install.packages("mgsub")
library(mgsub)
#install.packages("utf8")
library(utf8)

remDr <- remoteDr(remoteServerAddr = "http://localhost",
                  port= 4444,
                  browserName = "chrome",
                  newSession = TRUE)

?remoteDr

# remDr$getStatus()


#remDr %>% go("https://flixable.com/genre/movies/#filterForm")

remDr %>% go("https://flixable.com/")
webElem <- remDr%>% findElement("css", "body")
scrollHeight<- remDr %>%executeScript("return document.body.scrollHeight", args = list(""))
lastScrollHeight <- scrollHeight[[1]]
repeat{
  remDr %>% executeScript("window.scrollTo(0, document.body.scrollHeight);")
  #window.scrollBy(x,y)
  #remDr %>% executeScript("window.scrollTo(0, document.querySelector(\".scrollingContainer\").scrollHeight);")
  
  
  Sys.sleep(1)
  scrollHeight<- remDr %>% executeScript("return document.body.scrollHeight", args = list(""))
  #remDr %>% executeScript("window.scrollTo(0, 1);")
  remDr %>% executeScript("window.scrollBy(0,-100);")
  newScrollHeight <- scrollHeight[[1]]
  if (lastScrollHeight == newScrollHeight){
    break
  }
}
?html_text

#remDr %>% go("https://flixable.com/?min-rating=0&min-year=1920&max-year=2022&order=title#filterForm")
#webElem <- remDr %>% findElement("css", "body")
#webElem$sendKeysToElement(list(key = "end"))
?findElement

wektorLinkow <- c()

elems <- remDr %>% findElements(using="class name", "card-header-image")
for(j in 1:length(elems)){
  e<-findElementsFromElement(elems[[j]], using = "tag name", "a")
  if(length(e)>0){
    link<- e[[1]]%>%getElementAttribute("href")
    wektorLinkow<-c(wektorLinkow,link)
  }
}
write.csv(wektorLinkow, file ="myVectors.csv", row.names=FALSE)
#?findElementsFromElement
?write.csv
wektorLinkow

?html_node
length(wektorLinkow) # 5982 filmy sieriale, stan na 12.08.2021 
wektorLinkowU<- wektorLinkow%>%unique()
length(wektorLinkowU)
df1<- NULL
zrobWierszRvest<-function(w,wektorLinkow,remDr){
  newUrl<-wektorLinkow[w]
  page<-read_html(newUrl)
  page<-read_html("https://flixable.com/title/lf-mbrwk/", encoding="UTF-8")
  #details<-html_node(page,".card-category")%>%html_text()
  movie_name <- html_node(page,".title")%>%html_text()
  guess_encoding(movie_name)
  year <- page%>%xml_find_all("/html/body/div[3]/div/div[2]/div[1]/div/div[2]/div[1]/div[2]/h6/span[1]")%>%html_text()
  length<- page %>% xml_find_all("/html/body/div[3]/div/div[2]/div[1]/div/div[2]/div[1]/div[2]/h6/span[3]")%>%html_text()
  rating<- page %>% xml_find_all("/html/body/div[3]/div/div[2]/div[1]/div/div[2]/div[1]/div[2]/h6/span[5]")%>%html_text()
  if (identical(rating, character(0))){
    rating <-NA
  }
  v1<-page %>% xml_find_all("/html/body/div[3]/div/div[2]/div[1]/div/div[2]/div[1]/div[2]/p[2]/*")%>%html_text()%>%na.omit() #genres
  v2<-page %>% xml_find_all("/html/body/div[3]/div/div[2]/div[1]/div/div[2]/div[1]/div[2]/p[3]/*")%>%html_text()%>%na.omit() #director
  v3<-page %>% xml_find_all("/html/body/div[3]/div/div[2]/div[1]/div/div[2]/div[1]/div[2]/p[4]/*")%>%html_text()%>%na.omit() #cast
  v4<-page %>% xml_find_all("/html/body/div[3]/div/div[2]/div[1]/div/div[2]/div[1]/div[2]/p[5]/*")%>%html_text()%>%na.omit() #country
  v5<-page %>% xml_find_all("/html/body/div[3]/div/div[2]/div[1]/div/div[2]/div[1]/div[2]/p[6]/*")%>%html_text()%>%na.omit() #rate
  v6<-page %>% xml_find_all("/html/body/div[3]/div/div[2]/div[1]/div/div[2]/div[1]/div[2]/p[7]/*")%>%html_text()%>%na.omit() #added to netlix
  
  v <- c(v1,v2,v3,v4,v5,v6)
  #print(v)
  indexy<-seq(1,length(v),1)
  nazwyKolumn<-v[indexy%%2==1]
  nazwykolumn<- gsub(":","",nazwyKolumn)
  wartosci<-v[indexy%%2==0]
  df1<- data.frame( matrix(wartosci,nrow=1,ncol=length(wartosci)))
  names(df1)<-nazwyKolumn
  #df1<-data.frame(1,1)
  df1<-cbind(movie_name,df1)
  df1<-cbind(length,df1)
  df1<-cbind(rating,df1)
  df1<-cbind(year,df1)
  df1
}
#zrobWierszRvest(1, wektorLinkow, remDr)

movies<-NULL
for(w in 1: length(wektorLinkow)){
#for(w in 1: 100){
  skip<-FALSE
  tryCatch(
    df2<-zrobWierszRvest(w,wektorLinkow,remDr),error=function(e){skip<<-TRUE}
  )
  if(skip){next}
  if(is.null(movies)){
    movies<-df2
  }else{
    movies<-smartbind(movies,df2)
    
  }
}
View(movies)

write.csv(movies, file ="mymovies2.csv", row.names=FALSE, fileEncoding = "UTF-8")

movies <- read.csv("mymovies.csv", header = TRUE, sep = ",")
?smartbind
?data.frame

#############################
# data analysis             #
# podstawowe rzeczy:
# Najpopularniejszyc 10 aktorów
# Sredni rating z danego roku 
# najwiecej filmów z danego roku
# najpopularniejszy gatunek per rok i w ogóle
# 
#############################
# oczyszczenie danych 
movies_bck <- movies
View(movies_bck)


movies$movie_name <- mgsub(movies$movie_name, c("'", "!","#","[()]","[<.*>]"), c("", "","", "",""))
movies <- subset (movies, select = -Rate.) 
View(movies)
#moviesPoland <- movies%>%filter(`Production Country:`=="Poland")%>%collect()
#View(moviesPoland)

# this is to breakdown a column with multiple values


View(movies2)

summary(movies)
str(movies)
moviesCast <- movies%>%
  mutate(unpacked = str_split(Cast., ",")) %>%
  unnest(cols = c(unpacked)) %>%
  mutate(Cast. = str_trim(unpacked))

View(moviesCast)
mostPopActor <- names(which.max(table(moviesCast$Cast.)))

moviesActor <- moviesCast%>%filter(Cast.==mostPopActor)%>%collect() 
?count
actorsList <-  moviesCast%>% group_by(Cast.)%>%tally(sort = TRUE)%>%na.omit()
actorsList[1:10,] #top 10 actors who are present most often in the cast of Netflix movie library

topRateMovies <-  movies%>% group_by(rating)%>%tally(sort = TRUE)%>%na.omit() # najczęściej dawane oceny
topRatedMovies <- movies %>% arrange(desc(rating)) %>% slice(1:10) # 10 najelpiej ocenianych filmow
movies$rating <- as.numeric(str_replace_all(movies$rating, "\\/\\d+", "") )
noYearMovies <- movies%>% group_by(year)%>%tally(sort = TRUE)%>%na.omit() # liczb filmów dostępnych w serwisie wyprodukowanych w danym roku
avgRatingPerYear <- movies%>% group_by(year)%>%
  summarize(mean_size = mean(rating , na.rm = TRUE), count_film= n()) # średnia ocen dla filmów per rok


moviesProduction <- movies%>%
  mutate(unpacked = str_split(`Production.Country.`, ",")) %>%
  unnest(cols = c(unpacked)) %>%
  mutate(`Production.Country.` = str_trim(unpacked))


