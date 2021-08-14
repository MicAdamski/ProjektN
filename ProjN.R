library(RSelenium)
library(seleniumPipes)
library(dplyr)
library(stringr)
library(gtools)
library(xml2)
library(rvest)

remDr <- remoteDr(remoteServerAddr = "http://localhost",
                  port= 4444,
                  browserName = "chrome",
                  newSession = TRUE)

remdir <- remoteDriver(
  remoteServerAddr = "localhost",
  port = 4444L, # change port according to terminal 
  browserName = "chrome"
)
#?remoteDriver
?remoteDr
remdir$open()


# remDr$getStatus()


#windowSize <- remdir$getWindowPosition(windowId = "current")
#scrollHeight<- remdir$executeScript("return document.body.scrollHeight", args = list(""))
#scrollHeight[[1]]
#
#for (i in 1:20000){
#  webElem$sendKeysToElement(list(key = "end"))
#}
#
#remdir$navigate("https://flixable.com/")
remdir$navigate("https://flixable.com/genre/movies/#filterForm")
webElem <- remdir$findElement("css", "body")
scrollHeight<- remdir$executeScript("return document.body.scrollHeight", args = list(""))
lastScrollHeight <- scrollHeight[[1]]
repeat{
  webElem$sendKeysToElement(list(key = "end"))
  Sys.sleep(1)
  scrollHeight<- remdir$executeScript("return document.body.scrollHeight", args = list(""))
  newScrollHeight <- scrollHeight[[1]]
  if (lastScrollHeight == newScrollHeight){
    break
  }
}


#remDr %>% go("https://flixable.com/?min-rating=0&min-year=1920&max-year=2022&order=title#filterForm")
#webElem <- remDr %>% findElement("css", "body")
#webElem$sendKeysToElement(list(key = "end"))
?findElement

wetkorLinkow <- c()

 elems <- remdir$findElements(using="class name", "card-header-image")
  for(j in 1:length(elems)){
    e<-findElementsFromElement(elems[[j]], using = "tag name", "a")
    if(length(e)>0){
      link<- e[[1]]%>%getElementAttribute("href")
      wektorLinkow<-c(wektorLinkow,link)
    }
  }
?findElementsFromElement

View(wektorLinkow)