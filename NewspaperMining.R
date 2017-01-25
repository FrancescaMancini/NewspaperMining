#########################
#Francesca Mancini
#########################

####The Guardian####

library(GuardianR)
library(stringr)

#create empty dataframe to store articles
articles<-NULL

#loop through months and years to create GET requests to the API

for(y in 1999:2016){
  for(m in 1:12){
    if (m!=12){#any month but december to avoid going into 2017 at the end of 2016
    articles_tmp<-get_guardian("conservation%20AND%20wildlife",from.date=paste(y,"-",m,"-01",sep=""),to.date=paste(y,"-",m+1,"-01",sep=""),api.key=key)
    #get all articles that contain both the words conservation and wildlife
    } else {articles_tmp<-get_guardian("conservation%20AND%20wildlife",from.date=paste(y,"-",m,"-02",sep=""),to.date=paste(y,"-",m,"-31",sep=""),api.key=key)}
    #for december change the day to 31
    if(nrow(articles_tmp)!=0) {#if the response is not empty
      articles<-rbind(articles,articles_tmp)}#add results to dataframe
  }
}

#identify duplicates (if any)
articles_dupl<-duplicated(articles[,"id"])
#and delete them
articles_unique<-articles[-which(articles_dupl=="TRUE"),]

#subset to keep only columns we are interested in
articles_sub<-articles_unique[,c(1,4,5,6,7,22,23,27)]

#some article bodies are missing, replace with NA
articles_sub$body[which(articles_sub$body=="")]<-NA

#remove everything that is not a letter or a number from the body of the articles
articles_sub$body<- str_replace_all(articles_sub$body, "[^a-zA-Z\\s]", " ")

#save file
write.table(articles_sub,"Guardian_sub.txt",sep="\t",row.names=FALSE)

#read dataset in
guardian_art<-read.table("Guardian_sub.txt",header=T,sep="\t")


#########################
#New York Times
#code adapted from https://www.r-bloggers.com/new-york-times-article-search-api-to-mongodb/
#########################
library(httr)
library(XML)

#function to download text of articles
parseArticleBody <- function(artHTML) {
  xpath2try <- c('//div[@class="articleBody"]//p',
                 '//p[@class="story-body-text story-content"]',
                 '//p[@class="story-body-text"]'
  )
  for(xp in xpath2try) {
    bodyi <- paste(xpathSApply(htmlParse(artHTML), xp, xmlValue), collapse='')
    if(nchar(bodyi)>0) break
  }
  return(bodyi)
}


#make empty dataframe to store the articles
URLs<-data.frame(url=character(),source=character(),date=character(),text=character(),stringsAsFactors = F)

#loop through years to create GET requests to the API 
for (y in 1981:2016){
makeURL<-paste('https://api.nytimes.com/svc/search/v2/articlesearch.json?api-key=',NYTkey,'&fq=body:("conservation"AND"wildlife")&begin_date=',y,'0101&end_date=',y,'1231&facet_field=source&facet_filter=true',sep="")
##make the URL to search the API
#search for the words conservation AND wildlife in the body of articles

resp<-GET(makeURL)
#do the request to the API

#calculate number of pages to loop through
#10 articles for each page (n articles/10)
#+1 page for any article left
#0 decimal places
n_pages<-round(((content(resp, 'parsed')$response$meta$hits)/10),digits=0)

#make an empty list to store the metadata of each article
metalist<-list()

#loop through pages
for(i in 0:n_pages){
resp <- GET(paste(makeURL,"&page=",i,sep=""))#request to the API to get the metadata fro the articles in each page
pt <- content(resp, "parsed") ## retrieve contents of response formatted in a nested R list
metalist <- append(metalist, pt$response$docs) ## add articles to list
Sys.sleep(1)#pause between pages to avoid hitting API limits
}

#make a temporary dataframe to store metadata
tmpURLs<-data.frame(url=character(),source=character(),date=character(),text=character(),stringsAsFactors = F)

#loop through articles
for (i in 1:length(metalist)){
  tmpURLs[i,1]<-metalist[[i]]$web_url  #retrieve article URL
  tmpURLs[i,2]<-metalist[[i]]$source   #retrieve source ("NYT", "Herald Tribune" etc.)
  tmpURLs[i,3]<-metalist[[i]]$pub_date #retrieve date of publication
}
URLs<-rbind(URLs,tmpURLs) #add to dataframe
}
#Error in `[<-.data.frame`(`*tmp*`, i, 2, value = NULL) : 
#replacement has length zero

#API limit reached repeat 2014-2016 tomorrow

for (y in 2015:2016){
  makeURL<-paste('https://api.nytimes.com/svc/search/v2/articlesearch.json?api-key=',key,'&fq=body:("conservation"AND"wildlife")&begin_date=',y,'0101&end_date=',y,'1231&facet_field=source&facet_filter=true',sep="")
  
  resp<-GET(makeURL)
  
  #calculate number of pages to loop through
  #10 articles for each page (n articles/10)
  #+1 page for any article left
  #0 decimal places
  n_pages<-round(((content(resp, 'parsed')$response$meta$hits)/10),digits=0)
  
#get request to the API
  #loop through pages
  metalist<-list()
  for(i in 0:n_pages){
    resp <- GET(paste(makeURL,"&page=",i,sep=""))
    pt <- content(resp, "parsed") ## retrieve contents of response formatted in a nested R list
    metalist <- append(metalist, pt$response$docs) ## add articles to list
    Sys.sleep(1)
  }
  
  tmpURLs<-data.frame(url=character(),source=character(),date=character(),text=character(),stringsAsFactors = F)
  
  for (i in 1:length(metalist)){
    if (is.null(metalist[[i]]$web_url)==FALSE){
    tmpURLs[i,1]<-metalist[[i]]$web_url}
    else {tmpURLs[i,1]<-NA}
    if (is.null(metalist[[i]]$source)==FALSE){ #one of the articles had a null source
      tmpURLs[i,2]<-metalist[[i]]$source}
    else {tmpURLs[i,2]<-NA}
    if (is.null(metalist[[i]]$pub_date)==FALSE){
      tmpURLs[i,3]<-metalist[[i]]$pub_date}
    else {tmpURLs[i,3]<-NA}
  }
  URLs<-rbind(URLs,tmpURLs)
  }


URL_dupl<-duplicated(URLs[,"url"])             #find duplicates
URL_unique<-URLs[-which(URL_dupl=="TRUE"),]    #and delete them

write.table(URL_unique,"NYTurl.txt",row.names = F,sep="\t")  #save file

NYTurl<-read.table("NYTurl.txt",stringsAsFactors = F, sep="\t",header=T)  #read the dataset


#get article body
#loop through urls
for (i in 1:length(NYTurl$url)){
  if (startsWith(NYTurl$url[i],"http")){ #some url are wrong or missing
                                           #if url starts with http then...  
  article<-GET(NYTurl$url[i])  #...make the GET request
  html<- content(article,"text")  #get the text
  NYTurl[i,4]<-parseArticleBody(html)}# parse the article body
  else {NYTurl[i,4]<-NA} #if url does not start with http then it is wrong so assign NA
  Sys.sleep(1)
}


NYT_articles<-NYTurl

#some article bodies are missing, replace with NA
NYT_articles$text[which(NYT_articles$text=="")]<-NA

#remove everything that is not a letter or a number from the body of the articles
NYT_articles$text<- str_replace_all(NYT_articles$text, "[^a-zA-Z\\s]", " ")

#save file
write.table(NYT_articles,"NYT_articles.txt",sep="\t",row.names=FALSE)

#read dataset in
NYT<-read.table("NYT_articles.txt",header=T,sep="\t")





