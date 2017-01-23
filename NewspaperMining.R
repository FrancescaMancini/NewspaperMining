#########################
#Francesca Mancini
#Guardian
#########################
library(GuardianR)
library(stringr)

key<-"cd31f5ae-cf20-47f6-b6e0-e80b027825ec"
articles<-NULL

for(y in 1999:2016){
  for(m in 1:12){
    if (m!=12){
    articles_tmp<-get_guardian("conservation+wildlife+nature+biodiversity",from.date=paste(y,"-",m,"-01",sep=""),to.date=paste(y,"-",m+1,"-01",sep=""),section="environment",api.key=key)
    } else {articles_tmp<-get_guardian("conservation+wildlife+nature+biodiversity",from.date=paste(y,"-",m,"-02",sep=""),to.date=paste(y,"-",m,"-31",sep=""),section="environment",api.key=key)}
    if(nrow(articles_tmp)!=0) {
      articles<-rbind(articles,articles_tmp)}
  }
}

articles_dupl<-duplicated(articles[,"id"])
articles_unique<-articles[-which(articles_dupl=="TRUE"),]
#some article bodies are missing, replace with NA
articles_unique$body[which(articles_unique$body=="")]<-NA

#remove everything that is not a letter or a number from the body of the articles
articles_unique$body<- str_replace_all(articles_unique$body, "[^a-zA-Z\\s]", " ")

write.table(articles_unique,"Guardian.txt",sep="\t",row.names=FALSE)
guardian_all<-read.table("Guardian.txt",header=T,sep="\t")
#Error in scan(file, what, nmax, sep, dec, quote, skip, nlines, na.strings,  : 
#line 210 did not have 27 elements

#subset to keep only columns we are interested in
articles_sub<-articles_unique[,c(1,4,5,6,7,22,23,27)]

#some article bodies are missing, replace with NA
articles_sub$body[which(articles_sub$body=="")]<-NA

#remove everything that is not a letter or a number from the body of the articles
articles_sub$body<- str_replace_all(articles_sub$body, "[^a-zA-Z\\s]", " ")

write.table(articles_sub,"Guardian_sub.txt",sep="\t",row.names=FALSE)

#########################
#NYTimes
#########################
library(httr)
resp <- GET(paste('https://api.nytimes.com/svc/search/v2/articlesearch.json?api-key=',NYTkey,'&fq=body:("conservation"AND"wildlife")&begin_date=20010101&end_date=20020101&facet_field=source&facet_filter=true',sep=""))

print(content(resp, 'parsed')$response$meta$hits)



makeURL <- function(q=NULL, fq=NULL, begin_date=NULL, end_date=NULL, key=getOption("nyt_as_key"), page=0, 
                    sort=NULL, fl=NULL, hl=NULL, facet_field=NULL, facet_filter=NULL){
  arglist <- list(q=q, fq=fq, begin_date=begin_date, end_date=end_date, 'api-key'=key, page=page,
                  sort=sort, fl=fl, hl=hl, facet_field=facet_field, facet_filter=facet_filter)
  url <- 'http://api.nytimes.com/svc/search/v2/articlesearch.json?'
  for(i in 1:length(arglist)){
    if(is.null(unlist(arglist[i]))==F){
      url <- paste0(url, '&', names(arglist[i]), '=', arglist[i])
    }
  }
  return(url)
}

url1<-makeURL(q="conservation",begin_date="20010101",end_date="20020101",key=NYTkey)


getMeta <- function(url, pages=Inf, sleep=0.1, tryn=3) {
  art <- list()
  i <- 1
  e <- seq(-tryn, -tryn/2, length.out=tryn) # initialize list of failed pages with arbitrary negative numbers
  while(i<=pages){
    if(length(unique(e[(length(e)-(tryn-1)):length(e)]))==1) i <- i+1 ## attempt tryn times before moving on
    tryget <- try({
      urlp <- gsub("page='\d+'", paste0("page=", i), url) ## get the next page
      p <- GET(urlp) ## actually make GET request
      pt <- content(p, "parsed") ## retrieve contents of response formatted in a nested R list
      if(length(pt$response$docs)>0) art <- append(art, pt$response$docs) ## add articles to list
      else {print(paste0(i, "pages collected")); break}
      if(i %% 10 ==0) print(paste0(i, "pages of metadata collected"))
      i <- i+1
    })
    
    ## if there was  a fail...
    if(class(tryget)=='try-error') {
      print(paste0(i, ' error - metadata page not scraped'))
      e <- c(e, i) ## add page i to failed list
      e <- e[(length(e)-(tryn-1)):length(e)] ## keep the failed list to just tryn elements
      Sys.sleep(0.5) ## probably scraping too fast -- slowing down
    }
    Sys.sleep(sleep)
  }
  return(art)
}





