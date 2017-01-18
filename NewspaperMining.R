#########################
#Francesca Mancini
#Guardian
#########################
library(GuardianR)

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

write.table(articles,"Guardian.txt",sep="/t",row.names=FALSE)

#########################
#NYTimes
#########################




