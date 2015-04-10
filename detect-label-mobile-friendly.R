# load packages
library(jsonlite)
library(httr)
library(RCurl)
library(dplyr)
library(XML)
library(R2HTML)
library(stringr)
library(ggplot2)

# API PAGE INSIGHT
key_api = "XXXXXXXXXXXXXXXXXXXXXXXXXXXXXX"

###################################
# detect mention mobile friendly
# param
# site : website name
# country : google url
# mention : keyword for MF
##########################
detectMentionMF <- function(site,serp,mention) {  
  u <- paste("https://www.",serp,"/search?q=",site,sep="")
  useragent <- "Mozilla/5.0 (iPhone; CPU iPhone OS 6_0 like Mac OS X) AppleWebKit/536.26 (KHTML, like Gecko) Version/6.0 Mobile/10A5376e Safari/8536.25"
  html <- getURL(u,ssl.verifypeer = FALSE, httpheader = c('User-Agent' = useragent))
  doc <- htmlParse(html)
  attrs <- xpathApply(doc, "//span[@class='st']")
  mentions <- sapply(attrs, function(x) x[[1]])
  free(doc)
  return(str_detect(xmlValue(mentions[[1]]), 'Site mobile'))
}

###################################
# get Score API Google
# site : website name
##########################
getScoreMF <- function(site) {
  path <- paste("https://www.googleapis.com/pagespeedonline/v3beta1/mobileReady?screenshot=false&strategy=mobile&filter_third_party_resources=false&snapshots=false&url=http://www.", site,"&key=", key_api ,sep = "")    
  content <- getURL(path,ssl.verifypeer = FALSE)
  result <- fromJSON( content )
  if (length(result$ruleGroups)>0){
    return(result$ruleGroups$USABILITY$score)
  }
  else
    return(-1)
}

#Analyse
N <- 400
DF <- data.frame(num=rep(NA, N), 
                 site=rep("", N),  # as many cols as you need
                 pagespeed=rep("", N),
                 mention=rep("", N),
                 stringsAsFactors=FALSE) 

fileName="top500-alexa.txt"
conn=file(fileName,open="r")
linn=readLines(conn)
for (i in 1:length(linn)){
  site <- linn[i]
  # detect mention "Site Mobile -" in French
  dMMF <- detectMentionMF(site,"google.fr","Site Mobile")
  # get score api pagespeed
  gSMF <- getScoreMF(site)
  DF[i, ] <- c(i,site,gSMF,dMMF)
  print(i)
  print(site)
  if (i>=N) break;
}
close(conn)

#test
DF2 <- DF[(DF$pagespeed>0) & (complete.cases(DF$mention) ),]
list_100 <- agrep("100", DF2$pagespeed)
DF2$pagespeed[list_100]<- 99

gg <- ggplot(DF2, aes(x=num, y=pagespeed, fill=mention)) +
  geom_bar(stat="identity") +
  facet_grid(~mention) +
  labs(x="Rank Alexa.org - @VincentTerrasi", y=expression("Score")) +
  labs(title=expression("Répartition Mention Mobile Friendly sur le Top 400 Alexa en fonction du score d'ergonomie mobile")) +
  theme(axis.text.x = element_text(angle = 90, hjust = 0, size = rel(0))) +
  geom_hline(yintercept = 20, colour="green", linetype = "longdash")

print(gg)

