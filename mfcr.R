library(jsonlite)
library(httr)
library(RCurl)
library(dplyr)

###CONF####
mysite = "site.ndd"
# API SIMILARWEB
key_similarweb = "XXXXXXXXXXXXXXXXXXXXX"
# API PAGE INSIGHT
key_api = "XXXXXXXX-XXXXXXXXXXXXXXXXXXXXX"
###########

testResponsive <- function(site) {
  path <- paste("https://www.googleapis.com/pagespeedonline/v3beta1/mobileReady?screenshot=false&strategy=mobile&filter_third_party_resources=false&snapshots=false&url=http://", site,"&key=", key_api ,sep = "")    
  content <- getURL(path,ssl.verifypeer = FALSE)
  result <- fromJSON( content )
  print(length(result$ruleGroups))
  if (length(result$ruleGroups)>0){
    if (result$ruleGroups$USABILITY$pass==TRUE)
      return(1)
    return(0)
  }
  else
    return(-1)
}


# get top 20 competitors with api similar web
path <- paste("http://api.similarweb.com/Site/",mysite,"/v2/similarsites?limit=10&UserKey=", key_similarweb ,sep = "")    
content <- getURL(path)
list <- fromJSON( content )

DT <- do.call(rbind, lapply(list, data.frame, row.names=NULL, stringsAsFactors=TRUE))

# check each competitors
i=0
for(site in DT$Url) {
  #print(site)
  if(i<=12) {
    DT$Boost[DT$Url==site] <- testResponsive(site)
  }
  else
    DT$Boost[DT$Url==site] <- -1
  
  i = i + 1
}

# display results
DT_RANK <- DT[DT$Boost>=0,]
DT_RANK <- arrange(DT_RANK,desc(Boost),desc(Score))
print(DT_RANK)

#aggregate
DT_MF <- DT_RANK %>% group_by(Boost) %>% summarise(Total=n())
print(DT_MF$Total[2] / DT_MF$Total[1])
MFCR <- round((DT_MF$Total[2] / DT_MF$Total[1])*100,2)
MFCR_legend <- paste("Websites , Impact : ", MFCR," %")
MFCR_title <- paste("Mobile Friendly Competitor Rate : ",mysite)


barplot(DT_MF$Total, 
        beside = TRUE, 
        horiz = TRUE, 
        col = c("red", "green"),
        #ylab = c("Yes","No"),
        xlab = MFCR_legend,
        legend = c("No","Yes"))
title(main = MFCR_title, font.main = 4)


