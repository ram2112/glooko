knitr::opts_chunk$set(eval = FALSE, message=FALSE, warning=FALSE)


#  basic library packages
library(httr)
library(XML)
library(plyr)
library(dplyr)
library(magrittr)
library(twitteR)
library(ggplot2)
library(RKlout)
library(plotly)

require('ROAuth')
require('RCurl')

source("keys.R")

#Set up Twitter authentication

setup_twitter_oauth(id,pw,token,secret)
save(setup_twitter_oauth, file="twitter authentication.Rdata")

#Retrieve Tweets

MAX_TWEETS <- 3200
tweets_raw_MDT <- userTimeline('MDT_Diabetes', n = MAX_TWEETS,
                           includeRts = FALSE, excludeReplies = TRUE)

tweets_raw_Omni <- userTimeline('myomnipod', n = MAX_TWEETS,
                                includeRts = FALSE, excludeReplies = TRUE)

tweets_raw_Tandem <- userTimeline('TandemDiabetes', n = MAX_TWEETS,
                                includeRts = FALSE, excludeReplies = TRUE)

tweets_raw_Onetouch <- userTimeline('OneTouch', n = MAX_TWEETS,
                                  includeRts = FALSE, excludeReplies = TRUE)


tweets_raw_Accucheck <- userTimeline('accuchek_us', n = MAX_TWEETS,
                                  includeRts = FALSE, excludeReplies = TRUE)


tweets_raw_Abbott <- userTimeline('abbott', n = MAX_TWEETS,
                                     includeRts = FALSE, excludeReplies = TRUE)

# Retrieve Twitter User handle information

MDT <- getUser("MDT_Diabetes")
Omni <- getUser("myomnipod")
Tandem <- getUser("TandemDiabetes")
Onetouch <- getUser("OneTouch")
Accu <- getUser("accuchek_us")
Abbott <- getUser("FreeStyleDiabet")

#Creating data frames with necessary fields

tweets_MDT <- 
  ldply(tweets_raw_MDT, function(x) {
    data_frame(id = x$id,
               date = as.Date(x$created),
               day = weekdays(date),
               favorites = x$favoriteCount,
               retweets = x$retweetCount,
               title = x$text
    )
  })


tweets_Omni <- 
  ldply(tweets_raw_Omni, function(x) {
    data_frame(id = x$id,
               date = as.Date(x$created),
               day = weekdays(date),
               favorites = x$favoriteCount,
               retweets = x$retweetCount,
               title = x$text
    )
  })

tweets_Tandem <- 
  ldply(tweets_raw_Tandem, function(x) {
    data_frame(id = x$id,
               date = as.Date(x$created),
               day = weekdays(date),
               favorites = x$favoriteCount,
               retweets = x$retweetCount,
               title = x$text
    )
  })


tweets_Onetouch <- 
  ldply(tweets_raw_Onetouch, function(x) {
    data_frame(id = x$id,
               date = as.Date(x$created),
               day = weekdays(date),
               favorites = x$favoriteCount,
               retweets = x$retweetCount,
               title = x$text
    )
  })

tweets_Accucheck <- 
  ldply(tweets_raw_Accucheck, function(x) {
    data_frame(id = x$id,
               date = as.Date(x$created),
               day = weekdays(date),
               favorites = x$favoriteCount,
               retweets = x$retweetCount,
               title = x$text
    )
  })

tweets_Abbott <- 
  ldply(tweets_raw_Abbott, function(x) {
    data_frame(id = x$id,
               date = as.Date(x$created),
               day = weekdays(date),
               favorites = x$favoriteCount,
               retweets = x$retweetCount,
               title = x$text
    )
  })

#List of Glucose Monitor data frames
list_tweets_gm <- list(tweets_MDT,tweets_Omni,tweets_Tandem)

#List of Meter Maker data frames
list_tweets_mm <- list(tweets_Onetouch,tweets_Accucheck,tweets_Abbott)

# All possible punctuation symbols
punct <- '[]\\?!\"\'#$%&(){}+*/:;,._`|~\\[<=>@\\^-]'

# Punctuation without '#'
punct2 <- sub("#","", punct)

# Pre-processing Glucose Monitor companies' Data Frames containing tweets

for (i in 1:length(list_tweets_gm))
{
  list_tweets_gm[[i]]$title <- mapply(gsub, sprintf("http[^[:space:]]*"), "", list_tweets_gm[[i]]$title)
  list_tweets_gm[[i]]$title <- gsub('\\b+RT', '', list_tweets_gm[[i]]$title)
  list_tweets_gm[[i]]$title <- gsub('@\\S+', '', list_tweets_gm[[i]]$title)
  list_tweets_gm[[i]]$title <- gsub('[[:cntrl:]]', '', list_tweets_gm[[i]]$title)
  list_tweets_gm[[i]]$title <- gsub("\\d", '', list_tweets_gm[[i]]$title)
  list_tweets_gm[[i]]$title <- gsub(punct2, '', list_tweets_gm[[i]]$title)
  list_tweets_gm[[i]]$title <- gsub("^[[:space:]]*","", list_tweets_gm[[i]]$title)
  list_tweets_gm[[i]]$title <- gsub("[[:space:]]*$","", list_tweets_gm[[i]]$title)
  list_tweets_gm[[i]]$title <- gsub(' +',' ', list_tweets_gm[[i]]$title)
}

# Pre-processing Meter Maker companies' Data Frames containing tweets

for (i in 1:length(list_tweets_mm))
{
  list_tweets_mm[[i]]$title <- mapply(gsub, sprintf("http[^[:space:]]*"), "", list_tweets_mm[[i]]$title)
  list_tweets_mm[[i]]$title <- gsub('\\b+RT', '', list_tweets_mm[[i]]$title)
  list_tweets_mm[[i]]$title <- gsub('@\\S+', '', list_tweets_mm[[i]]$title)
  list_tweets_mm[[i]]$title <- gsub('[[:cntrl:]]', '', list_tweets_mm[[i]]$title)
  list_tweets_mm[[i]]$title <- gsub("\\d", '', list_tweets_mm[[i]]$title)
  list_tweets_mm[[i]]$title <- gsub(punct2, '', list_tweets_mm[[i]]$title)
  list_tweets_mm[[i]]$title <- gsub("^[[:space:]]*","", list_tweets_mm[[i]]$title)
  list_tweets_mm[[i]]$title <- gsub("[[:space:]]*$","", list_tweets_mm[[i]]$title)
  list_tweets_mm[[i]]$title <- gsub(' +',' ', list_tweets_mm[[i]]$title)
}

#Creating reference date value to match and retrive tweets in the month of November
date <- "2016-11-08"
temp <- as.Date(date)
temp1 <- format(temp, "%y-%m")

#Creating a subset with tweets posted in the month of November

tweets_MDT_nov <- subset(tweets_MDT, temp1 == (format(as.Date(tweets_MDT$date), "%y-%m")))

tweets_Omni_nov <- subset(tweets_Omni, temp1 == (format(as.Date(tweets_Omni$date), "%y-%m")))

tweets_Tandem_nov <- subset(tweets_Tandem, temp1 == (format(as.Date(tweets_Tandem$date), "%y-%m")))

tweets_Onetouch_nov <- subset(tweets_Onetouch, temp1 == (format(as.Date(tweets_Onetouch$date), "%y-%m")))

tweets_Accucheck_nov <- subset(tweets_Accucheck, temp1 == (format(as.Date(tweets_Accucheck$date), "%y-%m")))

tweets_Abbott_nov <- subset(tweets_Abbott, temp1 == (format(as.Date(tweets_Abbott$date), "%y-%m")))

#List of Glucose Monitor data frames for the month of November
list_tweets_gm_nov <- list(tweets_MDT_nov,tweets_Omni_nov,tweets_Tandem_nov)

#List of Meter Maker data frames for the month of November
list_tweets_mm_nov <- list(tweets_Onetouch_nov,tweets_Accucheck_nov,tweets_Abbott_nov)

Value_GM <- list(0,0,0)
Value_MM <- list(0,0,0)

#Calculating Score and Value Metric for Glucose Moniter Companies

# Value - Sum(Fav)/Sum(Retweets)
# Score - favorites + 2*Retweets

for (i in 1:length(list_tweets_gm_nov)){
  for (j in 1:nrow(list_tweets_gm_nov[[i]])){
  
    
    Value_GM[i] <- sum(list_tweets_gm_nov[[i]]$favorites) / sum(list_tweets_gm_nov[[i]]$retweets)
    list_tweets_gm_nov[[i]]$Score[j] <- list_tweets_gm_nov[[i]]$favorites[j] + list_tweets_gm_nov[[i]]$retweets[j] * 2
  }
}


    
#Calculating Score and Value Metric for Meter Maker Companies

for (i in 1:length(list_tweets_mm_nov)){
  for (j in 1:nrow(list_tweets_mm_nov[[i]])){
    
    
    Value_MM[i] <- sum(list_tweets_mm_nov[[i]]$favorites) / sum(list_tweets_mm_nov[[i]]$retweets)
    list_tweets_mm_nov[[i]]$Score[j] <- list_tweets_mm_nov[[i]]$favorites[j] + list_tweets_mm_nov[[i]]$retweets[j] * 2
  }
}

#Amalgamating all the dataframes into a single list and creating a big data frame

list_tweets_gm_nov[[1]]$User <- rep('MDT',nrow(list_tweets_gm_nov[[1]]))
list_tweets_gm_nov[[2]]$User <- rep('Omni',nrow(list_tweets_gm_nov[[2]]))
list_tweets_gm_nov[[3]]$User <- rep('Tandem',nrow(list_tweets_gm_nov[[3]]))


list_tweets_mm_nov[[1]]$User <- rep('Onetouch',nrow(list_tweets_mm_nov[[1]]))
list_tweets_mm_nov[[2]]$User <- rep('Accu',nrow(list_tweets_mm_nov[[2]]))
list_tweets_mm_nov[[3]]$User <- rep('Abbott',nrow(list_tweets_mm_nov[[3]]))


DF_GM_NOV <- data.frame(rbind(list_tweets_gm_nov[[1]],list_tweets_gm_nov[[2]],list_tweets_gm_nov[[3]]))


DF_MM_NOV <- data.frame(rbind(list_tweets_mm_nov[[1]],list_tweets_mm_nov[[2]],list_tweets_mm_nov[[3]]))



# Preliminary Look at Glucose Monitor Companies November Tweet Data

ggplot(DF_GM_NOV) +
  geom_point(aes(DF_GM_NOV$favorites,DF_GM_NOV$retweets, fill = DF_GM_NOV$User), size = 5, shape = 21) +
  theme_bw(30) +
  scale_fill_brewer("User", type = "qual", palette = 3) +
  ggtitle("Score of November tweets by Glucose Monitor Makers") +
  xlab("# favorites") + ylab("# retweets")

# Preliminary Look at Meter Maker Companies November Tweet Data

ggplot(DF_MM_NOV) +
  geom_point(aes(DF_MM_NOV$favorites,DF_MM_NOV$retweets, fill = DF_MM_NOV$User), size = 5, shape = 21) +
  theme_bw(30) +
  scale_fill_brewer("User", type = "qual", palette = 3) +
  ggtitle("Score of November tweets by Meter Makers") +
  xlab("# favorites") + ylab("# retweets")

#Glucose Monitor Companies Twitter User Related information 

list_gm_user <- list( c('MDT', 'Omni', 'Tandem'))
list_gm_followers <- list(c(MDT$followersCount,Omni$followersCount,Tandem$followersCount))
list_gm_tweetcount <- list(c(length(tweets_raw_MDT),length(tweets_raw_Omni),length(tweets_raw_Tandem)))
list_gm_Novtweetcount <- list(c(nrow(tweets_MDT_nov),nrow(tweets_Omni_nov),nrow(tweets_Tandem_nov)))
list_gm_Value <- list(c(Value_GM[[1]],Value_GM[[2]],Value_GM[[3]]))
list_gm_retweetcount <- list(c(sum(tweets_MDT$retweets),sum(tweets_Omni$retweets),sum(tweets_Tandem$retweets)))
list_gm_novretweetcount <- list(c(sum(tweets_MDT_nov$retweets),sum(tweets_Omni_nov$retweets),sum(tweets_Tandem_nov$retweets)))
list_gm_favorites <- list(c(sum(tweets_MDT$favorites),sum(tweets_Omni$favorites),sum(tweets_Tandem$favorites)))
list_gm_novfavoritescount <- list(c(sum(tweets_MDT_nov$favorites),sum(tweets_Omni_nov$favorites),sum(tweets_Tandem_nov$favorites)))
list_gm_novAvgScore <- list(c(mean(list_tweets_gm_nov[[1]]$Score),mean(list_tweets_gm_nov[[2]]$Score),mean(list_tweets_gm_nov[[3]]$Score)))
list_gm_novAvgFavorites <- list(c(mean(list_tweets_gm_nov[[1]]$favorites),mean(list_tweets_gm_nov[[2]]$favorites),mean(list_tweets_gm_nov[[3]]$favorites)))
list_gm_novAvgRetweets <- list(c(mean(list_tweets_gm_nov[[1]]$retweets),mean(list_tweets_gm_nov[[2]]$retweets),mean(list_tweets_gm_nov[[3]]$retweets)))
list_gm_Score <- list(c(sum(sum(tweets_MDT$favorites),2*sum(tweets_MDT$retweets)),sum(sum(tweets_Omni$favorites),2*sum(tweets_Omni$retweets)),sum(sum(tweets_Tandem$favorites),2*sum(tweets_Tandem$retweets))))


GM_DF <- data.frame(list_gm_user,list_gm_followers,list_gm_tweetcount,list_gm_Novtweetcount,list_gm_retweetcount,list_gm_novretweetcount,list_gm_Value,list_gm_novAvgFavorites,list_gm_novAvgRetweets,list_gm_novAvgScore,list_gm_favorites,list_gm_Score)

colnames(GM_DF)[1] <- 'gm_user'
colnames(GM_DF)[2] <- 'gm_followerscount'
colnames(GM_DF)[3] <- 'gm_tweetcount'
colnames(GM_DF)[4] <- 'gm_Novtweetcount'
colnames(GM_DF)[5] <- 'gm_retweetcount'
colnames(GM_DF)[6] <- 'gm_Novretweetcount'
colnames(GM_DF)[7] <- 'gm_NovValue'
colnames(GM_DF)[8] <- 'gm_NovAvgFavorites'
colnames(GM_DF)[9] <- 'gm_NovAvgRetweets'
colnames(GM_DF)[10] <- 'gm_NovAvgScore'
colnames(GM_DF)[11] <- 'gm_favorites'
colnames(GM_DF)[12] <- 'gm_score'

GM_DF$gm_user <- as.factor(GM_DF$gm_user)

# Comparing Followers and Tweets of a Glucose Monitor Makers twitter users 

ggplot(GM_DF) +
  geom_point(aes(GM_DF$gm_followerscount,GM_DF$gm_tweetcount, fill = GM_DF$gm_user), size = 5, shape = 21) +
  theme_bw(30) +
  scale_fill_brewer("User", type = "qual", palette = 3) +
  ggtitle("Followers vs Tweets Count Glucose Monitor") +
  xlab("# followers") + ylab("# tweets")

# Comparing Followers and (Score= (2*ReTweets+Favorites) of a Glocose Monitor Makers twitter users
ggplot(GM_DF) +
  geom_point(aes(GM_DF$gm_followerscount,GM_DF$gm_score, fill = GM_DF$gm_user), size = 5, shape = 21) +
  theme_bw(30) +
  scale_fill_brewer("User", type = "qual", palette = 3) +
  ggtitle("Impact of Followers") +
  xlab("# followers") + ylab("Score")



#Meter Maker Companies Twitter User Related information

list_mm_user <- list( c('OneTouch', 'Accu', 'Abbott'))
list_mm_followers <- list(c(Onetouch$followersCount,Accu$followersCount,Abbott$followersCount))
list_mm_tweetcount <- list(c(length(tweets_raw_Onetouch),length(tweets_raw_Accucheck),length(tweets_raw_Abbott)))
list_mm_Novtweetcount <- list(c(nrow(tweets_Onetouch_nov),nrow(tweets_Accucheck_nov),nrow(tweets_Abbott_nov)))
list_mm_Value <- list(c(Value_MM[[1]],Value_MM[[2]],Value_MM[[3]]))
list_mm_retweetcount <- list(c(sum(tweets_Onetouch$retweets),sum(tweets_Accucheck$retweets),sum(tweets_Abbott$retweets)))
list_mm_novretweetcount <- list(c(sum(tweets_Onetouch_nov$retweets),sum(tweets_Accucheck_nov$retweets),sum(tweets_Abbott_nov$retweets)))
list_mm_favorites <- list(c(sum(tweets_Onetouch$favorites),sum(tweets_Accucheck$favorites),sum(tweets_Abbott$favorites)))
list_mm_novfavoritescount <- list(c(sum(tweets_Onetouch_nov$favorites),sum(tweets_Accucheck_nov$favorites),sum(tweets_Abbott_nov$favorites)))
list_mm_novAvgScore <- list(c(mean(list_tweets_mm_nov[[1]]$Score),mean(list_tweets_mm_nov[[2]]$Score),mean(list_tweets_mm_nov[[3]]$Score)))
list_mm_novAvgFavorites <- list(c(mean(list_tweets_mm_nov[[1]]$favorites),mean(list_tweets_mm_nov[[2]]$favorites),mean(list_tweets_mm_nov[[3]]$favorites)))
list_mm_novAvgRetweets <- list(c(mean(list_tweets_mm_nov[[1]]$retweets),mean(list_tweets_mm_nov[[2]]$retweets),mean(list_tweets_mm_nov[[3]]$retweets)))
list_mm_Score <- list(c(sum(sum(tweets_Onetouch$favorites),2*sum(tweets_Onetouch$retweets)),sum(sum(tweets_Accucheck$favorites),2*sum(tweets_Accucheck$retweets)),sum(sum(tweets_Abbott$favorites),2*sum(tweets_Abbott$retweets))))


MM_DF <- data.frame(list_mm_user,list_mm_followers,list_mm_tweetcount,list_mm_Novtweetcount,list_mm_retweetcount,list_mm_novretweetcount,list_mm_Value,list_mm_novAvgFavorites,list_mm_novAvgRetweets,list_mm_novAvgScore,list_mm_favorites,list_mm_Score)

colnames(MM_DF)[1] <- 'mm_user'
colnames(MM_DF)[2] <- 'mm_followerscount'
colnames(MM_DF)[3] <- 'mm_tweetcount'
colnames(MM_DF)[4] <- 'mm_Novtweetcount'
colnames(MM_DF)[5] <- 'mm_retweetcount'
colnames(MM_DF)[6] <- 'mm_Novretweetcount'
colnames(MM_DF)[7] <- 'mm_NovValue'
colnames(MM_DF)[8] <- 'mm_NovAvgFavorites'
colnames(MM_DF)[9] <- 'mm_NovAvgRetweets'
colnames(MM_DF)[10] <- 'mm_NovAvgScore'
colnames(MM_DF)[11] <- 'mm_favorites'
colnames(MM_DF)[12] <- 'mm_score'

MM_DF$mm_user <- as.factor(MM_DF$mm_user)

# Comparing Followers and Tweets of a Meter Maker Monitor Makers twitter users

ggplot(MM_DF) +
  geom_point(aes(MM_DF$mm_followerscount,MM_DF$mm_tweetcount, fill = MM_DF$mm_user), size = 5, shape = 21) +
  theme_bw(30) +
  scale_fill_brewer("User", type = "qual", palette = 3) +
  ggtitle("Followers vs Tweets Count Meter Maker") +
  xlab("# followers") + ylab("# tweets")

# Comparing Followers and (Score= (2*ReTweets+Favorites) of a Meter Maker Monitor Makers twitter users
ggplot(MM_DF) +
  geom_point(aes(MM_DF$mm_followerscount,MM_DF$mm_score, fill = MM_DF$mm_user), size = 5, shape = 21) +
  theme_bw(30) +
  scale_fill_brewer("User", type = "qual", palette = 3) +
  ggtitle("Impact of Followers") +
  xlab("# followers") + ylab("Score")

# Tabular Report of Twitter User Information of Glucose Monitor Makers

d.table_GM <- xtable(GM_DF[,c(1,4,8,9,10)])
grid.table(d.table_GM)

# Tabular Report of Twitter User Information of Meter Makers

d.table_MM <- xtable(MM_DF[,c(1,4,8,9,10)])
grid.table(d.table_MM)

#Calculating Klout Score

MDT_Klout <- RKlout(klout_key,'MDT_Diabetes')
Omni_Klout <- RKlout(klout_key,'myomnipod')
Tandem_Klout <- RKlout(klout_key,'TandemDiabetes')
Onetouch_Klout <- RKlout(klout_key,'OneTouch')
Accuchek_Klout <- RKlout(klout_key,'accuchek_us')
Abbott_Klout <- RKlout(klout_key,'FreeStyleDiabet')

#Klout Score Graph
plot_ly(
  x = c("MDT", "Omni", "Tandem"),
  y = c(MDT_Klout,Omni_Klout,Tandem_Klout),
  name = "GM Klout Score",
  type = "bar"
)

plot_ly(
  x = c("OneToch", "Accu-chek", "Abbott"),
  y = c(Onetouch_Klout,Accuchek_Klout,Abbott_Klout),
  name = "MM Klout Score",
  type = "bar"
)

#Plot determining the performance in November

# Comparing Followers and (Score= (2*ReTweets+Favorites) of a Glucose Monitor Maker twitter users

plot_ly(data = GM_DF, x = GM_DF$gm_followerscount, y = GM_DF$gm_NovAvgScore, color = GM_DF$gm_user) %>%
layout(title = "November Followers Impact for Glucose Monitor Makers",
         xaxis = list(title = "Followers"), 
         yaxis = list(title = "Average Score"))

# Comparing Followers and (Score= (2*ReTweets+Favorites) of a Meter Maker twitter users
plot_ly(data = MM_DF, x = MM_DF$mm_followerscount, y = MM_DF$mm_NovAvgScore, color = MM_DF$mm_user) %>%
  layout(title = "November Followers Impact for Meter Makers",
         xaxis = list(title = "Followers"), 
         yaxis = list(title = "Average Score"))


