#GOOGLE_MAPS_KEY = #

install.packages(c("rtweet","dplyr","jsonlite","syuzhet","rcurl","httr","sentimentr","ggmap","ggplot2","colorramps"))

library(rtweet)
library(dplyr)
library(jsonlite)
library(syuzhet)
library(RCurl)
library(httr)
library(sentimentr)
library(ggmap)
library(ggplot2)
library(colorRamps)
library(rworldmap)

#rm(archie_royal_us,citizens_united_us,dianne_feinstein_us,kraft_heinz_us,met_gala_us,nipsey_us,precious_harris_us,school_shooting_us,sri_lanka_us,unemployment_rate_us)
#rm(clean_loc,i,lat,lng,loc,map,str,temp2)

################
# Gather tweets
################


#get tweets
tweets_all <- search_tweets(type = 'recent',
                            q = '(archie royal) OR (royal baby) OR (archie baby)',
                            include_rts = FALSE,
                            n = 18000,
                            langs = 'EN',
                            geocode = lookup_coords('country:us',key = GOOGLE_MAPS_KEY),
                            since = '2019-05-08',
                            until = '2019-05-09')



#get columns we're interested in
tweets_wanted <- dianne_feinstein_us %>%
  select(user_id,status_id,created_at,lang,account_lang, account_created_at, followers_count,
         location, country,country_code,geo_coords,coords_coords,is_retweet, bbox_coords,text)

#add new columns
tweets_wanted$c_lat = 0
tweets_wanted$c_long = 0
tweets_wanted$new_loc = ""


#gets coords from loc from each tweet
for (i in 1:nrow(tweets_wanted)) {
  get_loc <- tweets_wanted$location[i]
  clean_loc <- rm_accent(get_loc)
  clean_loc <- gsub("[^A-z,-]", "", clean_loc)
  clean_loc <- gsub("[,-]", " ", clean_loc)

  str <- funct(clean_loc)

  if(length(str) == 0){
    str <- funct("singapore")
  }

  loc <- str[1]
  lat <- str[2]
  lng <- str[3]

  tweets_wanted$new_loc[i]      <- loc
  tweets_wanted$c_lat[i]         <- as.numeric(lat)
  tweets_wanted$c_long[i]        <- as.numeric(lng)
  tweets_wanted$coords_coords[i] <- paste(lat,",",lng)
}

################
# Sentiment
################

#clean
tweets_wanted$text = gsub("&amp", "", tweets_wanted$text)
tweets_wanted$text = gsub("&amp", "", tweets_wanted$text)
tweets_wanted$text = gsub("(RT|via)((?:\\b\\W*@\\w+)+)", "", tweets_wanted$text)
tweets_wanted$text = gsub("@\\w+", "", tweets_wanted$text)
tweets_wanted$text = gsub("[[:punct:]]", "", tweets_wanted$text)
tweets_wanted$text = gsub("[[:digit:]]", "", tweets_wanted$text)
tweets_wanted$text = gsub("http\\w+", "", tweets_wanted$text)
tweets_wanted$text = gsub("[ \t]{2,}", "", tweets_wanted$text)
tweets_wanted$text = gsub("^\\s+|\\s+$", "", tweets_wanted$text)
tweets_wanted$text <- iconv(tweets_wanted$text, "UTF-8", "ASCII", sub="")

#new column
tweets_wanted$sentiment = 0

for (i in 1:nrow(tweets_wanted)) {
  tweets_wanted$sentiment[i] = sentiment(get_sentences(tweets_wanted$text[i]))$sentiment
}


################
# Lat Long corr
# Final matrix
################




#dianne_feinstein_us <- dianne_feinstein_us[!(substr(dianne_feinstein_us$created_at,1,10) == '2019-02-22'),]
#kraft_heinz_us <- kraft_heinz_us[!(substr(kraft_heinz_us$created_at,1,10) == '2019-02-23'),]
#precious_harris_us <- precious_harris_us[!(substr(precious_harris_us$created_at,1,10) == '2019-02-23'),]


tweets_wanted <- dianne_feinstein_us

#remove non-existing lat long (from errors before) and that whole row
tweets_wanted <- tweets_wanted[!(as.numeric(tweets_wanted$c_lat) < -125),]
tweets_wanted <- tweets_wanted[!(as.numeric(tweets_wanted$c_lat) > -70),]

tweets_wanted <- tweets_wanted[!(abs(as.numeric(tweets_wanted$c_long)) > 50),]
tweets_wanted <- tweets_wanted[!(abs(as.numeric(tweets_wanted$c_long)) < 25),]

tweets_wanted <- tweets_wanted[rowSums(is.na(tweets_wanted)) != ncol(tweets_wanted),]



# final matrix form
tweets_wanted <- tweets_wanted %>%
  select(status_id,created_at,c_lat,c_long,sentiment,text)

################
# Mapping
################

register_google(GOOGLE_MAPS_KEY)


cnt <- 0
for (i in 1:nrow(tweets_wanted)) {
  #tweets_wanted$created_at[i] <- substr(tweets_wanted$created_at[i],12,19)
  if (substr(tweets_wanted$created_at[1],11,19) >= "23:30:01") {
    cnt <- cnt +1
  }
}



tmp <- sqldf("
      select distinct a.c_lat,a.c_long,min(b.created_at) as tm,b.sentiment from unique_pos as a
      join tweets_wanted as b on a.c_lat = b.c_lat and a.c_long = b.c_long
      group by a.c_lat,a.c_long
      ")




us <- map_data("usa")
states <- map_data("state")

gg1 <- ggplot() + geom_polygon(data = us, aes(x=long, y = lat, group = group)) +
  geom_polygon(data = states, aes(x = long, y = lat, color = 'white', group = group), color = "white") +
  coord_fixed(1.3)

cc <- scales::seq_gradient_pal("green", "red", "Lab")(seq(0,1,length.out=nrow(tmp)))


gg1 + geom_point(data = tmp,aes(x = c_lat,y = c_long,color = tmp$tm),show.legend = FALSE) +
  scale_color_manual(values = cc)


unique_pos <- unique(tweets_wanted[,c('c_lat','c_long')])






####### GOOGLE MAP HÄR ########
map <- get_map(location="united states", zoom=4, maptype = "toner", source = "google",scale = 2)
ggmap(map)

tweets_wanted2 <- tweets_wanted
tweets_wanted2$created_at <- cut(tweets_wanted2$created_at,breaks = '00:30:00')

ggmap(map) +
  geom_point(data = tweets_wanted2,show.legend = FALSE,
             aes(x = c_lat,y = c_long, color = tweets_wanted2$created_at,size = 1.5)) +
  scale_color_discrete()


scale_fill_gradient(low='green',high = 'red')
####### GOOGLE MAP HÄR ########








saveRDS(tweets_wanted,file =
          "C:\\Users//Diar//Desktop//Ny mapp//KEX19//KEX19//News//us//P2//Map//Sentiment//Fin/citizens_united_us.rds")


