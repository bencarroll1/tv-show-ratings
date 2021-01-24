#IMDb

#Basics CSV:
#This file contains: tconst(id for each title), titleType, primaryTitle, isAdult, startYear, endYear, runtimeMinutes, genres
#This file was originally called: imdb-basics.tsv

#Ratings and Votes CSV:
#This file contains: tconst, averageRating, numVotes
#This file was originally called: imdb-ratings.tsv


#Setting local working directory of project

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

getwd()

#Installing all packages needed for the project
#tutorial used for packages installation check: 
#https://stackoverflow.com/questions/9341635/check-for-installed-packages-before-running-install-packages
packages <- c("dplyr", "httr", "jsonlite", "rlist", "data.table", "rtidy", "ggthemes",
              "ggplot2", "reshape", "tm", "wordcloud")
if (length(setdiff(packages, rownames(installed.packages()))) > 0) {
  install.packages(setdiff(packages, rownames(installed.packages())))  
}


#Setting max print/loading of the csv files to above the limit for each of them
#so as not to lose any records for the merge file.
options(max.print = 100600000)

#Loading in Basics and Ratings IMDB files.
imdbBasics <-
  read.csv("imdb-basics.csv", na.strings = c("\\N", "NA"), head = T)
imdbRatings <-
  read.csv("imdb-ratings.csv", head = T)



#Objectives: Purpose is to open the IMDB csv files,
#merging them all into one dataset
#to then be cleaned.


#Merging both of the IMDB CSV files into one for ease of use
#and to be cleaned for use in the project.
mergedImdbData <-
  merge(
    imdbBasics,
    imdbRatings,
    by = c('ï..tconst', 'ï..tconst'),
    all.x = T
  )

#Objectives: To clean the IMDB dataset
#including removing rows with NA values and those that are not related to tv shows

#remove unneeded columns on basics file.
#Adding dplyr library for this.
library(dplyr)

#Removing columns from the basics file that is not needed for the project.
imdbMergedData = select(mergedImdbData,-4,-5)

#Removing rows that do not contain 'tvSeries' in the titleType column
#as this data is not needed
imdbMergedDataTVSeries <-
  imdbMergedData[grep("tvSeries", imdbMergedData$titleType),]

#Data for the avgRating is missing for a lot of tvSeries
#To deal with this, I got the mean averageRating and
#injected the value into averageRating where the value was NA
avgRatingMean <-
  round(
    mean(
      imdbMergedDataTVSeries$averageRating,
      na.rm = TRUE
    ),
    digits = 1
  )
injectAvgRatingMean <-
  imdbMergedDataTVSeries$averageRating[is.na(imdbMergedDataTVSeries$averageRating)] <-
  avgRatingMean


#Data for the numVotes is missing for a lot of tvSeries
#To deal with this, I got the mean numVotes and
#injected the value into numVotes where the value was NA
numVotesMean <-
  round(mean(
    imdbMergedDataTVSeries$numVotes,
    na.rm = TRUE
  ))
injectNumVotesMean <-
  imdbMergedDataTVSeries$numVotes[is.na(imdbMergedDataTVSeries$numVotes)] <-
  numVotesMean

#Omitting rows for the final IMDB dataset where entries contained NA values
imdbMergedDataTVSeries <- na.omit(imdbMergedDataTVSeries)

#Saving IMDB dataset to a CSV file
write.csv(imdbMergedDataTVSeries,
          "imdb-tv-show-dataset.csv",
          row.names = FALSE)


#Objectives: To get TV show data from The Movie Database API.
#The data i got was from the tv/popular endpoint which 
#a list, which is updated daily, of the current most popular TV shows from The Movie Database

#Tutorials used: 
#https://developers.themoviedb.org/3/tv/get-popular-tv-shows
#https://rpubs.com/ankc/480665
#https://stackoverflow.com/questions/48521027/remove-all-rows-which-do-not-contain-a-specific-string-in-r


#Below are the libraries used
library(httr) 
library(jsonlite) 
library(rlist) 
library(data.table)

#loading in the CSV file that will be used to store the data
tmdbCSV <- read.csv("tmdb.csv", head = T)

#For loop to get each page of data from tv/popular
#This endpoint is paginated which means i had to gather the data from each individual page using the loop
for (i in 1:500) {
  
  #Using httr to make a get to the tv/popular endpoint 
  #and passed the query params containing i from the for loop
  jsonResponse <-GET("https://api.themoviedb.org/3/tv/popular?api_key=112bf231f7a9171354436a84763a4393", query = list(language = "en-US", page = i))
  jsonResponseParsed <- content(jsonResponse, as = "parsed")
  
  #taking the json data from the endpoint and parsing it and formatting it as a data table
  jsonResponseParsedDataTable <-
    lapply(jsonResponseParsed[["results"]], as.data.table)
  
  #Adding the data table to a bound list and then bound list for ease of formatting
  boundList <- rbindlist(jsonResponseParsedDataTable, fill = TRUE)
  TMDBData <-
    boundList %>% bind_rows %>% select(name, popularity, vote_count, vote_average, genre_ids)
  
  #Removing NULL values across all columns
  tmdbRemoveNullNames <-
    TMDBData[!grepl("NULL", TMDBData$name),]
  
  tmdbRemoveNullPopularity <-
    tmdbRemoveNullNames[!grepl("NULL", tmdbRemoveNullNames$popularity),]
  
  tmdbRemoveNullVoteCount <-
    tmdbRemoveNullPopularity[!grepl("NULL", tmdbRemoveNullPopularity$vote_count),]
  
  tmdbRemoveNullVoteAvg <-
    tmdbRemoveNullVoteCount[!grepl("NULL", tmdbRemoveNullVoteCount$vote_average),]
  
  tmdbRemoveNullGenreIds <-
    tmdbRemoveNullVoteAvg[!grepl("NULL", tmdbRemoveNullVoteAvg$genre_ids),]
  
  #Writing/appending data free of NULL values to the CSV file
  fwrite(tmdbRemoveNullGenreIds, file = "tmdb.csv", append = TRUE)
}


#Objectives:To clean the TMDB dataset
#including removing rows with NA values and those that are not related to the project

#Loading in uncleaned TMDB dataset CSV
tmdbUncleanedData <-
  read.csv("tmdb.csv", head = T)

#Removing rows with duplicate names
tmdbRemoveDuplicateNameEntries <- tmdbUncleanedData[!duplicated(tmdbUncleanedData[ , c("ï..name")]),]

#Checking how many unique genres there are in the unclean data
UniqueGenreIds<- tmdbUncleanedData[!duplicated(tmdbUncleanedData[ , c("genre_ids")]),]

#Replace genre id with genre name
#genre id to name list is on the tmdb API
tmdbReplaceGenreIDWithGenreName <- as.character(tmdbRemoveDuplicateNameEntries$genre_ids)

tmdbRemoveDuplicateNameEntries$genre_ids[which(tmdbRemoveDuplicateNameEntries$genre_ids=="12")] <- "Adventure"
tmdbRemoveDuplicateNameEntries$genre_ids[which(tmdbRemoveDuplicateNameEntries$genre_ids=="14")] <- "Fantasy"
tmdbRemoveDuplicateNameEntries$genre_ids[which(tmdbRemoveDuplicateNameEntries$genre_ids=="16")] <- "Animation"
tmdbRemoveDuplicateNameEntries$genre_ids[which(tmdbRemoveDuplicateNameEntries$genre_ids=="18")] <- "Drama"
tmdbRemoveDuplicateNameEntries$genre_ids[which(tmdbRemoveDuplicateNameEntries$genre_ids=="22")] <- "50"
tmdbRemoveDuplicateNameEntries$genre_ids[which(tmdbRemoveDuplicateNameEntries$genre_ids=="27")] <- "Horror"
tmdbRemoveDuplicateNameEntries$genre_ids[which(tmdbRemoveDuplicateNameEntries$genre_ids=="28")] <- "Action"
tmdbRemoveDuplicateNameEntries$genre_ids[which(tmdbRemoveDuplicateNameEntries$genre_ids=="35")] <- "Comedy"
tmdbRemoveDuplicateNameEntries$genre_ids[which(tmdbRemoveDuplicateNameEntries$genre_ids=="36")] <- "History"
tmdbRemoveDuplicateNameEntries$genre_ids[which(tmdbRemoveDuplicateNameEntries$genre_ids=="37")] <- "Western"
tmdbRemoveDuplicateNameEntries$genre_ids[which(tmdbRemoveDuplicateNameEntries$genre_ids=="53")] <- "Thriller"
tmdbRemoveDuplicateNameEntries$genre_ids[which(tmdbRemoveDuplicateNameEntries$genre_ids=="80")] <- "Crime"
tmdbRemoveDuplicateNameEntries$genre_ids[which(tmdbRemoveDuplicateNameEntries$genre_ids=="99")] <- "Documentary"
tmdbRemoveDuplicateNameEntries$genre_ids[which(tmdbRemoveDuplicateNameEntries$genre_ids=="878")] <- "Science Fiction"
tmdbRemoveDuplicateNameEntries$genre_ids[which(tmdbRemoveDuplicateNameEntries$genre_ids=="9648")] <- "Mystery"
tmdbRemoveDuplicateNameEntries$genre_ids[which(tmdbRemoveDuplicateNameEntries$genre_ids=="10402")] <- "Music"
tmdbRemoveDuplicateNameEntries$genre_ids[which(tmdbRemoveDuplicateNameEntries$genre_ids=="10749")] <- "Romance"
tmdbRemoveDuplicateNameEntries$genre_ids[which(tmdbRemoveDuplicateNameEntries$genre_ids=="10751")] <- "Family"
tmdbRemoveDuplicateNameEntries$genre_ids[which(tmdbRemoveDuplicateNameEntries$genre_ids=="10752")] <- "War"
tmdbRemoveDuplicateNameEntries$genre_ids[which(tmdbRemoveDuplicateNameEntries$genre_ids=="10759")] <- "Action & Adventure"
tmdbRemoveDuplicateNameEntries$genre_ids[which(tmdbRemoveDuplicateNameEntries$genre_ids=="10762")] <- "Kids"
tmdbRemoveDuplicateNameEntries$genre_ids[which(tmdbRemoveDuplicateNameEntries$genre_ids=="10763")] <- "News"
tmdbRemoveDuplicateNameEntries$genre_ids[which(tmdbRemoveDuplicateNameEntries$genre_ids=="10764")] <- "Reality"
tmdbRemoveDuplicateNameEntries$genre_ids[which(tmdbRemoveDuplicateNameEntries$genre_ids=="10765")] <- "Sci-Fi & Fantasy"
tmdbRemoveDuplicateNameEntries$genre_ids[which(tmdbRemoveDuplicateNameEntries$genre_ids=="10766")] <- "Soap"
tmdbRemoveDuplicateNameEntries$genre_ids[which(tmdbRemoveDuplicateNameEntries$genre_ids=="10767")] <- "Talk"
tmdbRemoveDuplicateNameEntries$genre_ids[which(tmdbRemoveDuplicateNameEntries$genre_ids=="10768")] <- "War & Politics"

tmdbWithGenreName <- as.character(tmdbRemoveDuplicateNameEntries$genre_ids)

#Checking there are no more genre_ids and only genres
UniqueGenres <- tmdbRemoveDuplicateNameEntries[!duplicated(tmdbRemoveDuplicateNameEntries[ , c("genre_ids")]),]

#Changing other column names for standardisation
colnames(tmdbRemoveDuplicateNameEntries)[which(names(tmdbRemoveDuplicateNameEntries) == "vote_average")] <- "tmdbAverageRating"
colnames(tmdbRemoveDuplicateNameEntries)[which(names(tmdbRemoveDuplicateNameEntries) == "vote_count")] <- "tmdbNoOfVotes"
colnames(tmdbRemoveDuplicateNameEntries)[which(names(tmdbRemoveDuplicateNameEntries) == "genres")] <- "tmdbGenres"

#How tmdb defines/places a numerical value on popularity:
#TV Shows
#Number of votes for the day
#Number of views for the day
#Number of users who marked it as a "favourite" for the day
#Number of users who added it to their "watchlist" for the day
#Next/last episode to air date
#Number of total votes
#Previous days score

#From reading this, I believe that there will be no harmful impact 
#on the dataset if i round each popularity figure
#I believe this may aid the exploratory analysis

tmdbRemoveDuplicateNameEntries$popularity <- round(tmdbRemoveDuplicateNameEntries$popularity, 0)

#Saving final TMDB dataset to a CSV file
tmdbDataset <- tmdbRemoveDuplicateNameEntries[!apply(is.na(tmdbRemoveDuplicateNameEntries) | tmdbRemoveDuplicateNameEntries == "", 1, all),]
write.csv(tmdbDataset,
          "tmdb-tv-show-dataset.csv",
          row.names = FALSE)

#Objectives: To merge the IMDB and TMDB datasets together by the name column
#and perform any cleaning necessary

#Loading in IMDB dataset
imdbDataset <-
  read.csv("imdb-tv-show-dataset.csv", head = T)
imdbDatasetDF <- data.frame(imdbDataset)

#Loading in TMDB dataset
tmdbDataset <-
  read.csv("tmdb-tv-show-dataset.csv", head = T)
tmdbDatasetDF <- data.frame(tmdbDataset)

#Changing column names for ease of use and to specify what dataset they came from
colnames(tmdbDatasetDF)[which(names(tmdbDatasetDF) == "ï..name")] <- "name"
colnames(imdbDatasetDF)[which(names(imdbDatasetDF) == "primaryTitle")] <- "name"

#Merging IMDB and TMDB datasets
mergeImdbAndTmdbDatasets <-
  merge(
    imdbDatasetDF,
    tmdbDatasetDF,
    by = c('name', 'name'),
    all.x = T
  )

#Changing column names to reflect they are from the imdb data
colnames(mergeImdbAndTmdbDatasets)[which(names(mergeImdbAndTmdbDatasets) == "genres")] <- "imdbGenres"
colnames(mergeImdbAndTmdbDatasets)[which(names(mergeImdbAndTmdbDatasets) == "averageRating")] <- "imdbAverageRating"
colnames(mergeImdbAndTmdbDatasets)[which(names(mergeImdbAndTmdbDatasets) == "numVotes")] <- "imdbNoOfVotes"

#Finding all rows that have NA values to decide best way to deal with them
findNAValues <- mergeImdbAndTmdbDatasets[rowSums(is.na(mergeImdbAndTmdbDatasets)) > 0,]

#Data for the tmdbNoOfVotes is missing for a lot of rows
#To deal with this, I got the mean tmdbNoOfVotes and
#injected the value into tmdbNoOfVotes where the value was NA
tmdbNoOfVotes <-
  round(mean(
    mergeImdbAndTmdbDatasets$tmdbNoOfVotes,
    na.rm = TRUE
  ))
injecttmdbNoOfVotes <-
  mergeImdbAndTmdbDatasets$tmdbNoOfVotes[is.na(mergeImdbAndTmdbDatasets$tmdbNoOfVotes)] <-
  tmdbNoOfVotes

#Doing the same for tmdbAverageRating
tmdbAverageRating <-
  round(mean(
    mergeImdbAndTmdbDatasets$tmdbAverageRating,
    na.rm = TRUE
  ))
injecttmdbAverageRating <-
  mergeImdbAndTmdbDatasets$tmdbAverageRating[is.na(mergeImdbAndTmdbDatasets$tmdbAverageRating)] <-
  tmdbAverageRating

#Doing the same for the popularity
showPopularity <-
  round(mean(
    mergeImdbAndTmdbDatasets$popularity,
    na.rm = TRUE
  ))
injectShowPopularity <-
  mergeImdbAndTmdbDatasets$popularity[is.na(mergeImdbAndTmdbDatasets$popularity)] <-
  showPopularity

#Merging imdbGenres and tmdbGenres into one column
library(tidyr)
imdbAndTmdbGenresOld <- data.frame(imdbGenres=mergeImdbAndTmdbDatasets$imdbGenres, tmdbGenres=mergeImdbAndTmdbDatasets$genre_ids)
imdbAndTmdbGenresOld$genres <- paste(imdbAndTmdbGenresOld$imdbGenres, imdbAndTmdbGenresOld$genre_ids, sep=",")

#Removing NA values that got added to genres column rows
imdbAndTmdbGenresOld <- gsub("NA", "", imdbAndTmdbGenresOld$genres)

#Removing duplicate genre name entries on same row e.g. "Comedy, Action, Comedy" -> "Comedy, Action"
imdbAndTmdbGenres <- data.frame(sapply(imdbAndTmdbGenresOld, function(x) paste(unique(unlist(strsplit(x, ","))), collapse = ", ")))

#Renaming column to genres
colnames(imdbAndTmdbGenres)[1]<-"genres"

#Adding imdbAndTmdbGenres$genres to mergeImdbAndTmdbDatasets under new genres column
mergeImdbAndTmdbDatasets['genres']= imdbAndTmdbGenres['genres']

#Removing mergeImdbAndTmdbDatasets$imdbGenres and mergeImdbAndTmdbDatasets$tmdbGenres
mergeImdbAndTmdbDatasets <- subset(mergeImdbAndTmdbDatasets, select = -c(imdbGenres, genre_ids))

#Saving final dataset to a CSV file
write.csv(mergeImdbAndTmdbDatasets,
          "imdb-tmdb-tv-show-dataset.csv",
          row.names = FALSE)



#libraries needed for data visualistion
library(ggplot2)
library(ggthemes)
library(reshape)

#selecting mergeImdbAndTmdbDatasets data that will be needed for objective one:
#Find any correlation between each dataset's ratings of television shows
objOneData <- data.frame(mergeImdbAndTmdbDatasets[, c("imdbAverageRating", "tmdbAverageRating")])

#Creating bar plot of IMDB Ratings
win.metafile("Graphs/IMDbRatingVsCountPlot.wmf")
barplot(table(objOneData$imdbAverageRating), main="IMDb Ratings", xlab = "IMDb Rating", ylab="Count")
dev.off()
#Creating bar plot of TMDB Ratings
win.metafile("Graphs/TMDbRatingVsCountPlot.wmf")
barplot(table(objOneData$tmdbAverageRating), main="TMDb Ratings", xlab = "TMDb Rating", ylab="Count")
dev.off()

#Creating scatter plot for imdb vs tmdb ratings
win.metafile("Graphs/TMDBVsIMDBRatingsScatterPlot.wmf")
ggplot(objOneData, aes(x = imdbAverageRating, y = tmdbAverageRating)) + geom_point() +
  ggtitle("IMDb vs. TMDb Ratings") + ylab("TMDb Rating") + xlab("IMDb Rating")
dev.off()

#selecting mergeImdbAndTmdbDatasets data that will be needed for objective two:
#The correlation between the popularity of television shows and ratings
objTwoPopData <- data.frame(mergeImdbAndTmdbDatasets[, c("imdbAverageRating", "tmdbAverageRating", "popularity")])

#Plotting imdb and tmdb ratings against popularity on a scatter plot
Molten <- melt(objTwoPopData, id.vars = "popularity")

#Plotting imdb ratings against popularity
win.metafile("Graphs/IMDBVsPopularity.wmf")
plot(main="IMDb Ratings vs. Popularity",
     objTwoPopData$imdbAverageRating, objTwoPopData$popularity,
     xlab="Ratings", ylab="Popularity")
dev.off()

#Plotting tmdb ratings against popularity
win.metafile("Graphs/TMDBVsPopularity.wmf")
plot(main="TMDb Ratings vs. Popularity",
     objTwoPopData$tmdbAverageRating, objTwoPopData$popularity,
     xlab="Ratings", ylab="Popularity")
dev.off()

win.metafile("Graphs/PopularityVsImdbAndTmdbRatings.wmf")
ggplot(Molten, aes(x = value, y = popularity, colour = variable)) + geom_point() +
  ggtitle("IMDb and TMDb Ratings vs Popularity") + ylab("Popularity") + xlab("Rating")
dev.off()

#selecting mergeImdbAndTmdbDatasets data that will be needed for objective three:
#Correlation between genre and ratings 
#to see if certain genres, on average, get higher ratings than others.

#Making a corpus of the genre data
library(tm)
mergeImdbAndTmdbDatasetsGenres <- Corpus(VectorSource(mergeImdbAndTmdbDatasets$genres))

# Convert the text to lower case - to eliminate differentiation between sane word with upper /lower case 
#starting letter
mergeImdbAndTmdbDatasetsGenres <- tm_map(mergeImdbAndTmdbDatasetsGenres, content_transformer(tolower))
# Get rid of punctuation from corpus
mergeImdbAndTmdbDatasetsGenres <- tm_map(mergeImdbAndTmdbDatasetsGenres, removePunctuation)
# Get rid of extra white space
mergeImdbAndTmdbDatasetsGenres <- tm_map(mergeImdbAndTmdbDatasetsGenres, stripWhitespace)

#making a term document matrix to store frequency of each genre type
dtmGenres <- TermDocumentMatrix(mergeImdbAndTmdbDatasetsGenres)
dtmGenres
matrixGenres <- as.matrix(dtmGenres)
v <- sort(rowSums(matrixGenres),decreasing=TRUE)
dataFrameGenres <- data.frame(word = names(v),freq=v)
head(dataFrameGenres, 26)

#Making a wordcloud of the most common genre names
#with most common words as largest words and least common words as smallest
library(wordcloud)
win.metafile("Graphs/MostFreqWordsWordcloud.wmf")
wordcloud(words = dataFrameGenres$word, freq = dataFrameGenres$freq, min.freq = 1,
          max.words=150, random.order=FALSE, rot.per=0.35,
          colors=brewer.pal(8, "Dark2"))
dev.off()

#getting mean average rating for each row containg most common genres
#this is done for the top 4 most common genres as per dtmGenres
#comedy, drama, family, animation, adventure
comedyGenre <- mergeImdbAndTmdbDatasets[mergeImdbAndTmdbDatasets$genres %in% c("Comedy"), ]
comedyGenreData <- data.frame(comedyGenre[, c("imdbAverageRating", "tmdbAverageRating", "genres")])
comedyGenreIMDBAvgRatingMean <-
  round(
    mean(
      comedyGenreData$imdbAverageRating,
      na.rm = TRUE
    ),
    digits = 1
  )
comedyGenreTMDBAvgRatingMean <-
  round(
    mean(
      comedyGenreData$tmdbAverageRating,
      na.rm=TRUE
    ),
    digits = 1
  )

comedy <- (comedyGenreIMDBAvgRatingMean + comedyGenreTMDBAvgRatingMean) / 2


dramaGenre <- mergeImdbAndTmdbDatasets[mergeImdbAndTmdbDatasets$genres %in% c("Drama"), ]
dramaGenreData <- data.frame(dramaGenre[, c("imdbAverageRating", "tmdbAverageRating", "genres")])
dramaGenreIMDBAvgRatingMean <-
  round(
    mean(
      dramaGenreData$imdbAverageRating,
      na.rm = TRUE
    ),
    digits = 1
  )
dramaGenreTMDBAvgRatingMean <-
  round(
    mean(
      dramaGenreData$tmdbAverageRating,
      na.rm=TRUE
    ),
    digits = 1
  )

drama <- (dramaGenreIMDBAvgRatingMean + dramaGenreTMDBAvgRatingMean) / 2


familyGenre <- mergeImdbAndTmdbDatasets[mergeImdbAndTmdbDatasets$genres %in% c("Family"), ]
familyGenreData <- data.frame(familyGenre[, c("imdbAverageRating", "tmdbAverageRating", "genres")])
familyGenreIMDBAvgRatingMean <-
  round(
    mean(
      familyGenreData$imdbAverageRating,
      na.rm = TRUE
    ),
    digits = 1
  )
familyGenreTMDBAvgRatingMean <-
  round(
    mean(
      familyGenreData$tmdbAverageRating,
      na.rm=TRUE
    ),
    digits = 1
  )

family <- (familyGenreIMDBAvgRatingMean + familyGenreTMDBAvgRatingMean) / 2


animationGenre <- mergeImdbAndTmdbDatasets[mergeImdbAndTmdbDatasets$genres %in% c("Animation"), ]
animationGenreData <- data.frame(animationGenre[, c("imdbAverageRating", "tmdbAverageRating", "genres")])
animationGenreIMDBAvgRatingMean <-
  round(
    mean(
      animationGenreData$imdbAverageRating,
      na.rm = TRUE
    ),
    digits = 1
  )
animationGenreTMDBAvgRatingMean <-
  round(
    mean(
      animationGenreData$tmdbAverageRating,
      na.rm=TRUE
    ),
    digits = 1
  )

animation <- (animationGenreIMDBAvgRatingMean + animationGenreTMDBAvgRatingMean) / 2

#getting average rating for each row containg least common genres
#this is done for the top 4 least common genres as per dtmGenres
#short, biography, war, horror, thriller
shortGenre <- mergeImdbAndTmdbDatasets[mergeImdbAndTmdbDatasets$genres %in% c("Short"), ]
shortGenreData <- data.frame(shortGenre[, c("imdbAverageRating", "tmdbAverageRating", "genres")])
shortGenreIMDBAvgRatingMean <-
  round(
    mean(
      shortGenreData$imdbAverageRating,
      na.rm = TRUE
    ),
    digits = 1
  )
shortGenreTMDBAvgRatingMean <-
  round(
    mean(
      shortGenreData$tmdbAverageRating,
      na.rm=TRUE
    ),
    digits = 1
  )

short <- (shortGenreIMDBAvgRatingMean + shortGenreTMDBAvgRatingMean) / 2


biographyGenre <- mergeImdbAndTmdbDatasets[mergeImdbAndTmdbDatasets$genres %in% c("Biography"), ]
biographyGenreData <- data.frame(biographyGenre[, c("imdbAverageRating", "tmdbAverageRating", "genres")])
biographyGenreIMDBAvgRatingMean <-
  round(
    mean(
      biographyGenreData$imdbAverageRating,
      na.rm = TRUE
    ),
    digits = 1
  )
biographyGenreTMDBAvgRatingMean <-
  round(
    mean(
      biographyGenreData$tmdbAverageRating,
      na.rm=TRUE
    ),
    digits = 1
  )

biography <- (biographyGenreIMDBAvgRatingMean + biographyGenreTMDBAvgRatingMean) / 2


warGenre <- mergeImdbAndTmdbDatasets[mergeImdbAndTmdbDatasets$genres %in% c("War"), ]
warGenreData <- data.frame(warGenre[, c("imdbAverageRating", "tmdbAverageRating", "genres")])
warGenreIMDBAvgRatingMean <-
  round(
    mean(
      warGenreData$imdbAverageRating,
      na.rm = TRUE
    ),
    digits = 1
  )
warGenreTMDBAvgRatingMean <-
  round(
    mean(
      warGenreData$tmdbAverageRating,
      na.rm=TRUE
    ),
    digits = 1
  )

war <- (warGenreIMDBAvgRatingMean + warGenreTMDBAvgRatingMean) / 2


horrorGenre <- mergeImdbAndTmdbDatasets[mergeImdbAndTmdbDatasets$genres %in% c("Horror"), ]
horrorGenreData <- data.frame(horrorGenre[, c("imdbAverageRating", "tmdbAverageRating", "genres")])
horrorGenreIMDBAvgRatingMean <-
  round(
    mean(
      horrorGenreData$imdbAverageRating,
      na.rm = TRUE
    ),
    digits = 1
  )
horrorGenreTMDBAvgRatingMean <-
  round(
    mean(
      horrorGenreData$tmdbAverageRating,
      na.rm=TRUE
    ),
    digits = 1
  )

horror <- (horrorGenreIMDBAvgRatingMean + horrorGenreTMDBAvgRatingMean) / 2

#adding average rating for each genre as gotten by imdbAverageRating +tmdbAverageRating /2 for each, to a dataframe
datasetGenres <- data.frame(comedy, drama, family, animation, short, biography, war, horror)

#adjusting format of data frame
datasetGenres <- data.frame(r1=names(datasetGenres), t(datasetGenres))
colnames(datasetGenres)[1]<-"genre"
colnames(datasetGenres)[2]<-"avgRating"

#plotting genre vs rating data to show correlation between genre and rating 
#for top 4 most common and top 4 least common genres
win.metafile("Graphs/GenreVsAverageRating.wmf")
p <- ggplot(data.frame(datasetGenres$genre, datasetGenres$avgRating), aes(datasetGenres$genre, datasetGenres$avgRating, fill = datasetGenres$genre)) + geom_col()
p + ggtitle("Genre vs Average Genre Rating") +
  xlab("Genre") + ylab("Average Rating") + labs(fill = "Genre")
dev.off()

