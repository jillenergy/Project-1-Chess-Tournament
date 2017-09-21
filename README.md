## Assignment: Read in the raw data of a chess tournament.
## Create a table comprised of the following attributes: Player’s Name, Player’s State, Total Points, Player’s Pre-Rating, and Average Pre-Rating Rating of Opponents 
## For the first player, the information would be: Gary Hua, ON, 6.0, 1794, 1605

## Load library to import data
library(stringr)
library(DT)
library(ggplot2)

## Load raw data file into R Studio
tournamentraw <- readLines("~/Desktop/tournamentinfo.txt")

## Evaluate and describe the raw data format to determine what steps will be required to work with the desired attributes
head(tournamentraw)
tail(tournamentraw)
NROW(tournamentraw)
NCOL(tournamentraw)

## Header: described in two rows
## Columns: information is separated by | but the table is only one row
## Rows: separated between entries with a series of hyphens
## Each entry: described in two rows; 
## the first row includes the player's Name, Total Points, Results of each Round; 
## the second row includes the player's USCF ID, rating before and after the tournament
## There are 64 players

## Tidy and process the data in R to make it more usable. 
## Primary objective, bring all of the data for each player into one row and extract the requested attributes. 

## Determine the the total rows in the dataset
tournamentrows <- length(tournamentraw)

## Create a new table with just the rows that start with the Player Names
## Start with the fifth row to account for the header and hyphens
PlayerNameRows <- tournamentraw[seq(5, tournamentrows, 3)]
head(PlayerNameRows)

## Create a new table with just the rows that start with the Player State
## Start with the sixth row
PlayerStateRows <- tournamentraw[seq(6, tournamentrows, 3)]
head(PlayerStateRows)

## Player Names
## From PlayerNameRows, extract the Player's Name
PlayerName <- str_trim(str_extract(PlayerNameRows, "(\\w+\\s){2,3}"))
head(PlayerName)

## Player States
## From PlayerStateRows, extract the Player State taking the first two letters in the string
PlayerState <- str_extract(PlayerStateRows, "\\w+")
head(PlayerState)

## Total Points
## From the PlayerNameRows, extract the Total Points
TotalPoints <- as.numeric(str_extract(PlayerNameRows, "\\d+\\.\\d+"))
head(TotalPoints)

## Player Pre-Rating
## From the PlayerStateRows, extract the Player's Pre-Rating by first taking the string and then pulling out the number to remove the spaces
PlayerPreRating <- str_extract(PlayerStateRows, "[^\\d]\\d{3,4}[^\\d]")
PlayerPreRating <- as.integer(str_extract(PlayerPreRating, "\\d+"))
head(PlayerPreRating)

## Calculate the average of the Opponents' Pre-Ratings for each player
## Find Opponents
## From the PlayerStateRows, determine who the opponents of each player were
FindOpponents <- str_extract_all(PlayerNameRows, "\\d+\\|")
FindOpponents <- str_extract_all(FindOpponents, "\\d+")
head(FindOpponents)

## Pair Numbers
## From the PlayerNameRows, extract the pair numbers for use in the opponent pre-rating calculation
Pair <- as.integer(str_extract(PlayerNameRows, "\\d+"))
head(Pair)

## Run a loop to calculate the mean rating of the opponents of each player using the Pair numbers for all rows
AveOpponentRating <- Pair
for (i in 1:NROW(Pair)) { 
  AveOpponentRating[i] <- mean(PlayerPreRating[as.numeric(unlist(FindOpponents[Pair[i]]))]) 
}
head(AveOpponentRating)

## Round up the calculated average ratings to the nearest whole integer
AveOpponentRating <- round(AveOpponentRating)
head(AveOpponentRating)

## Put together all of the extracted data into a final set
## Create a new dataframe with the extracted desired data attributes
FinalData <- data.frame(PlayerName, PlayerState, TotalPoints, PlayerPreRating, AveOpponentRating)
head(FinalData)

## Rename the columns to improve formating
colnames(FinalData) <- c("Player's Name", "Player's State", "Total Number of Points", "Player's Pre-Rating", "Average Rating of Opponents")
head(FinalData)

## Export final data set
write.csv(FinalData,file="Chess Data Extracted Jill Anderson.csv")

## Reformat data table for display
datatable(FinalData)

## Analyze the data

## Basic data analytics
## Statistical spread of Player's Pre-Ratings
summary(FinalData$`Player's Pre-Rating`)

## Statistical spread of Average Opponents' Pre-Ratings 
summary(FinalData$`Average Rating of Opponents`)

## Visualize the data
## Create a historgram of the Player Pre-Ratings
hist(FinalData$`Player's Pre-Rating`, breaks = 30, main = "Distribution of Player Ratings Pre-Tournament", xlab = "Player's Pre-Rating", ylab = "Count")

## Create a historgram of the Total Points per Player
hist(FinalData$`Total Number of Points`, breaks = 10, main = "Distribution of Total Points", xlab = "Total Points per Player", ylab = "Count")

## Compare players from each state in the following columns: "Player State", "Average Total Number of Points", "Average Player's Pre-Rating", "Average Rating of Opponents"
## PlayerState = MI
FinalDataMI = subset(FinalData, PlayerState == "MI")
FinalDataMI_Num = round(NROW(FinalDataMI))
FinalDataMI_MeanPoints = round(mean(FinalDataMI$`Total Number of Points`))
FinalDataMI_PlayerPreRating = round(mean(FinalDataMI$`Player's Pre-Rating`))
FinalDataMI_AveOpponent = round(mean(FinalDataMI$`Average Rating of Opponents`))
FinalDataMISum <- data.frame("MI", FinalDataMI_Num, FinalDataMI_MeanPoints, FinalDataMI_PlayerPreRating, FinalDataMI_AveOpponent)
colnames(FinalDataMISum) <- c("State", "Number of Players", "Average Total Number of Points", "Average Player Pre-Rating", "Average Rating of Opponents")
FinalDataMISum

## PlayerState = ON
FinalDataON = subset(FinalData, PlayerState == "ON")
FinalDataON_Num = round(NROW(FinalDataON))
FinalDataON_MeanPoints = round(mean(FinalDataON$`Total Number of Points`))
FinalDataON_PlayerPreRating = round(mean(FinalDataON$`Player's Pre-Rating`))
FinalDataON_AveOpponent = round(mean(FinalDataON$`Average Rating of Opponents`))
FinalDataONSum <- data.frame("ON", FinalDataON_Num, FinalDataON_MeanPoints, FinalDataON_PlayerPreRating, FinalDataON_AveOpponent)
colnames(FinalDataONSum) <- c("State", "Number of Players", "Average Total Number of Points", "Average Player Pre-Rating", "Average Rating of Opponents")
FinalDataONSum

## PlayerState = OH
FinalDataOH = subset(FinalData, PlayerState == "OH")
FinalDataOH_Num = round(NROW(FinalDataOH))
FinalDataOH_MeanPoints = round(mean(FinalDataOH$`Total Number of Points`))
FinalDataOH_PlayerPreRating = round(mean(FinalDataOH$`Player's Pre-Rating`))
FinalDataOH_AveOpponent = round(mean(FinalDataOH$`Average Rating of Opponents`))
FinalDataOHSum <- data.frame("OH", FinalDataOH_Num, FinalDataOH_MeanPoints, FinalDataOH_PlayerPreRating, FinalDataOH_AveOpponent)
colnames(FinalDataOHSum) <- c("State", "Number of Players", "Average Total Number of Points", "Average Player Pre-Rating", "Average Rating of Opponents")
FinalDataOHSum

## Combine all summary data by state into one table to compare
FinalDataSum <- rbind(FinalDataMISum, FinalDataONSum, FinalDataOHSum)
datatable(FinalDataSum)
