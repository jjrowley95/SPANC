players <- c("MCENTIRE", "FLEMING","MOODY","KNIGHT","MARIN", "JONES","FAUSETT","LATHAM","SPURGIN","BUTLER" 
             ,"BOX","MADUNIC","COOPER","BARNES","MOORE","WILLIAMS")

plus_minus_eff <- function(data,start_five,int){

players <- c("Mcentire", "Fleming", "Moody", "Knight", "Marin", "Jones", "Fausett", "Latham", "Spurgin", "Butler"
             ,"Box", "Madunic", "Cooper", "Barnes", "Moore", "Williams")
players_CAPS <- c("MCENTIRE", "FLEMING","MOODY","KNIGHT","MARIN", "JONES","FAUSETT","LATHAM","SPURGIN","BUTLER" 
             ,"BOX","MADUNIC","COOPER","BARNES","MOORE","WILLIAMS")
players_full <- c("MCENTIRE,DAMANI","FLEMING,NICK","MOODY,AANEN","KNIGHT III,JOHN","MARIN,DRE","JONES,TEVIAN","FAUSETT,MAIZEN","LATHAM,DARNELL","SPURGIN,JASON",
                  "BUTLER,HARRISON","BOX,KINGSLEY","MADUNIC,IVAN","COOPER,COURTESE","BARNES,DEE","MOORE,MARQUIS","WILLIAMS,MARTEL")

data <- cbind(data, Mcentire = "A")
data <- cbind(data, Fleming = "A")
data <- cbind(data, Moody = "A")
data <- cbind(data, Knight = "A")
data <- cbind(data, Marin = "A")
data <- cbind(data, Jones = "A")
data <- cbind(data, Fausett = "A")
data <- cbind(data, Latham = "A")
data <- cbind(data, Spurgin = "A")
data <- cbind(data, Butler = "A")
data <- cbind(data, Box = "A")
data <- cbind(data, Madunic = "A")
data <- cbind(data, Cooper = "A")
data <- cbind(data, Barnes = "A")
data <- cbind(data, Moore = "A")
data <- cbind(data, Williams = "A")

#str(data)
#This loop puts a 1 if the player is in the game, and zero if they are not#

#Starting 5 

for(i in 1:length(players_CAPS)){
  if(players_CAPS[i] %in% start_five){
    data[1,players[i]]<- 1
  }else{
    data[1,players[i]] <- 0
  }
}

for(i in 1:length(players)){
  period <- 1
  for(j in 1:nrow(data)){
    
    if(data[j,"action"] == "SUB"){
      if(data[j,"name"] == players_full[i]){
        if(data[j,"type"] == "IN"){
          data[j,players[i]] <-1
        }else{
          data[j,players[i]] <- 0
        }
      }else{
        data[j,players[i]] <- data[j-1, players[i]]
      }
    }else{
      if(j != 1){
        data[j,players[i]] <- data[j-1,players[i]]
      }
    }
  }
}

#str(data)

#This loop adds or subtracts points to a players performance score if they are in the game

for(i in 1:length(players)){
  for(j in 1:nrow(data)){
    if(data[j,players[i]] == 1){
      if(data[j,"action"] == "GOOD"){
        if(data[j,"team"] == "SUU"){
          if(data[j,"type"] == "FT"){
            data[j,players[i]] <- 1
          }else if(data[j,"type"] == "3PTR"){
            data[j,players[i]] <- 3
          }else{
            data[j,players[i]] <- 2
          }
        }else{
          if(data[j,"type"] == "FT"){
            data[j,players[i]] <- -1
          }else if(data[j,"type"] == "3PTR"){
            data[j,players[i]] <- -3
          }else{
            data[j,players[i]] <- -2
          }
        }
      }else{
        data[j,players[i]] <- 0
      }
    }
  }
}

#str(data)
#These functions convert the columns into numeric values and sums the up into a dataframe
player_scores <- NULL
for(i in 10:25){
  data[,i] <- as.numeric(data[,i])
  player_scores <- c(player_scores,sum(data[,i]))
}



#str(data)
test_df <- data
return(player_scores)

}


####Start_Fives####
start_five_BET1 <- c("MARIN","MOORE","SPURGIN", "BUTLER","JONES")
start_five_LMU <- c("KNIGHT","JONES","FAUSETT","MADUNIC","MARIN")
start_five_SK <- c("JONES","MARIN","BUTLER","FAUSETT","MADUNIC")
start_five_MONT1 <- c("KNIGHT","FAUSETT","MARIN","MADUNIC","JONES")
start_five_MONT2 <- c("JONES","MARIN","FAUSETT","MADUNIC","KNIGHT")
start_five_UVU <- c("MARIN","FAUSETT","MADUNIC","KNIGHT","JONES") 
start_five_DIXIE <- c("KNIGHT","JONES","MADUNIC","FAUSETT","MARIN")
start_five_BET2 <- c("FAUSETT","MARIN","JONES","KNIGHT","MADUNIC")
start_five_UI1 <- c("JONES","MARIN","MADUNIC","BUTLER","KNIGHT")
start_five_UI2 <- c("JONES","MARIN","MADUNIC","BUTLER","KNIGHT")
start_five_EW <- c("FAUSETT","JONES","KNIGHT","MARIN","MADUNIC")
start_five_EW2 <- c("FAUSETT","JONES","KNIGHT","MARIN","MADUNIC")
start_five_WEBER1 <- c("FAUSETT","MARIN","JONES","MADUNIC","KNIGHT")
start_five_WEBER2 <- c("FAUSETT","MARIN","JONES","MADUNIC","KNIGHT")



####START####


#Builds DataFrame
player_scores_All_df <- data.frame(players)
player_scores_All_df$LMU <- plus_minus_eff(LMU,start_five_LMU)
player_scores_All_df$SK <- plus_minus_eff(SK,start_five_SK)
player_scores_All_df$MONT1 <-plus_minus_eff(Mont,start_five_MONT1)
player_scores_All_df$MONT2 <-plus_minus_eff(Mont_2,start_five_MONT2)
player_scores_All_df$UVU <-plus_minus_eff(UVU,start_five_UVU)
player_scores_All_df$DIXIE <-plus_minus_eff(Dixie,start_five_DIXIE)
player_scores_All_df$BET1 <- plus_minus_eff(Bet_1, start_five_BET1)
player_scores_All_df$BET2 <- plus_minus_eff(Bet_2,start_five_BET2)
player_scores_All_df$UI1 <-plus_minus_eff(UI_1,start_five_UI1)
player_scores_All_df$UI2 <-plus_minus_eff(UI_2,start_five_UI2)
player_scores_All_df$EW1 <- plus_minus_eff(EW,start_five_EW)
player_scores_All_df$EW2 <-plus_minus_eff(EW_2,start_five_EW2)
player_scores_All_df$WEBER1 <-plus_minus_eff(Weber_1,start_five_WEBER1)
player_scores_All_df$WEBER2 <-plus_minus_eff(Weber_2,start_five_WEBER2)



#Creates a total column
player_scores_All_df$TOTAL <- 0
for(i in 1:nrow(player_scores_All_df)){
  vect <- NULL
  vect <- player_scores_All_df[i,]
  vect <- vect[-1]
  player_scores_All_df[i,"TOTAL"] <- sum(vect)
}

player_scores_All_df


write.csv(player_scores_All_df,"/Users/devinwarner/Desktop/player_scores.csv")


