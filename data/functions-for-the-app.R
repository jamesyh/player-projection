library(ggplot2)
library(XML)

dat <- readRDS("data/df.RDS")
matrixSim <- readRDS("data/matrixSim.RDS")
date <- readRDS('data/date.RDS')
players2015 <- unique(dat[dat$Year == 2015,'Player'])
allPlayers <- unique(dat$Player) 
dat$lastSeason <- NA


for(i in 1:length(allPlayers)){
  
  dat[dat$Player == allPlayers[i],'lastSeason'] = max(dat[dat$Player == allPlayers[i],'NumberofSeasons'])
  
}

if(date < Sys.Date()){
  
  saveRDS(Sys.Date(),'data/date.RDS')
  
  updated2017 <- as.data.frame(readHTMLTable("http://www.basketball-reference.com/leagues/NBA_2017_per_poss.html")[[1]] )
  
  updated2017 <- updated2017[!c(updated2017$Player == 'Player' ),1:29]
  
  names(updated2017) <- c("Rank","Player","Pos","Age","Tm","Games","GS","MP","FieldGoals",
                          "FieldGoalsAttempted","FieldGoalPercentage","ThreePoints","ThreePointsAttempted",
                          "3p%","TwoPointFieldGoals","TwoPointFieldGoalsAttempted","2p%","FreeThrows",
                          "FreeThrowsAttempted","FT%","OffensiveRebounds","DefensiveRebounds","TotalRebounds",
                          "Assists","Steals","Blocks","Turnovers","PersonalFouls","TotalPoints")
  
  updated2017$Year <- 2017
  updated2017$NumberofSeasons <- 0
  updated2017$lastSeason <- 0
  
  nam <- names(dat)
  
  for(i in 3:length(dat)){
    
    updated2017[,nam[i]] <- as.numeric( as.character( updated2017[,nam[i]] ) )
    
  }
  
  saveRDS(updated2017,"data/updated2017.RDS")
  
  
}
updated2017 <- readRDS("data/updated2017.RDS")










nba.Prediction <- function(player.name,Var='Games',scaled = FALSE,
                           vars.of.interest = c("ThreePoints","TwoPointFieldGoals","ThreePointsAttempted"
                                                ,"TwoPointFieldGoalsAttempted","DefensiveRebounds",
                                                "OffensiveRebounds","Assists","Steals","Blocks","Turnovers","TotalPoints",
                                                "FreeThrows","FreeThrowsAttempted")){
  
  dat <- dat[,c("Rank","Player",vars.of.interest,"NumberofSeasons")]
  
  if(scaled == TRUE){
    
    for(i in 1:length(vars.of.interest)){
      
      dat[,vars.of.interest[i]] <- scale(dat[,vars.of.interest[i]])
      
    }
  }
  
  n.v.o.i <- length(vars.of.interest)+2
  
  indiv <- dat[dat$Player==player.name,]
  n <- max(indiv[,'NumberofSeasons'])
  
  
  sub <- dat[dat$NumberofSeasons > n,]
  compare.players <- unique(sub$Player)
  sub <- dat[dat$Player %in% compare.players,]
  
  
  for( k in 1:n){
    
    sub1 <- (sub[sub$NumberofSeasons == k,])
    
    indiv1 <- (indiv[indiv$NumberofSeasons == k,])
    
    for (j in c(3:n.v.o.i)){
      sub1[,j] <- sub1[,j] - as.numeric(indiv1[,j])
    }
    
    
    for (j in c(3:n.v.o.i)){
      sub1[,j] <- sub1[,j]^2
    }
    
    
    ss <- rep(0,length(sub1[,1]))
    for (j in 1:length(sub1[,1])){
      ss[j] <- sum(sub1[j,3:n.v.o.i]) 
    }
    if(k==1){tss <- ss}
    else{tss <- tss + ss}
    
  }
  
  tss <- cbind(ss,1:length(tss))
  tss <- tss[which(tss[,1]!='NA'),]
  tss <- tss[order(tss[,1]),]
  close.players <- sub1[tss[1:10,2],][,2]
  
  
  
  return(close.players)
  
}



plotCreator <- function(player.name,sim,var){
  
  
  
  
  
  # Data for individual player
  indiv <- dat[dat$Player == player.name,]
  
  # Data for all the similar players
  sims <- dat[dat$Player %in% sim,]
  # sorting data
  sims <- sims[order(sims$Player,sims$NumberofSeasons),]
  
  # Max number of seasons for player
  n <- max(indiv$NumberofSeasons)
  
  # Removing 'known data'
  sims <- sims[!(sims$NumberofSeasons %in% 1:n),]
  
  sims <- rbind(indiv[indiv$NumberofSeasons == n, ],sims)
  
  
  
  all <- rbind(indiv,sims)
  
  xMax <- (n+4)
  xMin <- min(all$NumberofSeasons)
  
  
  
  upper <- NA
  lower <- NA
  mid <- NA
  
  current <- updated2017[updated2017$Player == player.name,]
  current$NumberofSeasons = n + 1
  current$lastSeason <- 0
  
  season <- dat[dat$Player == player.name & dat$NumberofSeasons == n,]
  
  nam <- names( dat[dat$Player == player.name & dat$NumberofSeasons == n,] )
  
  current <- current[,nam]
  
  current2 <- rbind(season,current)
  
  for(i in (n + 1):(n+4)){
    
    m <- mean(sims[sims$NumberofSeasons == i,var])
    s <- sd(sims[sims$NumberofSeasons == i,var])
    nP <- length( sims[sims$NumberofSeasons == i,var] )
    
    mid[i-n] <- m
    upper[i-n] <- m + qnorm(0.995)*s/sqrt(nP)
    lower[i-n] <- m - qnorm(0.995)*s/sqrt(nP)
    
    if(is.na(upper[i-n])) upper[i-n] = m +10
    if(is.na(lower[i-n])) lower[i-n] = m - 10
    
  }
  
  yMax <- max(c(upper,all[,var],current2[,var]))
  yMin <- min(c(lower,all[,var],current2[,var]))
  
  newData = cbind(mid,(n+1):(n+4))
  colnames(newData) <- c('mid','NumberofSeason')
  newData <- as.data.frame(newData)
  
  
  ggplot(data = indiv, aes(x = indiv$NumberofSeasons,y = indiv[,var]) ) + 
    geom_line(size=2, color = 'Blue') +
    xlim(xMin, xMax) + ylim(yMin, yMax) +
    geom_line(data = newData, aes(x = NumberofSeason, y = mid ),
              size = 1, color = 'Blue',linetype = "dashed" ) + 
    geom_line(data = current2, aes(x = current2$NumberofSeason, y = current2[,var] , group = 1),
              size = 1, color = 'Red',linetype = "dashed" ) +
    geom_point(data = current, aes(x = current$NumberofSeason, y = current[,var]),
               size = 2, color = 'Red') + 
    geom_errorbar(data = newData, aes(x = NumberofSeason, y = mid,
                                      ymax = upper,ymin = lower, width = 0,size = 1),
                  color = rgb(.03,.3,.3, 0.2)) +
    xlab('Number of Seasons') + ylab(var)+ theme(legend.position = "none")  + 
    ggtitle(player.name)
  
}


simPlayerPlot <- function(player.name,simPlayer,var = "TotalPoints"){
  
  indiv <- dat[dat$Player == player.name,]
  simIndiv <- dat[dat$Player == simPlayer, ]
  
  xMax = max(c(indiv$NumberofSeasons,simIndiv$NumberofSeasons))
  xMin = min(c(indiv$NumberofSeasons,simIndiv$NumberofSeasons))
  
  yMax = max(c(indiv[,var],simIndiv[,var]))
  yMin = min(c(indiv[,var],simIndiv[,var]))
  
  
  g = ggplot(data = indiv, aes(x = indiv$NumberofSeasons,y = indiv[,var]) ) + 
    geom_line( color = 'Blue') +
    geom_line(data = simIndiv, aes(x = simIndiv$NumberofSeasons,y = simIndiv[,var]),
               color = 'Red',linetype = "dashed") +
    xlab("Number of Seasons") + ylab(var) + ggtitle(simPlayer) 
  
  return(g)
}





playerComparisonApp <- function(player1,player2,player3,var) {
  
  dataP1 <- dat[dat$Player == player1,]
  dataP2 <- dat[dat$Player == player2,]
  dataP3 <- dat[dat$Player == player3,]

  
  allData <- rbind(dataP1,dataP2,dataP3)
  Player <- allData$Player
  
  ggplot(data=allData, aes(x=allData$NumberofSeason, y=allData[,var],
                           group=Player, colour=Player)) +
    geom_line(size=2) + xlab('Number of Seasons') + ylab(var) +
    theme(legend.position="bottom")
}





