library(ggplot2)


dat <- readRDS("data/df.RDS")
matrixSim <- readRDS("data/matrixSim.RDS")

allPlayers <- unique(dat$Player) 
dat$lastSeason <- NA

for(i in 1:length(allPlayers)){
  
  dat[dat$Player == allPlayers[i],'lastSeason'] = max(dat[dat$Player == allPlayers[i],'NumberofSeasons'])
  
  
  
  
}

players2015 <- unique(dat[dat$Year == 2015,'Player'])



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
  
  yMax <- max(c(upper,all[,var]))
  yMin <- min(c(lower,all[,var]))
  
  newData = cbind(mid,(n+1):(n+4))
  colnames(newData) <- c('mid','NumberofSeason')
  newData <- as.data.frame(newData)
  
  
  ggplot(data = indiv, aes(x = indiv$NumberofSeasons,y = indiv[,var]) ) + 
    geom_line(size=2, color = 'Blue') +
    xlim(xMin, xMax) + ylim(yMin, yMax) +
    geom_line(data = newData, aes(x = NumberofSeason, y = mid ),size = 1, color = 'Blue',linetype = "dashed" ) +
    geom_errorbar(data = newData, aes(x = NumberofSeason, y = mid,
                                      ymax = upper,ymin = lower, width = 0,size = 1),
                  color = rgb(.03,.3,.3, 0.2)) +
    xlab('Number of Seasons') + ylab(var)+ theme(legend.position = "none")  + 
    ggtitle(player.name) #+
   # theme(plot.background = element_rect(fill = "red"))
    #theme(panel.border = element_rect(colour = "black", fill=NA, size=2))
  
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




# Multiple plot function
#
# ggplot objects can be passed in ..., or to plotlist (as a list of ggplot objects)
# - cols:   Number of columns in layout
# - layout: A matrix specifying the layout. If present, 'cols' is ignored.
#
# If the layout is something like matrix(c(1,2,3,3), nrow=2, byrow=TRUE),
# then plot 1 will go in the upper left, 2 will go in the upper right, and
# 3 will go all the way across the bottom.
#
multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  library(grid)
  
  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)
  
  numPlots = length(plots)
  
  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }
  
  if (numPlots==1) {
    print(plots[[1]])
    
  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    
    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
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


