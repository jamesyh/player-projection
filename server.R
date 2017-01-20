library(shiny)

# setwd("C:/Users/James/Documents/Player Comparing App")

# Does this work? Why isn't 
# Define server logic for random distribution application
shinyServer(function(input, output) {
  
  source("data/functions-for-the-app.R")
  
  # Reactive expression to generate the requested distribution. This is 
  # called whenever the inputs change. The output renderers defined 
  # below then all used the value computed from this expression
  player <- reactive({  input$player })
  
  var <- reactive({  input$variable })
  
  change <- reactive({  input$change })
  
  other <- reactive({  input$other })
  
  
  sim.players <- reactive({matrixSim[rownames(matrixSim) == player(),]})
  
 
  
  n = reactive({  which( sim() == change() ) })
  
  
  player1 <- reactive({ input$player1 })
  player2 <- reactive({ input$player2 })
  player3 <- reactive({ input$player3 })
  
  # if(length(n() != 0)){ sim.players = reactive({c(sim()[-n()],  other() )}) }
  # else sim.players =   sim.players = reactive({sim()})
  # 
  
  
  
  #else sim.players <- reactive({ sim.players()[which(sim.players() == change())] = other()
  
  # Generate a plot of the data. Also uses the inputs to build the 
  # plot label. Note that the dependencies on both the inputs and
  # the data reactive expression are both tracked, and all expressions 
  # are called in the sequence implied by the dependency graph
  output$plotdude <- renderPlot({
    
    
    plotCreator(player(),sim.players(),var())
  })
  
  

  output$plotplayer1 <- renderPlot({
    par(mar=c(2,2,2,1),cex=0.72)
    for(m in 1){
      plot(
        dat[dat$Player == sim.players()[m],][,'NumberofSeasons'],xlab= "",
        dat[dat$Player == sim.players()[m],][,var()],ylab = "",
        type = 'l',main = sim.players()[m],lwd=2,lty=2,ylim=c(min(dat[dat$Player == sim.players()[m],][,var()],dat[dat$Player == player(),][,var()])
                                                              ,max(dat[dat$Player == sim.players()[m],][,var()],dat[dat$Player == player(),][,var()])))
      lines(dat[dat$Player == player(),][,'NumberofSeasons'],
            dat[dat$Player == player(),][,var()],lwd=2,col='blue')
    }  
  })
  
  output$plotplayer2 <- renderPlot({
    par(mar=c(2,2,2,1),cex=0.72)
    for(m in 2){
      plot(
        dat[dat$Player == sim.players()[m],][,'NumberofSeasons'],xlab= "",
        dat[dat$Player == sim.players()[m],][,var()],ylab = "",
        type = 'l',main = sim.players()[m],lwd=2,lty=2,ylim=c(min(dat[dat$Player == sim.players()[m],][,var()],dat[dat$Player == player(),][,var()])
                                                              ,max(dat[dat$Player == sim.players()[m],][,var()],dat[dat$Player == player(),][,var()])))
      lines(dat[dat$Player == player(),][,'NumberofSeasons'],
            dat[dat$Player == player(),][,var()],lwd=2,col='blue')
    }  
  })
  
  output$plotplayer3 <- renderPlot({
    par(mar=c(2,2,2,1),cex=0.72)
    for(m in 3){
      plot(
        dat[dat$Player == sim.players()[m],][,'NumberofSeasons'],xlab= "",
        dat[dat$Player == sim.players()[m],][,var()],ylab = "",
        type = 'l',main = sim.players()[m],lwd=2,lty=2,ylim=c(min(dat[dat$Player == sim.players()[m],][,var()],dat[dat$Player == player(),][,var()])
                                                              ,max(dat[dat$Player == sim.players()[m],][,var()],dat[dat$Player == player(),][,var()])))
      lines(dat[dat$Player == player(),][,'NumberofSeasons'],
            dat[dat$Player == player(),][,var()],lwd=2,col='blue')
    }  
  })
  
  output$plotplayer4 <- renderPlot({
    par(mar=c(2,2,2,1),cex=0.72)
    for(m in 4){
      plot(
        dat[dat$Player == sim.players()[m],][,'NumberofSeasons'],xlab= "",
        dat[dat$Player == sim.players()[m],][,var()],ylab = "",
        type = 'l',main = sim.players()[m],lwd=2,lty=2,ylim=c(min(dat[dat$Player == sim.players()[m],][,var()],dat[dat$Player == player(),][,var()])
                                                              ,max(dat[dat$Player == sim.players()[m],][,var()],dat[dat$Player == player(),][,var()])))
      lines(dat[dat$Player == player(),][,'NumberofSeasons'],
            dat[dat$Player == player(),][,var()],lwd=2,col='blue')
    }  
  })
  
  output$plotplayer5 <- renderPlot({
    par(mar=c(2,2,2,1),cex=0.72)
    for(m in 5){
      plot(
        dat[dat$Player == sim.players()[m],][,'NumberofSeasons'],xlab= "",
        dat[dat$Player == sim.players()[m],][,var()],ylab = "",
        type = 'l',main = sim.players()[m],lwd=2,lty=2,ylim=c(min(dat[dat$Player == sim.players()[m],][,var()],dat[dat$Player == player(),][,var()])
                                                              ,max(dat[dat$Player == sim.players()[m],][,var()],dat[dat$Player == player(),][,var()])))
      lines(dat[dat$Player == player(),][,'NumberofSeasons'],
            dat[dat$Player == player(),][,var()],lwd=2,col='blue')
    }  
  })

  output$plotplayer6 <- renderPlot({
    par(mar=c(2,2,2,1),cex=0.72)
    for(m in 6){
      plot(
        dat[dat$Player == sim.players()[m],][,'NumberofSeasons'],xlab= "",
        dat[dat$Player == sim.players()[m],][,var()],ylab = "",
        type = 'l',main = sim.players()[m],lwd=2,lty=2,ylim=c(min(dat[dat$Player == sim.players()[m],][,var()],dat[dat$Player == player(),][,var()])
                                                              ,max(dat[dat$Player == sim.players()[m],][,var()],dat[dat$Player == player(),][,var()])))
      lines(dat[dat$Player == player(),][,'NumberofSeasons'],
            dat[dat$Player == player(),][,var()],lwd=2,col='blue')
    }  
  })
  
  output$plotplayer7 <- renderPlot({
    par(mar=c(2,2,2,1),cex=0.72)
    for(m in 7){
      plot(
        dat[dat$Player == sim.players()[m],][,'NumberofSeasons'],xlab= "",
        dat[dat$Player == sim.players()[m],][,var()],ylab = "",
        type = 'l',main = sim.players()[m],lwd=2,lty=2,ylim=c(min(dat[dat$Player == sim.players()[m],][,var()],dat[dat$Player == player(),][,var()])
                                                              ,max(dat[dat$Player == sim.players()[m],][,var()],dat[dat$Player == player(),][,var()])))
      lines(dat[dat$Player == player(),][,'NumberofSeasons'],
            dat[dat$Player == player(),][,var()],lwd=2,col='blue')
    }  
  })
  
  output$plotplayer8 <- renderPlot({
    par(mar=c(2,2,2,1),cex=0.72)
    for(m in 8){
      plot(
        dat[dat$Player == sim.players()[m],][,'NumberofSeasons'],xlab= "",
        dat[dat$Player == sim.players()[m],][,var()],ylab = "",
        type = 'l',main = sim.players()[m],lwd=2,lty=2,ylim=c(min(dat[dat$Player == sim.players()[m],][,var()],dat[dat$Player == player(),][,var()])
                                                              ,max(dat[dat$Player == sim.players()[m],][,var()],dat[dat$Player == player(),][,var()])))
      lines(dat[dat$Player == player(),][,'NumberofSeasons'],
            dat[dat$Player == player(),][,var()],lwd=2,col='blue')
    }  
  })
  
  output$plotplayer9 <- renderPlot({
    par(mar=c(2,2,2,1),cex=0.72)
    for(m in 9){
      plot(
        dat[dat$Player == sim.players()[m],][,'NumberofSeasons'],xlab= "",
        dat[dat$Player == sim.players()[m],][,var()],ylab = "",
        type = 'l',main = sim.players()[m],lwd=2,lty=2,ylim=c(min(dat[dat$Player == sim.players()[m],][,var()],dat[dat$Player == player(),][,var()])
                                                              ,max(dat[dat$Player == sim.players()[m],][,var()],dat[dat$Player == player(),][,var()])))
      lines(dat[dat$Player == player(),][,'NumberofSeasons'],
            dat[dat$Player == player(),][,var()],lwd=2,col='blue')
    }  
  })
  
  
  output$plotplayer10 <- renderPlot({
    par(mar=c(2,2,2,1),cex=0.72)
    for(m in 10){
      plot(
        dat[dat$Player == sim.players()[m],][,'NumberofSeasons'],xlab= "",
        dat[dat$Player == sim.players()[m],][,var()],ylab = "",
        type = 'l',main = sim.players()[m],lwd=2,lty=2,ylim=c(min(dat[dat$Player == sim.players()[m],][,var()],dat[dat$Player == player(),][,var()])
                                                              ,max(dat[dat$Player == sim.players()[m],][,var()],dat[dat$Player == player(),][,var()])))
      lines(dat[dat$Player == player(),][,'NumberofSeasons'],
            dat[dat$Player == player(),][,var()],lwd=2,col='blue')
    }  
  })
  
  output$comparisonPlot <- renderPlot({
    
    playerComparisonApp(player1(),player2(),player3(),var())
    
  })
  
})
