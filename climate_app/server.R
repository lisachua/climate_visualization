library(shiny)
library(stats)
library(ggplot2)

shinyServer(function(input, output) {
  selectedData <- reactive({
    if(input$ycol=="temperature_anomaly") temp
    else coAvg
  })

  diagData <- reactive({
    model <- lm(y~x,data=selectedData())
    cook <- cooks.distance(model)
    dffit <- dffits(model)
    hats <- hatvalues(model)
    studRes <- rstudent(model)
    residuals <- residuals(model)
    diagnostics <- data.frame(cooksDistance = cook, difFits = dffit,leverage = hats, studentizedResiduals = studRes,residuals=residuals)
    })
  
  
  vals <- reactiveValues(
    kept = rep(TRUE, nrow(regresData)),
    selected = rep(FALSE, nrow(regresData))
  )
  
  output$scatterPlot <- renderPlot({
    includedPoints <- selectedData()[vals$kept,]
    excludedPoints <- selectedData()[!vals$kept,]
    selectedPoints <- selectedData()[vals$selected,]
    ggplot(includedPoints,aes(x,y))+geom_point()+
      geom_smooth(method="lm",se=F,fullrange=T)+
      geom_point(data=excludedPoints,aes(x, y),color="grey")+
      geom_point(data=selectedPoints,aes(x, y),color="red",size=5,shape=1)+
      coord_cartesian(xlim = c(min(selectedData()$x)-1, max(selectedData()$x)+1), ylim = c(min(selectedData()$y)-.4, max(selectedData()$y)+.4))+
      xlab("Year")+ ylab({
        if(input$ycol=="temperature_anomaly") "Temperature Anomaly (°C)"
        else "CO2 Emissions World Average (tonnes pp)"
        })+
      theme_minimal()
  })
  
  observe({
    print(which(vals$kept))
    print(which(vals$selected))
  })
  
  observeEvent(input$clickPoint, {
    res <- nearPoints(selectedData(),input$clickPoint,threshold=50,maxpoints=1,allRows = T)
    vals$kept <- xor(vals$kept, res$selected_)
  })
  
  observeEvent(input$clickPoint2, {
    res <- nearPoints(diagData(),input$clickPoint2,threshold=50,maxpoints=1,allRows = T)
    vals$selected <- xor(vals$selected, res$selected_)
  })
  
  observeEvent(input$ycol, {
    vals$kept <-  rep(TRUE, nrow(regresData))
    vals$selected = rep(FALSE, nrow(regresData))
  })
  
  observeEvent(input$reset, {
    vals$kept <-  rep(TRUE, nrow(regresData))
    vals$selected = rep(FALSE, nrow(regresData))
  })
  
  observeEvent(input$toggle, {
    res <- brushedPoints(diagData(),input$brushPoints2,allRows = T)
    vals$kept <- xor(vals$kept, res$selected_)
  })
  
  observeEvent(input$toggle, {
    res <- brushedPoints(selectedData(),input$brushPoints,allRows = T)
    vals$kept <- xor(vals$kept, res$selected_)
  })
  
  
  output$info <- renderText({
    if (input$ycol=="temperature_anomaly") {
      paste("Year = ", round(as.numeric(input$plot_hover$x), 0),
            "\nTemp Anomaly (°C)= ",round(as.numeric(input$plot_hover$y), 4))
    }
    else {
      paste("Year = ", round(as.numeric(input$plot_hover$x), 0),
            "\nCO2 Emissions = ", round(as.numeric(input$plot_hover$y), 4))
    }
  })
  
  #GRAPH 2
  output$spiralizedPlot <- renderPlot({
    #filter by year on slider
    plot_data <- filter(spiralData,year<=input$year)
    
    g <- ggplot(plot_data)+
      geom_line(aes(x=as.numeric(month), y=get(input$yVar), group=year, color=get(input$colorVar)), size=.3, show.legend=F )+
      scale_x_continuous(breaks=c(2, 4, 6, 8, 10, 12), labels=c("Feb", "Apr", "Jun", "Aug", "Oct", "Dec"))+
      scale_y_continuous(labels=NULL)+
      ylab("")+
      xlab("")+
      theme_minimal() +
      coord_polar()+
      if(input$colorVar == "worldAvg") {
        scale_color_gradientn(colors=rev(rainbow(2)),limits=c(0,max(spiralData$worldAvg, na.rm=T)))
      } else {
      scale_color_gradientn(colors=rev(rainbow(2)),limits=c(min(spiralData$temperature_anomaly,na.rm=T), 0))
        }
    
    if(input$legend)
    {
      g <- g + geom_line(aes(x=as.numeric(month), y=get(input$yVar), group=year, color=get(input$colorVar)), size=.3) + labs(color="")
    } 
    
    g
  })
  
  #GRAPH 3
  output$mapPlot <- renderPlot({
    #filter by year on slider
    plot_data <- filter(joinedMapData_long,year==input$year2)
    
    p <- ggplot()+geom_polygon(data=plot_data, aes(x=long, y=lat, group = group, fill = co2))+coord_map(xlim = c(-180,180), ylim = c(-90,90))
    p + scale_fill_distiller(limits=c(0,max(joinedMapData_long$co2, na.rm=T)))
  })
})