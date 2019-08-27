library(httr)
library(jsonlite)
library(data.table)
library(shiny)

version = "v4.5.2"

# library(rsconnect)
# deployApp('Team Growth App')

# Define UI for app ----
ui <- fluidPage(
  
  # App title ----
  titlePanel("Team Growth"),
  
  # Sidebar layout with input and output definitions ----
  sidebarLayout(position = "right",
    
    # Sidebar panel for inputs ----
    sidebarPanel(
      # actionButton("removedata",label = "Delete Data",
      #              style="color:white;
      #              background-color:#FF0000"),
      h2(version),
      br(),
      # numericInput("maxyear","Most Recent Season",value = (as.numeric(substr(Sys.Date(),1,4)))-1),
      # actionButton("loaddata",label = "Load Data",
      #              style="color:black;
      #              background-color:#919191"),
      # br(),
      # br(),
      numericInput("teamnumber","Enter Team Number",value = 2877),
      actionButton("go",label = "Go!",
                   style="color:white;
                   background-color:#2E59FF"),
      br(),
      h3("Data Status"),
      h4(textOutput("cf")),
      h4(textOutput("yearrange")),
      br(),
      br(),
      h5("Download differences chart"),
      h5("(First graph)"),
      downloadButton("downloadData2","Download"),
      h5("Download points chart"),
      h5("(Second graph)"),
      downloadButton("downloadData","Download")
      
    ),
    
    # Main panel for displaying outputs ----
    mainPanel(
      textOutput("teamexist"),
      plotOutput("dataplot"),
      plotOutput("df2")
      
    )
  )
)
server <- function(input, output) {
  
  output$finalyear <- renderText(input$maxyear)
  
  observeEvent(input$go, {
    try({
      
      keys0 <- read.csv(file = "config", colClasses = c("x" = "character"))
      keys <- c(keys0$x)
      
      input <- input$teamnumber
      teamkey <- paste("frc",input,sep="")
      
      year1 <- keys[1]
      year1 <- as.numeric(substr(year1,1,4))
      year2 <- tail(keys,n=1)
      year2 <- as.numeric(substr(year2,1,4))
      
      districtranking=list()
      for(i in 1:length(keys)){
        districtranking[[i]]=read.csv(file = paste("Data/",keys[i],sep = ""))
      }
      
      districtranking=setNames(districtranking,keys)
      
      points=c()
      year=c()
      for(j in 1:length(districtranking)){
        ind=which(districtranking[[j]]$team_key==teamkey)
        counter=length(points)
        points=append(points,districtranking[[j]][ind,1])
        counter2=length(points)
        if (counter2>counter){
          tempyear=names(districtranking)[j]
          tempyear2=substr(tempyear,start = 1,stop = 4)
          year=append(year,tempyear2)
        }
      }
      
      teamexist=F
      
      try({
        
        df2=data.frame(year,points,input)
        
        df3 <<- df2
        df4 <<- data.frame(tail(df2$year,-1),diff(df2$points),tail(df2$input,-1))
        
        names(districtranking)
        
        y=year2-year1
        x=(y-(length(df2$year)))+2
        
        AllData=do.call(rbind,districtranking)
        AllData$year=substr(rownames(AllData),1,4)
        
        dt_AllData=as.data.table(AllData)
        
        setkeyv(dt_AllData,c('team_key','year')) # easy sorting for data.table class type
        imp_dt_AllData=dt_AllData[,list(year=year,improv=c(-99999,diff(points))),by=c('team_key')]
        imp_dt_AllData=subset(imp_dt_AllData, improv!=-99999)
        
        teamexist=T
        
        output$dataplot <- renderPlot({
          boxplot(imp_dt_AllData$improv~as.numeric(imp_dt_AllData$year),xlab = "Year/Season",ylab = "Improvement")
          points(x:y,diff(df2$points),type='l',lwd=2,col='red')
        })
        output$df2 <- renderPlot({
          plot(as.character(df2$year),df2$points,type = "l",xlab = "Year/Season",ylab = "District Points",ylim = c(0,400))
        })
      })
      
      if (teamexist==T){
        output$teamexist <- renderText("")
      } else {
        output$teamexist <- renderText("Team not found")
      }
      
    })
    
  })
  
  observeEvent(input$loaddata,{
    maxyear <- input$maxyear
    year <- 2009:maxyear
    
    rawdistrictlist=list()
    j=1
    for(i in year){
      rawdistricts <- GET(paste("https://www.thebluealliance.com/api/v3/districts/",i,"?X-TBA-Auth-Key=DzzDoXPk1JshyNjKpjkdDP2RHaqXNVD44xksasNYSxJu5YSmWYkTWvzA9stCcqrB",sep=""))
      rawkey <- rawToChar(rawdistricts$content)
      rawdistrictlist[[j]]  <- fromJSON(rawkey)
      j=j+1
      showNotification("",duration = 2)
    }
    
    fulldistricts=do.call('rbind', rawdistrictlist)
    keyvector=fulldistricts$key
    
    rankingurl <- paste("https://www.thebluealliance.com/api/v3/district/",keyvector,"/rankings?X-TBA-Auth-Key=DzzDoXPk1JshyNjKpjkdDP2RHaqXNVD44xksasNYSxJu5YSmWYkTWvzA9stCcqrB",sep="")
    fullrankings=list()
    k=1
    for(year in rankingurl){
      temp <- GET(year)
      rawranking <- rawToChar(temp$content)
      fullrankings[[k]]  <- fromJSON(rawranking)
      k=k+1
      showNotification("",duration = 2)
    }
    
    l=1
    reducedrankings=list()
    for(m in fullrankings){
      reducedrankings[[l]]=data.frame(fullrankings[[l]]$point_total,fullrankings[[l]]$team_key)
      names(reducedrankings[[l]])=c("points", "team_key")
      l=l+1
    }
    
    reducedrankings=setNames(reducedrankings,keyvector)
    
    o=1
    for(n in reducedrankings){
      fname <- paste("Data/",keyvector[o],sep="")
      write.csv(reducedrankings[[o]], file=fname,row.names = F,quote = F) 
      o=o+1
    }
    
    write.csv(keyvector, file="config",row.names = F,quote = F)
    
    if(file.exists("config")){
      output$cf <- renderText({"Data Loaded!"})
      output$yearrange <- renderText({paste("2009 -",input$maxyear)})
    } else {
      output$cf <- renderText({"Data not found. Please load data."})
      output$yearrange <- renderText({""})
    }
  })
  
  if(file.exists("config")){
    endyear <- substr(tail(list.files("Data"),1),1,4)
    output$cf <- renderText({"Data Loaded!"})
    output$yearrange <- renderText({paste("2009 -",endyear)})
  } else {
    output$cf <- renderText({"Data not found."})
    output$yearrange <- renderText({""})
  }
  
  observeEvent(input$removedata,{
    showModal(
      modalDialog("Are you sure? This cannot be undone. Cancel by pressing 'Cancel,' or by clicking anywhere outside this box, or by pressing Esc.",footer = tagList(modalButton("Cancel"),actionButton("removedatafinal",label = "Delete Data",style="color:white; background-color:#FF0000")),easyClose = T)
    )
  })
  
  observeEvent(input$removedatafinal,{
    removeModal()
    do.call(file.remove, list(list.files("Data", full.names = TRUE)))
    file.remove("config")
    if(file.exists("config")){
      endyear <- substr(tail(list.files("Data"),1),1,4)
      output$cf <- renderText({"Data Loaded!"})
      output$yearrange <- renderText({paste("2009 -",endyear)})
    } else {
      output$cf <- renderText({"Data not found."})
      output$yearrange <- renderText({""})
    }
  })
  
  # datasetInput <- reactive({
  #   df3
  # })
  
  output$downloadData <- downloadHandler(
    filename = function() {
      paste(input$teamname, ".csv", sep = "")
    },
    content = function(file) {
      write.csv(df3, file, row.names = FALSE)
    }
  )
  output$downloadData2 <- downloadHandler(
    filename = function() {
      paste(input$teamname, ".csv", sep = "")
    },
    content = function(file) {
      write.csv(df4, file, row.names = FALSE)
    }
  )
}

# Create Shiny app ----
shinyApp(ui = ui, server = server)
