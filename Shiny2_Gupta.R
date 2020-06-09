   
  ## Shiny: shiny app using Chicago Crime Data
  ## Name:  Fiza Gupta
  ## ID:    fxg180000

 
  # ------------ Auto install and load required packages -------------------

 # if (!require('pacman'))install.packages('pacman')
 # pacman::p_load(tidyverse,readr,animation,gtools,chron,plyr,htmltools,dplyr,data.table,lubridate,maps,plotly,
   #              animation,gridExtra,tigris,leaflet,animation, readxl, shinyjs, DT,
  #               scales,rgdal,ggplot2,shiny)
  
  if (!require('pacman'))install.packages('pacman')
  pacman::p_load(tidyverse,animation,gtools,chron,plyr,dplyr,lubridate,maps,plotly,
                 animation,gridExtra,tigris,leaflet, DT, scales,ggplot2,shiny)


  # --------------READ CSV FILE------------------------------------------

  #load data
  df.crime <- read.csv("Chicago_Crimes_2018.csv")

  # ---------------CLEAN DATA--------------------------------------------

  # Clean data and create new columns

  # delete first column as it is not required
  df.crime <- df.crime[ , -1]

  # these case number are being repeated and there is a typo in these values as same values are already present
  df.crime<-df.crime[!(df.crime$Case.Number=="464266"),]
  df.crime<-df.crime[!(df.crime$Case.Number==".JB299184"),]

  # Certain case number are repeated so i am taking only the unique cases
  df.crime <- df.crime[!duplicated(df.crime$Case.Number), ]

  # Create new columns for the Date and time
  df.crime$DateTime <- strptime(df.crime$Date,format="%m/%d/%Y %I:%M:%S %p")
  df.crime$Time <- format(strptime(df.crime$DateTime, "%Y-%m-%d %H:%M:%S"), "%H")
  df.crime$Month <- format(strptime(df.crime$DateTime, "%Y-%m-%d %H:%M:%S"), "%m")
  df.crime$Date <- format(strptime(df.crime$DateTime, "%Y-%m-%d %H:%M:%S"), "%Y-%m-%d")


  # --------------- CONVERSION and KNOW ABOUT YOUR DATA------------------

  #convert datatypes and have a overview about the data
  df.crime$DateTime <- as.POSIXct(df.crime$DateTime)
  dim(df.crime)
  #write.csv(df.crime, "df.crime.csv")

  # for all different tabs I am using different data frames according to the empty cells
  #for the map tab i am using different because there are allot of values missing in the column, and for type there is value missing so deleting that
  # rows doesnot require. So I am deleting the empty rows as required
  df.crime_map <- df.crime[complete.cases(df.crime),]
  df.crime_loc <- df.crime[-which(df.crime$Location.Description == ""), ]

  # Create unique values for the below options
  lst.month <- unique(df.crime$Month)
  lst.type <- unique(df.crime$Primary.Type)
  lst.loc <- unique(df.crime_loc$Location.Description)
  lst.date <- unique(df.crime_loc$Date)

  #order Month and Date
  lst.month <-  mixedsort(lst.month)
  lst.date <-  mixedsort(lst.date)
  
  # convert to numeric
  lst.month <-  as.numeric(lst.month)
  df.crime$Month <- as.numeric(df.crime$Month)


  # ------ CREATE A SHINY APP -------------------------------------

  # Define UI for application that plots features of Crime
  # Reactivity using  files Crime data

  ui <- fluidPage(
  
  # formatting of the titlePanel heading
  titlePanel(h1(div(HTML("<em>Crime In Chicago(USA) - 2018</em>")),
                style='background-color:#A1342C;
                      padding-left: 15px; color:#EAEDED ;padding-left:350px;font-size:30px')),
  
  # formatting of the tablabel 
  tags$style(HTML("
     .tabbable > .nav > li > a[data-value='Freq of Crime - Month & Type'] {background-color: #808080;   color:black}
     .tabbable > .nav > li > a[data-value='Freq of Crime - Location'] {background-color:#83C5F0;   color:black}
     .tabbable > .nav > li > a[data-value='Relation - Hour & Crime'] {background-color: #808080;   color:black}
     .tabbable > .nav > li > a[data-value='Locate Crime Area'] {background-color: #83C5F0;   color:black}
   ")),
  
  #create 4 tabs
  tabsetPanel(
    
    #tab1
    tabPanel( "Freq of Crime - Month & Type",
              
              # formatting of tabPanel heading
              h2(div(HTML("<em> Freqency of Crime by Month & Crime Type </em>")), align = 'center',
                 style= 'background-color:#F2F3F4; color:#2C3E50 ;padding-left:70px;font-size: 20px'),
              
              # Sidebar layout with a input and output definitions
              sidebarLayout( 
                sidebarPanel( width = 3,
                              h6(HTML(paste0("<b> *Uncheck Box to use slidebar</b>"))),
                              sliderInput("month_select", "Select Month", 1, 12, step = 1, value = c(1,12)),
                              selectInput( 
                                inputId = "type_select",
                                label = "Select Crime Type" ,
                                choices = (lst.type),
                                selected = "THEFT" ),
                              h5(HTML(paste0("<b> Check Box: See all Data in Stacked Bar Chart </b>"))),
                              checkboxInput( "selectall", "SELECT" ,FALSE)),
                
                # Outputs variables
                mainPanel( width = 40,
                           mainPanel( plotlyOutput("bar_1")),
                           column(12,htmlOutput(outputId = "val_tab1")))
              )
    ),
    
    #tab2
    tabPanel( "Freq of Crime - Location" , 
              # formatting of tabPanel heading
              h2(div(HTML("<em> Frequency of Crime by Location </em>")), align = 'center',
                 style= 'background-color:#F2F3F4; color:#2C3E50 ;padding-left:70px;font-size: 20px'),
              
              # Sidebar layout with a input and output definitions
              sidebarLayout( 
                sidebarPanel( width = 3,
                              sliderInput("slide2", "N: Top Locations of Crime ", 1, 132, step =1, value = 20),
                              h5(HTML(paste0("Max Locations: 132")))
                              ),
                
                # Outputs variables
                mainPanel( width = 40,
                           mainPanel( plotlyOutput("bar_2")),
                           column(12,htmlOutput(outputId = "val_tab21"))
                )
              )
    ),
    
    #tab3
    tabPanel( "Relation - Hour & Crime",
              h2(div(HTML("<em> Frequency of Crime by Type and Hour it Occured </em>")), align = 'center',
                 style= 'background-color:#F2F3F4; color:#2C3E50 ;padding-left:70px;font-size: 20px'),
              
              # Sidebar layout with a input and output definitions
              sidebarLayout( 
                sidebarPanel( width = 2,
                              h6(HTML(paste0("<b> *Check/Uncheck Box: </b>"," Uncheck Box to use SliderBar"))), 
                              checkboxInput( "selectall3" , "All Data" ,FALSE ),
                              sliderInput("slide1", "Select Frequency Range", 1, 5000,step =1, value = c(500,4500)),
                              h6(HTML(paste0("Min/Max: 1 - 4548")))
                               ),
                
                # Outputs variables
                mainPanel( width = 10,
                           plotlyOutput("heatmap_1"),
                           column(12, htmlOutput(outputId = "val3")))
              )
    ),
    
    #tab4
    tabPanel( "Locate Crime Area" ,
              h2(div(HTML("<em> Navigate using Date, The location of the Crime by Date</em>")), align = 'center',
                 style= 'background-color:#F2F3F4; color:#2C3E50 ;padding-left:70px;font-size: 20px'),
              
              # Sidebar layout with a input and output definitions
              sidebarLayout( 
                sidebarPanel( width = 2,
                              selectInput( 
                                inputId = "date_select",
                                label = "Date From:" ,
                                choices = c(lst.date),
                                selected =  NULL
                              ),
                              
                              selectInput( 
                                inputId = "date_select1",
                                label = "Date To:" ,
                                choices = c(lst.date),
                                selected =  NULL
                              ),
                              h5("Select Date Range from dropdown")
                ),
                
                # Outputs variables
                mainPanel( width = 10,
                           height = 15,
                           leafletOutput("map_1"),  
                           htmlOutput(outputId = "val4")
                )
              )
    )
    
  ) 
  
  )

  # CREATE SERVER FUCNTION
  # Define server function required to create graph and table

  server <- function(input, output, session) {
  
  # TAB1 PLOTTING
  
  output$bar_1 <- renderPlotly({
    
    req(input$month_select)
    
    if ( input$selectall == TRUE) {
      df.crime%>%
        dplyr::select(Month,Primary.Type)%>%
        dplyr::group_by(Month,Primary.Type)%>%
        dplyr::summarise(Frequency=n())->by_Month
      
        by_Month<-arrange(by_Month,-Frequency)
      
        plot_ly(by_Month, x= ~Month, y = ~Frequency, color = ~Primary.Type,  type = "bar", alpha = 0.9) %>%
        layout(title = "Barplot : Crime Type | THEFT | Frequency by Month", xaxis = list(title ="Month", yaxis = list(title = "Frequency")))%>% layout(barmode = "stack")
    }
    
    else {
      data1 = filter(df.crime, Month >= input$month_select[1] & Month <= input$month_select[2] & Primary.Type %in% input$type_select)
      data1%>%
        dplyr::select(Month,Primary.Type)%>%
        dplyr::group_by(Month,Primary.Type)%>%
        dplyr::summarise(Frequency=n())->by_Month
      by_Month<-arrange(by_Month,-Frequency)
      plot_ly(by_Month, x= ~Month, y = ~Frequency, color = ~Primary.Type, type = "bar", alpha = 0.9) %>%
        layout(title = "Barplot : Crime Type | Frequency by Month",  
               xaxis = list(title ="Month", yaxis = list(title = "Frequency")))%>% layout(barmode = "dodge")
    }
  
  })
  
  output$val_tab1 <- renderUI({
    div(HTML(
      paste0("<b> ***Note: </b>",
             " This is an interactive chart, go to any point to know the frequency of crime occured each month" ,'<br>',
             "<b> Intersting point: </b>", "Most of the crime occured was theft",'<br>'
      )),  
      style='background-color:#F2F3F4  ;
                      padding-left: 15px; color:black ;padding-left:20px;', align = 'center')
    
  })
  
  # TAB2 OUTPUT
  
  #Frequency of Crime ooccured based on locations
  output$bar_2 <- renderPlotly({
    
    by_location <- df.crime_loc %>% dplyr::group_by(Location.Description) %>% dplyr::summarise(Frequency = n()) %>% arrange(desc(Frequency))
    by_location$Location.Description <- tolower(by_location$Location.Description)
    
    p <-  ggplot(by_location[1:(input$slide2), ],aes(x=reorder(Location.Description,-Frequency),y=Frequency)) +
      geom_bar(stat="identity",fill='cyan',alpha=0.7)+
      labs(title = "Barplot : Location | Frequency of Crime", x= "Location",y = "Frequency")+
      theme(axis.text.x = element_text(angle=45,hjust=1, size = 7),
            plot.title = element_text(hjust=0.5),
            panel.grid.major = element_line(colour = "grey40"),
            panel.grid.minor = element_blank(),
            panel.background = element_rect(fill = "grey40")
      )
    
    ggplotly(p)
    
  })
  
  # Display the Note on Map below
  output$val_tab21 <- renderUI({
    div(HTML(
      paste0("<b> ***Note: </b>",
             "This is an interactive chart, go to any point to know the value of frequency where crime occured"
      )),  
      style='background-color:#F2F3F4  ;
                      padding-left: 15px; color:black ;padding-left:20px;', align = 'center')
  })

  # TAB3 
  output$heatmap_1 <- renderPlotly({
  
    if(input$selectall3 == TRUE){
      df.crime%>%
        dplyr::select(Time,Primary.Type)%>%
        dplyr::group_by(Time,Primary.Type)%>%
        dplyr::summarise(Frequency=n())->by_Time
      
      by_Time<-arrange(by_Time,-Frequency)
      by_Time$Primary.Type <- tolower(by_Time$Primary.Type)
      
      q <- ggplot(by_Time, aes(Time, Primary.Type)) +
        geom_tile(aes(fill = Frequency),colour = "white") +
        scale_fill_gradient(low = "gray33",high = "cyan") +
        labs(title = "HeatMap : Type | Hour | Frequency of Crime", x= "Hour",y = "Crime Type", fill = "Frequency")+
        theme(plot.title = element_text(hjust=0.5),
              panel.grid.major = element_line(colour = "azure3"),
              panel.grid.minor = element_blank(),
              panel.background = element_rect(fill = "white"))
      
      ggplotly(q)
    }
    else{
      df.crime%>%
        dplyr::select(Time,Primary.Type)%>%
        dplyr::group_by(Time,Primary.Type)%>%
        dplyr::summarise(Frequency=n())->by_Time
       by_Time<-arrange(by_Time,-Frequency)
       
      by_Time$Primary.Type <- tolower(by_Time$Primary.Type)
      
      # slider input
      by_Time_sam <- by_Time %>% filter(Frequency >=input$slide1[1] & Frequency <= input$slide1[2])
      
      q <- ggplot(by_Time_sam, aes(Time, Primary.Type)) +
        geom_tile(aes(fill = Frequency),colour = "white") +
        scale_fill_gradient(low = "gray33",high = "cyan") +
        labs(title = "HeatMap : Type | Hour | Frequency of Crime", x= "Hour of Crime",y = "Crime Type", fill = "Frequency")+
        theme(plot.title = element_text(hjust=0.5),
              panel.grid.major = element_line(colour = "azure3"),
              panel.grid.minor = element_blank(),
              panel.background = element_rect(fill = "white"))
      
      ggplotly(q)
    }
  })
  output$val3 <- renderUI({
    div(HTML(
      paste0("<b> ***Note: </b>",
             " This is an interactive chart, go to any point to know the value of frequency crime occured each hour" ,'<br>',
             " 00-01 means crime occured in first hour" ,'<br>',
             "<b> Intersting point: </b>", "Most of the crime occured was theft",'<br>'
      )),  
      style='background-color:#F2F3F4  ;
                      padding-left: 15px; color:black ;padding-left:20px;', align = 'center')
    
  })
  
  # TAB4 OUTPUT
  output$map_1 <- renderLeaflet({
    
    # Set the initial stage of the Map
    initial_lat = 41.8781
    initial_lng = -87.66892109
    initial_zoom = 4
    
    # check if enter dates are valid, if valid display the map
    if (input$date_select<=input$date_select1) {
      data1<- df.crime_map%>% filter((Date>=input$date_select & Date<=input$date_select1))
      Map_Output <- leaflet(data1) %>%
        setView(lat = initial_lat, lng = initial_lng, zoom = initial_zoom) %>%
        addTiles() %>%
        addMarkers(lng = ~Longitude,
                   lat = ~Latitude,
                   clusterOptions = markerClusterOptions() ,
                   popup = ~ paste( 
                     'Date: ', DateTime, '<br/>',
                     'Location: ', Location.Description , '<br/>'  ,
                     'Type: ', Primary.Type, '<br/>', 
                     'Arrest: ', Arrest , '<br/>',
                     'Case Number: ', Case.Number, '<br/>', 
                     'Block: ', Block , '<br/>')
                   #label = ~htmlEscape(Date)
                   #label = ~as.character(Date)
        )
      Map_Output
    }
    else {
      validate(
        need(input$data != "", "Enter Valid Date! 'Date To' is less than 'Date From'")
      )
    }
  })
  
  # Display the Note on Map below
  output$val4 <- renderUI({
    div(HTML(
      paste0("<b> ***Note: </b>",
             "<b> Click on cluster to expand locations of crime and Clicking on the location mark tell details about the crime </b>", '<br>',
             " The graph output is in form of clusters and shows the Frequency of cases occured in that area." ,'<br>',
             "Zoom in will lead to a specific location of the crime " ,'<br>'
      )),  
      style='background-color:#F2D7D5  ;
                      padding-left: 15px; color:black ;padding-left:20px;', align = 'center')
  })
  
}


# ---------------------RUN THE APP-------------------------------------

  


