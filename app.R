library(shiny)
library(tidyverse)
library(leaflet)
library(shinyWidgets)
library(plotly)
library(lubridate)
#import and preprocessing

len <- nchar(getwd())
if (substr(getwd(),len-4,len) != "/data"){
  setwd(dir = "data/")
}

source(file = "updateDB.R", local = TRUE)
load("updatedRocketsDB.RData")


icons <- awesomeIcons(
  icon = "fa-rocket",
  iconColor = "GhostWhite",
  library = 'fa'
)

yearsChoices <- rev(unique(format(previousRocketsDF$NET_DATE, "%Y")))


plotIndexes = list()
for (i in unique(1:length(yearsChoices)%/%10)){ #create breaks list for plotting launches/years
  if (i==1){
    plotIndexes <- append(plotIndexes, yearsChoices[i])
  }
  plotIndexes <- append(plotIndexes, yearsChoices[10*i])  
}
plotIndexes <- append(plotIndexes, yearsChoices[length(yearsChoices)])
plotIndexes <- unique(plotIndexes) #because every year divisible by 10 (2030, 2040, etc) last yearsChoices[10*i] = yearsChoices[length(yearsChoices)]

get_names <- function(countries){ #country names are included in the launch location data at the end
  clist = list()
  for (c in countries) {
    pays = str_split(c, ", ") #we split the string of launch location
    pays = pays[[1]][length(pays[[1]])] #we always take the last element of the returned list
    clist = append(clist, pays) #and we create a list of country names following the order
  }
  return(clist)
}

#build UI

ui <- bootstrapPage(
  navbarPage("Rocket Launches", id="nav",
             tabPanel("Previous Data Map",
                      
                      div(class="outer",
                          
                          
                          tags$style(
                            type = "text/css",
                            "html, body {width:100%;height:100%} 
                            #controls {background-color: white; padding: 20px 20px 20px;}
                            .leaflet-container {background-color: white !important;}
                            div.outer {
                                        position: fixed;
                                        top: 41px;
                                        left: 0;
                                        right: 0;
                                        bottom: 0;
                                        overflow: hidden;
                                        padding: 0;}"
                          ),
                          
                          #display the map 
                          leafletOutput("rocketMap",width = "100%", height="100%"),
                          
                          # panel with a slider input
                          absolutePanel(id = "controls", class = "panel panel-default", fixed = TRUE,
                                        draggable = FALSE, top = 60, left = "auto", right = 20, bottom = "auto",
                                        width = 330, height = "auto", 
                                        sliderTextInput("selYear",
                                                        "check for a year:",
                                                        choices = yearsChoices,
                                                        selected = yearsChoices[length(yearsChoices)],
                                        ),
                                        plotOutput("numHistory", height= 250),
                                        plotlyOutput("histHistory",width=300, height = 400)
                          ),
                      ),
             ),
             
             # Upcoming map
             tabPanel("Upcoming Launches Map",
                      
                      div(class="outer",
                          
                          
                          tags$style(
                            type = "text/css",
                            "html, body {width:100%;height:100%} 
                            .leaflet-container {background-color: white !important;}
                            div.outer {
                                        position: fixed;
                                        top: 41px;
                                        left: 0;
                                        right: 0;
                                        bottom: 0;
                                        overflow: hidden;
                                        padding: 0;}"
                          ),
                          
                          #display the map 
                          leafletOutput("upcomingRocketMap",width = "100%", height="100%"),
                          
                          
                      ),
             ),
             
             
  ),
  
  
)

#Server side
garbageCounter <- 0

server <- function(input, output, session) {
  #Building the map
  filteredData <- reactive({
    previousRocketsDF %>%
      mutate(YEAR = (format(previousRocketsDF$NET_DATE, "%Y"))) %>%
      filter(YEAR == input$selYear)
  })
  output$rocketMap <- renderLeaflet({
    leaflet(previousRocketsDF) %>%
      addProviderTiles(providers$CartoDB.Positron) %>%
      fitBounds(~min(LONGITUDE), ~min(LATITUDE), ~max(LONGITUDE), ~max(LATITUDE))
  })
  output$upcomingRocketMap <- renderLeaflet({
    leaflet(options = leafletOptions(preferCanvas = TRUE), upcomingRocketsDF) %>%
      addProviderTiles(providers$CartoDB.Positron) %>%
      fitBounds(~min(LONGITUDE), ~min(LATITUDE), ~max(LONGITUDE), ~max(LATITUDE)) %>%
      addAwesomeMarkers(icon = icons, label = sprintf("Mission name: %s <br/>Type: %s <br/>",
                                                      as.character(upcomingRocketsDF$MISSION),
                                                      as.character(upcomingRocketsDF$MISSION_TYPE)
                                                      ) %>%
                          lapply(htmltools::HTML))
  })
  observe({
    leafletProxy("rocketMap", data = filteredData()) %>%
      clearMarkers() %>%
      addProviderTiles(providers$CartoDB.Positron) %>%
      addAwesomeMarkers(icon = icons, label = sprintf("Mission name: %s <br/>Type: %s <br/>Date of launch: %s <br/>Result: %s", 
                                                      as.character(filteredData()$MISSION), 
                                                      as.character(filteredData()$MISSION_TYPE), 
                                                      as.character(filteredData()$NET_DATE), 
                                                      as.character(filteredData()$LAUNCH_RESULT)) %>%
                          lapply(htmltools::HTML))
  })
  observe({
    invalidateLater(1000, session) # every second we repeat: garbage takes +1, when equals 10 then clearMarkers and reset garbageCounter 
    if (garbageCounter < 10){
      Systime = with_tz(Sys.time(), "UTC")
      upcomingfilteredData <- upcomingRocketsDF %>%
        mutate(time_to_launch = round(seconds_to_period(difftime(Systime, NET_DATE, units = "secs"))))
      leafletProxy("upcomingRocketMap", data = upcomingfilteredData) %>%
        addAwesomeMarkers(icon = icons, label = sprintf("Mission name: %s <br/>Type: %s <br/>Time to launch: %s <br/>",
                                                        as.character(upcomingfilteredData$MISSION),
                                                        as.character(upcomingfilteredData$MISSION_TYPE),
                                                        as.character(upcomingfilteredData$time_to_launch)) %>%
                            lapply(htmltools::HTML))
      garbageCounter <<- garbageCounter+1
    } else if (garbageCounter == 10){
      Systime = with_tz(Sys.time(), "UTC")
      upcomingfilteredData <- upcomingRocketsDF %>%
        mutate(time_to_launch = round(seconds_to_period(difftime(Systime, NET_DATE, units = "secs"))))
      leafletProxy("upcomingRocketMap", data = upcomingfilteredData) %>%
        clearMarkers() %>%                                                                                # <- IMPORTANT DIFFERENCE
        addAwesomeMarkers(icon = icons, label = sprintf("Mission name: %s <br/>Type: %s <br/>Time to launch: %s <br/>",
                                                        as.character(upcomingfilteredData$MISSION),
                                                        as.character(upcomingfilteredData$MISSION_TYPE),
                                                        as.character(upcomingfilteredData$time_to_launch)) %>%
                            lapply(htmltools::HTML))
      garbageCounter <<- 0
    }
  })
  #Building the line plot
  output$numHistory <- renderPlot({
    selYear_ = input$selYear
    by_years = tibble("year" = format(previousRocketsDF$NET_DATE, "%Y"), "status" = previousRocketsDF$LAUNCH_RESULT)
    by_years %>%
      group_by(year) %>% #create a pivot table by year
      summarise(launch_count = n()) %>% #count the number of launch for each year then plot it
      ggplot(aes(x = year, group=1)) +
      geom_line(aes(y=launch_count), size=1, color="blue", show.legend = "Launches")+
      geom_vline(xintercept = selYear_, color="red") + 
      ylab(label = "launches")+
      scale_x_discrete(breaks = plotIndexes)+
      theme_minimal()
  })
  #Building the hist plot
  output$histHistory <- renderPlotly({
    historyDF = tibble("country" = as.character(get_names(filteredData()$LAUNCH_LOCATION)), "Date" = filteredData()$NET_DATE, "result" = filteredData()$LAUNCH_RESULT)
    wrappedCountries = 
      p <- historyDF %>%
      group_by(country) %>% #we group by country to create a pivot table
      summarise(count = n(), success_rate = round(sum(result == "Success")/n()*100),2)%>% #compute the success rate
      mutate(rank = rank(-count)) %>%#rank them in descending success rate
      filter(rank<=5)%>%
      ggplot(aes(
        y = success_rate,
        x = reorder(country,success_rate),
        fill = country, 
        label=count,
      )
      ) + 
      geom_col()+
      ylab(label="success rate")+
      xlab(label="countries")+
      theme_minimal()+ 
      theme(axis.text.x = element_text(angle=45))
    p <- ggplotly(p, tooltip = c("country", "label", "success_rate"))
    hide_legend(p)
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
