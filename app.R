library(sp)
library(rgdal)

library(tidyr)
library(dplyr)
library(lubridate)
library(maps)
library(readr)
library(leaflet)
library(htmltools)
library(shiny)
library(plotly)
library(shinythemes)
library(DT)
library(thematic)
thematic_shiny()
###########
# Data Download #
url = 'https://github.com/owid/covid-19-data/raw/master/public/data/owid-covid-data.csv'
covid <- read.csv(url(url))
covid <- covid[c('iso_code','location','date','total_cases','new_cases','total_deaths','new_deaths','total_deaths_per_million','population')]
covid$date <- ymd(covid$date)
covid$total_deaths_per_million <- round(covid$total_deaths_per_million,0)

list <- covid %>% group_by(iso_code)%>% summarise(total_death_per_million = max(total_deaths_per_million,na.rm = TRUE))



#### Preprocessing

quant <-quantile(list$total_death_per_million,c(.25,.60,.947,1),na.rm = TRUE)
list <- list %>% mutate(color = ifelse(total_death_per_million<=quant[1],"#83AF9B",
                                       ifelse(total_death_per_million<=quant[2],"#C8C8A9",
                                              ifelse(total_death_per_million<=quant[3],"#F9CDAD",
                                                     ifelse(total_death_per_million<=quant[4],"#FE4365",
                                                            "#000000")))))

Last <- covid %>% group_by(iso_code) %>% summarise(date=max(date))
Last_Deaths <- covid %>% dplyr::select(iso_code,date,new_cases,new_deaths,total_cases,total_deaths)

latest_data <- left_join(Last,Last_Deaths)

countries <- readOGR("covid", "TM_WORLD_BORDERS-0.2")

merge <- merge(countries, list, by.x ="ISO3" , by.y = "iso_code")
merge$color[is.na(merge$color)] <- "#C8C8A9"
merge <- merge(merge, latest_data, by.x ="ISO3" , by.y = "iso_code")

labs <- lapply(seq(nrow(merge@data)), function(i) {
  paste0( '<H2>', merge@data[i, "NAME"],'</H2><br/><strong>Last Update:',merge@data[i,"date"],"<br/>",
          '<p> <Strong> New Deaths:</strong>',merge@data[i,"new_deaths"],"</p><p> New Cases:",merge@data[i,"new_cases"],"</p>
          <p> Total Cases:",merge@data[i,"total_cases"],"</p><P>Total Deaths:",merge@data[i,"total_deaths"],"</p><p>Deaths Per 1 Million:",merge@data[i,"total_death_per_million"],"</p>" )
})



## User Interface

ui <- navbarPage("Covid Statistics",theme = shinytheme("superhero"),
            tabPanel("Interactive Map", leafletOutput('leafty',height=1000)),
            tabPanel("Data",selectInput('location','Filter by Country:',unique(covid$location),selected = "Saudi Arabia"),
                     downloadButton(outputId = 'download_data', label = 'Download'),
                     p("Showing the top 15 rows:"),
                     plotOutput('plot'),
                     tableOutput('table'))
                     ,
            tabPanel("About"))


## Server

server <- function(input,output){
  
  
  #### Map
  output$leafty <- renderLeaflet(leaflet(data = merge) %>% addProviderTiles(providers$CartoDB.Positron) %>% setView(lat = 23.8859,lng=45.0792,zoom = 4) %>%
    addPolygons(fillColor = ~color, stroke = FALSE) %>%addLegend(position = 'bottomright',
                                                                 colors = c("#83AF9B","#C8C8A9","#F9CDAD","#FE4365"),
                                                                 labels=  c('Bellow 5',
                                                                            'Between 5 and 50 ',
                                                                            'Between 50 and 500 ',
                                                                            'Above 500 '),
                                                                 title = 'Death per 1 million Capita') %>% 
    addCircleMarkers(data = merge,lng=~LON,lat=~LAT,radius=~(new_deaths)^(1/5)*5,weight = 1,fillOpacity = 0.5,color = ~color,
                     label =lapply(labs, htmltools::HTML)))
  
  ### Data 
  
  filtered_data <- reactive({
    # Filter the data (copied from previous exercise)
    data <- covid
    data <- subset(
      data,
      location==input$location
    )
    
    data
  })
  
  
  
  output$download_data <- downloadHandler(
    filename =  "filtered_data.csv",
    content = function(file) {
      # The code for filtering the data is copied from the
      # renderTable() function
      data <- filtered_data()
      
      
      # Write the filtered data into a CSV file
      write.csv(data, file, row.names = FALSE)
    })
  
  
  
      
  
  
  
  
  

  output$table <- renderTable({
    data <- filtered_data()
    
    data$date <- as.character(data$date)
    head(data[order(-data$total_cases),],15)
  })
  
  
  
  
  output$plot <- renderPlot({data <- filtered_data()
                       ggplot(data,aes(x=date,y=total_cases))+
                         geom_line()
  
    
  
  })
  
  
  
}


shinyApp(ui=ui,server=server)