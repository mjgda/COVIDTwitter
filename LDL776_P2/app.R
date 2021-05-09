#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(tidyverse)
library(ggplot2)
library(plotly)


TweetWords <- read.csv(url("https://raw.githubusercontent.com/mjgda/COVIDTwitter/main/FFF.csv"))
OriginCase <- read.csv(url("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_US.csv"))
OriginCase<- OriginCase %>% filter(OriginCase[,ncol(OriginCase)]>5000)

Counties<-select(OriginCase,c("Admin2","Province_State","Country_Region","Combined_Key"))
datedata<-select(OriginCase,-c("UID","iso2","iso3","code3","FIPS","Lat","Long_","Admin2","Combined_Key","Province_State","Country_Region"))

tform<-data.frame(Admin2=character(),Province_State=character(),Country_Region=character(),
                  Combined_Key=character(),Date=as.Date(character()),Cases=integer(),stringsAsFactors=FALSE)

TweetWords$date<-as.Date(TweetWords$date,format = "%m/%d/%Y")

for (name in names(datedata)){
  Date<-as.Date(substr(name,start=2,stop=nchar(name)),format = "%m.%d.%y")
  dateset<-cbind(Counties,Date,select(datedata,name) %>% rename(Cases=name))
  tform<-rbind(tform,dateset)
  
}


tform<-tform %>% rename(County=Admin2, State=Province_State)



# Define UI for application that draws a histogram
ui <- fluidPage(

    
    fluidRow(
      column(12,"COVID Cases and Tweets from Most Affected Cities")
    ),
    fluidRow(
      column(1,
             checkboxInput("matchup",
                         "Only Counties with Tweet Word Frequency",
                         TRUE)
      ),
      column(2,
             selectInput("State",
                         "State",
                         unique(tform$State),
                         selected='Texas',
                         multiple=TRUE)
      ),
      column(3,
             selectInput("County",
                         "County",
                         unique(tform$County),
                         multiple=TRUE)
      ),
      
      column(3,
             sliderInput("DatesMerge",
                         "Dates:",
                         min = as.Date(min(tform$Date),"%Y-%m-%d"),
                         max = as.Date(max(tform$Date),"%Y-%m-%d"),
                         value=c(as.Date(min(tform$Date),"%Y-%m-%d"),
                                 as.Date(max(tform$Date),"%Y-%m-%d")),
                         timeFormat="%m.%d.%y")
      ),
      column(3,
             selectInput("View",
                         "Level",
                         c("State","County"),
                         selected ="County")
      )
    ),
    fluidRow(
      column(12,
             tabsetPanel(
               tabPanel("Time Series",plotlyOutput("TimeSeries")),
               tabPanel("Most Frequent Words in Tweets",DT::dataTableOutput(outputId="datasheet")))
      )
      
    )
)

    
 

# Define server logic required to draw a histogram
server <- function(input, output) {


    
    output$TimeSeries <- renderPlotly({
      Statefilter<-if(length(input$State)==0){unique(tform$State)}else{input$State}
      Countyfilter<-if(length(input$County)==0){unique(tform$County)}else{input$County}
      Countyfilter2<-if(input$matchup==TRUE){unique(TweetWords$county)}else{unique(tform$County)}
      ggplot(data=tform %>% filter(State %in% Statefilter & County %in% Countyfilter & County %in% Countyfilter2 & between(Date, input$DatesMerge[1], input$DatesMerge[2])),aes(Date,Cases,color=get(input$View)))+
        geom_line(show.legend = FALSE)
    })
    
    output$datasheet<-DT::renderDataTable({
      Statefilter<-if(length(input$State)==0){unique(TweetWords$state)}else{input$State}
      Countyfilter<-if(length(input$County)==0){unique(TweetWords$county)}else{input$County}
      #DT::datatable(data=TweetWords,
      #              options=list(pageLength= 20),
      #              rownames=FALSE)
      DT::datatable(data=TweetWords %>% filter(state %in% Statefilter & county %in% Countyfilter & between(date, input$DatesMerge[1], input$DatesMerge[2]))  
                    %>% spread(key = threshold,value = frequency),
                    options=list(pageLength= 20),
                    caption="Columns 1-4 are the days when the County hit 1K, 30K, 75K, and 100K COVID Cases. Count is how many times word appeared in last 3,000 tweets that day.",
                    rownames=FALSE)
      
    })
    
}

# Run the application 
shinyApp(ui = ui, server = server)
