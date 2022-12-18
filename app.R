library(shiny)
library(leaflet)
library(tidyverse)
library(dplyr)

stop_times_11_21 <- read.csv("stop_times0.txt")
stop_times_12_21 <- read.csv("stop_times1.txt")
stop_times_01_22 <- read.csv("stop_times2.txt")
stop_times_02_22 <- read.csv("stop_times3.txt")
stop_times_03_22_1 <- read.csv("stop_times4_1.txt")
stop_times_03_22_2 <- read.csv("stop_times4_2.txt")
stop_times_03_22_3 <- read.csv("stop_times4_3.txt")
stop_times_03_22 <- rbind(stop_times_03_22_1, stop_times_03_22_2, stop_times_03_22_3)
stop_times_04_22 <- read.csv("stop_times5.txt")
stop_times_05_22 <- read.csv("stop_times6.txt")
stop_times_06_22 <- read.csv("stop_times7.txt")
stop_times_07_22 <- read.csv("stop_times8.txt")
stop_times_08_22 <- read.csv("stop_times9.txt")
stop_times_09_22 <- read.csv("stop_times10.txt")
stop_times_10_22 <- read.csv("stop_times11.txt")

checkpoint <- read.csv("checkpoints0.txt")

stop_times_11_21 <- inner_join(stop_times_11_21,checkpoint) %>% mutate(Season = 'Winter')
stop_times_12_21 <- inner_join(stop_times_12_21,checkpoint) %>% mutate(Season = 'Winter')
stop_times_01_22 <- inner_join(stop_times_01_22,checkpoint) %>% mutate(Season = 'Winter')
stop_times_02_22 <- inner_join(stop_times_02_22,checkpoint) %>% mutate(Season = 'Winter')
stop_times_03_22 <- inner_join(stop_times_03_22,checkpoint) %>% mutate(Season = 'Spring')
stop_times_04_22 <- inner_join(stop_times_04_22,checkpoint) %>% mutate(Season = 'Spring')
stop_times_05_22 <- inner_join(stop_times_05_22,checkpoint) %>% mutate(Season = 'Spring')
stop_times_06_22 <- inner_join(stop_times_06_22,checkpoint) %>% mutate(Season = 'Summer')
stop_times_07_22 <- inner_join(stop_times_07_22,checkpoint) %>% mutate(Season = 'Summer')
stop_times_08_22 <- inner_join(stop_times_08_22,checkpoint) %>% mutate(Season = 'Summer')
stop_times_09_22 <- inner_join(stop_times_09_22,checkpoint) %>% mutate(Season = 'Fall')
stop_times_10_22 <- inner_join(stop_times_10_22,checkpoint) %>% mutate(Season = 'Fall')

stop_time <- rbind(stop_times_11_21,stop_times_12_21,stop_times_01_22,stop_times_02_22,stop_times_03_22,stop_times_04_22,stop_times_05_22,stop_times_06_22,stop_times_07_22,stop_times_08_22,stop_times_09_22,stop_times_10_22)

stop_1 <- read.csv("stops1.txt")
stop_2 <- read.csv("stops2.txt")
stop_3 <- read.csv("stops3.txt")
stop_4 <- read.csv("stops4.txt")
stop_5 <- read.csv("stops5.txt")
stop_6 <- read.csv("stops6.txt")
stop_7 <- read.csv("stops7.txt")
stop_8 <- read.csv("stops8.txt")
stop_9 <- read.csv("stops9.txt")
stop_10 <- read.csv("stops10.txt")
stop_11 <- read.csv("stops11.txt")
stop_12 <- read.csv("stops12.txt")
stop_13 <- read.csv("stops13.txt")
stop_14 <- read.csv("stops14.txt")

v <- rbind(stop_1,stop_2,stop_3,stop_4,stop_5,stop_6,stop_7,stop_8,stop_9,stop_10,stop_11,stop_12,stop_13,stop_14)
v <- v %>% select(stop_name, stop_lat, stop_lon)
v <- v[!duplicated(v[ ,c("stop_name")]),]
names(v)[1] ="checkpoint_name"
v <- inner_join(stop_time,v) %>% distinct()

s1 <- read.csv("trips0.txt")
s2 <- read.csv("trips1.txt")
s3 <- read.csv("trips2.txt")
s4 <- read.csv("trips3.txt")
s5_1 <- read.csv("trips4_1.txt")
s5_2 <- read.csv("trips4_2.txt")
s5_3 <- read.csv("trips4_3.txt")
s5 <- rbind(s5_1, s5_2, s5_3)
s6 <- read.csv("trips5.txt")
s7 <- read.csv("trips6.txt")
s8 <- read.csv("trips7.txt")
s9 <- read.csv("trips8.txt")
s10 <- read.csv("trips9.txt")
s11 <- read.csv("trips10.txt")
s12 <- read.csv("trips11.txt")
s <- rbind(s1,s2,s3,s4,s5,s6,s7,s8,s9,s10,s11,s12)
s <- s %>% select(route_id,trip_id)

k <- inner_join(v,s) %>% distinct()
k_new <- k %>% filter(route_id == 'Orange'|route_id == 'Red'|route_id == 'Green-B'|route_id == 'Green-C'|route_id == 'Green-D'|route_id == 'Green-E'|route_id == 'Mattapan')
K_new2 <- k_new %>% select(arrival_time,departure_time,checkpoint_name,Season,stop_lat,stop_lon,route_id)


Season <- unique(K_new2$Season)
Transport <- unique(K_new2$route_id)
Time <- unique(K_new2$arrival_time)

ui <- fluidPage(
  titlePanel("Boston Transportation (MBTA)"),
  sidebarLayout(
    mainPanel(
      tabsetPanel(type = "tabs",
                  tabPanel("Map",leafletOutput("map")),
                  tabPanel("Table",tableOutput("table"))
      )
    ),
    sidebarPanel(
      selectInput("Time", "Please select a time to go?",Time),
      br(),
      selectInput("Transport", "Which lines you want to select?",Transport),
      br()
    )
  )
)

server <- function(input,output){
  newdf <- reactive({
    K_new2 %>% filter(route_id%in%input$Transport, arrival_time%in%input$Time)
  })
  output$map <- renderLeaflet({
    leaflet() %>%
      addTiles() %>%
      addMarkers(lat = newdf()$stop_lat, lng = newdf()$stop_lon,
                 popup= newdf()$checkpoint_name)
  })
  output$table <- renderTable({newdf()})
}

shinyApp(ui = ui, server = server)
