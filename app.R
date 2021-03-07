rm(list=ls())

library(shiny)
#webscrape packages
library(rvest)
#data processing packages
library(tidyverse)
#mapping packages
library(leaflet)
#geocoding packages
library(ggmap)
library(geosphere)

register_google(key = "", write = T)

pg1 <- read_html("https://pharmacy.giantfood.com/dc/washington")
pg2 <- read_html("https://local.pharmacy.safeway.com/dc/washington.html")
pg3 <- read_html("https://coronavirus.dc.gov/vaccinatedc")

giants <- pg1 %>% 
  html_nodes("h2 a") %>% 
  html_text() %>%
  as.data.frame() %>%
  setNames("address")

giants$center <- "Giant Pharmacy"
giants$links <- "https://giantfood.com/pages/covid-info?_loopback=1"
safeways <- pg2 %>% 
  html_nodes("article h2") %>% 
  html_text() %>%
  as.data.frame() %>%
  setNames("address")

safeways$center <- "Safeway Pharmacy"
safeways$links <- "https://www.safeway.com/pharmacy/covid-19.html?icmpid=swy_yxt_r6_covc_ih"

#bind pharmacy dfs together
df <- rbind(safeways, giants)
df$type <- "Pharmacy"

df_geo <- geocode(df$address)
df <- data.frame(df, df_geo)

df <- df %>%
  select(-address)

address <- do.call(rbind,
                   lapply(1:nrow(df),
                          function(i)revgeocode(as.numeric(df[i,4:5]))))

df <-data.frame(df, address)
df$address <- gsub(", USA", "", df$address)

###extract vaccine sites from DC Gov
links <- pg3 %>% 
  html_nodes(., ".rteindent1 a") %>% 
  html_attr(., "href")

center <- pg3 %>% 
  html_nodes(., ".rteindent1 a") %>% 
  html_text

df1 <- data.frame(links = links, center = center, 
                 stringsAsFactors = F)

##remove informational links (last 5 rows)
df1 <- df1 %>% 
  head(-5)
##include both Kaiser hospitals (Northwest and Capitol Hill)
df1$center[df1$center == "kp.org/DCvaccine"] <- "Kaiser (Northwest)"
df1 <- df1 %>% 
  add_row(center = "Kaiser (Capitol Hill)", 
          links = "kp.org/DCvaccine")

df1$input <- paste0(df1$center, ", DC")

df1_geo <- geocode(df1$input)

df1 <- data.frame(df1, df1_geo)
address <- do.call(rbind,
                   lapply(1:nrow(df1),
                          function(i)revgeocode(as.numeric(df1[i,4:5]))))
df1 <- data.frame(df1, address)

df1$address <- gsub(", USA", "", df1$address)
df1<- df1 %>%
select(-input)

df1$type <- "Healthcare provider"

df <- rbind(df, df1)
df$address <- gsub(", Washington", "", df$address)

#create urls for popups
df <- df %>% 
  mutate(tag = paste0("<a href=", links,">",'Click here to register', "</a>"))
#generate contents of popups
df$popup_text <-  
  paste0('<strong>', df$center, '</strong>',
         '<br>', df$address, '<br/>', df$tag) %>% 
  lapply(htmltools::HTML)

####begin shiny app
ui <- shinyUI(fluidPage(
  sidebarLayout(
    sidebarPanel(
      textInput(inputId = "userlocation",
                label = "Enter address",
                value = "",
                placeholder = "Include city"),
      actionButton("go","Submit"),
      p(""),
      HTML("<p>This interactive map shows the vaccination sites listed on the
      <a href='https://coronavirus.dc.gov/vaccinatedc'>DC Government's coronavirus page</a>
      as well as pharmacies that are distributing the vaccine.
      The map can also be used to locate the closest center to your 
      address.</p>"), 
      p("Note that vaccination registration is currently restricted to 
      residents who are 65 and older; 18-64 with a qualifying health condition; or
        a member of an eligible workforce.")
      , width = 3),
    mainPanel(
      h3("DC COVID-19 Vaccination Site Map"),
      leafletOutput('map' ,height = "500px"), width = 9
    ),
  )
)
)

server <- shinyServer(function(input, output, session) {
  
  output$map <- renderLeaflet({
    groups = unique(df$type)

    
    map = leaflet() %>% 
      addProviderTiles(providers$CartoDB.Positron)
    for(x in groups){
      d = df[df$type == x, ]
    map = map %>%
        addCircleMarkers(data = d, 
                         lng = ~lon, 
                         lat = ~lat, 
                         popup = d$popup_text,
                         radius = 8,
                         group = x)
    }
    map %>% addLayersControl(overlayGroups = groups)

  })
  
  closest_address <- eventReactive(input$go,{
    
    user_coord <- geocode(input$userlocation)
    dist <- distm(x = matrix(data = c(df$lon, df$lat), ncol = 2), 
                  y = c(lon = user_coord$lon, lat = user_coord$lat), 
                  fun = distHaversine)
    newdf <- cbind(df, dist)
    newdf <- newdf[which.min(dist),]
    
  })
  
  observeEvent(input$go, {
    
    leafletProxy('map', session) %>% 
      addMarkers(data = closest_address(),
                 lng = ~lon,
                 lat = ~lat,
                 popup = ~popup_text) %>%
      setView(lng = closest_address()$lon,
              lat = closest_address()$lat, 
              zoom = 13)
    
  })
  
})

shinyApp(ui = ui, server = server)
