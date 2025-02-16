library(shiny)
library(bslib)
library(shinythemes)
library(shinydashboard)
library(shinyWidgets)
library(DT)
library(leaflet)
library(sf)
library(tidyr)
library(dplyr)
library(sp)
library(dygraphs)
library(tmap)
library(GGally)
library(feasts)
library(tsibble)
library(tidyverse)
library(gridExtra)
library(ggpubr)
library(base64enc)


#https://lt-maynooth2024.shinyapps.io/The_Rain_State/


# Load the GeoJSON file
rainstat <- st_read('weather_stations.geojson', quiet = TRUE)

# Convert to data frame and select relevant columns
rainstat_df <- rainstat %>%
    as.data.frame() %>%
    select(Year, Month, Rainfall, Station)

# Get a list of unique stations
stat_list <- rainstat_df %>%
    distinct(Station) %>%
    pull(Station)



### shiny app
#### TODO ####

### Points are inaccurate
### Add name to the markers
### Comments
### - Rewrite analysis.html



ui <- navbarPage(
    theme = shinytheme("sandstone"),
    title = "Rainfall in Ireland",
    tabsetPanel(
        tabPanel(
            title = "Map and Time series plot",
            sidebarPanel(
                leafletOutput(outputId = "map"),
                selectInput(
                    inputId = "variableselected",
                    label = "Select Station",
                    multiple = TRUE,
                    choices = c("All", stat_list),
                    selected = "All" # preset choice
                ),
                p("Map of Ireland and Stations")
            ),
            mainPanel(
                dygraphOutput(outputId = "timetrend")
            )
        ),
        tabPanel(
            title = "Analysis",
            
            # mainPanel(
            #     style = "height: 700px; overflow-y: scroll;", # Set a fixed height and enable scrolling
            #     includeHTML("analysis.html")
            # )
            # mainPanel(
                
            # tags$iframe(
            #     src = "www/Assignment 2 - Q2.pdf",
            #     width = "100%",
            #     height = "600px",
            #     frameborder = 0,
            #     scrolling = "auto"
            # )
            #)
            mainPanel(
                style = "height: 700px; overflow-y: scroll;", # Set a fixed height and enable scrolling
                htmlOutput("display_html")
            )
                
            
        ),
        tabPanel(
            title = "Plots",
            sidebarPanel(
                leafletOutput(outputId = "map1"),
                selectInput(
                    inputId = "variableselected",
                    label = "Select Station",
                    multiple = TRUE,
                    choices = c("All", stat_list),
                    selected = "All" # preset choice
                ),
                p("Map of Ireland and Stations")
            ),
            mainPanel(
                tags$div(
                    style = "height: 800px; overflow-y: scroll;", # Set a fixed height and enable scrolling
                    tags$div(
                        style = "margin-bottom: 30px;",
                        h3("Box Plot (2.1)"),
                        plotOutput(outputId = "boxplot")
                    ),
                    tags$div(
                        style = "margin-bottom: 30px;",
                        h3("Long Term Time Series (2.2)"),
                        dygraphOutput(outputId = "long_term")
                    ),
                    fillPage(
                        h3("Seasonal Analysis (2.3)"),
                        plotOutput(outputId = "seasonal", height = "200%")
                    )
                )
            )
        ),
    )
)

server <- function(input, output, session) {
    # makes the necessary tables
    rainstat <- reactive({
        df = st_read('weather_stations.geojson', quiet=TRUE)
        as.data.frame(df) %>%
            dplyr::select(Year, Month, Rainfall, Station)
    })
    
    map_data <- reactive({
        st_read('weather_stations.geojson', quiet=TRUE) %>%
            dplyr::select(Station, geometry)
    })
    
    
    #b64 <- base64enc::dataURI(file = "analysis.html", mime = "text/html")
    # output$display_html <- renderUI({
    #     includeHTML(
    #         rmarkdown::render(
    #             input="analysis.Rmd", 
    #             params = list(selection = input$state, data=librarians_filtered)
    #         )
    #     )
    # })
        
    output$display_html <- renderUI({
        htmltools::HTML(readLines("analysis.html"))
    })
    
    # makes rainstat into an interactive table.
    output$table <- renderDT(rainstat)
    
    #boxplot from inputs
    output$boxplot <- renderPlot({
        rainstat <- rainstat()
        
        if (is.null(input$variableselected)){
            showModal(modalDialog(
                title = "No Station Selected",
                "Please select at least one station.",
                easyClose = TRUE,
                footer = NULL
            ))
        }else{
            
            if( "All" %in% input$variableselected){
                filter_stats <- stat_list
            }else{
                filter_stats <- as.list(input$variableselected)
            }
            
            rainstat%>%
                filter(Station %in% filter_stats)%>%
                summarise(mean_rainfall = mean(Rainfall, na.rm = TRUE)) %>%
                pull(mean_rainfall) -> mean_rainfall
            
            rainstat%>%
                filter(Station %in% filter_stats)%>%
                ggplot(aes(x=Station, y=Rainfall)) +
                geom_boxplot() + 
                geom_hline(aes(yintercept = mean_rainfall), color="red")+
                labs(y = "Rainfall (mm)")
            
        }
    })
    
    #longterm plot
    output$long_term <- renderDygraph({
        rainstat <- rainstat()
        dataxts <- NULL
        
        
        if (is.null(input$variableselected)){
            showModal(modalDialog(
                title = "No Station Selected",
                "Please select at least one station.",
                easyClose = TRUE,
                footer = NULL
            ))
        }else{
            
            if( "All" %in% input$variableselected){
                filter_stats <- stat_list
            }else{
                filter_stats <- as.list(input$variableselected)
            }
            
            for(l in 1:length(filter_stats)){
                rainstat %>% as.data.frame() %>%
                    dplyr::filter(Station == filter_stats[[l]]) %>% 
                    dplyr::summarise(Rainfall=sum(Rainfall), .by = c(Year,Month)) %>%
                    pull(Rainfall)  -> data_station
                dd <- ts(
                    data <- as.data.frame(data_station),
                    start=c(1850,1), freq=12
                )
                
                dataxts <- cbind(dataxts,dd)
            }
            colnames(dataxts) <- filter_stats
            dygraph(dataxts, ylab = "Rainfall (mm)") %>%
                dyRangeSelector(retainDateWindow = TRUE) %>% dyRoller(rollPeriod = 600) %>%
                dyHighlight(highlightSeriesBackgroundAlpha = 0.4) -> d1
            d1$x$css <- "
.dygraph-legend > span {display:none;}
.dygraph-legend > span.highlight { display: inline; }
"      
            d1
            
        }
    })
    
    #seasonal
    output$seasonal <- renderPlot({
        rainstat <- rainstat()
        p_i <- list()
        
        if (is.null(input$variableselected)){
            showModal(modalDialog(
                title = "No Station Selected",
                "Please select at least one station.",
                easyClose = TRUE,
                footer = NULL
            ))
        }else{
            
            if( "All" %in% input$variableselected){
                filter_stats <- stat_list
            }else{
                filter_stats <- as.list(input$variableselected)
            }
            
            RAIN_pre <- rainstat %>%
                dplyr::filter(Station %in% filter_stats) %>%
                mutate(date = as.POSIXct(as.yearmon(paste0(Year, Month), "%Y %B")))
            for(i in 1:length(filter_stats)){
                RAIN <- as_tibble(RAIN_pre) %>%
                    filter(Station == filter_stats[i]) %>%
                    distinct(date, .keep_all = TRUE) %>%
                    as_tsibble(index = date, regular = FALSE)
                
                p <- RAIN %>%
                    gg_season(Rainfall, period = "year") + 
                    labs(title = paste0("Seasonal Plot of Rainfall - ", filter_stats[i]), x = "Month", y = "Rainfall")
                p
                
                p_i[[i]] <- p
            }
            
            # Combine the plots into one
            grid.arrange(grobs = p_i, ncol = 2, nrow = ceiling(length(p_i) / 2))
            #ggarrange(p_i, ncol = 2)
            
        }
    })
    
    # makes the time series graph 
    # input: list of stations/all stations
    output$timetrend <- renderDygraph({
        rainstat <- rainstat()
        dataxts <- NULL
        
        if (is.null(input$variableselected)){
            showModal(modalDialog(
                title = "No Station Selected",
                "Please select at least one station.",
                easyClose = TRUE,
                footer = NULL
            ))
        }else{
            
            if( "All" %in% input$variableselected){
                filter_stats <- stat_list
            }else{
                filter_stats <- as.list(input$variableselected)
            }
            # print(length(filter_stats))
            
            for(l in 1:length(filter_stats)){
                rainstat %>% as.data.frame() %>%
                    dplyr::filter(Station == filter_stats[[l]]) %>% 
                    dplyr::summarise(Rainfall=sum(Rainfall), .by = c(Year,Month)) %>%
                    pull(Rainfall)  -> data_station
                dd <- ts(
                    data <- as.data.frame(data_station),
                    start=c(1850,1), freq=12
                )
                
                dataxts <- cbind(dataxts,dd)
            }
            colnames(dataxts) <- filter_stats
            dygraph(dataxts, ylab = "Rainfall (mm)") %>%
                dyRangeSelector(retainDateWindow = TRUE) %>%
                dyHighlight(highlightSeriesBackgroundAlpha = 0.2) -> d1
            d1$x$css <- "
 .dygraph-legend > span {display:none;}
 .dygraph-legend > span.highlight { display: inline; }
 "      
            d1
        }
    })
    
    output$map <- renderLeaflet({
        rainstat <- rainstat()
        map <- map_data()
        
        if (!is.null(input$variableselected)){
            
            if (!( "All" %in% input$variableselected)){
                map <- map %>% filter(Station %in% as.list(input$variableselected))
            }
            
            map <- unique(map)
            
            #Transform
            print(head(map$geometry))
            map <- st_transform(map, crs = 4258 )
            print(head(map$geometry))
            
            
            coords <- st_coordinates(map$geometry)
            map <- map %>%
                mutate(longitude = coords[, 1], latitude = coords[, 2])
            

            labels <- sprintf("%s", map$Station) %>%
                lapply(htmltools::HTML)
            
            leaflet() %>%
                addTiles() %>%
                addMarkers(data = map,
                           label = ~Station,
                           lng = ~longitude, lat = ~latitude,
                           popup = ~Station,
                           layerId = ~Station)
        }

    })
    
    output$map1 <- renderLeaflet({
        rainstat <- rainstat()
        map <- map_data()
        
        if (!is.null(input$variableselected)){
            
            if (!( "All" %in% input$variableselected)){
                map <- map %>% filter(Station %in% as.list(input$variableselected))
            }
            
            map <- unique(map)
            
            #Transform
            print(head(map$geometry))
            map <- st_transform(map, crs = 4258 )
            print(head(map$geometry))
            
            
            coords <- st_coordinates(map$geometry)
            map <- map %>%
                mutate(longitude = coords[, 1], latitude = coords[, 2])
            
            
            labels <- sprintf("%s", map$Station) %>%
                lapply(htmltools::HTML)
            
            leaflet() %>%
                addTiles() %>%
                addMarkers(data = map,
                           label = ~Station,
                           lng = ~longitude, lat = ~latitude,
                           popup = ~Station,
                           layerId = ~Station)
        }
        
    })
    
    observeEvent(input$map_marker_click, {
        click <- input$map_marker_click
        station <- click$id
        updateSelectInput(session, "variableselected", selected = station)
    })
}

shinyApp(ui = ui, server = server, options = list(height=700))