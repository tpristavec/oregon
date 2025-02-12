library(tidyverse)
library(leaflet)
library(shiny)
library(shinythemes)
library(sf)
library(dplyr)
library(readr)


# Load data
data <- read_rds("oregon.Rds")
superfunds <- read_rds("superfunds.Rds")
data <- st_transform(data, 4269)
superfunds <- st_transform(superfunds, 4269)

# Prepare
varchoices <- c("totalpop", "age65", "under18", "hispanic", "black", "female", "inpov", "noba", "unempl","renters")
names(varchoices) <- c("Total tract population", 
                       "Percent population age 65 and over",
                       "Percent population age 18 and under",
                       "Percent population Hispanic",
                       "Percent population Black",
                       "Percent population female",
                       "Percent population with income below poverty level",
                       "Percent working-age population without BA",
                       "Percent labor force population unemployed",
                       "Percent population renters")

zipchoices <- c("97203", "97208", "97209", "97210", "97211", "97212", "97217", "97227", "97228", "97231", "97283")
names(zipchoices) <- c("97203", "97208", "97209", "97210", "97211", "97212", "97217", "97227", "97228", "97231", "97283")

# Fix leaflet legend NA issue
css_fix <- "div.info.legend.leaflet-control br {clear: both;}"
html_fix <- as.character(htmltools::tags$style(type = "text/css", css_fix))

# UI
ui <- fluidPage(theme = shinytheme("cosmo"),
                tags$head(tags$style('.selectize-dropdown {z-index: 10000}')),
                
                title = "North Oregon Superfund Area Demographics",
                
                fluidRow(style = "margin: 6px;",
                         img(src = "logo-uva.jpeg", style = "display: block; margin-left: auto; margin-right: auto;", width = "20%"),
                         h1(strong("North Oregon Superfund Area Demographics"), align = "center")
                ),
                br(),
                fluidRow(style = "margin: 6px;",
                         selectInput("whichvar", "Select Variable", width = "100%", choices = varchoices)
                ),
                fluidRow(style = "margin: 6px;",
                         selectInput("whichzip", "Select ZIP", width = "100%", choices = zipchoices)
                ),
                p(),
                fluidRow(style = "margin: 6px;",
                         p(strong("Census Tract-Level Map")),
                         leafletOutput("plot_var", height = 550),
                         p(tags$small("Data Sources: US Census Bureau American Community Survey, 2015/19 5-Year Estimates; Environmental Protection Agency Superfund Sites, 2020."))
                ),
                p(),
                hr(),
                p(tags$small("Last updated: April 2021. Direct inquiries to"), 
                  tags$small(a(href = "https://biocomplexity.virginia.edu/person/teja-pristavec", "Teja Pristavec.", target = "_blank")),
                  align = "center")
                
)

# Server
server <- function(input, output, session) {
  
  #
  # OPTIONS ------------------------------------------------------
  #
  
  map_colors <- c("#5d286d", "#6f3d7e", "#82528f", "#9467a0", "#a77db2", "#ba94c4", "#ceaad6", "#e1c1e8", "#f5d9fb")
  
  
  #
  # FUNCTION: Map: Base maps ------------------------------------------
  #
  
  create_plot <- function(data, myvar, myvarlabel) {
    
    pal <- colorBin(map_colors, bins = 5, domain = myvar, na.color = "grey")
    
    labels_data <- lapply(
      paste("<strong>Area: </strong>",
            data$NAME.y,
            "<br />",
            "<strong>", myvarlabel, ": </strong>",
            format(myvar, big.mark = ",", decimal.mark = ".", digits = 2, zero.print = T),
            "<br />",
            "<strong>Part of ZIP code(s): </strong>",
            data$whichzips
      ),
      
      htmltools::HTML
    )
    
    labels_superfunds <- lapply(
      paste("<strong>Site Name: </strong>",
            superfunds$siteName,
            "<br />",
            "<strong>Address: </strong>",
            paste0(superfunds$streetAddr, ", ", superfunds$cityName, ", ", superfunds$state, " ", superfunds$zipCode),
            "<br />",
            "<strong>Site Status: </strong>",
            superfunds$nationalPr
      ),
      
      htmltools::HTML
    )
    
    leaflet(data = data) %>%
      addProviderTiles(providers$CartoDB.Positron, group = "CartoDB Positron") %>%
      addTiles(group = "OpenStreetMap") %>%
      addProviderTiles(providers$OpenTopoMap, group = "OpenTopoMap") %>%
      addProviderTiles(providers$Esri.WorldStreetMap, group = "ESRI WorldStreetMap") %>%
      addCircles(data = superfunds, 
                 radius = 250,
                 stroke = TRUE,
                 color = "black",
                 opacity = 1,
                 weight = 3,
                 fill = TRUE,
                 fillColor = "red",
                 fillOpacity = 1, 
                 group = "Superfund Sites",
                 label = labels_superfunds,
                 labelOptions = labelOptions(direction = "bottom",
                                             style = list(
                                               "font-size" = "12px",
                                               "border-color" = "rgba(0,0,0,0.5)",
                                               direction = "auto"
                                             ))) %>%
      addPolygons(fillColor = ~pal(myvar), 
                  fillOpacity = 0.6, 
                  stroke = TRUE, smoothFactor = 0.8, weight = 0.5, color = "#202020",
                  group = "Variable Values",
                  label = labels_data,
                  labelOptions = labelOptions(direction = "bottom",
                                              style = list(
                                                "font-size" = "12px",
                                                "border-color" = "rgba(0,0,0,0.5)",
                                                direction = "auto"
                                              ))) %>%
      addLayersControl(
        baseGroups = c("CartoDB Positron", "OpenStreetMap", "OpenTopoMap", "ESRI WorldStreetMap"),
        overlayGroups = c("Variable Values", "Superfund Sites"),
        options = layersControlOptions(collapsed = FALSE)
      ) %>% 
      addLegend("bottomleft",
                pal = pal,
                values =  ~(myvar),
                title = "Value",
                opacity = 0.8,
                na.label = "Not Available", 
                labFormat = labelFormat(digits = 4, big.mark = ","))
  }
  
  #
  # OUTPUT: Variable plots ------------------------------------------
  #
  
  
  plot_which_data <- reactive({data %>% filter(str_detect(whichzips, input$whichzip))})
  plot_which_var <- reactive({plot_which_data()[[input$whichvar]]})
  
  output$plot_var <- renderLeaflet({
    
    var_label <- names(varchoices)[varchoices == input$whichvar]
    
    create_plot(plot_which_data(), plot_which_var(), var_label)
  })
  
  
}

# Run the application 
shinyApp(ui = ui, server = server)

