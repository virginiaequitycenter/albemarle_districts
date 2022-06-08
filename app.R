# ....................................
# County District Evaluation App
# Authors: Michele Claibourn
# Updated: February 10, 2022
# ....................................

# ....................................
# Load libraries and data
library(shiny)
library(tidyverse)
library(sf)
library(leaflet)
library(plotly)
library(scales)
library(viridis)

load("data/app_data.Rdata")
load("data/summaries.Rdata")
crd_geo <- readRDS("data/crd_data_geo.RDS")
# prepared in prepdata.r

# ....................................
# Define User Interface ----
ui <- 
    navbarPage(
      includeCSS("www/style.css"),
      title=div(
        div(
          id = "img-id",
          img(src = "Equity_Center_Logo-STAMP-transparent.png", height = '50')
        ),
        "Albemarle Proposed District Boundaries"), 

                 # img(src = "ACLGSeal-Color-WEB.png", height='80', style="float:right; padding-right:25px"),
                 # img(src="Equity_Center_Logo-STAMP-transparent.png", height = '80'),

                 ## ACS data map ----
                 tabPanel("District Maps",
                         # App title ----
                         titlePanel(div(
                           windowTitle = "Albemarle County: Proposed Districts",
                           # tags$a(href = "https://virginiaequitycenter.org/", img(src = "Equity_Center_Logo-STAMP-transparent.png", height='100')),
                           # tags$a(href = "https://www.albemarle.org/", img(src = "ACLGSeal-Color-WEB.png", height='100')),
                         )
                         ),
                         
                          sidebarLayout(
                            
                            # Sidebar for indicator 1
                            sidebarPanel(selectInput(inputId = "indicator",
                                               label = "Select an Indicator",
                                               choices = ind_choices_bg,
                                               selected = ind_choices_bg$People['Estimated Population']),
                                   
                                   div(
                                     tags$p("The spatial distribution of the selected indicator will be shown on the map."),
                                     tags$i("Click on a shaded area to view the indicator values!")
                                   ),
                                   
                                   tags$br(),
                                   
                                   # Select level
                                   radioButtons(inputId = "geo_level",
                                                label = "Select a Geographic Level:",
                                                choices = c("Census Tract", "Block Group"),
                                                selected = "Block Group",
                                                inline = TRUE),
                                   
                                   div(tags$p("The geographic level will determine whether census tracts or the smaller block groups are shown on the map."),
                                   tags$i("Some indicators are only available at the census tract level.")
                                   ),
                                   tags$br(),
                                   
                                   # Select a base map
                                   radioButtons(inputId = "map_base",
                                                label = "Select a Base Map:",
                                                choices = c("Minimal" = "CartoDB.Positron",
                                                            "Detailed" = "OpenStreetMap.Mapnik"),
                                                selected = "CartoDB.Positron",
                                                inline = TRUE),
                                   
                                   div(
                                     tags$p("To see geographic features of the area more clearly, choose the detailed base map."),
                                     tags$i("Zooming in will reveal additional features and street names.")
                                   ),
                                   tags$br(),
                                   tags$br(),
                                   
                                   tags$hr(style="border-top: 1px solid #000000;"),
                                   
                                   tags$p("Citation: Michele Claibourn. 2022. Visualizing Albemarle County Magisterial District Boundaries. UVA Equity Center."),
                                   
                            ),
                            
                            # Place scatterplot
                            mainPanel(title = "Map",
                                      tags$p("Click the magisterial district options to add (or remove) the boundaries on the map. Add multiple district boundaries together to view the differences between them. To see the name of a district, hover over a line for a district and click when the boundary line is highlighted."),
                                      leafletOutput("map", height=600)
                            )
                          )
                          
                          
                 ),
                 
                 ## Demographic summaries ----
                 tabPanel("Demographic Summaries",

                          # Place summary figures
                          mainPanel(title = "Dem",
                                    #tags$h2("Demographic summaries for proposed boundaries"),
                                    
                                    tags$p("Albemarle County staff provide the population and population by race and ethnicity counts for each of the proposed district boundary options in tables on the County webpage."),

                                    tags$a(href = "https://albemarle.legistar.com/View.ashx?M=F&ID=10428563&GUID=51C74483-49E2-41D9-97C8-F9B94CB37C6B", "See the demographic summaries table"),
                                    
                                    tags$br(),
                                    tags$br(),
                                    
                                    tags$p("The figures below present these population counts and percents visually to ease comparison between the options and current districts."),
                                    
                                    tags$h3("Population Counts by District"),
                                    
                                    tags$p("This figure shows each of Albemarle County's six magisterial districts by total population size and by the size of racial and ethnic subpopulations. The first panel (Total Population), for example, shows that the population within each current district ranges from 17,366 (Scottsville) to 20,526 (Rio). Option 1 boundaries reduce this range from a low of 17,962 (Samuel Miller) to a high of 19,461 (White Hall). Hover over the points to see the district name and counts."),

                                    plotlyOutput("count", height=800),
                                    
                                    tags$hr(),
                                    
                                    tags$h3("Population Percents by District"),
                                    
                                    tags$p("The figure below represents the same data but in terms of population percents. For example, the first panel shows that the percent of the population in each current district that identifies as non-Hispanic ranges from 89.5% (Jack Jouett) to 96% (White Hall). This range remains approximately the same for each of the proposed new boundaries. Hover over the points to see the district name and percents."),
                                    
                                    plotlyOutput("percent", height=600),

                                    tags$hr(),
                                    
                                    tags$br(),

                                    tags$p("Additional summaries of current and proposed district boundaries by social, demographic, or economic characteristics are not directly available with the data provided by the census."),
                                    
                                    width = 10
                          )
                        ),
      
      ## CDR Data ----
      tabPanel("Block Data",
               sidebarLayout(
                 
                 # Sidebar for indicator 1
                 sidebarPanel(selectInput(inputId = "blockpop",
                                          label = "Select a Population Measure",
                                          choices = c("Total Population",
                                                      "Percent White", 
                                                      "Percent Black", 
                                                      "Percent Hispanic",
                                                      "Percent Asian"),
                                          selected = "Total Population"),
                              
                              div(
                                tags$p("These data come from the Census Redistricting Data Program and represent block-level population counts. The spatial distribution of the selected population variable will be shown on the map."),
                                tags$i("Only racial and ethnic groups that make up more than 5% of the county population are present.")
                              ),
                              
                 ),
                 
                 # Place scatterplot
                 mainPanel(title = "Population by Blocks",
                           tags$p("Click the magisterial district options to add (or remove) the boundaries on the map. To see the name of a district, hover over a line for a district and click when the boundary line is highlighted."),
                           leafletOutput("blocks", height=600)
                 )
               )
      ),
                 
      ## About tab ----
      tabPanel("About",
               uiOutput("about")
               )
                 
)


# ....................................
# Define Server Logic ----
server <- function(input, output, session) {

  ## ACS selectors ----
  # update selection of indicators based on geo level
  observeEvent(input$geo_level, {
    if (input$geo_level == "Census Tract"){
      updateSelectInput(session, "indicator", choices = ind_choices_ct, 
                        selected = ind_choices_ct$People['Estimated Population']
      )
    } else {
      updateSelectInput(session, "indicator", choices = ind_choices_bg,
                        selected = ind_choices_bg$People['Estimated Population']
      )
    }
  })
  
  
  ## get ACS map data ----
  df <- reactive({
    data %>% filter(GEO_LEVEL == input$geo_level)
  })
  
  ## ACS Base map ----
  output$map <- renderLeaflet({
    df() %>%
      leaflet() %>%
      
      # keep polygons below district boundaries
      addMapPane(name = "polygons", zIndex = 410) %>% 
      addMapPane(name = "districts", zIndex = 420) %>% # higher zIndex rendered on top
      
      # add base map
      addProviderTiles(input$map_base) %>%
      
      # add base polygons
      addPolygons(color = "grey",
                  fill = FALSE,
                  weight = 1,
                  options = leafletOptions(pane = "polygons")) %>% 
    
      # add district layers
      addPolygons(data = mcd, 
                  group="Current Magisterial Boundaries",
                  color = "grey", fill = FALSE, weight = 2,
                  popup = ~NAMELSAD,
                  highlight = highlightOptions(weight = 4,
                                               color = "grey",
                                               bringToFront = TRUE),
                  options = leafletOptions(pane = "districts")) %>% 
      addPolygons(data = d1, 
                  group="Magisterial Boundaries, Option 1",
                  color = "purple", fill = FALSE, weight = 2,
                  popup = ~LONGNAME,
                  highlight = highlightOptions(weight = 4,
                                               color = "purple",
                                               bringToFront = TRUE),
                  options = leafletOptions(pane = "districts")) %>% 
      addPolygons(data = d2, 
                  group="Magisterial Boundaries, Option 2",
                  color = "red", fill = FALSE, weight = 2,
                  popup = ~LONGNAME,
                  highlight = highlightOptions(weight = 4,
                                               color = "red",
                                               bringToFront = TRUE),
                  options = leafletOptions(pane = "districts")) %>% 
      addPolygons(data = d3, 
                  group="Magisterial Boundaries, Option 3",
                  color = "#ff7f00", fill = FALSE, weight = 2,
                  popup = ~LONGNAME,
                  highlight = highlightOptions(weight = 4,
                                               color = "#ff7f00",
                                               bringToFront = TRUE),
                  options = leafletOptions(pane = "districts")) %>% 
      addLayersControl(
        overlayGroups = c("Current Magisterial Boundaries", 
                          "Magisterial Boundaries, Option 1",
                          "Magisterial Boundaries, Option 2",
                          "Magisterial Boundaries, Option 3"),
        options = layersControlOptions(collapsed = FALSE), 
        position = "topright"
      ) %>% 
      #hideGroup("Current Magisterial Boundaries") %>% 
      hideGroup("Magisterial Boundaries, Option 1") %>% 
      hideGroup("Magisterial Boundaries, Option 2") %>% 
      hideGroup("Magisterial Boundaries, Option 3") 
    
    
  })

  ## ACS Map Metric ----
  observe(

    if(!is.na(input$indicator)){

      # pull variable
      ind1 <- df() %>% 
        filter(!is.na(.data[[input$indicator]])) %>% 
        pull(input$indicator)
      
      if (all(is.na(ind1))){
        showModal(modalDialog(
          title = "Data not available",
          "Data not available for the current Geographic Level." ))
      } else {
        leafletProxy("map", data = df()
        ) %>%
          clearControls() %>%
          addProviderTiles(input$map_base) %>%
          addPolygons(fillColor = colorNumeric(mycolors, domain = ind1)(ind1),
                      fillOpacity = 0.3,
                      color = "white",
                      weight = 2,
                      smoothFactor = 0.2,
                      popup = paste0(attr(ind1, "goodname"), ": ",
                                     ind1, "<br>",
                                     df()[["NAME"]], "<br>"),
                      options = leafletOptions(pane = "polygons")) %>%
          addLegend(pal = colorNumeric(mycolors, domain = ind1),
                    values = ind1,
                    position = "bottomleft",
                    opacity = 0.25,
                    title = attr(ind1, "goodname"))
      }
    }
  )
  
  ## Dem summaries ----
  output$count <- renderPlotly({
    counts <- crd_counts_long %>% 
      ggplot(aes(x = fct_rev(option), y = counts, color = option, label = district)) +
      geom_point() +
      scale_color_manual(values = c("grey", "purple", "red", "#ff7f00")) +
      coord_flip() +
      facet_wrap(~population, scales = "free_x", ncol = 3) +
      labs(y = "Population Counts", x = "") +
      guides(color = "none")
    
    ggplotly(counts, tooltip = c("y", "color", "label")) %>% 
      layout(showlegend = FALSE)
  })
  
  
  output$percent <- renderPlotly({
    percent <- crd_percent_long %>% 
      ggplot(aes(x = fct_rev(option), y = percent, color = option, label = district)) +
      geom_point() +
      scale_color_manual(values = c("grey", "purple", "red", "#ff7f00")) +
      scale_y_continuous(labels = scales::label_percent(scale = 1, accuracy = 0.1)) +
      coord_flip() +
      facet_wrap(~population, scales = "free_x") +
      labs(y = "Population Percents", x = "") +
      guides(color = "none")
    
    ggplotly(percent, tooltip = c("y", "color", "label")) %>% 
      layout(showlegend = FALSE)
    
  })
  
  ## get CRD map data ----
  crd <- reactive({
    crd_geo %>% 
      select(x = input$blockpop)
  })

    ## CRD block map ----
  output$blocks <- renderLeaflet({
    
    binpal <- colorBin(
      palette = viridis_pal(option = "plasma", direction = -1)(5),
      domain = crd()$x,
      bins = 5,
      pretty = TRUE)
    
    crd() %>%
      leaflet() %>%

      # keep polygons below district boundaries
      addMapPane(name = "polygons", zIndex = 410) %>%
      addMapPane(name = "districts", zIndex = 420) %>% # higher zIndex rendered on top

      # add base map
      addProviderTiles("CartoDB.Positron") %>%

      # add population polygons
      addPolygons(fillColor = ~binpal(x),
                  fillOpacity = 0.3,
                  color = "white",
                  weight = 2,
                  smoothFactor = 0.2,
                  popup = paste0(input$blockpop, ": ", round(crd()$x, 1)),
                  options = leafletOptions(pane = "polygons")) %>%

      # population legend
      addLegend(pal = binpal, values = ~x,
                position = "bottomleft",
                opacity = 0.25,
                title = input$blockpop) %>% 
    
      # district layers
      addPolygons(data = mcd,
                  group="Current Magisterial Boundaries",
                  color = "grey", fill = FALSE, weight = 2,
                  popup = ~NAMELSAD,
                  highlight = highlightOptions(weight = 4,
                                               color = "grey",
                                               bringToFront = TRUE),
                  options = leafletOptions(pane = "districts")) %>%
      addPolygons(data = d1,
                  group="Magisterial Boundaries, Option 1",
                  color = "purple", fill = FALSE, weight = 2,
                  popup = ~LONGNAME,
                  highlight = highlightOptions(weight = 4,
                                               color = "purple",
                                               bringToFront = TRUE),
                  options = leafletOptions(pane = "districts")) %>%
      addPolygons(data = d2,
                  group="Magisterial Boundaries, Option 2",
                  color = "red", fill = FALSE, weight = 2,
                  popup = ~LONGNAME,
                  highlight = highlightOptions(weight = 4,
                                               color = "red",
                                               bringToFront = TRUE),
                  options = leafletOptions(pane = "districts")) %>%
      addPolygons(data = d3,
                  group="Magisterial Boundaries, Option 3",
                  color = "#ff7f00", fill = FALSE, weight = 2,
                  popup = ~LONGNAME,
                  highlight = highlightOptions(weight = 4,
                                               color = "#ff7f00",
                                               bringToFront = TRUE),
                  options = leafletOptions(pane = "districts")) %>%
      addLayersControl(
        overlayGroups = c("Current Magisterial Boundaries",
                          "Magisterial Boundaries, Option 1",
                          "Magisterial Boundaries, Option 2",
                          "Magisterial Boundaries, Option 3"),
        options = layersControlOptions(collapsed = FALSE),
        position = "topright"
      ) %>%
      #hideGroup("Current Magisterial Boundaries") %>%
      hideGroup("Magisterial Boundaries, Option 1") %>%
      hideGroup("Magisterial Boundaries, Option 2") %>%
      hideGroup("Magisterial Boundaries, Option 3")

  })

  ## about page ----
  output$about <- renderUI(includeHTML("about.html"))
  
}

# Run the application ----
shinyApp(ui = ui, server = server)