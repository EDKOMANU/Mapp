library(shiny)
library(readr)
library(tidyverse)
library(sf)
library(leaflet)
library(htmlwidgets)
library(leaflet.minicharts)
library(plotly)

# Increase the maximum request size to 100MB
options(shiny.maxRequestSize = 100*1024^2)

ui <- fluidPage(
  titlePanel("Industrial Symbiosis in Ghana"),
  sidebarLayout(
    sidebarPanel(
      tags$div(
        style = "height: 300px; overflow-y: scroll;",  # Add scrollbar and set height
        checkboxGroupInput("companies", "Select Companies:", choices = NULL)
      ),
      actionButton("update", "Update Map")
    ),
    mainPanel(
      leafletOutput("link_map", height = "800px"),
      plotlyOutput("overall_chart")
    )
  )
)

server <- function(input, output, session) {
  
  data <- read_csv("industrial_symbiosis_data.csv")
  data <- data %>% filter(!is.na(Longitude), !is.na(Latitude))
  
  new_data <- data %>% 
    rename(Company.name = `Company Name`, 
           Resource.Needs = `Resource Needs`,
           By.products = `By-Products`) %>%
    mutate(
      raw_input = case_when(Resource.Needs == "Paper" ~ "Wood shavings"),
      Resource_Needs_List = strsplit(as.character(Resource.Needs), " //| "),
      By_Products_List = strsplit(as.character(By.products), " //| ")
    )
  
  # Initialize an empty data frame to store links
  links <- data.frame(
    Source_Industry = character(),
    Target_Industry = character(),
    Matching_Item = character(),
    source_company = character(),
    target_company = character(),
    stringsAsFactors = FALSE
  )
  
  # Loop through each industry to find matches
  for (i in 1:nrow(new_data)) {
    for (j in 1:nrow(new_data)) {
      if (i != j) { # Avoid self-linking
        matching_items <- intersect(new_data$Resource_Needs_List[[i]], new_data$By_Products_List[[j]])
        if (length(matching_items) > 0) {
          links <- rbind(links, data.frame(
            source_company = new_data$Company.name[i],
            target_company = new_data$Company.name[j],
            Source_Industry = new_data$Industry[i],
            Target_Industry = new_data$Industry[j],
            Matching_Item = paste(matching_items, collapse = ", "),
            stringsAsFactors = FALSE
          ))
        }
      }
    }
  }
  
  location <- new_data %>% 
    select(Company.name, Latitude, Longitude, Industry, Resource.Needs, By.products)
  
  links <- links %>% 
    left_join(location, by = c("source_company" = "Company.name")) %>% 
    rename(
      source_lat = Latitude, source_long = Longitude, 
      source_industry = Industry, source_resource_needs = Resource.Needs, 
      source_by_products = By.products
    ) %>% 
    left_join(location, by = c("target_company" = "Company.name")) %>% 
    rename(
      target_lat = Latitude, target_long = Longitude, 
      target_industry = Industry, target_resource_needs = Resource.Needs, 
      target_by_products = By.products
    )
  
  missing <- links %>% 
    filter(is.na(source_by_products), is.na(source_resource_needs)) 
  
  links <- links %>% 
    anti_join(missing, by = c("source_company" = "source_company"))
  
  missing <- missing %>% 
    select(source_company, source_lat, source_long, source_resource_needs)
  
  # Update the company selection choices
  observe({
    updateCheckboxGroupInput(session, "companies", choices = unique(new_data$Company.name))
  })
  
  observeEvent(input$update, {
    selected_companies <- input$companies
    
    selected_links <- links %>% 
      filter(source_company %in% selected_companies | target_company %in% selected_companies)
    
    selected_missing <- missing %>% 
      filter(source_company %in% selected_companies)
    
    # Load Ghana polygon shapefile
    ghana_shapefile <- st_read("Ghana Country Boundary.shp/ghana_Ghana_Country_Boundary.shp")
    district <- st_read("Geo FIles/District_272.shp")
    # Create a leaflet map
    icon <- "pngtree-industry-icon-png-image_1641009.jpg"
    
    selected_links <- na.omit(selected_links)
    selected_missing <- distinct(selected_missing, source_company, .keep_all = TRUE)
    
    # Create the leaflet map
    output$link_map <- renderLeaflet({
      link_map <- leaflet() %>%
        addTiles() %>%
        setView(lng = -1.0232, lat = 7.9465, zoom = 8) %>%# Add base map tiles
        addPolygons(
          data = ghana_shapefile,
          color = "red", weight = 5, fill = FALSE,
          popup = "Ghana"
        ) %>%
        addPolygons(
          data = district,
          color = "red", weight = 0.7, fill = FALSE,
          popup = "Ghana"
        ) %>%
        addMarkers(
          lng = selected_links$source_long, lat = selected_links$source_lat, 
          popup = paste(
            "<b>Source Company:</b> ", selected_links$source_company, "<br>",
            "<b>Industry:</b> ", selected_links$source_industry, "<br>",
            "<b>Input:</b> ", selected_links$source_resource_needs, "<br>",
            "<b>By-Products:</b> ", selected_links$source_by_products, "<br>"
          ),  
          icon = icons(
            iconUrl = icon, 
            iconWidth = 25,
            iconHeight = 41,
            iconAnchorX = 12.5,
            iconAnchorY = 41
          )
        ) %>%
        addMarkers(
          lng = selected_links$target_long, lat = selected_links$target_lat, 
          popup = paste(
            "<b>Company:</b> ", selected_links$target_company, "<br>",
            "<b>Industry:</b> ", selected_links$target_industry, "<br>",
            "<b>Input:</b> ", selected_links$target_resource_needs, "<br>",
            "<b>By-Products:</b> ", selected_links$target_by_products, "<br>"
          ),  
          icon = icons(
            iconUrl = icon, 
            iconWidth = 25,
            iconHeight = 41,
            iconAnchorX = 12.5,
            iconAnchorY = 41
          )
        ) %>%
        addMarkers(
          lng = selected_missing$source_long, lat = selected_missing$source_lat, 
          popup = paste(
            "<b>Company:</b> ", selected_missing$source_company, "<br>",
            "<b>Industry:</b> ", selected_missing$source_industry, "<br>",
            "<b>Input:</b> ", selected_missing$source_resource_needs, "<br>",
            "<b>By-Products:</b> ", selected_missing$source_by_products, "<br>"
          ),  
          icon = icons(
            iconUrl = icon, 
            iconWidth = 25,
            iconHeight = 41,
            iconAnchorX = 12.5,
            iconAnchorY = 41
          )
        ) %>%
        addFlows(
          lng0 = selected_links$source_long, lat0 = selected_links$source_lat,  # Source coordinates
          lng1 = selected_links$target_long, lat1 = selected_links$target_lat,  # Target coordinates
          color = "blue",  # Red arrow for outgoing, blue for incoming
          opacity = 0.5,
          maxThickness = 2,
          popup = popupArgs(html = paste(
            "<b>Flow Information:</b><br>",
            "<b>Source Company:</b> ", selected_links$source_company, "<br>",
            "<b>Target Company:</b> ", selected_links$target_company, "<br>",
            "<b>Matching Item:</b> ", selected_links$Matching_Item, "<br>"
          ))
        ) %>%
        addControl(
          html = "<h1>Industrial Symbiosis in Ghana</h1><p>This map visualizes the industrial symbiosis network in Ghana.</p><p>linking companies based on their inputs and by-product </p>",
          position = "topright"
        )
      
      link_map
    })
    
    chartdata <- selected_links %>%
      group_by(source_company, source_industry) %>%
      summarize(
        Needs = sum(str_count(source_resource_needs, "\\|")) + 1,
        Byproducts = sum(str_count(source_by_products, "\\|")) + 1
      ) %>%
      ungroup()
    
    output$overall_chart <- renderPlotly({
      overall_chart <- plot_ly(
        chartdata,
        x = ~source_industry,
        y = ~Needs,
        type = 'bar',
        name = 'Needs'
      ) %>%
        add_trace(y = ~Byproducts, name = 'By-Products') %>%
        layout(
          title = "Resource Needs and By-Products for Selected Companies",
          xaxis = list(title = "Industry"),
          yaxis = list(title = "Count"),
          barmode = 'group'
        )
      
      overall_chart
    })
    
  })
}

shinyApp(ui, server)