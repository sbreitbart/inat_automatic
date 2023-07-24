# load libraries-----
library(shiny)
library(shinydashboard)
library(leaflet)
library(tidyverse)
library(magrittr)
library(extrafont)
library(scales)
library(shinyWidgets)
library(DT)
library(reactable)
library(htmltools)
library(backports)
library(crosstalk)
library(rinat)
library(viridis)


# import data----
inat_auto <- get_inat_obs_user(username = "sophiebreitbart",
                               maxresults = 5000) %>%
  dplyr::select(-user_name) %>%
  dplyr::mutate(year = substr(datetime, 1, 4)) %>%
  as.data.frame() %>%
  drop_na(latitude, longitude) %>%
  dplyr::mutate_at(c("scientific_name", "datetime"), na_if, "") %>%
  drop_na(datetime, scientific_name)
  
str(inat_auto)

inat_auto %<>%
  mutate(common_name = ifelse(
    common_name == "", "No common name", common_name))

# make color paletttes-----

# make color palette for all sightings map
pal <- colorFactor(palette = 'magma',
                   domain = inat_auto$year)



# ui-----

ui <- fluidPage(
    titlePanel("Sophie's iNaturalist Sightings!"),

    h5(tags$a(href= "https://sbreitbart.github.io/", "Sophie Breitbart, 2023",
              target="_blank")),
    
       
         mainPanel( tabsetPanel(
            type = "tabs",
            
            tabPanel("All Sightings",
                     h4("Click the map waypoints!"),
                     fluidPage(shinyWidgets::pickerInput(inputId = "iconic_taxon_name",
                                               label = h3("Select taxonomic group"),
                                               choices = as.list(
                                                   sort(na.exclude(unique(inat_auto$iconic_taxon_name)))),
                                               selected = "Fungi",
                                               options = list(`actions-box` = TRUE,
                                                              `selected-text-format` = "count > 2"),
                                               multiple = TRUE),
                               leafletOutput("all_map"),
                               DT::dataTableOutput('inat_table')))
                        
         )
    ))


# server-----
# server <- function(input, output, session) {
#   # Reactive expression for filtered data
#   inat_map <- eventReactive(input$iconic_taxon_name, {
#     inat_auto %>%
#       filter(iconic_taxon_name %in% input$iconic_taxon_name)
#   })
#   
#   output$inat_table <- DT::renderDataTable({
#     inat_map() %>%
#       dplyr::select(c("common_name", "scientific_name",
#                       "place_guess", "year",
#                       "iconic_taxon_name")) %>%
#       dplyr::rename("Common Name" = 1,
#                     "Scientific Name" = 2,
#                     "Location" = 3,
#                     "Year" = 4,
#                     "Taxonomic Group" = 5)
#   }, selection = 'single')  # Restrict row selection to one at a time
#   
#   
#   output$all_map <- renderLeaflet({
#     leaflet() %>%
#       addTiles(
#         options = providerTileOptions(opacity = 0.55)
#       ) %>%
#       addCircleMarkers(data = inat_map(),
#                        lng = ~longitude,
#                        lat = ~latitude,
#                        radius = 8,  # Initial default size, you can adjust this value
#                        popup = paste0(
#                          "Scientific name: ",
#                          ifelse(
#                            !is.na(inat_map()$url),
#                            paste0('<a href="', inat_map()$url, '" target="_blank">', inat_map()$scientific_name, "</a>"),
#                            inat_map()$scientific_name
#                          ),
#                          ".",
#                          "<br>", # line break
#                          "Common name: ",
#                          inat_map()$common_name, 
#                          ".",
#                          "<br>", # line break
#                          "Location: ",
#                          inat_map()$place_guess,
#                          ".",
#                          "<br>",
#                          "Year: ", inat_map()$year, ".",
#                          "<br>", # Add a line break
#                          # Add image links if image_url column exists
#                          ifelse(
#                            !is.na(inat_map()$image_url),
#                            paste0(
#                              "Image: ",
#                              '<a href="', inat_map()$image_url, '" target="_blank">',
#                              '<img src="', inat_map()$image_url, '" style="max-height:150px; max-width:150px">',
#                              '</a>'
#                            ),
#                            ""
#                          )
#                        ),
#                        fill = T,
#                        fillOpacity = 0.5,
#                        color = ~pal(year)
#       ) %>%
#       addLegend("bottomleft",
#                 pal = pal,
#                 values = inat_auto$year,
#                 opacity = 0.8)
#   })
#   
#   # Observe selected row in the data table
#   observeEvent(input$inat_table_rows_selected, {
#     selected_row <- input$inat_table_rows_selected
#     if (length(selected_row) > 0) {
#       # Get the selected row data
#       selected_data <- inat_map()[selected_row, c("latitude", "longitude")]
#       
#       # Check if latitude and longitude exist and zoom to the selected point
#       if (!is.na(selected_data$latitude) && !is.na(selected_data$longitude)) {
#         leafletProxy("all_map") %>%
#           setView(lng = selected_data$longitude, lat = selected_data$latitude, zoom = 14)
#       }
#     }
#   })
# }

server <- function(input, output, session) {
  # Reactive expression for filtered data
  inat_map <- eventReactive(input$iconic_taxon_name, {
    inat_auto %>%
      filter(iconic_taxon_name %in% input$iconic_taxon_name)
  })
  
  # Reactive variable to store the index of the selected point
  selected_point_index <- reactiveVal()
  
  output$inat_table <- DT::renderDataTable({
    inat_map() %>%
      dplyr::select(c("common_name", "scientific_name",
                      "place_guess", "year",
                      "iconic_taxon_name")) %>%
      dplyr::rename("Common Name" = 1,
                    "Scientific Name" = 2,
                    "Location" = 3,
                    "Year" = 4,
                    "Taxonomic Group" = 5)
  }, selection = 'single')  # Restrict row selection to one at a time
  
  output$all_map <- renderLeaflet({
    leaflet() %>%
      addTiles(
        options = providerTileOptions(opacity = 0.55)
      ) %>%
          addCircleMarkers(data = inat_map(),
                           lng = ~longitude,
                           lat = ~latitude,
                           radius = 8,  # Initial default size, you can adjust this value
                           popup = paste0(
                             "Scientific name: ",
                             ifelse(
                               !is.na(inat_map()$url),
                               paste0('<a href="', inat_map()$url, '" target="_blank">', inat_map()$scientific_name, "</a>"),
                               inat_map()$scientific_name
                             ),
                             ".",
                             "<br>", # line break
                             "Common name: ",
                             inat_map()$common_name,
                             ".",
                             "<br>", # line break
                             "Location: ",
                             inat_map()$place_guess,
                             ".",
                             "<br>",
                             "Year: ", inat_map()$year, ".",
                             "<br>", # Add a line break
                             # Add image links if image_url column exists
                             ifelse(
                               !is.na(inat_map()$image_url),
                               paste0(
                                 "Image: ",
                                 '<a href="', inat_map()$image_url, '" target="_blank">',
                                 '<img src="', inat_map()$image_url, '" style="max-height:150px; max-width:150px">',
                                 '</a>'
                               ),
                               ""
                             )
                           ),
                           fill = T,
                           fillOpacity = 0.5,
                           color = ~pal(year)
          ) %>%
          addLegend("bottomleft",
                    pal = pal,
                    values = inat_auto$year,
                    opacity = 0.8) %>%
      # Add triangle markers on top of the selected point
      addMarkers(data = inat_map(),
                 lng = ~longitude[selected_point_index()],
                 lat = ~latitude[selected_point_index()],
                 icon = makeIcon(iconUrl = "https://raw.githubusercontent.com/pointhi/leaflet-color-markers/master/img/marker-icon-2x-black.png",
                                 iconWidth = 16, iconHeight = 26, iconAnchorX = 8, iconAnchorY = 26),
                 # Zoom in to the selected point
                 labelOptions = labelOptions(noHide = TRUE, textOnly = TRUE),
                 popupOptions = popupOptions(closeOnClick = FALSE)
      ) %>%
      # Set initial view to show all points
      setView(lng = mean(inat_map()$longitude), 
              lat = mean(inat_map()$latitude),
              zoom = 2)
  })
  
  # Observe selected row in the data table and update the selected_point_index
  observeEvent(input$inat_table_rows_selected, {
    selected_row <- input$inat_table_rows_selected
    if (!is.null(selected_row)) {
      selected_point_index(selected_row)
      # Zoom in to the selected point
      leafletProxy("all_map") %>%
        setView(lng = inat_map()$longitude[selected_row], 
                lat = inat_map()$latitude[selected_row],
                zoom = 14)
    }
  })
}




# the app-----
shinyApp(ui, server)
