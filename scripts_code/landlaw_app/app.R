library(shiny)
library(leaflet)
library(tableHTML)
library(sf)
library(crosstalk)
library(tidyverse)
library(leaflet.extras)

load("d_bills_states_sf.rdata")

d2_sf <- d_bills_states_sf

# Define reactiveValues object
rv <- reactiveValues(selected = NULL, color_palette = NULL)

ui <- fluidPage(
        tags$head(
                tags$link(rel = "stylesheet", type = "text/css", href = "https://cdnjs.cloudflare.com/ajax/libs/font-awesome/5.15.3/css/all.min.css"), # include fontawesome for home button icon
                includeCSS("www/styles.css")
        ),
        tabsetPanel(id = "tabsetPanel",
                    tabPanel("All bills",
                             fluidRow(
                                     column(
                                             width = 8,
                                             wellPanel(
                                                     id = "mainpanel",
                                                     tags$style(type = "text/css", "#mymap_all {height: calc(50vh) !important;}"),
                                                     leafletOutput("mymap_all")
                                             )
                                     ),
                                     column(
                                             width = 4,
                                             h4("Select bill provisions:"),
                                             wellPanel(id = "selectpanel",
                                                       selectInput(
                                                               "prohibited_entities_all",
                                                               "Entities prohibited from owning property",
                                                               choices = c(
                                                                       "At least one category" = "any_entity",
                                                                       "PRC citizens" = "PRC_citizen",
                                                                       "Permanent U.S. residents" = "Resident_status",
                                                                       "PRC companies and organizations" = "PRC_Companies_Orgs_Entities",
                                                                       "PRC government and entities" = "PRC_Govt_Entities",
                                                                       "CCP members and companies" = "CCP_Members_Companies",
                                                                       "Foreign governments, entities, and individuals" = "Foreign_Govts_Entities_Individuals",
                                                                       "Foreign adversaries" = "Foreign_Adversaries_Including_PRC",
                                                                       "Existing owners must sell property" = "Existing_Owners_Must_Sell_Property"
                                                               ),
                                                               selected = "any_entity",
                                                               multiple = TRUE
                                                       ),
                                                       selectInput(
                                                               "prohibited_land_types_all",
                                                               "Properties prohibited from ownership",
                                                               choices = c(
                                                                       "At least one category" = "any_land",
                                                                       "All property" = "All_types",
                                                                       "Residential" = "Residential",
                                                                       "Commercial or not for personal residence" = "Commercial_or_Not_for_Personal_Use",
                                                                       "Agricultural or natural resources" = "Agricultural_Natural_Resource",
                                                                       "State land" = "State_Land",
                                                                       "Near federal land or critical infrastructure" = "Near_Federal_Land_Critical_Infrastructure",
                                                                       "Near military facilities" = "Near_Military_Facilities",
                                                                       "Of a certain size" = "Of_a_Certain_Size"
                                                               ),
                                                               selected = "any_land",
                                                               multiple = TRUE
                                                       )
                                             )
                                     )
                             ),
                             fluidRow(
                                     column(
                                             width = 12,
                                             h4("Summary of bills"),
                                             wellPanel(id = "textpanel", htmlOutput("feat_selected_all"))
                                     )
                             )
                    ),
                    tabPanel("Passed bills",
                             fluidRow(
                                     column(
                                             width = 8,
                                             wellPanel(
                                                     id = "mainpanel",
                                                     tags$style(type = "text/css", "#mymap_passed {height: calc(50vh) !important;}"),
                                                     leafletOutput("mymap_passed")
                                             )
                                     ),
                                     column(
                                             width = 4,
                                             h4("Select bill provisions:"),
                                             wellPanel(id = "selectpanel",
                                                       selectInput(
                                                               "prohibited_entities_passed",
                                                               "Entities prohibited from owning property",
                                                               choices = c(
                                                                       "At least one category" = "any_entity",
                                                                       "PRC citizens" = "PRC_citizen",
                                                                       "Permanent U.S. residents" = "Resident_status",
                                                                       "PRC companies and organizations" = "PRC_Companies_Orgs_Entities",
                                                                       "PRC government and entities" = "PRC_Govt_Entities",
                                                                       "CCP members and companies" = "CCP_Members_Companies",
                                                                       "Foreign governments, entities, and individuals" = "Foreign_Govts_Entities_Individuals",
                                                                       "Foreign adversaries" = "Foreign_Adversaries_Including_PRC",
                                                                       "Existing owners must sell property" = "Existing_Owners_Must_Sell_Property"
                                                                       
                                                               ),
                                                               selected = "any_entity",
                                                               multiple = TRUE
                                                       ),
                                                       selectInput(
                                                               "prohibited_land_types_passed",
                                                               "Properties prohibited from ownership",
                                                               choices = c(
                                                                       "At least one category" = "any_land",
                                                                       "All property" = "All_types",
                                                                       "Residential" = "Residential",
                                                                       "Commercial or not for personal residence" = "Commercial_or_Not_for_Personal_Use",
                                                                       "Agricultural or natural resources" = "Agricultural_Natural_Resource",
                                                                       "State land" = "State_Land",
                                                                       "Near federal land or critical infrastructure" = "Near_Federal_Land_Critical_Infrastructure",
                                                                       "Near military facilities" = "Near_Military_Facilities",
                                                                       "Of a certain size" = "Of_a_Certain_Size"
                                                               ),
                                                               selected = "any_land",
                                                               multiple = TRUE
                                                       )
                                             )
                                     )
                             ),
                             fluidRow(
                                     column(
                                             width = 12,
                                             h4("State bills summary"),
                                             wellPanel(id = "textpanel", htmlOutput("feat_selected_passed"))
                                     )
                             )
                    ),
                    tabPanel("Bills under consideration",
                             fluidRow(
                                     column(
                                             width = 8,
                                             wellPanel(
                                                     id = "mainpanel",
                                                     tags$style(type = "text/css", "#mymap_considering {height: calc(50vh) !important;}"),
                                                     leafletOutput("mymap_considering")
                                             )
                                     ),
                                     column(
                                             width = 4,
                                             h4("Select bill provisions:"),
                                             wellPanel(id = "selectpanel",
                                                       selectInput(
                                                               "prohibited_entities_considering",
                                                               "Entities prohibited from owning property",
                                                               choices = c(
                                                                       "At least one category" = "any_entity",
                                                                       "PRC citizens" = "PRC_citizen",
                                                                       "Permanent U.S. residents" = "Resident_status",
                                                                       "PRC companies and organizations" = "PRC_Companies_Orgs_Entities",
                                                                       "PRC government and entities" = "PRC_Govt_Entities",
                                                                       "CCP members and companies" = "CCP_Members_Companies",
                                                                       "Foreign governments, entities, and individuals" = "Foreign_Govts_Entities_Individuals",
                                                                       "Foreign adversaries" = "Foreign_Adversaries_Including_PRC",
                                                                       "Existing owners must sell property" = "Existing_Owners_Must_Sell_Property"
                                                                       
                                                               ),
                                                               selected = "any_entity",
                                                               multiple = TRUE
                                                       ),
                                                       selectInput(
                                                               "prohibited_land_types_considering",
                                                               "Properties prohibited from ownership",
                                                               choices = c(
                                                                       "At least one category" = "any_land",
                                                                       "All property" = "All_types",
                                                                       "Residential" = "Residential",
                                                                       "Commercial or not for personal residence" = "Commercial_or_Not_for_Personal_Use",
                                                                       "Agricultural or natural resources" = "Agricultural_Natural_Resource",
                                                                       "State land" = "State_Land",
                                                                       "Near federal land or critical infrastructure" = "Near_Federal_Land_Critical_Infrastructure",
                                                                       "Near military facilities" = "Near_Military_Facilities",
                                                                       "Of a certain size" = "Of_a_Certain_Size"
                                                               ),
                                                               selected = "any_land",
                                                               multiple = TRUE
                                                       )
                                             )
                                     )
                             ),
                             fluidRow(
                                     column(
                                             width = 12,
                                             h4("State bills summary"),
                                             wellPanel(id = "textpanel", htmlOutput("feat_selected_considering"))
                                     )
                             )
                    ),
        )
)


server <- function(input, output, session) {
        
        filtered_data <- reactive({
                # Check if input$tabsetPanel has been initialized
                if (is.null(input$tabsetPanel) || length(input$tabsetPanel) == 0) {
                        return(d2_sf)  # Default to returning full dataset
                }
                
                # Determine the filtered dataset based on the active tab
                if (input$tabsetPanel == "All bills") {
                        req(input$prohibited_entities_all, input$prohibited_land_types_all)
                        return(d2_sf %>% filter(map_type == "all"))
                } else if (input$tabsetPanel == "Passed bills") {
                        req(input$prohibited_entities_passed, input$prohibited_land_types_passed)
                        return(d2_sf %>% filter(map_type == "passed"))
                } else if (input$tabsetPanel == "Bills under consideration") {
                        req(input$prohibited_entities_considering, input$prohibited_land_types_considering)
                        return(d2_sf %>% filter(map_type == "considering"))
                } else {
                        return(d2_sf)  # Default to returning full dataset
                }
        })
        
        updated_data <- reactive({
                data <- filtered_data()
                
                # Get the selected columns
                if (input$tabsetPanel == "All bills") {
                        selected_columns <- c(input$prohibited_entities_all, input$prohibited_land_types_all)
                } else if (input$tabsetPanel == "Passed bills") {
                        selected_columns <- c(input$prohibited_entities_passed, input$prohibited_land_types_passed)
                } else if (input$tabsetPanel == "Bills under consideration") {
                        selected_columns <- c(input$prohibited_entities_considering, input$prohibited_land_types_considering)
                } else {
                        stop("Unexpected tab selected.")  # A failsafe in case an unrecognized tab is selected
                }
                
                # Filter out any selections that are not columns in the data
                valid_columns <- intersect(selected_columns, names(data))

                # Remove geometry for combo generation
                non_geom_data <- st_set_geometry(data, NULL)
                data$combo <- apply(non_geom_data[, valid_columns, drop = FALSE], 1, paste, collapse = "")
                
                # Generate the 'palette_combo' column
                data$palette_combo <- factor(ifelse(grepl("^1+$", data$combo), "Yes", "No"), levels = c("Yes", "No"))
                
                return(data)
        })
        
        color_palette <- reactive({
                pal <- colorFactor(c("#d89099", "#e3ebed"), levels = c("Yes", "No"), ordered = F)
                return(pal)
        })
        
        output$mymap_all <- renderLeaflet({
                leaflet_data <- updated_data()
                
                leaflet(options = leafletOptions(maxZoom = 7, minZoom = 3.75, zoomControl = TRUE,
                                                 zoomSnap = 0.25, zoomDelta = 0.5)) %>%
                        addPolygons(
                                layerId = ~ID_1,
                                group = "states",
                                data = leaflet_data,
                                fillColor = ~color_palette()(leaflet_data$palette_combo),
                                weight = ~ ifelse(leaflet_data$state == "US", 4, 1),
                                color = ~ ifelse(leaflet_data$state == "US", "#f2c508", "#666666") ,
                                opacity = 0.4,
                                fillOpacity = 0.8,
                                highlightOptions = highlightOptions(color = "#b22233", weight = 3, bringToFront = TRUE),
                                smoothFactor = 0.5
                        ) %>%
                        addLabelOnlyMarkers(
                                data = leaflet_data,
                                lat = as.numeric(leaflet_data$latitude),
                                lng = as.numeric(leaflet_data$longitude),
                                label = as.character(leaflet_data$state),
                                labelOptions = labelOptions(noHide = TRUE, direction = "center", textOnly = TRUE)
                        ) %>%
                        addEasyButton(
                                easyButton(
                                        icon = "fas fa-home",
                                        title = "Original zoom",
                                        position = "topleft",
                                        onClick = JS(
                                                paste0(
                                                        "function(btn, map) {
                 map.fitBounds([[", 19, ",", -124, "], [", 50, ",", -67, "]])
               }"
                                                )
                                        )
                                )
                        ) %>%
                        addLegend(
                                "bottomright",
                                pal = color_palette(),
                                values = leaflet_data$palette_combo,
                                title = "State considered a bill<br>that includes all<br>selected provisions:",
                                opacity = 0.8
                        )
        })
        
        output$mymap_passed <- renderLeaflet({
                leaflet_data <- updated_data()
                
                leaflet(options = leafletOptions(maxZoom = 7, minZoom = 3.75, zoomControl = TRUE,
                                                 zoomSnap = 0.25, zoomDelta = 0.5)) %>%
                        addPolygons(
                                layerId = ~ID_1,
                                group = "states",
                                data = leaflet_data,
                                fillColor = ~color_palette()(leaflet_data$palette_combo),
                                weight = ~ ifelse(leaflet_data$state == "US", 4, 1),
                                color = ~ ifelse(leaflet_data$state == "US", "#f2c508", "#666666") ,
                                opacity = 0.4,
                                fillOpacity = 0.8,
                                highlightOptions = highlightOptions(color = "#b22233", weight = 3, bringToFront = TRUE),
                                smoothFactor = 0.5
                        ) %>%
                        addLabelOnlyMarkers(
                                data = leaflet_data,
                                lat = as.numeric(leaflet_data$latitude),
                                lng = as.numeric(leaflet_data$longitude),
                                label = as.character(leaflet_data$state),
                                labelOptions = labelOptions(noHide = TRUE, direction = "center", textOnly = TRUE)
                        ) %>%
                        addEasyButton(
                                easyButton(
                                        icon = "fas fa-home",
                                        title = "Original zoom",
                                        position = "topleft",
                                        onClick = JS(
                                                paste0(
                                                        "function(btn, map) {
                 map.fitBounds([[", 18.58959, ",", -125.25379, "], [", 49.36949, ",", -66.97732, "]])
               }"
                                                )
                                        )
                                )
                        ) %>%
                        addLegend(
                                "bottomright",
                                pal = color_palette(),
                                values = leaflet_data$palette_combo,
                                title = "State passed a bill<br>that includes all<br>selected provisions:",
                                opacity = 0.8
                        )
        })
        
        output$mymap_considering <- renderLeaflet({
                leaflet_data <- updated_data()
                
                leaflet(options = leafletOptions(maxZoom = 7, minZoom = 3.75, zoomControl = TRUE,
                                                 zoomSnap = 0.25, zoomDelta = 0.5)) %>%
                        addPolygons(
                                layerId = ~ID_1,
                                group = "states",
                                data = leaflet_data,
                                fillColor = ~color_palette()(leaflet_data$palette_combo),
                                weight = ~ ifelse(leaflet_data$state == "US", 4, 1),
                                color = ~ ifelse(leaflet_data$state == "US", "#f2c508", "#666666") ,
                                opacity = 0.4,
                                fillOpacity = 0.8,
                                highlightOptions = highlightOptions(color = "#b22233", weight = 3, bringToFront = TRUE),
                                smoothFactor = 0.5
                        ) %>%
                        addLabelOnlyMarkers(
                                data = leaflet_data,
                                lat = as.numeric(leaflet_data$latitude),
                                lng = as.numeric(leaflet_data$longitude),
                                label = as.character(leaflet_data$state),
                                labelOptions = labelOptions(noHide = TRUE, direction = "center", textOnly = TRUE)
                        ) %>%
                        addEasyButton(
                                easyButton(
                                        icon = "fas fa-home",
                                        title = "Original zoom",
                                        position = "topleft",
                                        onClick = JS(
                                                paste0(
                                                        "function(btn, map) {
                 map.fitBounds([[", 18.58959, ",", -125.25379, "], [", 49.36949, ",", -66.97732, "]])
               }"
                                                )
                                        )
                                )
                        ) %>%
                        addLegend(
                                "bottomright",
                                pal = color_palette(),
                                values = leaflet_data$palette_combo,
                                title = "State is considering a bill<br>that includes all<br>selected provisions:",
                                opacity = 0.8
                        )
        })
        
        output$feat_selected_all <- renderUI({
                feature <- req(rv$selected)
                i <- which(filtered_data()$ID_1 == feature$id)
                HTML(filtered_data()$Summary[i])
        })
        
        output$feat_selected_passed <- renderUI({
                feature <- req(rv$selected)
                i <- which(filtered_data()$ID_1 == feature$id)
                HTML(filtered_data()$Summary[i])
        })
        
        output$feat_selected_considering <- renderUI({
                feature <- req(rv$selected)
                i <- which(filtered_data()$ID_1 == feature$id)
                HTML(filtered_data()$Summary[i])
        })
        
        # Observer for "mymap_all"
        observeEvent(input$mymap_all_shape_click, {
                handle_map_click(input$mymap_all_shape_click, "mymap_all")
        })
        
        # Observer for "mymap_passed"
        observeEvent(input$mymap_passed_shape_click, {
                handle_map_click(input$mymap_passed_shape_click, "mymap_passed")
        })
        
        # Observer for "mymap_considering"
        observeEvent(input$mymap_considering_shape_click, {
                handle_map_click(input$mymap_considering_shape_click, "mymap_considering")
        })
        
        
        handle_map_click <- function(clicked_shape, map_id) {
                new_selected <- req(clicked_shape)
                isolate(old_selected <- rv$selected)
                
                if (is.null(old_selected) || new_selected$.nonce != old_selected$.nonce) {
                        rv$selected <- new_selected
                        i <- which(filtered_data()$ID_1 == new_selected$id)
                        selected_data <- filtered_data()[i, ]
                        
                        leafletProxy(map_id) %>%
                                clearGroup("selection") %>%
                                addPolygons(
                                        layerId = ~ID_1,
                                        group = "selection",
                                        data = selected_data,
                                        fillColor = "#b22233",
                                        weight = 1.2,
                                        color = "#666666",
                                        opacity = 0.4,
                                        fillOpacity = 0.8
                                )
                } else {
                        rv$selected <- NULL
                        leafletProxy(map_id) %>%
                                clearGroup("selection")
                }
        }
}

shinyApp(ui, server)