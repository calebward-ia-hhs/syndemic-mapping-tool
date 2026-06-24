# --- Load Libraries ---
library(shiny)
library(leaflet)
library(leaflet.extras)
library(readr)
library(dplyr)
library(sf)
library(readxl)
library(htmltools)
library(DT)

# setwd("C:/Users/cward1/OneDrive - State of Iowa HHS/Syndemic Epidemiology/Disease Rates/County Rates/Shiny App/Syndemic_Bivariate_Maps/")

# --- Load Data ---
iowa_data <- sf::st_read("data/SYNTHETIC_Iowa_Data_Syndemic.geojson")
iowa_data <- st_transform(iowa_data, crs = 4326)

iowa_data <- iowa_data %>%
  mutate(
    HIV_rate = suppressWarnings(as.numeric(HIV_rate))
  )

svi_county <- read_csv("data/iowa_counties_svi.csv", show_col_types = F)
cbss_sites <- read_csv("data/CBSS_Sites.csv", show_col_types = F)
cbss_sites <- st_as_sf(cbss_sites, coords = c("Longitude", "Latitude"), crs = 4326)
cbss_sites <- sf::st_transform(cbss_sites, crs = 4326)
rw_sites <- read_csv("data/rw_pb_pc_sites.csv", show_col_types = F)
rw_sites <- st_as_sf(rw_sites, coords = c("Longitude", "Latitude"), crs = 4326)
rw_sites <- sf::st_transform(rw_sites, crs = 4326)

spots_jurisdictions <- read_csv("data/SPOTS_counties.csv", show_col_types = F)

county_sdoh <- read_csv("data/county_sdoh.csv", show_col_types = F)
iowa_data <- iowa_data %>% left_join(county_sdoh, by = c("NAME" = "county"))

# --- Color Palette ---
bivar_palette_3x3 <- c(
  "11" = "#f2f5ef", "21" = "#c5e4d7", "31" = "#5ec6b0",
  "12" = "#e1edd1", "22" = "#a3c79c", "32" = "#48907c",
  "13" = "#c0d45c", "23" = "#7e9e47", "33" = "#4a766a"
)

# bivar_palette_3x3 <- c(
#   "11" = "#d9e6db", "21" = "#c5e4d7", "31" = "#5ec6b0",
#   "12" = "#cadfc2", "22" = "#a3c79c", "32" = "#48907c",
#   "13" = "#c0d45c", "23" = "#7e9e47", "33" = "#4a766a"
# )

# --- UI ---
ui <- fluidPage(
  tags$head(
    tags$style(HTML("
      body { background-color: #03617A; margin: 0; padding: 0; font-family: sans-serif; }
      #top-bar { display: flex; justify-content: space-between; align-items: center; padding: 10px 20px; }
      #logo-wrapper { background: white; padding: 10px 10px; border-radius: 10px; max-height: 80px; display: flex; align-items: center; }
      #logo { max-height: 60px; }
      #title { color: white; font-weight: bold; font-size: 38px; text-align: right; flex-grow: 1; padding-left: 20px; }
      #main-content { display: flex; padding: 20px; gap: 20px; }
      #inputs { background: white; border-radius: 6px; padding: 15px 20px; max-width: 320px; flex-shrink: 0; }
      #map-container { flex-grow: 1; height: 85vh; }

      /* Inactive tabs — white text */
      .nav-tabs > li > a {
         color: white !important;
         font-weight: bold !important;
      }

      /* Active tab — black text */
      .nav-tabs > li.active > a,
      .nav-tabs > li.active > a:focus,
      .nav-tabs > li.active > a:hover {
        color: black !important;
        font-weight: bold !important;
      }

      /* Optional hover effect for inactive tabs */
      .nav-tabs > li > a:hover {
        color: #e6f7ff !important;
      }
      
      
    "))
  ),
  # Top bar
  div(id = "top-bar",
      div(id = "logo-wrapper", tags$img(id = "logo", src = "HHS_horiz_color.png", alt = "Iowa Logo")),
      div(id = "title", "Iowa Syndemic Health Map")
  ),
  # Main content
  div(
    id = "main-content",
    style = "display: flex; padding: 20px; gap: 20px;",
    
    # LEFT SIDE — TABS WITH MAP + TABLE
    div(
      style = "flex-grow: 1;",
      
      tabsetPanel(
        id = "maptable_tabs",
        type = "tabs",
        
        tabPanel(
          "Map",
          div(
            id = "map-container",
            style = "height: 85vh;",
            leafletOutput("bivar_map", width = "100%", height = "100%")
          )
        ),
        
        tabPanel(
          "County Table",
          div(
            style = "background:white; padding:10px; border-radius:6px; margin-top:10px;",
            DT::dataTableOutput("countyTable")
          )
        )
      )
    ),
      div(id = "inputs",
          selectInput("xvar_type", "X Variable Type", choices = c("Disease Rates", "Social Vulnerability Index", "CDC Places Data"), selected = "Disease Rates"),
          selectInput("xvar", "X Variable", choices = NULL),
          selectInput("yvar_type", "Y Variable Type", choices = c("Disease Rates", "Social Vulnerability Index", "CDC Places Data"), selected = "Disease Rates"),
          selectInput("yvar", "Y Variable", choices = NULL),
          checkboxInput("show_sites", "Show CBSS Testing Sites", value = FALSE),
          checkboxInput("show_spots", "Show SPOTS Jurisdictions", value = FALSE),
          checkboxInput("show_rwpb_sites", "Show Ryan White Part B Subrecipients", value = FALSE),
          checkboxInput("show_rwpc_sites", "Show Ryan White Part C Clinics", value  = FALSE)
      )
  )
)

# --- Server ---
server <- function(input, output, session) {
  disease_vars <- c("HIV", "HCV", "HBV", "Syphilis", "Gonorrhea", "Chlamydia")
  svi_vars <- c(
    "Socioeconomic" = "Socioeconomic",
    "Household Characteristics" = "Household_Characteristics",
    "Minority Status" = "Minority_Status",
    "Housing & Transport" = "Housing_Transport",
    "Overall SVI" = "SVI_Overall"
  )
  cdc_places_vars <- c(
    "Below Poverty" = "below_poverty",
    "Crowding" = "crowding",
    "No High School Diploma" = "no_diploma",
    "Unemployment" = "unemployment",
    "No Broadband" = "no_broadband",
    "Minority Status" = "minority_status",
    "Single Parent Households" = "single_parent",
    "Housing Cost Burden" = "housing_cost_burden",
    "Age 65+" = "age_65_plus"
  )
  
  site_type_colors <- c(
    "Public" = "#694b5f",
    "Student Health" = "#fed401",
    "Corrections" = "#f27024"
  )
  
  rw_type_colors <- c(
    "Part B Subrecipient" = "#3b9c9c",
    "Part C Clinic" = "#e07b91"
  )
  
  spots_site_colors <- list(
    "Black Hawk County Public Health" = "#1f77b4",
    "Cerro Gordo Public Health" = "#ff7f0e",
    "Hillcrest Family Services" = "#2ca02c",
    "Johnson County Public Health" = "#d62728",
    "Linn County Public Health" = "#9467bd",
    "Polk County Health Department" = "#8c564b",
    "Pottawattamie County Public Health" = "#e377c2",
    "Primary Health Care" = "#7f7f7f",
    "Scott County Health Department" = "#bcbd22",
    "Siouxland Community Health Center" = "#17becf",
    "Webster County Health Department" = "#aec7e8",
    "Dubuque Visiting Nurse Association" = "#ffbb78",
    "River Hills Community Health Center" = "#98df8a",
    "Nebraska AIDS Project" = "#ff9896",
    "Project of the Quad Cities" = "#c5b0d5"
  )
  
  rw_sites <- rw_sites %>%
    mutate(marker_color = unname(rw_type_colors[Type]))
  
  cbss_sites <- cbss_sites %>% mutate(marker_color = unname(site_type_colors[Type]))
  
  observeEvent(input$xvar_type, {
    choices <- switch(input$xvar_type,
                      "Disease Rates" = disease_vars,
                      "Social Vulnerability Index" = svi_vars,
                      "CDC Places Data" = cdc_places_vars)
    
    default <- if (input$xvar_type == "Disease Rates") "HIV" else choices[[1]]
    
    updateSelectInput(session, "xvar", choices = choices, selected = default)
  })
  
  observeEvent(input$yvar_type, {
    choices <- switch(input$yvar_type,
                      "Disease Rates" = disease_vars,
                      "Social Vulnerability Index" = svi_vars,
                      "CDC Places Data" = cdc_places_vars)
    
    default <- if (input$yvar_type == "Disease Rates") "HCV" else choices[[1]]
    
    updateSelectInput(session, "yvar", choices = choices, selected = default)
  })
  
  get_tooltip_label <- function(var, value) {
    
    display_value <- ifelse(is.na(value), "< 5 Cases (Suppressed)", round(value,1))
    
    if (var %in% disease_vars)
      return(paste0(var, " Rate per 100k: ", display_value))
    
    if (var %in% svi_vars)
      return(paste0(names(svi_vars)[which(svi_vars == var)], ": ", round(value,2)))
    
    if (var %in% cdc_places_vars)
      return(paste0(names(cdc_places_vars)[which(cdc_places_vars == var)], ": ", round(value,2)))
    
    return(paste0(var, ": ", display_value))
  }
  
  data_bivar <- reactive({
    req(input$xvar, input$yvar, input$xvar_type, input$yvar_type)
    
    get_rate_tert <- function(type, var) {
      if (type == "Disease Rates") return(c(paste0(var, "_rate"), paste0(var, "_tertile")))
      if (type == "Social Vulnerability Index") return(c(var, paste0(var, "_tertile")))
      if (type == "CDC Places Data") return(c(paste0(var, "_value"), paste0(var, "_tertile")))
    }
    
    rate_x <- get_rate_tert(input$xvar_type, input$xvar)[1]
    tert_x <- get_rate_tert(input$xvar_type, input$xvar)[2]
    rate_y <- get_rate_tert(input$yvar_type, input$yvar)[1]
    tert_y <- get_rate_tert(input$yvar_type, input$yvar)[2]
    
    iowa_data %>%
      mutate(
        bivar_cat = ifelse(is.na(.data[[tert_x]]) | is.na(.data[[tert_y]]), NA, paste0(.data[[tert_x]], .data[[tert_y]])),
        fill_color = ifelse(is.na(bivar_cat), "#cccccc", bivar_palette_3x3[bivar_cat]),
        tooltip = paste0(
          "<b>", NAMELSAD, "</b><br>",
          get_tooltip_label(input$xvar, .data[[rate_x]]), "<br>",
          get_tooltip_label(input$yvar, .data[[rate_y]])
        )
      )
  })

  generate_legend_html <- function(xvar, yvar, xvar_type, yvar_type, svi_vars, disease_vars, palette) {
    get_cat_label <- function(var_type) {
      if (var_type == "Disease Rates") return("Disease Rate")
      if (var_type == "Social Vulnerability Index") return("SVI Value")
      if (var_type == "CDC Places Data") return("CDC Places")
      return("Value")
    }
    
    x_cat_label <- get_cat_label(xvar_type)
    y_cat_label <- get_cat_label(yvar_type)
    
    html <- paste0(
      '<div style="font-family:sans-serif; margin-bottom: 6px; font-size: 18px;">',
      '<b>X: <span style="color: #5ec6b0;">', sub(":.*", "", get_tooltip_label(xvar, 0)), '</span></b><br>',
      '<b>Y: <span style="color: #c0d45c;">', sub(":.*", "", get_tooltip_label(yvar, 0)), '</span></b>',
      '</div>',
      '<div style="background:white; padding:12px; border:1px solid #ccc; font-family:sans-serif; max-width: 220px;">',
      '<div style="display: flex; flex-direction: row; align-items: center;">',
      '<div style="writing-mode: vertical-rl; transform: rotate(180deg); font-weight: 600; font-size: 18px; margin-right: 9px;">',
      y_cat_label, ' →</div>',
      '<div><table style="border-collapse: collapse;"><tbody>'
    )
    
    for (y in 3:1) {
      html <- paste0(html, "<tr>")
      for (x in 1:3) {
        cat_code <- paste0(x, y)
        color <- palette[cat_code]
        html <- paste0(html, "<td style='background:", color, "; width:40px; height:40px;'></td>")
      }
      html <- paste0(html, "</tr>")
    }
    
    html <- paste0(
      html, "</tbody></table>",
      "<div style='text-align: center; font-weight: 700; font-size: 18px; margin-top: 6px;'>",
      x_cat_label, " →</div></div></div></div>"
    )
    htmltools::HTML(html)
  }
  
  ########TABLE
  
  table_data <- reactive({
    req(input$xvar, input$yvar, input$xvar_type, input$yvar_type)
    
    get_rate_col <- function(type, var) {
      if (type == "Disease Rates") return(paste0(var, "_rate"))
      if (type == "Social Vulnerability Index") return(var)
      if (type == "CDC Places Data") return(paste0(var, "_value"))
    }
    
    x_col <- get_rate_col(input$xvar_type, input$xvar)
    y_col <- get_rate_col(input$yvar_type, input$yvar)
    
    df <- iowa_data %>%
      st_set_geometry(NULL) %>%
      select(county = NAME, !!x_col) %>%   # always select X
      mutate(across(where(is.numeric), ~ round(., 2)))
    
    # If X and Y are different, add Y column; else duplicate X
    if (x_col != y_col) {
      df <- df %>%
        mutate(!!y_col := round(iowa_data[[y_col]], 2))
    } else {
      df <- df %>%
        mutate(!!paste0(y_col, "_Y") := .[[x_col]])
    }
    
    # Friendly column names
    x_label <- gsub(":.*", "", get_tooltip_label(input$xvar, 0))
    y_label <- gsub(":.*", "", get_tooltip_label(input$yvar, 0))
    if (x_col == y_col) y_label <- paste0(y_label, " (Y)")
    
    colnames(df) <- c("County", x_label, y_label)
    
    df
  })
  

  
  output$countyTable <- DT::renderDataTable({
    DT::datatable(
      table_data(),
      rownames = FALSE,
      options = list(
        pageLength = 10,
        columnDefs = list(
          list(
            targets = "_all",
            render = JS(
              "function(data,type,row){ 
             if(type === 'display' && data === null) return '< 5 cases (Suppressed)';
             return data;
          }"
            )))))
  })
  
  
  

  output$bivar_map <- renderLeaflet({
    data <- data_bivar()  # your existing data for counties
    
    # Base map setup
    map <- leaflet(data) %>%
      addMapPane("cbssPane", zIndex = 650) %>%
      setView(lng = -93.5, lat = 42.0, zoom = 7) %>%
      addProviderTiles("CartoDB.PositronNoLabels") %>%
      addFullscreenControl(position = "topleft") %>%
      addPolygons(
        fillColor = ~fill_color,
        color = "#ffffff",
        weight = 1,
        opacity = 1,
        fillOpacity = 0.9,
        label = lapply(data$tooltip, HTML),
        popup = lapply(data$tooltip, HTML),
        highlightOptions = highlightOptions(
          weight = 2,
          color = "#000",
          fillOpacity = 0.9,
          bringToFront = TRUE
        ),
        group = "Counties"
      )
    
    # RW Part B
    if (input$show_rwpb_sites) {
      map <- map %>%
        addCircleMarkers(
          data = rw_sites %>% filter(Type == "Part B Subrecipient"),
          radius = 6,
          fillColor = ~marker_color,
          fillOpacity = 0.9,
          color = "#FFFFFF",
          weight = 1,
          popup = ~paste0("<b>", Name, "</b><br>Type: ", Type),
          label = ~Name,
          options = pathOptions(pane = "cbssPane"),
          group = "Ryan White Part B"
        )
    }
    
    # RW Part C
    if (input$show_rwpc_sites) {
      map <- map %>%
        addCircleMarkers(
          data = rw_sites %>% filter(Type == "Part C Clinic"),
          radius = 6,
          fillColor = ~marker_color,
          fillOpacity = 0.9,
          color = "#FFFFFF",
          weight = 1,
          popup = ~paste0("<b>", Name, "</b><br>Type: ", Type),
          label = ~Name,
          options = pathOptions(pane = "cbssPane"),
          group = "Ryan White Part C"
        )
    }
    
    # Add CBSS markers if checked
    if (input$show_sites) {
      map <- map %>%
        addCircleMarkers(
          data = cbss_sites,
          radius = 6,
          fillColor = cbss_sites$marker_color,
          fillOpacity = 0.9,
          color = "#FFFFFF",
          weight = 1,
          popup = ~paste0("<b>", Facility_Name, "</b><br>Type: ", Type),
          label = ~Facility_Name,
          options = pathOptions(pane = "cbssPane"),
          group = "CBSS Sites"
        )
    }
    
    if (input$show_spots) {
      # Create a mapping of counties to all their associated Site_Names
      county_site_map <- spots_jurisdictions %>%
        group_by(County) %>%
        summarise(Sites = paste(unique(Site_Name), collapse = ", "), .groups = "drop")
      
      # Join this to the iowa_data (your county polygons)
      site_counties <- iowa_data %>%
        inner_join(county_site_map, by = c("NAME" = "County")) %>%
        mutate(
          fill_color = as.character(sapply(NAME, function(county_name) {
            # Pick the color of the first site (just to shade something)
            site_list <- spots_jurisdictions %>%
              filter(County == county_name) %>%
              pull(Site_Name) %>%
              unique()
            spots_site_colors[[site_list[1]]]  # Use first site's color
          })),
          tooltip = paste0(
            "<strong>County: </strong>", NAME, "<br>",
            "<strong>SPOTS: </strong>", Sites
          )
        )
      
      # Add polygons for the SPOTS jurisdictions
      map <- map %>%
        addPolygons(
          data = site_counties,
          fillColor = ~fill_color,
          fillOpacity = 0.5,
          color = ~fill_color,
          weight = 2,
          group = "SPOTS Jurisdictions",
          label = lapply(site_counties$tooltip, HTML),
          popup = lapply(site_counties$tooltip, HTML),
          highlightOptions = highlightOptions(
            weight = 3,
            color = "#000000",
            fillOpacity = 0.7,
            bringToFront = TRUE
          )
        )
    }
    

    
    
    # Add bivariate legend (existing)
    map <- map %>%
      addControl(
        generate_legend_html(
          input$xvar,
          input$yvar,
          input$xvar_type,
          input$yvar_type,
          svi_vars,
          disease_vars,
          bivar_palette_3x3
        ),
        position = "topright"
      )
    
    # Add RW legend if shown
    if (input$show_rwpb_sites | input$show_rwpc_sites) {
      map <- map %>%
        addLegend(
          position = "bottomright",
          title = "Ryan White Sites",
          colors = rw_type_colors,
          labels = names(rw_type_colors),
          opacity = 1
        )
    }
    
    # Add CBSS legend if shown
    if (input$show_sites) {
      map <- map %>%
        addLegend(
          position = "bottomright",
          title = "CBSS Site Type",
          colors = site_type_colors,
          labels = names(site_type_colors),
          opacity = 1
        )
    }
    
    # Add SPOTS legend if shown
    if (input$show_spots) {
      map <- map %>%
        addLegend(
          position = "bottomleft",
          title = "SPOTS Jurisdictions",
          colors = spots_site_colors,
          labels = names(spots_site_colors),
          opacity = 1
        )
    }
    
    map
  })
  
}

# --- Run App ---
shinyApp(ui, server)
