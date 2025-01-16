#'
#' title: Home range sizes trough CPE
#' description: Implements a the Minimum Convex Polygon Estimator to compute the home range sizes for each of 4 monkey groups under study
#' 
#' 

#
# 0.0 research questions
#

#
# 0.1 set-up (libraries)
#

# Load required libraries
library(dplyr)          # Data manipulation
library(tidyr)          # Data tidying
library(magrittr)       # Pipe operators
library(ggplot2)        # Data visualization
library(purrr)          # Functional programming
library(openxlsx)       # Excel file handling
library(sf)             # Simple features for spatial data
library(leaflet)        # Interactive maps
library(adehabitatHR)   # Habitat analysis tools
library(sp)             # Spatial data handling
library(plyr)           # Data manipulation, including mapvalues
library(RSQLite)        # SQLite database interaction
library(htmlwidgets)    # HTML widgets for web-based visualization
library(rgdal)          # For CRS transformations
library(lubridate)      # Date and time manipulation

#
# 0.1 set-up (work environment)
#

rm(list=ls())
HOME_RANGES_HOME=dirname(rstudioapi::getActiveDocumentContext()$path)
setwd(HOME_RANGES_HOME)
list.files()

#
# 0.2 set-up (custom libraries)
#

source(sprintf("%s/config.R","C:/Users/57320/Desktop/consultorias 2024/6267839/monkey_research_collaboration/monkeys"))
source(sprintf( "%s/%s", "C:/Users/57320/Desktop/consultorias 2024/6267839/monkey_research_collaboration/monkeys", "setup_db_conn.R"))
source(sprintf( "%s/%s", TRANSVERSAL_CODE_FOLDER,"resampling_dataframes.R"))
source(sprintf( "%s/%s", TRANSVERSAL_CODE_FOLDER,"dataframe_extentions.R"))
source(sprintf( "%s/%s", TRANSVERSAL_CODE_FOLDER,"kruskal_dunn_test.R"))
source(sprintf( "%s/%s", TRANSVERSAL_CODE_FOLDER,"contingency_analysis_for_group_features.R"))
dbListTables(db_con)

#
# (0.3) set-up (configuration)
#

PERCENT_SAMPLE_HOME_RANGES=98 # winsorizing parameter to cap the home ranges computations
MIN_SAMPLE_SIZE <- 15 # min sample size for monthly computations
TILE_PROVIDER <- "Esri.WorldImagery"

#
# 1. Reading/inspecting waypoint data: We load to memory a
#    dataset of over 4000 records about sights of monkey groups
#    of each of 4 monkey groups under study. Spatial information
#    such as location each sight is the primary information needed
#    for the computation of the home ranges.
#

#
#
#
gpx_file <- "Home ranges WPS (1).gpx"
setwd(HOME_RANGES_HOME)
#
# layers analysis
#
st_layers(gpx_file)
#
#
#
waypoints <- st_read(gpx_file, layer = "waypoints")
waypoints %>% nrow()
# > waypoints %>% nrow()
# [1] 4253
waypoints %>% dplyr::sample_n(30)

#
# 2. Exploratoy map for the waypoints
#

leaflet(data = waypoints) %>%
  addTiles() %>%  # Add default OpenStreetMap tiles
  addCircleMarkers(, color = "blue", radius = 2, opacity = .00001) 

#
# 2. Discriminating per monkey group, recovering the group labels with k-means
#


#
# (1) recovering the group labels trough a k-means algorithm 
#

set.seed(123)  # For reproducibility

# Read the .gpx file and extract waypoints
waypoints <- st_read(gpx_file, layer = "waypoints")

# Extract coordinates from the geometry
coords <- st_coordinates(waypoints)  # Get longitude and latitude

# Apply k-means clustering on the coordinates
kmeans_result <- kmeans(coords, centers = 4)

# Add the cluster labels as a new column in the waypoints data
waypoints$cluster <- as.character(kmeans_result$cluster)

# Assuming waypoints has already been clustered and has a 'cluster' column
coords <- st_coordinates(waypoints)  # Get the longitude and latitude


# Step 1: Split the data by cluster
split_data <- split(waypoints, waypoints$cluster)

# Step 2: Use lapply to calculate the average latitude for each group
cluster_means <- lapply(split_data, function(group) {
  data.frame(
    cluster = unique(group$cluster),
    avg_lat = mean(st_coordinates(group)[, 2])  # Calculate average latitude (Y)
  )
})

# Step 3: Combine the results into a single data frame
cluster_means <- do.call(rbind, cluster_means)

# Step 4: Sort by decreasing latitude and assign group labels
cluster_means <- cluster_means %>%
  arrange(desc(avg_lat))  # Sort by decreasing average latitude



# Assign species group labels based on latitude order
group_labels <- c("Wangwamani", "Deep Forest", "Joshi", "Camp")
cluster_means$species_group <- group_labels

cluster_means

# Step 5: Map these species group labels back to the original waypoints data
waypoints$species_group <- plyr::mapvalues(waypoints$cluster, 
                                           from = cluster_means$cluster, 
                                           to = cluster_means$species_group)


# Define the function to map waypoints by species group with customizable tile provider
map_waypoints_all_groups <- function(waypoints, tile_provider = "Esri.WorldImagery", polygon_data = NULL, polygon_color = "blue") {
  # Sanity check to ensure 'species_group' column exists
  if (!"species_group" %in% colnames(waypoints)) {
    stop("The 'species_group' column does not exist in the waypoints data.")
  }
  
  # Define color palette for species groups
  pal <- colorFactor(palette = "Set1", domain = waypoints$species_group)
  
  # Create the leaflet map with customizable tile layers
  leaflet_map <- leaflet() %>%
    addTiles(
      urlTemplate = switch(
        tile_provider,
        "OSM" = "https://{s}.tile.openstreetmap.org/{z}/{x}/{y}.png",
        "Stamen.Terrain" = "http://{s}.tile.stamen.com/terrain/{z}/{x}/{y}.png",
        "Stamen.Toner" = "http://{s}.tile.stamen.com/toner/{z}/{x}/{y}.png",
        "Esri.WorldImagery" = "https://server.arcgisonline.com/ArcGIS/rest/services/World_Imagery/MapServer/tile/{z}/{y}/{x}.jpg",
        "Esri.WorldStreetMap" = "https://server.arcgisonline.com/ArcGIS/rest/services/World_Street_Map/MapServer/tile/{z}/{y}/{x}.jpg"
      ),
      attribution = "Tiles &copy; OpenStreetMap contributors"
    ) %>%
    addCircleMarkers(
      data = waypoints,
      color = ~pal(species_group),
      radius = 1,
      opacity = 0.2,
      label = ~paste("Group:", species_group)
    ) %>%
    addScaleBar(position = "bottomleft") %>%
    addLegend("bottomright", pal = pal, values = waypoints$species_group, 
              title = "Species Group") 
  
  # Optionally add polygon data if provided
  if (!is.null(polygon_data)) {
    leaflet_map <- leaflet_map %>%
      addPolygons(
        data = polygon_data,
        fillColor = "transparent",
        color = polygon_color,
        weight = 2,
        opacity = 1
      )
  }
  
  return(leaflet_map)
}
#
map_waypoints_all_groups(waypoints)

# Example usage with OpenStreetMap tiles
map_waypoints_all_groups(waypoints, tile_provider = "OSM")

# Example usage with Esri World Imagery tiles (default)
map_waypoints_all_groups(waypoints, tile_provider = "Esri.WorldImagery")

# Example usage with Esri World Street Map tiles
map_waypoints_all_groups(waypoints, tile_provider = "Esri.WorldStreetMap")


#
# 4. home range size computation
#


#
# (0)
#
home_range_size_for_group <- function(
    waypoints, 
    group_label,
    percent = PERCENT_SAMPLE_HOME_RANGES
) {
  tryCatch({
    # Validate that the group label exists in the waypoints data
    if (!(group_label %in% unique(waypoints$species_group))) {
      stop("Group label not found in the available species groups.")
    }
    
    # Filter waypoints for the specified group
    filtered_waypoints <- waypoints %>% 
      filter(species_group == group_label)
    
    # Check if there are waypoints for the specified group
    if (nrow(filtered_waypoints) == 0) {
      stop("No waypoints found for the specified group.")
    }
    
    # Convert to SpatialPoints
    spatial_points <- as(filtered_waypoints, "Spatial")
    
    # Calculate the centroid of the spatial points
    centroid <- coordinates(spatial_points) %>%
      colMeans()  # Average of longitude and latitude
    
    # Determine the UTM zone based on the centroid longitude
    longitude <- centroid[1]
    utm_zone <- floor((longitude + 180) / 6) + 1
    
    # Create the UTM projection string
    utm_projection <- paste0("+proj=utm +zone=", utm_zone, " +datum=WGS84 +units=m +no_defs")
    
    # Transform spatial points to UTM using the CRS object
    spatial_points_utm <- spTransform(spatial_points, CRS(utm_projection))
    
    # Calculate MCP for the specified group
    waypoints.mcp <- mcp(spatial_points_utm, 
                         percent = percent,
                         unin = "m",      # Input coordinates are now in meters
                         unout = "ha"     # Output area in square meters
    ) %>%
      as.data.frame() %>%
      pull(area) %>%
      round(3)
    
    # Return the result as a data frame
    data.frame(
      Group = group_label,
      Area = waypoints.mcp,
      Percent =percent 
    )
  }, error = function(e) {
    # Log error details to the console
    message(sprintf("Error processing group '%s' with sample size %d: %s",
                    group_label, nrow(waypoints %>% filter(species_group == group_label)), e$message))
    
    # Return a data frame with NA if an error occurs
    data.frame(
      Group = group_label,
      Area = NA_real_,
      Percent =percent 
    )
  })
}



#
# 5. main method to compute the home range sizes
#


#
# (0)
#
map_waypoints_all_groups(waypoints, TILE_PROVIDER)

#
# (1) Overall Home Ranges
#
home_range_sizes <- waypoints$species_group %>%
  unique() %>%
  lapply(function(each_group) {
    home_range_size_for_group(waypoints, group_label = each_group, percent = PERCENT_SAMPLE_HOME_RANGES)
  }) %>%
  bind_rows() %>%
  mutate(Methodology = "Annual-Overall")
home_range_sizes %>% View()
#
# (2) Monthly Home Ranges
#
monthly_home_range_sizes <- waypoints %>%
  mutate(month_year = format(as.Date(time), "%m-%Y")) %>%  # Create a 'month_year' column
  group_split(species_group, month_year) %>%              # Split data by group and month-year
  lapply(function(subset) {
    # Extract metadata
    species_group <- unique(subset$species_group)
    month_year <- unique(subset$month_year)
    sample_size <- nrow(subset)
    
    # Log the sample size
    message(sprintf("Processing group '%s' for month '%s' with sample size: %d",
                    species_group, month_year, sample_size))
    
    # Skip if the sample size is below the minimum threshold
    if (sample_size < MIN_SAMPLE_SIZE) {
      message(sprintf("Skipping group '%s' for month '%s' due to insufficient sample size: %d",
                      species_group, month_year, sample_size))
      return(NULL)
    }
    
    # Compute home range size
    result <- home_range_size_for_group(subset, 
                                        group_label = species_group, 
                                        percent = PERCENT_SAMPLE_HOME_RANGES)
    
    # Add metadata
    result %>%
      mutate(
        Month_Year = month_year,
        Sample_Size = sample_size
      )
  }) %>%
  compact() %>%  # Remove NULLs
  bind_rows()
monthly_home_range_sizes %>% View()





#
# (3) Define the function to generate the plot
#

plot_monthly_home_range <- function(data, percent_threshold = 98) {
  
  # Filter the data based on the specified percent threshold
  filtered_data <- data %>%
    filter(Percent == percent_threshold)
  
  # Convert Month_Year to Date format for chronological ordering
  filtered_data$Month_Year <- dmy(paste("01", filtered_data$Month_Year))
  
  # Sort data by Month_Year
  filtered_data <- filtered_data %>% 
    arrange(Month_Year)
  
  # Perform Kruskal-Wallis test between groups
  kruskal_test <- kruskal.test(Area ~ Group, data = filtered_data)
  
  # Extract the test statistic and p-value
  test_statistic <- round(kruskal_test$statistic, 2)
  p_value <- round(kruskal_test$p.value, 3)
  
  # Create a date sequence for x-axis ticks
  date_breaks <- seq(min(filtered_data$Month_Year), max(filtered_data$Month_Year), by = "1 month")
  
  # Create the ggplot
  plot <- ggplot(filtered_data, aes(x = Month_Year, y = Area, color = Group, shape = Group)) +
    geom_line() + 
    geom_point(alpha = 0.8, size = 3) +  # Alpha of 0.8 for the points, size adjusted
    theme_minimal() +
    labs(
      title = "Monthly Home Range Sizes by Group",
      subtitle = "Area calculated with a minimal convex hull method",
      x = "Month-Year",
      y = "Area",
      caption=sprintf("Using %.1f%% of the waypoints to control outliers", percent_threshold)
    ) +
    # In-plot annotation with Kruskal-Wallis test results
    annotate("text", x = max(filtered_data$Month_Year), y = max(filtered_data$Area), 
             label = sprintf("Kruskal-Wallis Test: Stat = %.2f, p = %.3f", test_statistic, p_value),
             hjust = 1, vjust = 1, size = 3, color = "black", fontface = "italic") +
    theme(
      axis.text.x = element_text(angle = 90, hjust = 1, size = 8),  # Rotate labels 90 degrees with smaller text size
      axis.title.x = element_text(size = 10),  # Smaller axis title font
      axis.title.y = element_text(size = 10),  # Smaller axis title font
      plot.title = element_text(size = 14),  # Reduce plot title font size
      plot.subtitle = element_text(size = 10),  # Reduce subtitle font size
      legend.position = "bottom",
      legend.text = element_text(size = 8),  # Reduce legend text size
      legend.title = element_text(size = 10)  # Reduce legend title size
    ) +
    scale_color_brewer(palette = "Set3") +  # Color palette for groups
    scale_x_date(breaks = date_breaks, labels = scales::date_format("%m-%Y"))  # Ticks every month with Month-Year format
  
  # Print the plot
  return(plot)
}

# Call the function with your dataset (replace 'monthly_home_range_sizes' with your actual dataset)
plot_monthly_home_range(monthly_home_range_sizes, percent_threshold = 98)


#
# 5. waypoints per group + convex polygon tecnique
#

#
waypoints_for_group <- function(
    waypoints, 
    group_label, 
    tile_provider = "Esri.WorldImagery",
    percent = PERCENT_SAMPLE_HOME_RANGES  # Default percentage for home range calculation
) {
  
  # Validate that the group label exists in the waypoints data
  if (!(group_label %in% unique(waypoints$species_group))) {
    stop("Group label not found in the available species groups.")
  }
  
  # Filter waypoints for the specified group
  filtered_waypoints <- waypoints %>% 
    filter(species_group == group_label)
  
  # Check if there are waypoints for the specified group
  if (nrow(filtered_waypoints) == 0) {
    stop("No waypoints found for the specified group.")
  }
  
  # Create a color palette for the species groups
  pal <- colorFactor(palette = "Set1", domain = waypoints$species_group)
  
  # Convert to SpatialPoints
  spatial_points <- as(filtered_waypoints, "Spatial")
  
  # Calculate MCP for the specified group
  waypoints_mcp <- mcp(spatial_points, percent = percent)
  
  # Convert the MCP to a suitable format for leaflet
  mcp_polygon <- st_as_sf(waypoints_mcp)
  
  # Determine the color for the polygon based on the group
  polygon_color <- pal(group_label)
  
  # Create the leaflet map
  map <- leaflet() %>%
    addTiles(urlTemplate = switch(tile_provider,
                                  "OSM" = "https://{s}.tile.openstreetmap.org/{z}/{x}/{y}.png",
                                  "Stamen.Terrain" = "http://{s}.tile.stamen.com/terrain/{z}/{x}/{y}.png",
                                  "Stamen.Toner" = "http://{s}.tile.stamen.com/toner/{z}/{x}/{y}.png",
                                  "Esri.WorldImagery" = "https://server.arcgisonline.com/ArcGIS/rest/services/World_Imagery/MapServer/tile/{z}/{y}/{x}.jpg",
                                  "Esri.WorldStreetMap" = "https://server.arcgisonline.com/ArcGIS/rest/services/World_Street_Map/MapServer/tile/{z}/{y}/{x}.jpg"),
             attribution = "Tiles &copy; <a href='http://openstreetmap.org/copyright'>OpenStreetMap</a> contributors") %>%
    addCircleMarkers(data = filtered_waypoints, 
                     color = ~pal(species_group), 
                     radius = 2, 
                     opacity = 0.8,
                     label = ~paste("Group:", species_group)) %>%
    addPolygons(data = mcp_polygon, 
                fillColor = "transparent",
                color = polygon_color,
                weight = 2,
                opacity = 1) %>%
    addScaleBar(position = "bottomleft") %>%
    setView(lng = mean(st_coordinates(filtered_waypoints)[, 1]), 
            lat = mean(st_coordinates(filtered_waypoints)[, 2]), 
            zoom = 16)
  
  # Return the created map object
  return(map)
}

# Example usage
waypoints_for_group(waypoints, group_label="Wangwamani", tile_provider = "Esri.WorldImagery")
waypoints_for_group(waypoints, group_label="Deep Forest", tile_provider = "Esri.WorldImagery")
waypoints_for_group(waypoints, group_label="Joshi", tile_provider = "Esri.WorldImagery")
waypoints_for_group(waypoints, group_label="Camp", tile_provider = "Esri.WorldImagery")

#
# 6. main method to export the results
#

#
# (1)
#
setwd(FILE_FOLDER)
home_range_sizes %>%
  dplyr::mutate(
    percent_of_sample=PERCENT_SAMPLE_HOME_RANGES
  ) %>%
  openxlsx::write.xlsx(
    "home_range_sizes.xlsx", 
    overwrite = TRUE,
  )
#
# (2) Export maps for each group to HTML files in the current folder
#
setwd(FILE_FOLDER)
waypoints$species_group %>%
  unique() %>%
  lapply(function(a_group) {
    # Generate the map for the group
    map <- waypoints_for_group(waypoints, group_label = a_group)
    
    # Save the map as an HTML file with a unique name
    file_name <- paste0("waypoints_map_", gsub(" ", "_", a_group), ".html")
    htmlwidgets::saveWidget(map, file = file_name)
  })


#
# 6. export monthly sizes computation to the database 
#

setwd(RESULTS_DB_FOLDER)
con <- dbConnect(RSQLite::SQLite(), dbname = RESULTS_DB_NAME)
#
# Define a SQL statement to create a new table for home range sizes
#
sql_statement <- "
CREATE TABLE IF NOT EXISTS home_range_sizes (
    id INTEGER PRIMARY KEY AUTOINCREMENT,
    monkey_group TEXT NOT NULL,
    Area REAL NOT NULL
);"
#
# Execute the SQL statement to create the table
#
dbExecute(con, sql_statement)
#
# Insert data into the SQLite table
#
dbWriteTable(con, "home_range_sizes", 
             home_range_sizes %>% 
               dplyr::rename(
                 monkey_group=Group
               ),
             append = FALSE,
             overwrite =TRUE,
             row.names = FALSE)
#
dbGetQuery(con, "SELECT * FROM home_range_sizes;")



#
# 5.
#
setwd(FILE_FOLDER)
save.image("home_ranges_core_logic.R")






