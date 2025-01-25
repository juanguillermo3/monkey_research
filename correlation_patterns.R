#'
#' title: Spatial correlation analysis
#' description: Computes correlation between home ranges sizes and habitat parameters
#' report_url:https://docs.google.com/spreadsheets/d/1KMhoqoPNrcP8Gp5eimwHmUeiFZZhKekL/edit?gid=521562748#gid=521562748
#

#
# 0.0 research questions
#

#
# 0.1 set-up (libraries)
#

library(dplyr)            # Data manipulation
library(ggplot2)          # Data visualization
library(stringr)          # String manipulation
library(leaflet)          # Interactive maps
library(sf)               # Simple features for spatial data
library(sp)               # Spatial data handling
library(cluster)          # Clustering algorithms
library(tmap)             # Thematic maps
library(adehabitatHR)     # Home range analysis and plotting with labels
library(RColorBrewer)     # Color palettes



#
# 0.1 set-up (work environment)
#

rm(list=ls())
SPATIAL_CORRELATIONS_HOME=dirname(rstudioapi::getActiveDocumentContext()$path)
setwd(SPATIAL_CORRELATIONS_HOME)
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
source(sprintf( "%s/%s", TRANSVERSAL_CODE_FOLDER,"flexible_test_across_groups.R"))
source(sprintf( "%s/%s", TRANSVERSAL_CODE_FOLDER,"flexible_excel_reports.R"))
source(sprintf( "%s/%s", TRANSVERSAL_CODE_FOLDER,"recover_entity_table_from_df.R"))
dbListTables(db_con)

#
# (0.3) set-up (configuration)
#

TILE_PROVIDER <- "Esri.WorldImagery"

#
# 1. Exploratory queries over the home ranges
#

#
# (1)
#
dbListTables(db_con)
dbGetQuery(db_con, "SELECT * FROM home_range_sizes ORDER BY RANDOM() LIMIT 6;")
#
# (2)
#
dbGetQuery(db_con, 
           "
           WITH summary_per_group AS (
           SELECT monkey_group, Methodology, COUNT(*) as num_obs
           FROM home_range_sizes
           WHERE PERCENT=95.0
           GROUP BY 1,2
           
           )
           SELECT * FROM summary_per_group
           ")
# monkey_group    Methodology num_obs
# 1         Camp Annual-Overall       1
# 2         Camp        Monthly      13
# 3  Deep Forest Annual-Overall       1
# 4  Deep Forest        Monthly      13
# 5        Joshi Annual-Overall       1
# 6        Joshi        Monthly      14
# 7   Wangwamani Annual-Overall       1
# 8   Wangwamani        Monthly       9

#
# At the database, we have annual and month wise computations of the home
# ranges for each monkey species. It describes general trends on the home
# ranges, but sample size is to small for spatial correlation analysis
#

#
# 2. Exploratory queries on habitat parameters
#

#
# (1) Random sample on vegetation data
#
dbGetQuery(db_con, "SELECT * FROM vegetation_data ORDER BY RANDOM() LIMIT 6;")
#
# (2) Random sample on vegetation data
#
per_plot_vegetation_summary_query=
"

-- 0.1 precalculate the percentile 99 of DBH for outlier control purposes
WITH OrderedData AS (
 SELECT
 dbh,
 ROW_NUMBER() OVER (ORDER BY dbh) AS row_num,
 COUNT(*) OVER () AS total_rows
 FROM vegetation_data
),
PercentileCutoff AS (
 SELECT
 dbh AS dbh_99
 FROM OrderedData
 WHERE row_num = CAST(total_rows * 0.999 AS INT)
),

-- 0.2 precalculate the total land per plot as a constant for the plot level summaries
LandHA AS (
 SELECT
 0.02 AS total_land_ha
),

-- 1.0 summarize the data at the plot level, calculating the habitat parameters
plot_summary AS (

 SELECT
 monkey_group,
 plot,
 

 total_land_ha,
 
 
 COUNT(DISTINCT species) AS abundance,
 COUNT(DISTINCT species) / (total_land_ha) AS species_richness,
 
 COUNT(*) AS number_of_trees,
 COUNT(*) / (total_land_ha) AS density,
 
 SUM(dbh) AS sum_dbh,
 3.1416 * ( (SUM(dbh) / 2) * (SUM(dbh) / 2) ) AS basal_area_cm2,
 3.1416 * ( (SUM(dbh) / 2) * (SUM(dbh) / 2) ) / 10000 AS basal_area_m2,
 (3.1416 * ( (SUM(dbh) / 2) * (SUM(dbh) / 2) ) / 10000) / (total_land_ha) AS dominance
 
 -- Source of the data is vegetation records, merged with constant land constant,
 -- filtered to trees over 5 dbh as required by the researcher
 
 FROM
 vegetation_data,LandHA
 WHERE
 dbh >=5 AND dbh < (SELECT dbh_99 FROM PercentileCutoff)
 
 GROUP BY  plot
)

SELECT
 a.*,
 'all vegetation species' AS species
 FROM plot_summary a
;
"
#
dbGetQuery(db_con,
           per_plot_vegetation_parameters_query 
) %>%
  View()

#
# 3. Recovering location information for the plots
# 

# 
# (0)
#
setwd(VEGETATION_FOLDER)
vegetation_data = readxl::read_excel("Transect analysis.xlsx")
vegetation_data %>% names()
vegetation_data =vegetation_data %>%
  dplyr::transmute(
    monkey_group = tolower(.$"Group...2"),    # Convert group names to lowercase
    plot = tolower(.$"Plot code"),      # Convert plot code to lowercase
    location= tolower(.$"Location") # Convert species name to lowercase
  )
# 
# (1)
#
plot_level_data=vegetation_data  %>%
  recover_entity_table(
    entity_col = "plot", field_cols = c("monkey_group", "location")
  ) %>%
  dplyr::filter(
    location!='?'
  )
#
plot_level_data %>% View()
#
plot_level_data %>%
  head()
# entity monkey_group                   Location
# 1     a1         camp    WP 19, 50m EAST to WP20
# 2    a10         camp WP2422, 50m N/NW to WP2423
# 3    a11         camp    WP2572, 50m E to WP2573
# 4    a12         camp WP2935, 50m N/NW to WP2936
# 5    a13         camp      WP3525, 50m to WP3526
# 6    a14         camp      WP3593, 50m to WP3594

# 
# (4) Regex to extract and clean the first and second waypoint by removing non-integer characters and trimming whitespaces
#
plot_level_data <- plot_level_data %>%
  mutate(
    first_waypoint = str_extract(location, "wp\\s*\\d+") %>%
      str_replace_all("[^0-9]", "") %>%  # Remove non-numeric characters
      str_trim(),  # Trim leading/trailing spaces
    second_waypoint = str_extract(location, "(?<=to\\s)wp\\s*\\d+") %>%
      str_replace_all("[^0-9]", "") %>%  # Remove non-numeric characters
      str_trim()  # Trim leading/trailing spaces
  )  %>%
  drop_na()
#
plot_level_data %>% View()

# 
# (5) loading waypoint location
#

setwd(HOME_RANGES_HOME)
waypoints = read_sf("waypoint.shp") %>%
  dplyr::rename(
    waypoint_id=wypnt_d,
    species_group=spcs_gr
  )
waypoints %>% head()
# Simple feature collection with 6 features and 2 fields
# Geometry type: POINT
# Dimension:     XY
# Bounding box:  xmin: 39.41875 ymin: -6.27232 xmax: 39.42065 ymax: -6.271712
# Geodetic CRS:  WGS 84
# # A tibble: 6 × 3
# wypnt_d spcs_gr             geometry
# <chr>   <chr>            <POINT [°]>
#   1 1       Camp    (39.42061 -6.271773)
# 2 2       Camp    (39.42065 -6.271766)
# 3 3       Camp    (39.41875 -6.271712)
# 4 4       Camp    (39.41893 -6.271926)
# 5 5       Camp    (39.41896 -6.272217)
# 6 6       Camp     (39.41917 -6.27232)

# 
# (6) merging waypoint location
#

#
# (1) First left join 
#
plot_level_data <- plot_level_data %>%
  left_join(
    waypoints %>% 
      dplyr::select(waypoint_id,geometry,species_group),
    by = c("first_waypoint" = "waypoint_id")
  ) %>% #View()
  mutate(
    first_coordinates = geometry,
    first_species_group = species_group
  ) %>%
  dplyr::select(-geometry,-species_group)  # Remove the old columns if not needed
#
# (2) Second left join 
#
plot_level_data <- plot_level_data %>%
  left_join(
    waypoints %>% 
      dplyr::select(waypoint_id,geometry,species_group),
    by = c("second_waypoint" = "waypoint_id")
  ) %>%
  mutate(
    second_coordinates = geometry,
    second_species_group = species_group
  )  %>%
  dplyr::select(-geometry,-species_group)
# View the updated data
plot_level_data= plot_level_data %>%drop_na()
plot_level_data %>% View()


# 
# (7) 
#

#
# (0)
#
plot_level_data %>%  View()
#
# (1) Quality check for consistency between first_species_group and 
#     second_species_group
#
inconsistent_rows <- plot_level_data %>%
  filter(first_species_group != second_species_group)

# Report the inconsistencies (if any)
if (nrow(inconsistent_rows) > 0) {
  warning("Inconsistent species groups found. See below:")
  print(inconsistent_rows)
}

#
# (3) 
#

# Remove the inconsistent rows and return the cleaned data
plot_level_data <- plot_level_data %>%
  filter(first_species_group == second_species_group)

# Return the cleaned plot_level_data
plot_level_data


# 
# (8) compute plot centroids
#

plot_level_data <- plot_level_data %>%
  rowwise() %>%
  mutate(plot_centroid = st_sfc(
    st_point(c(
      (st_coordinates(first_coordinates)[1] + st_coordinates(second_coordinates)[1]) / 2,  # X midpoint
      (st_coordinates(first_coordinates)[2] + st_coordinates(second_coordinates)[2]) / 2   # Y midpoint
    )),
    crs = st_crs(first_coordinates)  # Ensures the CRS matches the original data
  )) %>%
  ungroup()  # Ungroup after the rowwise operation
# View the result
View(plot_level_data)


#
# (9) compute plot centroids
#

# Define the function to plot data
plot_data <- function(data) {
  # Check if plot_centroid is available
  if ("plot_centroid" %in% colnames(data)) {
    leaflet(data) %>%
      addTiles(
        urlTemplate = switch(
          TILE_PROVIDER,
          "OSM" = "https://{s}.tile.openstreetmap.org/{z}/{x}/{y}.png",
          "Stamen.Terrain" = "http://{s}.tile.stamen.com/terrain/{z}/{x}/{y}.png",
          "Stamen.Toner" = "http://{s}.tile.stamen.com/toner/{z}/{x}/{y}.png",
          "Esri.WorldImagery" = "https://server.arcgisonline.com/ArcGIS/rest/services/World_Imagery/MapServer/tile/{z}/{y}/{x}.jpg",
          "Esri.WorldStreetMap" = "https://server.arcgisonline.com/ArcGIS/rest/services/World_Street_Map/MapServer/tile/{z}/{y}/{x}.jpg"
        ),
        attribution = "Tiles &copy; OpenStreetMap contributors"
      ) %>%
      # Add plot centroid coordinates as markers
      addCircleMarkers(
        lng = ~st_coordinates(plot_centroid)[, 1],  # Longitude of the centroid
        lat = ~st_coordinates(plot_centroid)[, 2],  # Latitude of the centroid
        color = ~group_palette(monkey_group),  # Color based on monkey_group
        popup = ~paste("Entity: ", entity, "<br>Group: ", monkey_group),  # Popup with entity and group info
        radius = 5,  # Marker size
        fillOpacity = 0.8  # Fill opacity
      )
  } else {
    message("plot_centroid is not available in the data")
    return(NULL)
  }
}

# Create a color palette based on the monkey_group
group_palette <- colorFactor(c("blue", "green", "red", "purple"), domain = unique(plot_level_data$monkey_group))

# Call the plot_data function with the plot_level_data
plot_data(plot_level_data)


#
# 4. joining plot level features 
# 

#
plot_level_data %>% View()
#
plot_level_statistical_parameters=
dbGetQuery(db_con,
           per_plot_vegetation_summary_query
)
plot_level_statistical_parameters %>% names()
#
plot_level_data <- plot_level_data %>%
  left_join(plot_level_statistical_parameters, by = c("entity" = "plot"))
#
plot_level_data %>% View()


#
# 5. Convert plot_centroid to sf point geometry and save as shapefile
# 

# 
plot_level_data_sf=plot_level_data %>%
  mutate(longitude = st_coordinates(plot_centroid)[, 1],
         latitude = st_coordinates(plot_centroid)[, 2]) %>%
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326) 
#
plot_level_data_sf


#
# 6. discover habitat islands within each home range
# 

# 
# (1) load a shapefile of waypoints labelled with monkey group location
#

# setwd(HOME_RANGES_HOME)
# waypoints = read_sf("waypoint.shp") %>%
#   dplyr::rename(
#     waypoint_id=wypnt_d,
#     species_group=spcs_gr
#   )
# waypoints %>% View()
# 
# 
# monkey_group="Camp"
# 
# #
# table(waypoints$species_group)
# # Camp Deep Forest       Joshi  Wangwamani 
# # 1595         952        1124         582 
# 
# 
# percent_grid=seq(100, 95, by = -1)
# num_clusters =seq(4, 12, by = 2)
# target_balance=0.05
# 
# filtered_waypoints=waypoints %>%
#   dplyr::filter(species_group==monkey_group)
# 
# #
# # Create a dataframe out of the coordinate data
# #
# coords <- st_coordinates(filtered_waypoints$geometry)
# coords_df <- as.data.frame(coords)
# 
# #
# # comnpute cluster assignation with the current number of clusters
# #
# 
# results <- NULL  # Store results for each cluster and percentage
# 
# 
# cluster_balance_per_num_clusters_experiment=num_clusters %>% lapply(function(num_clusters){
#   
#   kmeans_result <- kmeans(coords_df, centers = num_clusters, nstart = 25)
#   filtered_waypoints$cluster <- as.factor(kmeans_result$cluster)
#   
#   filtered_waypoints %>% split(.$cluster) %>%
#     lapply(function(cluster_waypoints){
#     
#       cluster_waypoints
#       spatial_points <- as(cluster_waypoints, "Spatial")
#       waypoints_mcp_current <- mcp.area(as(cluster_waypoints, "Spatial"), percent = percent_grid) %>%
#         mutate(
#           percent=row.names(.)
#         )
#       
#       names(waypoints_mcp_current)=c( "area", "percent")
#       row.names(waypoints_mcp_current)=NULL
#       
#       waypoints_mcp_current %>%
#         mutate(
#           area_loss=(lag(area)-area)/lag(area)
#         ) %>%
#         dplyr::mutate(
#           cluster_id=cluster_waypoints$cluster %>% head(1)
#         )
#     }) %>% bind_rows() %>%
#     dplyr::mutate(
#       num_clusters=num_clusters
#     )
#   
# }) %>%
#   bind_rows() 
# 
# 
# #
# # create experimental data to decide on the optimal number of clusters
# #
# 
# 
# #
# cluster_balance_per_num_clusters_experiment %>% View()
# cluster_balance_per_num_clusters_experiment %>% head()
# # area percent  area_loss cluster_id num_clusters
# # 1 1.766476e-09     100         NA          1            1
# # 2 1.492800e-09      99 0.15492756          1            1
# # 3 1.281548e-09      98 0.14151373          1            1
# # 4 1.228392e-09      97 0.04147849          1            1
# # 5 1.177820e-09      96 0.04116874          1            1
# # 6 1.155974e-09      95 0.01854826          1            1
# 
# 
# #
# # sort, find the optimal number of clustets as constrained by  the
# # percentage and target
# #
# 
# summarized_of_balance_per_per_num_clusters=cluster_balance_per_num_clusters_experiment %>% 
#   #dplyr::mutate(num_clusters = as.factor(num_clusters)) %>% 
#   dplyr::group_by(num_clusters, percent) %>% 
#   dplyr::summarize(mean_area_loss = max(area_loss, na.rm = TRUE), .groups = 'drop') %>% 
#   # dplyr::filter(
#   #   mean_area_loss<target_balance
#   # ) %>%
#   dplyr::mutate(
#     percent=as.numeric(percent)
#   ) %>%
#   dplyr::arrange(
#     num_clusters*(-1), percent*(-1)
#   ) %>%
#   na.omit() %>%
#   dplyr::mutate(
#     best_solution=1:nrow(.)==1
#   )
# 
# #
# summarized_of_balance_per_per_num_clusters%>%
#     View()
# #
# summarized_of_balance_per_per_num_clusters %>% 
#   dplyr::mutate(num_clusters = as.factor(num_clusters)) %>% 
#   ggplot(aes(x = percent, 
#              y = mean_area_loss,
#              group = num_clusters,
#              fill = num_clusters, 
#              color = num_clusters)) + 
#   geom_line(alpha = 0.6, show.legend = TRUE) +   # Apply transparency to the lines
#   geom_point(aes(alpha = ifelse(best_solution, 1, 0.6)), show.legend = FALSE) +  # Full opacity for the best solution, 60% for others
#   labs(x = "Percentage", y = "Mean Area Loss", title = "Mean Area Loss by Percentage and Number of Clusters") +
#   geom_hline(yintercept = target_balance, linetype = "dotted", color = "red") +
#   scale_alpha(guide = "none") +  # Remove alpha from the legend
#   theme_minimal()  # Optional theme for cleaner visualization
# 
# 
# #
# summarized_of_balance_per_per_num_clusters %>%
#   dplyr::filter(
#     best_solution
#   )




# Function to find the optimal number of clusters based on a target percentage change in variance
autotune_kmeans <- function(coords_df, target_pct_change = 5) {
  prev_wcss <- NULL
  num_clusters <- 2  # Start with 2 clusters
  while (TRUE) {
    # Perform K-means clustering
    kmeans_result <- kmeans(coords_df, centers = num_clusters, nstart = 25)
    
    # Calculate the Within-Cluster Sum of Squares (WCSS) for the current number of clusters
    wcss <- sum(kmeans_result$withinss)
    
    # Log the WCSS value and percentage change
    if (!is.null(prev_wcss)) {
      pct_change <- (prev_wcss - wcss) / prev_wcss * 100
      message(sprintf("K-means for %d clusters: WCSS = %.2f, Change = %.2f%%", num_clusters, wcss, pct_change))
      if (pct_change <= target_pct_change) {
        message(sprintf("Autotune stopped: %.2f%% change in WCSS for %d clusters.", pct_change, num_clusters))
        break
      }
    } else {
      message(sprintf("K-means for %d clusters: WCSS = %.2f", num_clusters, wcss))
    }
    
    # Update for the next iteration
    prev_wcss <- wcss
    num_clusters <- num_clusters + 1
  }
  
  # Return the final optimal number of clusters
  return(num_clusters)
}
  


# Function to perform clustering and calculate home ranges, returning polygons as an in-memory shapefile
waypoints_for_group_with_clusters_and_inmemory_shapefile <- 
  function(waypoints, 
           group_label, 
           tile_provider = "Esri.WorldImagery", 
           target_pct_change = 5,
           percent = 95) {
  
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
  
  # Extract coordinates (longitude, latitude) from the geometry column
  coords <- st_coordinates(filtered_waypoints$geometry)
  
  # Convert the extracted coordinates to a data frame for clustering
  coords_df <- as.data.frame(coords)
  
  # Autotune the number of clusters
  num_clusters <- autotune_kmeans(coords_df, target_pct_change)
  message(sprintf("Optimal number of clusters: %d", num_clusters))
  
  # Apply K-means clustering with the tuned number of clusters
  kmeans_result <- kmeans(coords_df, centers = num_clusters, nstart = 25)
  
  # Add the cluster assignment to the filtered waypoints data
  filtered_waypoints$cluster <- as.factor(kmeans_result$cluster)
  
  # Create a color palette for clusters
  pal <- colorFactor(palette = "Set1", domain = filtered_waypoints$cluster)
  
  # Initialize the map
  map <- leaflet() %>%
    addTiles(urlTemplate = switch(tile_provider,
                                  "OSM" = "https://{s}.tile.openstreetmap.org/{z}/{x}/{y}.png",
                                  "Stamen.Terrain" = "http://{s}.tile.stamen.com/terrain/{z}/{x}/{y}.png",
                                  "Stamen.Toner" = "http://{s}.tile.stamen.com/toner/{z}/{x}/{y}.png",
                                  "Esri.WorldImagery" = "https://server.arcgisonline.com/ArcGIS/rest/services/World_Imagery/MapServer/tile/{z}/{y}/{x}.jpg",
                                  "Esri.WorldStreetMap" = "https://server.arcgisonline.com/ArcGIS/rest/services/World_Street_Map/MapServer/tile/{z}/{y}/{x}.jpg"),
             attribution = "Tiles &copy; <a href='http://openstreetmap.org/copyright'>OpenStreetMap</a> contributors")
  
  # Initialize a list to store the MCP polygons for each cluster
  mcp_polygons_list <- list()
  
  # Plot the clusters and their home ranges
  for (cluster_id in unique(filtered_waypoints$cluster)) {
    # Filter waypoints for the current cluster
    cluster_waypoints <- filtered_waypoints %>% filter(cluster == cluster_id)
    
    # Convert to SpatialPoints
    spatial_points <- as(cluster_waypoints, "Spatial")
    
    # Calculate the MCP for the cluster (home range)
    waypoints_mcp <- mcp(spatial_points, percent = percent)
    
    # Convert the MCP to a suitable format for leaflet and store in the list
    mcp_polygon <- st_as_sf(waypoints_mcp)
    mcp_polygons_list[[as.character(cluster_id)]] <- mcp_polygon
    
    # Determine the color for the polygon based on the cluster
    polygon_color <- pal(cluster_id)
    
    # Plot the cluster waypoints
    map <- map %>%
      addCircleMarkers(data = cluster_waypoints, 
                       color = ~pal(cluster),
                       radius = 2, 
                       opacity = 0.6,
                       label = ~paste("Group:", species_group, "<br>Cluster:", cluster)) %>%
      addPolygons(data = mcp_polygon, 
                  fillColor = "transparent",
                  color = polygon_color,
                  weight = 2,
                  opacity = 1)
  }
  
  # Set the map view
  map <- map %>%
    setView(lng = mean(st_coordinates(filtered_waypoints)[, 1]), 
            lat = mean(st_coordinates(filtered_waypoints)[, 2]), 
            zoom = 16)
  
  # Combine all the MCP polygons into one sf object
  mcp_sf <- do.call(rbind, mcp_polygons_list)
  
  # Return both the map and the in-memory shapefile (sf object)
  return(list(map = map, home_ranges_sf = mcp_sf))
}

# Example usage
result <- waypoints_for_group_with_clusters_and_inmemory_shapefile(
  waypoints, 
  group_label="Wangwamani", 
  tile_provider = "Esri.WorldImagery", 
  target_pct_change = 10, 
  percent = 95
  )
result$map # View the interactive map
home_ranges_sf <- result$home_ranges_sf # Access the in-memory shapefile (sf object)
#result$home_ranges_sf %>% View()

#
#
#

home_range_islands=unique(waypoints$species_group) %>%
  lapply(function(a_group){
    
    result <- waypoints_for_group_with_clusters_and_inmemory_shapefile(
      waypoints, 
      group_label=a_group, 
      tile_provider = "Esri.WorldImagery", 
      target_pct_change = 10, 
      percent = 95
      #plot_level_data = plot_level_data 
    )
    result$home_ranges_sf %>%
      mutate(
        group=a_group,
        percent = 95
      )
    
  }) %>%
  bind_rows() 
#
home_range_islands %>%
  View()


#
#
#

plot_home_ranges_and_plots <- function(home_range_islands, plot_level_data_sf, exclusion_percentile = 90, group_label = NULL) {
  
  # Ensure plot_level_data_sf has centroids for label placement
  plot_level_data_sf <- plot_level_data_sf %>%
    mutate(centroid = st_centroid(geometry))  # Calculate centroids for label placement
  
  # Compute centroids for home range islands
  home_range_islands <- home_range_islands %>%
    mutate(centroid = st_centroid(geometry))  # Calculate centroids for home range polygons
  
  # Compute nearest neighbor distances for each island to the plots
  distances <- st_distance(home_range_islands$centroid, plot_level_data_sf$centroid)
  min_distances <- apply(distances, 1, min)  # Get the nearest neighbor distance for each island
  
  # Add the nearest neighbor distance and exclusion flag to the shapefile
  home_range_islands <- home_range_islands %>%
    mutate(
      nearest_distance = min_distances,
      exclude = nearest_distance > quantile(min_distances, exclusion_percentile / 100)  # Flag distances above the specified percentile
    )
  
  # Step 1: Assign colors based on groups
  groups <- unique(home_range_islands$group)
  group_colors <- brewer.pal(min(length(groups), 8), "Set1")  # Limit to available colors in Set1
  color_mapping <- setNames(group_colors, groups)
  home_range_islands <- home_range_islands %>%
    mutate(color = color_mapping[group])  # Assign group-specific colors
  
  # Step 2: Overwrite color column for excluded samples
  home_range_islands <- home_range_islands %>%
    mutate(color = ifelse(exclude, "gray", color))  # Assign gray only to excluded samples
  
  # Filter the data if a group_label is provided
  if (!is.null(group_label)) {
    home_range_islands <- home_range_islands %>%
      filter(group == group_label)
    plot_level_data_sf <-  plot_level_data_sf %>%
      filter(first_species_group == group_label)
  }
  
  # Define a color palette for the map
  pal <- colorFactor(palette = c(unname(group_colors), "gray"), 
                     domain = c(groups, "gray"))  # Include 'gray' in the domain
  
  # Create an annotation with parameters, including home range area, inside a semi-transparent container
  annotation_text <- sprintf(
    "<div style='background-color: rgba(0, 0, 0, 0.5); color: white; padding: 10px; border-radius: 5px;'>
      <b>Group:</b> %s<br>
      <b>Number of Plots:</b> %d<br>
      <b>Number of Habitat Islands:</b> %d<br>
      <b>Number of Islands Used:</b> %d
    </div>",
    ifelse(is.null(group_label), "All", group_label),  # Show 'All' if no group label is provided
    nrow(plot_level_data_sf),  # Number of plots
    nrow(home_range_islands),  # Number of habitat islands
    sum(!home_range_islands$exclude)  # Number of islands actually used (not excluded)
  )
  
  # Initialize the leaflet map with the specified TILE_PROVIDER
  map <- leaflet() %>%
    addTiles(urlTemplate = switch(TILE_PROVIDER,
                                  "OSM" = "https://{s}.tile.openstreetmap.org/{z}/{x}/{y}.png",
                                  "Stamen.Terrain" = "http://{s}.tile.stamen.com/terrain/{z}/{x}/{y}.png",
                                  "Stamen.Toner" = "http://{s}.tile.stamen.com/toner/{z}/{x}/{y}.png",
                                  "Esri.WorldImagery" = "https://server.arcgisonline.com/ArcGIS/rest/services/World_Imagery/MapServer/tile/{z}/{y}/{x}.jpg",
                                  "Esri.WorldStreetMap" = "https://server.arcgisonline.com/ArcGIS/rest/services/World_Street_Map/MapServer/tile/{z}/{y}/{x}.jpg"),
             attribution = "Tiles &copy; <a href='http://openstreetmap.org/copyright'>OpenStreetMap</a> contributors")
  
  # Plot home range polygons with color based on group or exclusion
  map <- map %>%
    addPolygons(data = home_range_islands, 
                fillColor = ~color,  # Use the 'color' column for fill color
                color = "black",  # Black border for polygons
                weight = 1, 
                opacity = 0.6, 
                fillOpacity = 0.3, 
                popup = ~paste("Group:", group, "<br>Excluded:", exclude))  # Popup with group and exclusion
  
  # Overlay entity labels
  map <- map %>%
    addLabelOnlyMarkers(data = plot_level_data_sf, 
                        lng = ~st_coordinates(centroid)[, 1],  # Longitude of centroid
                        lat = ~st_coordinates(centroid)[, 2],  # Latitude of centroid
                        label = ~entity,  # Label with the 'entity' field
                        labelOptions = labelOptions(noHide = TRUE, direction = "top", textOnly = FALSE))
  
  # Add the annotation in the top right corner
  map <- map %>%
    addControl(html = annotation_text, position = "topright", className = "annotation-control")
  
  # Center the map on the group if provided
  if (!is.null(group_label)) {
    # Get the mean coordinates of the filtered data
    mean_coords <- st_coordinates(home_range_islands) %>%
      as.data.frame() %>%
      summarise(lng = mean(X), lat = mean(Y))
    
    map <- map %>%
      setView(lng = mean_coords$lng, lat = mean_coords$lat, zoom = 17)
  }
  
  # Return the map and adjusted shapefile
  return(list(map = map, home_ranges_sf = home_range_islands))
}

# Example Usage
result <- plot_home_ranges_and_plots(home_range_islands, plot_level_data_sf, exclusion_percentile = 90)
result$map  # View the map
#View(result$home_ranges_sf)  # View the adjusted shapefile

result <- plot_home_ranges_and_plots(
  home_range_islands, 
  plot_level_data_sf, 
  exclusion_percentile = 90,
  group_label ="Camp"
  )
result$map  # View the map
#View(result$home_ranges_sf)  # View the adjusted shapefile


# aligning home ranges and plot level shapefiles
#

#
# (1)
#
home_range_islands %>% head()
# Simple feature collection with 6 features and 4 fields
# Geometry type: POLYGON
# Dimension:     XY
# Bounding box:  xmin: 39.41745 ymin: -6.273504 xmax: 39.42235 ymax: -6.269371
# Geodetic CRS:  +proj=longlat +datum=WGS84 +no_defs

#
# (2)
#
plot_level_data_sf  %>% head()
# Simple feature collection with 6 features and 18 fields
# Active geometry column: geometry
# Geometry type: POINT
# Dimension:     XY
# Bounding box:  xmin: 39.41762 ymin: -6.272567 xmax: 39.42166 ymax: -6.269341
# Geodetic CRS:  WGS 84



#
#
#

#
# alternative 1: using k means algorithm
#
correlate_home_ranges_with_plots <- function(
    home_range_islands, 
    plot_level_data_sf, 
    parameter, 
    k = 5, 
    home_param = "area"
    ) {
  
  # Check if parameter exists in plot_level_data_sf
  if (!parameter %in% colnames(plot_level_data_sf)) {
    stop(paste("Parameter", parameter, "not found in plot_level_data_sf columns."))
  }
  
  # Calculate the area for polygons in home_range_islands if not already done
  if (!home_param %in% colnames(home_range_islands)) {
    home_range_islands <- home_range_islands %>%
      mutate(!!home_param := st_area(geometry)) # Compute area if not already present
  }
  
  # Ensure geometries are projected for distance calculations
  home_range_islands <- st_transform(home_range_islands, crs = 3857)
  plot_level_data_sf <- st_transform(plot_level_data_sf, crs = 3857)
  
  # Perform k-nearest neighbor calculation for each polygon
  home_range_islands <- home_range_islands %>%
    rowwise() %>%
    mutate(
      avg_param = {
        distances <- st_distance(geometry, plot_level_data_sf$geometry) # Distance to all plots
        nearest_ids <- order(as.vector(distances))[1:k] # Get k nearest indices
        mean(plot_level_data_sf[[parameter]][nearest_ids], na.rm = TRUE) # Average the parameter
      }
    ) %>%
    ungroup()
  
  # Prepare correlation data
  correlation_data <- home_range_islands %>%
    st_drop_geometry() %>%
    mutate(
      home_param_value = .data[[home_param]],  # Rename home_param column
      plot_param_value = avg_param             # Rename avg_param column
    )
  
  # Calculate correlation coefficient
  r <- cor(correlation_data$home_param_value, correlation_data$plot_param_value, use = "complete.obs")
  
  # Correlation scatterplot with ggplot
  p <- ggplot(correlation_data, aes(x = plot_param_value, y = home_param_value, color = group)) +
    geom_point(size = 3, alpha = 0.7) +  # Points with increased alpha (70%)
    geom_smooth(method = "lm", se = FALSE, color = "navy", linetype = "dashed") +  # Dashed regression line
    labs(
      x = paste(parameter),  # Simplified axis label
      y = paste(home_param), # Simplified axis label
      title = paste("Correlation Analysis (r =", round(r, 2), ")"),  # Report correlation coefficient
      caption = str_wrap(paste("Methodological details: Home range polygons (", home_param, ") are spatially joined to the ", 
                               "k nearest plots, where k =", k, ". The plot-level parameter (", parameter, ") is averaged over ", 
                               "the k nearest plots. Correlation is assessed using Pearson's r.", sep = ""), width = 80)  # Wrap caption to width of 80
    ) +
    scale_color_brewer(palette = "Set1") +  # Group colors with a clean palette
    theme_minimal(base_size = 12) +  # Reduce font size by 20%
    theme(legend.title = element_blank(),  # Hide legend title
          plot.caption = element_text(hjust = 1, size = 9),  # Caption font size reduced and right-aligned
          plot.title = element_text(size = 14),  # Title font size reduced
          axis.title = element_text(size = 12),  # Axis title font size reduced
          axis.text = element_text(size = 10),  # Axis text font size reduced
          plot.margin = margin(10, 10, 10, 10))  # Adjust plot margin for better fit
  
  print(p)
  return(list(data = correlation_data, plot = p))
}
#
# alternative 2: using k means algorithm
#
correlate_home_ranges_with_plots <- function(
    home_range_islands, 
    plot_level_data_sf, 
    parameter, 
    k = 5, 
    home_param = "area", 
    sigma = 1000  # Kernel parameter for distance decay
) {
  
  # Check if parameter exists in plot_level_data_sf
  if (!parameter %in% colnames(plot_level_data_sf)) {
    stop(paste("Parameter", parameter, "not found in plot_level_data_sf columns."))
  }
  
  # Calculate the area for polygons in home_range_islands if not already done
  if (!home_param %in% colnames(home_range_islands)) {
    home_range_islands <- home_range_islands %>%
      mutate(!!home_param := st_area(geometry)) # Compute area if not already present
  }
  
  # Ensure geometries are projected for distance calculations
  home_range_islands <- st_transform(home_range_islands, crs = 3857)
  plot_level_data_sf <- st_transform(plot_level_data_sf, crs = 3857)
  
  # Define Gaussian kernel function for weighted averaging
  gaussian_kernel <- function(distance, sigma) {
    return(exp(-(distance^2) / (2 * sigma^2)))  # Gaussian kernel
  }
  
  # Perform k-nearest neighbor calculation for each polygon
  home_range_islands <- home_range_islands %>%
    rowwise() %>%
    mutate(
      avg_param = {
        distances <- st_distance(geometry, plot_level_data_sf$geometry)  # Distance to all plots
        distances_numeric <- as.numeric(distances)  # Convert distances to numeric (in meters)
        
        nearest_ids <- order(distances_numeric)[1:k]  # Get k nearest indices
        
        # Apply kernel to distances for the k nearest plots
        nearest_distances <- distances_numeric[nearest_ids]  # Distances to the k nearest plots
        weights <- sapply(nearest_distances, gaussian_kernel, sigma = sigma)  # Calculate kernel weights
        
        # Weighted average of the parameter values
        weighted_avg <- sum(weights * plot_level_data_sf[[parameter]][nearest_ids], na.rm = TRUE) / sum(weights, na.rm = TRUE)
        weighted_avg
      }
    ) %>%
    ungroup()
  
  # Prepare correlation data
  correlation_data <- home_range_islands %>%
    st_drop_geometry() %>%
    mutate(
      home_param_value = .data[[home_param]],  # Rename home_param column
      plot_param_value = avg_param             # Rename avg_param column
    )
  
  # Calculate correlation coefficient
  r <- cor(correlation_data$home_param_value, correlation_data$plot_param_value, use = "complete.obs")
  
  # Correlation scatterplot with ggplot
  p <- ggplot(correlation_data, aes(x = plot_param_value, y = home_param_value, color = group)) +
    geom_point(size = 3, alpha = 0.7) +  # Points with increased alpha (70%)
    geom_smooth(method = "lm", se = FALSE, color = "navy", linetype = "dashed") +  # Dashed regression line
    labs(
      x = paste(parameter),  # Simplified axis label
      y = paste(home_param), # Simplified axis label
      title = paste("Correlation Analysis (r =", round(r, 2), ")"),  # Report correlation coefficient
      caption = str_wrap(paste("Methodological details: Home range polygons (", home_param, ") are spatially joined to the ", 
                               "k nearest plots, where k =", k, ". The plot-level parameter (", parameter, ") is averaged over ", 
                               "the k nearest plots, using kernel weighting based on distance. Correlation is assessed using Pearson's r.", sep = ""), width = 80)  # Wrap caption to width of 80
    ) +
    scale_color_brewer(palette = "Set1") +  # Group colors with a clean palette
    theme_minimal(base_size = 12) +  # Reduce font size by 20%
    theme(legend.title = element_blank(),  # Hide legend title
          plot.caption = element_text(hjust = 1, size = 9),  # Caption font size reduced and right-aligned
          plot.title = element_text(size = 14),  # Title font size reduced
          axis.title = element_text(size = 12),  # Axis title font size reduced
          axis.text = element_text(size = 10),  # Axis text font size reduced
          plot.margin = margin(10, 10, 10, 10))  # Adjust plot margin for better fit
  
  print(p)
  return(list(data = correlation_data, plot = p))
}


#
# (1)
#
result <- correlate_home_ranges_with_plots(
  home_range_islands = home_range_islands,
  plot_level_data_sf = plot_level_data_sf,
  parameter = "species_richness"
)
result$plot  # Display the plot
#
# (2)
#
result <- correlate_home_ranges_with_plots(
  home_range_islands = home_range_islands,
  plot_level_data_sf = plot_level_data_sf,
  parameter = "number_of_trees"
)
result$plot  # Display the plot
#
# (3)
#
result <- correlate_home_ranges_with_plots(
  home_range_islands = home_range_islands,
  plot_level_data_sf = plot_level_data_sf,
  parameter = "dominance"
)
result$plot  # Display the plot


#
# main method to export all results
#

table(plot_level_data_sf$first_species_group)

result <- plot_home_ranges_and_plots(
  home_range_islands, 
  plot_level_data_sf, 
  exclusion_percentile = 90,
  group_label ="Camp"
)
camp_map=result$map

#
# (1)
#
result <- correlate_home_ranges_with_plots(
  home_range_islands = islands_near_plots,
  plot_level_data_sf = plot_level_data_sf,
  parameter = "species_richness"
)
result$plot  # Display the plot
#
# (2)
#
result <- correlate_home_ranges_with_plots(
  home_range_islands = islands_near_plots,
  plot_level_data_sf = plot_level_data_sf,
  parameter = "number_of_trees"
)
result$plot  # Display the plot
#
# (3)
#
result <- correlate_home_ranges_with_plots(
  home_range_islands = islands_near_plots,
  plot_level_data_sf = plot_level_data_sf,
  parameter = "density"
)
result$plot  # Display the plot
#
# (4)
#
result <- correlate_home_ranges_with_plots(
  home_range_islands = islands_near_plots,
  plot_level_data_sf = plot_level_data_sf,
  parameter = "dominance"
)
result$plot  # Display the plot
#
# (5)
#
result <- correlate_home_ranges_with_plots(
  home_range_islands = islands_near_plots
  plot_level_data_sf = plot_level_data_sf,
  parameter = "basal_area_m2"
)
result$plot  # Display the plot


#
# final report
#


# Initialize an empty list to store the report data
report_data <- list()

# Loop through each unique monkey group in plot_level_data_sf
unique_groups <- unique(plot_level_data_sf$first_species_group)

for (group in unique_groups) {
  # Generate the home ranges and plots for the current group with the dynamic group_label
  result <- plot_home_ranges_and_plots(
    home_range_islands, 
    plot_level_data_sf,  # Use the whole dataset for each group
    exclusion_percentile = 90,
    group_label = group
  )
  
  # Store the map in the report_data for the current group
  report_data[[group]] <- list("map" = result$map)
  
}
  
# Loop through each parameter to correlate home ranges with plots
for (parameter in c("species_richness", "number_of_trees", "density", "dominance", "basal_area_m2")) {
  message(paste("Processing parameter:", parameter, "for group:", group))
  
  # Call the correlate_home_ranges_with_plots function
  result <- correlate_home_ranges_with_plots(
    home_range_islands = home_range_islands,
    plot_level_data_sf = plot_level_data_sf,  # Again, using the whole dataset
    parameter = parameter
  )
  
  # Add the plot to the list of results for the current group
  report_data[[group]][[parameter]] <- result$plot
}


setwd(SPATIAL_CORRELATIONS_HOME)
# Pass the nested list to the excel_report function
excel_report(report_items = report_data)




