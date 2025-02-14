#'
#' title: Habitat islands
#' description: Applies k-means clustering within a self-calibration framework to
#'              discover smaller partitions habitat islands from sights data. It
#'              is useful in ecological studies to decompose the habitat of some 
#'              species into smaller habitats thus allowing statistical analysis of
#'              home range sizes, such as correlation with habitat parameters.
#

#
# (0.1) Load necessary libraries
#
library(dplyr)        # For data manipulation
library(sf)           # For handling spatial data
library(leaflet)      # For interactive maps
library(sp)           # For spatial objects
library(adehabitatHR) # For home range calculation (MCP)

#
# (0.2) load custom libraries
#
source(sprintf("%s/config.R","C:/Users/57320/Desktop/consultorias 2024/6267839/monkey_research_collaboration/monkeys"))
source(sprintf( "%s/%s", TRANSVERSAL_CODE_FOLDER,"outstanding_leaflet_maps.R"))

#
# (1) Function to find the optimal number of clusters based on a target percentage
#     change in variance
#
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


#
# (2) Function to perform clustering and calculate home ranges, returning polygons
#     as an in-memory shapefile
#
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
      
      # Plot the cluster waypoints with distinct shapes and background color for polygons
      map <- map %>%
        addCircleMarkers(data = cluster_waypoints, 
                         color = ~pal(cluster),
                         radius = 3,  # Increase radius for better visibility
                         opacity = 0.4,  # Make the markers more visible
                         #shape = ~ifelse(cluster %% 2 == 0, "square", "triangle"),  # Use distinct shapes
                         label = ~paste("Group:", species_group, "<br>Cluster:", cluster),
                         weight = 2
                         ) %>%
        addPolygons(data = mcp_polygon, 
                    fillColor = polygon_color,  # Background color for polygons
                    fillOpacity = 0.3,  # Transparent background fill
                    color = polygon_color,
                    weight = 2,  # Thicker border for better visibility
                    opacity = 1)
    }
    
    # Set the map view
    map <- map %>%
      setView(lng = mean(st_coordinates(filtered_waypoints)[, 1]), 
              lat = mean(st_coordinates(filtered_waypoints)[, 2]), 
              zoom = 17)
    
    map <- addCustomControl(map, 
                            title = sprintf("Habitat Islands for Group %s", group_label),
                            methodology_brief = "Performed self-calibrated K-means to discover habitat islands from sight data.", 
                            params = list("Relative Variance %" = target_pct_change, "Clusters Discovered" = num_clusters, "Percent of Sightings per Cluster" = percent),
                            position = "topleft"
                            )
    map <- disableZoomControls(map)
    
    # Combine all the MCP polygons into one sf object
    mcp_sf <- do.call(rbind, mcp_polygons_list)
    
    # Return both the map and the in-memory shapefile (sf object)
    return(list(map = map, home_ranges_sf = mcp_sf))
  }

# # Example usage
# result <- waypoints_for_group_with_clusters_and_inmemory_shapefile(
#   waypoints, 
#   group_label="Wangwamani", 
#   tile_provider = "Esri.WorldImagery", 
#   target_pct_change = 10, 
#   percent = 95
# )
# result$map # View the interactive map
# home_ranges_sf <- result$home_ranges_sf # Access the in-memory shapefile (sf object)
# #result$home_ranges_sf %>% View()
