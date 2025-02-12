#'
#' title: Geometric modelling tools
#' description: 
#

#
# (0)
#
library(reshape2)
library(ggcorrplot)
library(ggrepel)

#
# (1) for and assestment of correlation patterns on a dataframe of numerical data
#
plot_correlation_matrix_ggplot <- function(data, size_factor = 0.7) {
  
  # Calculate the correlation matrix
  cor_matrix <- cor(data, method = "pearson")
  
  # Calculate the determinant
  determinant_val <- round(det(cor_matrix), 4)
  
  # Calculate the KMO
  kmo_val <- round(psych::KMO(cor_matrix)$MSA[1], 4)
  
  # Define the size adjustments based on 'size_factor'
  point_label_size <- 2.5 * size_factor  # Adjust point label size
  
  # Create ggplot for the correlation matrix
  p <- ggcorrplot(cor_matrix, 
                  type = "upper", 
                  hc.order = TRUE, 
                  lab = TRUE,
                  lab_size = point_label_size) +
    theme(axis.text.x = element_text(size = rel(size_factor), angle = 90), 
          axis.text.y = element_text(size = rel(size_factor)),
          legend.position = c(0.9, 0.1),  # Position legend inside the plot area (bottom-right)
          legend.justification = c(1, 0)) +  # Align to bottom-right corner
    scale_fill_gradientn(colors = c("blue", "white", "red"), 
                         breaks = seq(-1, 1, by = 0.2),  # Legend ticks every 0.2 units
                         labels = round(seq(-1, 1, by = 0.2), 2)) +  # Round to two decimal places
    labs(caption = sprintf("Overall KMO: %.4f | Determinant: %.4f", kmo_val, determinant_val))
  
  return(p)
}


#
# (2) main service to project a sample of numerical measurements in 2d space, perform
#     analysis of locational patterns in the vein of Geometric Modelling on a group basis,
#
visualize_main_patterns <- function(
    data, 
    group = NULL, 
    k = 5, 
    threshold = 0.25,
    label=NULL
) {
  
  point_size=3
  if(!is.null(label)){
    data[["label"]]=data[[label]]
    point_size=2
  } else {
    data$label=NA_character_
  }
  
  numeric_data <- data %>% dplyr::select(where(is.numeric))
  message("Selected numeric variables: ", paste(names(numeric_data), collapse = ", "))
  
  if (ncol(numeric_data) < 2) stop("At least two numeric variables are required for PCA.")
  
  pca_result <- prcomp(numeric_data, scale. = TRUE)
  pca_data <- as.data.frame(pca_result$x[, 1:2]) 
  pca_data$label= data$label
  colnames(pca_data) <- c("PC1", "PC2", "label")
  
  var_explained <- round(summary(pca_result)$importance[2, 1:2] * 100, 2)
  cumulative_var_explained <- round(sum(var_explained), 2)
  
  main_drivers <- as.data.frame(pca_result$rotation[, 1:2]) %>%
    dplyr::mutate(var = row.names(.)) %>%
    tidyr::pivot_longer(c("PC1", "PC2"), names_to = "comp", values_to = "load") %>%
    dplyr::filter(abs(load) > threshold) %>%
    dplyr::mutate(
      type = ifelse(load > 0, "preferred", "avoidance"),
      label = paste0(var, ":", sprintf("%.2f", load))
    )
  
  has_group <- FALSE
  group_label <- NULL
  point_alpha=.8
  if (!is.null(group)) {
    if (!group %in% colnames(data)) stop("Grouping variable not found in the dataframe.")
    pca_data$Group <- as.factor(data[[group]])
    has_group <- TRUE
    group_label <- group
    point_alpha = 0.4
  }
  
  max_abs <- max(abs(c(pca_data$PC1, pca_data$PC2))) * 1.2
  
  quadrants <- data.frame(
    xmin = c(-max_abs, 0, -max_abs, 0),
    xmax = c(0, max_abs, 0, max_abs),
    ymin = c(-max_abs, -max_abs, 0, 0),
    ymax = c(0, 0, max_abs, max_abs),
    fill = c("#E0E0E0", "#A9A9A9", "#A9A9A9", "#E0E0E0")
  )
  
  label_data <- main_drivers %>%
    dplyr::group_by(comp, type) %>%
    dplyr::summarize(
      label = paste(label, collapse = "\n"),
      x_position = ifelse(comp == "PC1" & type == "preferred", max_abs * 0.8,
                          ifelse(comp == "PC1" & type == "avoidance", -max_abs * 0.8, 0)),
      y_position = ifelse(comp == "PC2" & type == "preferred", max_abs * 0.8 * 0.85, # Shift PC2 labels toward center
                          ifelse(comp == "PC2" & type == "avoidance", -max_abs * 0.8 * 0.85, 0))
    ) %>% dplyr::ungroup()
  
  p <- ggplot() +
    geom_rect(data = quadrants, aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax, fill = fill),
              alpha = 0.3, show.legend = FALSE) +
    scale_fill_identity() +
    geom_point(
      data = pca_data, 
      aes(x = PC1, y = PC2, color = if (has_group) Group else NULL),
      size = point_size, alpha = point_alpha) +
    geom_text_repel(
      data = pca_data, aes(x = PC1, y = PC2, label = label), size=2.5
    ) +
    geom_label(data = label_data, aes(x = x_position, y = y_position, label = label),
               color = "black", fill = alpha("gray", 0.3), label.padding = unit(0.3, "lines"), size = 2.3) +  
    labs(
      title = "PCA Projection",
      x = "Principal Component 1",
      y = "Principal Component 2",
      color = group_label,
      caption = paste0(
        "Variance Explained: PC1 = ", var_explained[1], "%, ",
        "PC2 = ", var_explained[2], "%, ",
        "Cumulative Variance = ", cumulative_var_explained, "%.\n",
        "Showing variables with loads over ", threshold, ".",
        if (has_group) "\nGroup centroids estimated using median to enhance robustness against outliers." else ""
      )
      
    ) +
    xlim(-max_abs, max_abs) +
    ylim(-max_abs, max_abs) +
    theme_minimal() +
    theme(
      legend.position = c(0.5, 0.05),
      legend.direction = "horizontal",
      legend.background = element_rect(fill = alpha("gray", 0.2), color = NA),
      legend.key = element_rect(fill = alpha("gray", 0.2), color = NA),
      legend.key.size = unit(0.5, "cm"),
      legend.title = element_text(size = 8),
      legend.text = element_text(size = 7),
      plot.caption = element_text(
        hjust = 1, vjust = 1, size = 7, face = "italic", color = "gray40", lineheight = 1.2, 
        margin = margin(t = 10, r = 10)
      ),
      plot.margin = margin(10, 10, 10, 10),
      axis.title.x = element_text(size = 10),
      axis.title.y = element_text(size = 10),
      axis.text.x = element_text(size = 8),
      axis.text.y = element_text(size = 8)
    )
  
  if (!has_group) {
    p <- p + theme(legend.position = "none")
  }
  
  if (has_group) {
    centroids <- pca_data %>%
      dplyr::group_by(Group) %>%
      dplyr::summarise(PC1 = median(PC1), PC2 = median(PC2), .groups = "drop")  # Robust centroid
    
    p <- p +
      geom_point(data = centroids, aes(x = PC1, y = PC2, color = Group), 
                 shape = 4, size = 3.5, stroke = 1.2, show.legend = FALSE)
  }
  
  print(p)
}





visualize_main_patterns <- function(
    data, 
    group = NULL, 
    k = 5, 
    threshold = 0.25,
    label = NULL
) {
  
  point_size = 3
  if (!is.null(label)) {
    data[["label"]] = data[[label]]
    point_size = 2
  } else {
    data$label = NA_character_
  }
  
  numeric_data <- data %>% dplyr::select(where(is.numeric))
  message("Selected numeric variables: ", paste(names(numeric_data), collapse = ", "))
  
  if (ncol(numeric_data) < 2) stop("At least two numeric variables are required for PCA.")
  
  pca_result <- prcomp(numeric_data, scale. = TRUE)
  pca_data <- as.data.frame(pca_result$x[, 1:2]) 
  pca_data$label = data$label
  colnames(pca_data) <- c("PC1", "PC2", "label")
  
  var_explained <- round(summary(pca_result)$importance[2, 1:2] * 100, 2)
  cumulative_var_explained <- round(sum(var_explained), 2)
  
  main_drivers <- as.data.frame(pca_result$rotation[, 1:2]) %>%
    dplyr::mutate(var = row.names(.)) %>%
    tidyr::pivot_longer(c("PC1", "PC2"), names_to = "comp", values_to = "load") %>%
    dplyr::filter(abs(load) > threshold) %>%
    dplyr::mutate(
      type = ifelse(load > 0, "preferred", "avoidance"),
      label = paste0(var, ":", sprintf("%.2f", load))
    )
  
  has_group <- FALSE
  group_label <- NULL
  point_alpha = .8
  show_legend = TRUE
  high_cardinality = FALSE
  
  if (!is.null(group)) {
    if (!group %in% colnames(data)) stop("Grouping variable not found in the dataframe.")
    
    pca_data$Group <- as.factor(data[[group]])
    has_group <- TRUE
    group_label <- group
    point_alpha = 0.3
    
    # Check cardinality
    num_groups <- length(unique(pca_data$Group))
    if (num_groups > 10) {
      show_legend = FALSE
      high_cardinality = TRUE
    }
  }
  
  max_abs <- max(abs(c(pca_data$PC1, pca_data$PC2))) * 1.2
  
  quadrants <- data.frame(
    xmin = c(-max_abs, 0, -max_abs, 0),
    xmax = c(0, max_abs, 0, max_abs),
    ymin = c(-max_abs, -max_abs, 0, 0),
    ymax = c(0, 0, max_abs, max_abs),
    fill = c("#E0E0E0", "#A9A9A9", "#A9A9A9", "#E0E0E0")
  )
  
  label_data <- main_drivers %>%
    dplyr::group_by(comp, type) %>%
    dplyr::summarize(
      label = paste(label, collapse = "\n"),
      x_position = ifelse(comp == "PC1" & type == "preferred", max_abs * 0.8,
                          ifelse(comp == "PC1" & type == "avoidance", -max_abs * 0.8, 0)),
      y_position = ifelse(comp == "PC2" & type == "preferred", max_abs * 0.8 * 0.85, 
                          ifelse(comp == "PC2" & type == "avoidance", -max_abs * 0.8 * 0.85, 0))
    ) %>% dplyr::ungroup()
  
  p <- ggplot() +
    geom_rect(data = quadrants, aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax, fill = fill),
              alpha = 0.3, show.legend = FALSE) +
    scale_fill_identity() +
    geom_point(
      data = pca_data, 
      aes(x = PC1, y = PC2, color = if (has_group) Group else NULL),
      size = point_size, alpha = point_alpha, show.legend = show_legend
    ) +
    geom_text_repel(
      data = pca_data, aes(x = PC1, y = PC2, label = label), size = 2.5
    )+
    labs(
      title = "PCA Projection",
      x = "Principal Component 1",
      y = "Principal Component 2",
      color = group_label,
      caption = paste0(
        "Variance Explained: PC1 = ", var_explained[1], "%, ",
        "PC2 = ", var_explained[2], "%, ",
        "Cumulative Variance = ", cumulative_var_explained, "%.\n",
        "Showing variables with loads over ", threshold, ".",
        if (has_group) "\nGroup centroids estimated using median to enhance robustness against outliers." else ""
      )
    ) +
    xlim(-max_abs, max_abs) +
    ylim(-max_abs, max_abs) +
    theme_minimal() +
    theme(
      legend.position = if (high_cardinality) "none" else c(0.5, 0.05),
      legend.direction = "horizontal",
      legend.background = element_rect(fill = alpha("gray", 0.2), color = NA),
      legend.key = element_rect(fill = alpha("gray", 0.2), color = NA),
      legend.key.size = unit(0.5, "cm"),
      legend.title = element_text(size = 8),
      legend.text = element_text(size = 7),
      plot.caption = element_text(
        hjust = 1, vjust = 1, size = 7, face = "italic", color = "gray40", lineheight = 1.2, 
        margin = margin(t = 10, r = 10)
      ),
      plot.margin = margin(10, 10, 10, 10),
      axis.title.x = element_text(size = 10),
      axis.title.y = element_text(size = 10),
      axis.text.x = element_text(size = 8),
      axis.text.y = element_text(size = 8)
    )
  
  if (!has_group) {
    p <- p + theme(legend.position = "none")
  }
  
  if (has_group) {
    centroids <- pca_data %>%
      dplyr::group_by(Group) %>%
      dplyr::summarise(PC1 = median(PC1), PC2 = median(PC2), .groups = "drop")  
    
    p <- p +
      geom_point(data = centroids, aes(x = PC1, y = PC2, color = Group), 
                 shape = 4, size = 3.5, stroke = 1.2, show.legend = FALSE)
    
    # If high cardinality, add centroid labels
    # If high cardinality, add centroid labels
    if (high_cardinality) {
      p <- p +
        geom_label_repel(
          data = centroids, 
          aes(x = PC1, y = PC2, label = Group, color = Group),  # Align color with centroid
          alpha = 0.8,  # Increased transparency
          fill = alpha("white", 0.7),  # Soft white background
          size = 2.5, 
          label.padding = unit(0.2, "lines"),
          show.legend = FALSE
        )
    }
    
    p<-p+geom_label(data = label_data, aes(x = x_position, y = y_position, label = label),
                 color = "black", fill = alpha("gray", 0.3), label.padding = unit(0.3, "lines"), size = 2.3)  
  }
  
  return(p)
}

