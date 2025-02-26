#' Title: Bootstrapping
#' Description: Custom implementation of blocked-bootstrapping algorithm


library(dplyr)
library(purrr)


bootstrap_parameter <- function(
    data,                    # DataFrame containing observations
    parameter_col,           # Column name of the parameter to analyze
    group_cols,              # List of columns defining grouping variables
    block_col,               # Column representing the blocking factor (e.g., session ID)
    behavior_col,            # Column representing behavioral categories
    normalize = FALSE,       # Whether to normalize within behavior groups
    alpha = 0.05,            # Significance level for confidence intervals
    num_bootstrap = 1000,    # Number of bootstrap replications
    cache_path = NULL        # File path for caching results (NULL to disable caching)
) {
  
  # Ensure required columns exist in the dataframe
  required_cols <- c(parameter_col, group_cols, block_col, behavior_col)
  missing_cols <- setdiff(required_cols, colnames(data))
  
  if (length(missing_cols) > 0) {
    stop("Error: The following required columns are missing in the provided data: ", 
         paste(missing_cols, collapse = ", "))
  }
  
  # Define percentiles for confidence intervals
  lower_quantile <- alpha / 2
  upper_quantile <- 1 - (alpha / 2)
  # Print the confidence interval range
  message(sprintf("Using confidence interval percentiles: %.2f (lower), %.2f (upper)", lower_quantile, upper_quantile))
  
  # Check if cache should be used
  use_cache <- FALSE
  cached_results <- NULL
  
  if (!is.null(cache_path) && file.exists(cache_path)) {
    load(cache_path)
    
    # Ensure cache structure exists
    if (exists("bootstrap_cache")) {
      for (entry in bootstrap_cache) {
        if (
          identical(entry$parameter_col, parameter_col) &&
          entry$num_bootstrap >= num_bootstrap &&
          identical(entry$group_cols, group_cols) &&
          identical(entry$block_col, block_col) &&
          identical(entry$behavior_col, behavior_col) &&
          identical(entry$normalize, normalize)
        ) {
          cached_results <- entry$results
          use_cache <- TRUE
          message("Using cached results.")
          break
        }
      }
    }
  }
  
  if (use_cache) {
    return(cached_results)
  }
  
  # Perform bootstrapping
  results <- data %>%
    
    
    #
    # (1) data is partitioned on the combination of the groupping factos
    #
    mutate(group_id = apply(select(., all_of(group_cols)), 1, paste, collapse = "_")) %>%
    split(paste(.$group_id)) %>%
    
    #
    # (2) outter-most loop over the groups
    #
    lapply(function(group_df) {
      
      
      print("running bootrapping")
      
      #
      # (3) loop over the bootstraps
      #
      bootstrap_samples <- lapply(1:num_bootstrap, function(iter) {
        if (iter %% (num_bootstrap / 10) == 0) {
          message(sprintf("Bootstrap progress: %d%% completed", round(iter / num_bootstrap * 100)))
        }
        
        blocks <- split(group_df, group_df[[block_col]])
        
        resampled_blocks <- lapply(blocks, function(block) {
          block[sample(1:nrow(block), size = nrow(block), replace = TRUE), ]
        })
        
        bind_rows(resampled_blocks)
      })
      
      #
      # summarize the measurement each boostrap
      #
      transformed_samples <- lapply(bootstrap_samples, function(subsample) {
        
        summed_values <- subsample %>%
          
          group_by(!!sym(behavior_col)) %>%
          summarize(sum_value = sum(!!sym(parameter_col), na.rm = TRUE))
        
        if (normalize) {
          total <- sum(summed_values$sum_value, na.rm = TRUE)
          summed_values=summed_values %>% dplyr::mutate(
            rel_sum_value = (sum_value/total) 
          )
        }
        
        return(summed_values)
      })
      
      #
      # group the data per each behaviour, measure the percentiles
      #
      concatenated_df <- bind_rows(transformed_samples)
      summaries <- concatenated_df %>%
        split(.[[behavior_col]]) %>%
        lapply(function(subsample) {
          data.frame(
            behavior = unique(subsample[[behavior_col]]),
            point_estimate = mean(subsample$rel_sum_value, na.rm = TRUE),
            lower_ci = quantile(subsample$rel_sum_value, lower_quantile, na.rm = TRUE),
            upper_ci = quantile(subsample$rel_sum_value, upper_quantile, na.rm = TRUE)
          )
        }) %>%
        bind_rows() %>%
        dplyr::arrange(
          point_estimate*(-1)
        )
      
      #labels <- as.list(group_df[1, all_of(group_cols)])
      #labeled_summaries <- bind_cols(data.frame(labels), summaries)
      
      summaries$group=dplyr::first(group_df$group_id)
      
      return(summaries)
    }) %>%
    bind_rows() %>%
    dplyr::mutate(
      
      #
      # formatting the results a bit
      #
      point_estimate = round(point_estimate * 100, 3),
      lower_ci = round(lower_ci * 100, 3),
      upper_ci = round(upper_ci * 100, 3)
    ) %>%
    {rownames(.) <- NULL; .}
  
  # Save results to cache if a valid path is provided
  if (!is.null(cache_path)) {
    if (!exists("bootstrap_cache")) {
      bootstrap_cache <- list()
    }
    
    new_entry <- list(
      parameter_col = parameter_col,
      num_bootstrap = num_bootstrap,
      group_cols = group_cols,
      block_col = block_col,
      behavior_col = behavior_col,
      normalize = normalize,
      results = results
    )
    
    bootstrap_cache <- c(bootstrap_cache, list(new_entry))
    save(bootstrap_cache, file = cache_path)
    message("Results saved to cache.")
  }
  
  return(results)
}
