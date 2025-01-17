#' Title: Flexible Comparison Across Groups
#' Description: Performs statistical tests to compare measurements of a dependent variable across groups defined by an independent variable. Supports multiple behaviors for ecological studies.
#' Report URL: Statistical tests summary and post hoc analysis for group comparisons in ecological studies.


library(dplyr)
library(purrr)
library(dunn.test)  # For the dunn.test function
library(coin)       # For the wilcox_test function with confidence intervals


flexible_comparison_across_groups <- function(
    parameter_df, 
    dependent_var, 
    independent_var, 
    item_var, 
    alpha = 0.05,
    post_hoc = FALSE
) {
  # flexible_comparison_across_groups
  #
  # This function performs statistical tests to compare measurements (dependent_var) 
  # across groups defined by the independent_var. It is designed for ecological studies 
  # where multiple behaviors (item_var) need to be compared across groups.
  #
  # Depending on the number of groups:
  # - For two groups, it performs the Mann-Whitney U test.
  # - For three or more groups, it performs the Kruskal-Wallis test, optionally followed 
  #   by Dunn's post hoc test if the overall test is significant.
  #
  # Args:
  #   parameter_df: A dataframe containing the data to be analyzed.
  #   dependent_var: The name of the dependent variable (numerical) in the dataframe.
  #   independent_var: The name of the independent variable used for grouping the data.
  #   item_var: The name of the variable representing behaviors or items of interest.
  #   alpha: Significance level for the tests (default is 0.05).
  #   post_hoc: Boolean, whether to perform Dunn's post hoc test if applicable (default is FALSE).
  #
  # Returns:
  #   A dataframe summarizing the statistical test results for each behavior (item_var).
  
  # Check if necessary columns are present in the dataframe
  necessary_columns <- c(dependent_var, item_var, independent_var)
  if (!all(necessary_columns %in% names(parameter_df))) {
    stop("Dataframe does not contain all necessary columns: ", 
         paste(necessary_columns, collapse = ", "))
  }
  
  message("Starting flexible_comparison_across_groups...")
  
  # Split the dataframe by item_var and process each subset
  results <- parameter_df %>%
    split(.[[item_var]]) %>%
    lapply(function(df) {
      unique_groups <- unique(df[[independent_var]])
      num_groups <- length(unique_groups)
      message(sprintf("Processing behavior '%s' with %d groups.", unique(df[[item_var]]), num_groups))
      
      if (num_groups < 2) {
        warning(sprintf("Behavior '%s' has insufficient groups for comparison.", unique(df[[item_var]])))
        return(data.frame(item = unique(df[[item_var]]), 
                          test_result = "Insufficient groups"))
      } else if (num_groups == 2) {
        message(sprintf("Performing Mann-Whitney U test for behavior '%s'.", unique(df[[item_var]])))
        group1 <- df[df[[independent_var]] == unique_groups[1], dependent_var]
        group2 <- df[df[[independent_var]] == unique_groups[2], dependent_var]
        
        mann_whitney_result <- wilcox.test(group1, group2, exact = FALSE)
        
        result_df <- data.frame(
          item = unique(df[[item_var]]),
          test_type = "Mann-Whitney U",
          test_result = sprintf("U = %.2f, p = %.4f", 
                                mann_whitney_result$statistic, 
                                mann_whitney_result$p.value),
          comparison_detail = sprintf("%s vs %s, Mean Difference = %.2f", 
                                      unique_groups[1], 
                                      unique_groups[2], 
                                      mean(group1) - mean(group2)),
          stringsAsFactors = FALSE
        )
      } else if (num_groups >= 3) {
        message(sprintf("Performing Kruskal-Wallis test for behavior '%s'.", unique(df[[item_var]])))
        formula_test <- as.formula(paste(dependent_var, "~", independent_var))
        kruskal_test_result <- kruskal.test(formula_test, data = df)
        
        result_df <- data.frame(
          item = unique(df[[item_var]]),
          test_type = "Kruskal-Wallis",
          test_result = sprintf("Chi^2 = %.2f, p = %.4f", 
                                kruskal_test_result$statistic, 
                                kruskal_test_result$p.value),
          stringsAsFactors = FALSE
        )
        
        if (post_hoc && !is.na(kruskal_test_result$p.value) && kruskal_test_result$p.value < alpha) {
          message(sprintf("Performing Dunn's post hoc test for behavior '%s'.", unique(df[[item_var]])))
          dunn_result <- dunn.test::dunn.test(x = df[[dependent_var]], g = df[[independent_var]])
          significant_comparisons <- which(dunn_result$P.adjusted < alpha)
          
          if (length(significant_comparisons) > 0) {
            message(sprintf("Significant comparisons found in Dunn's test for behavior '%s'.", unique(df[[item_var]])))
            for (i in significant_comparisons) {
              result_df <- bind_rows(
                result_df, 
                data.frame(
                  item = unique(df[[item_var]]),
                  test_type = "Dunn's Test",
                  test_result = sprintf(
                    "%s, Z = %.2f, p = %.4f", 
                    dunn_result$comparisons[i], 
                    dunn_result$Z[i], 
                    dunn_result$P.adjusted[i]),
                  stringsAsFactors = FALSE
                )
              )
            }
          }
        }
      }
      result_df
    }) %>%
    bind_rows()
  
  message("Completed flexible_comparison_across_groups.")
  return(results)
}

