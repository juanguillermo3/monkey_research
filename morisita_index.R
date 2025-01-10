#'
#' title: Computation of Morisita Index 
#' description: Computes the Morisita Index for spatial dispersion for the home range of each monkey species
#' portfolio: True
#' project: monkey_research
#' Version: 2.1.0
#' Date: 2025-01-02


#
# 0.0 research questions
#


#
# 0.1 set-up (libraries)
#

library(dplyr)
library(RSQLite)
library(openxlsx)
library(writexl)

#
# 0.1 set-up (work environment)
#

rm(list=ls())
THIS_FOLDER=dirname(rstudioapi::getActiveDocumentContext()$path)
setwd(THIS_FOLDER)
list.files()


#
# 0.2 (custom libraries)
#

source(sprintf("%s/config.R","C:/Users/57320/Desktop/consultorias 2024/6267839/monkey_research_collaboration/monkeys"))
source(sprintf( "%s/%s", "C:/Users/57320/Desktop/consultorias 2024/6267839/monkey_research_collaboration/monkeys", "setup_db_conn.R"))
source(sprintf( "%s/%s", TRANSVERSAL_CODE_FOLDER,"resampling_dataframes.R"))
source(sprintf( "%s/%s", TRANSVERSAL_CODE_FOLDER,"dataframe_extentions.R"))
source(sprintf( "%s/%s", TRANSVERSAL_CODE_FOLDER,"kruskal_dunn_test.R"))
dbListTables(db_con)


#
# 0.3 script configuration
#

#
MIN_DBH=5
#
rank_sizes_for_index=list(
  "Top 1 Species"=1,
  "Top 3 Species"=3,
  "All Food Consumptions"="all food",
  "All Vegetation"="all vegetation"
)
#
OUTPUT_FILE_NAME="morisita_index_per_mokey_group.xlsx"


#
# 1. load vegetation data
#

#
dbGetQuery(db_con, "SELECT * FROM vegetation_data;") %>%
  View()

#
# 2. Exploratory/Profilling work
#

#
dbGetQuery(db_con, "SELECT COUNT(*) AS num_obs FROM vegetation_data;") 
#
# (0)
#
vegetation_data=dbGetQuery(db_con, "SELECT * FROM vegetation_data;")
vegetation_data %>% View()
#
# (1) Number of plots per monkey group
#
dbGetQuery(db_con, 
    "
    WITH plot_level_summary AS (
    SELECT 
        monkey_group, 
        plot
     FROM vegetation_data
     GROUP BY monkey_group, plot
    )
    SELECT monkey_group, COUNT(*) AS num_plots 
    FROM plot_level_summary
    GROUP BY monkey_group
    ;
    "
) %>% View()
  

#
# 3. We use a inner join approach to filter the vegetation records to the k-sized
#    rank of main consumption per monkey group. In particular, the main consumptions
#    of each groups are loaded from the table species_cons_ranking and the filter is
#    applied there so that only highly ranked species per group remain. Thus the inner
#    join filter to vegetation record for the main species per monkey group.
#

#
dbListTables(db_con)
#
dbGetQuery(db_con, "SELECT * FROM vegetation_data;") %>%
  names()
#
dbGetQuery(db_con, "SELECT * FROM species_cons_ranking;") %>%
  names()
#
dbGetQuery(db_con, 
" 
          WITH std_vegetation_data AS (
             SELECT
             LOWER(monkey_group) AS monkey_group,
             plant_class,
             plot,
             LOWER(species) AS species
             FROM
             vegetation_data
             WHERE
             plant_class = 'tree'
           ),
           std_species_cons_ranking AS (
             SELECT
             LOWER(monkey_group) AS monkey_group,
             LOWER(scientific_name) AS species,
             rank_by_count
             FROM
             species_cons_ranking
             WHERE rank_by_count <=1
           ),
           ranked_vegetation_data AS (
           SELECT
           v.*,
           s.rank_by_count AS rank
           FROM
           std_vegetation_data v
           INNER JOIN
           std_species_cons_ranking s
           ON
           v.monkey_group = s.monkey_group
           AND v.species = s.species
           )
           SELECT * FROM ranked_vegetation_data
           ;"
           ) %>%
  View()


#
# 4. This query ilustrates the construction of the morisita index out from the 
#    vegetation records. As the Morisita measures the spatial dispersion across
#    a set of locations or plots, vegetation data is first summarized at the plot
#    level by taking the number of trees on the reelevant species (I assume that
#    the source was already filtered to interesting speciesl, as we do in step 3).
#    Several measurements are taken as required by the Morisita computation, and then
#    the index is finally constructed.
#

#
dbGetQuery(db_con, 
                                           "
  WITH plot_level_summary AS (
    SELECT 
        monkey_group, 
        plot, 
        COUNT(*) AS num_trees
    FROM vegetation_data
    WHERE plant_class=='tree'
    GROUP BY monkey_group, plot
    ORDER BY monkey_group, plot
  ),
  plot_level_summary_features AS (
    SELECT *, num_trees*(num_trees-1) AS trees_by_trees_minus_one FROM plot_level_summary
  ),
  morisita_index_inputs AS (
  SELECT 
    monkey_group, 
    COUNT(*) AS num_plots, 
    SUM(num_trees) AS total_trees_across_plots, 
    SUM(num_trees)-1 AS total_trees_across_plots_minus_one,  
    SUM(trees_by_trees_minus_one) AS sum_trees_by_trees_minus_one  
    FROM plot_level_summary_features
  GROUP BY 1
  ),
  morisita_index AS (
    SELECT *, 
           (CAST(num_plots AS FLOAT) * sum_trees_by_trees_minus_one) / 
           (CAST(total_trees_across_plots AS FLOAT) * CAST(total_trees_across_plots_minus_one AS FLOAT)) AS morisita_index
    FROM morisita_index_inputs
  )
  SELECT monkey_group,num_plots,morisita_index, 
         CASE 
           WHEN morisita_index < 1 THEN 'Random'
           WHEN morisita_index = 1 THEN 'Uniform'
           ELSE 'Clumped'
         END AS interpretation
  FROM morisita_index
  ;
  ") %>% View()



#
# 5. Steps 3,4 are combined into a single pipeline to more systematically  compute
#    the morisita index for a k-sized rank of main species consumed by each monkey
#    group. First, we use the inner join approach to the filtered rank information,
#    which effectively filters the vegetation data. A dynamically rendered sql strings
#    helps to propagates the rank size from the function signature, so that the client 
#    can use any integer valued rank size which suits the situations. A couple of
#    special cases are "all food", where we inner join with the whole rank of
#    species feed (so that all food species are preserved), and "all species", where
#    we use a left join approach instead, thus keeping all vegetation species regardless
#    of their appearance on the food rankings. The second part  feeds the filtered
#    vegetation records to the pipeline developed in step 4, for the summarization and
#    final computation of morisita index.
# 

get_morisita_for_top_species <- function(
    rank_size, 
    min_dbh = MIN_DBH,  # Default value for dbh filter
    db_con = db_con
) {
  # Determine JOIN type based on the original rank_size input
  join_type <- if (rank_size == "all vegetation") "LEFT JOIN" else "INNER JOIN"
  
  # Validate and map rank_size
  if (!is.numeric(rank_size) || rank_size <= 0) {
    if (rank_size == "all food") {
      rank_size <- 99
    } else if (rank_size == "all vegetation") {
      rank_size <- 99
    } else {
      stop("rank_size must be a positive integer, 'all food', or 'all vegetation'.")
    }
  }
  
  query <- sprintf(
    "
    -- Step 1: Rank and filter the vegetation data
    WITH std_vegetation_data AS (
       SELECT
       LOWER(monkey_group) AS monkey_group,
       plant_class,
       plot,
       LOWER(species) AS species
       FROM vegetation_data
       WHERE plant_class = 'tree' AND dbh >= %f
     ),
     std_species_cons_ranking AS (
       SELECT
       LOWER(monkey_group) AS monkey_group,
       LOWER(scientific_name) AS species,
       rank_by_count
       FROM species_cons_ranking
       WHERE rank_by_count <= %d
     ),
     ranked_vegetation_data AS (
     SELECT
     v.*,
     s.rank_by_count AS rank
     FROM std_vegetation_data v
     %s std_species_cons_ranking s
     ON v.monkey_group = s.monkey_group
     AND v.species = s.species
     ),
    
    -- Step 2: Summarizing and constructing the Morisita index
    plot_level_summary AS (
      SELECT 
          monkey_group, 
          plot, 
          COUNT(*) AS num_trees,
          COUNT(DISTINCT species) AS num_species
      FROM ranked_vegetation_data
      WHERE plant_class='tree'
      GROUP BY monkey_group, plot
      ORDER BY monkey_group, plot
    ),
    plot_level_summary_features AS (
      SELECT *, num_trees * (num_trees - 1) AS trees_by_trees_minus_one FROM plot_level_summary
    ),
    morisita_index_inputs AS (
    SELECT 
      monkey_group, 
      COUNT(*) AS num_plots, 
      SUM(num_trees) AS total_trees_across_plots, 
      SUM(num_trees) - 1 AS total_trees_across_plots_minus_one,  
      SUM(trees_by_trees_minus_one) AS sum_trees_by_trees_minus_one,
      MAX(num_species) AS num_species
    FROM plot_level_summary_features
    GROUP BY monkey_group
    ),
    morisita_index AS (
      SELECT *, 
             (CAST(num_plots AS FLOAT) * sum_trees_by_trees_minus_one) / 
             (CAST(total_trees_across_plots AS FLOAT) * CAST(total_trees_across_plots_minus_one AS FLOAT)) AS morisita_index
      FROM morisita_index_inputs
    )
    SELECT *
    FROM morisita_index
    ;", 
    min_dbh,    # Inject the dbh filter dynamically
    rank_size, 
    join_type
  )
  
  # Execute the query and return the result
  return(dbGetQuery(db_con, query))
}


#
# Example usage
#
get_morisita_for_top_species(1, MIN_DBH, db_con) %>% View()
#
get_morisita_for_top_species(3, MIN_DBH, db_con) %>% View()
#
get_morisita_for_top_species("all food", MIN_DBH, db_con) %>% View()
#
get_morisita_for_top_species("all vegetation", MIN_DBH, db_con) %>% View()

#
# 6. The following function takes on from a dataframe with the computations of the
#    morisita index created in step 5. Given the computed value of the index and the
#    number of plots for each row, we can compara with a chi squared distribution with
#    degrees of freedom num_plots - 1, agains a null hypothesis of the index not being
#    greater than one. We return the same dataframe witht the interpretation of the 
#    test results
#

# Define the function to test for randomness based on Morisita index
test_morisita_randomness <- function(
    morisita_index_per_monkey_group, 
    alpha = 0.05, 
    num_plots_override = NULL) {
  
  # Default values for num_plots based on monkey_group
  default_num_plots <- data.frame(
    monkey_group = c("camp", "deep forest", "joshi", "wangwani"),
    num_plots = c(16, 13, 12, 9) %>% as.integer()
  )
  
  # Use provided override or fallback to default
  if (!is.null(num_plots_override)) {
    if (!all(c("monkey_group", "num_plots") %in% names(num_plots_override))) {
      stop("The override list must contain 'monkey_group' and 'num_plots' columns.")
    }
    default_num_plots <- num_plots_override
  }
  
  # Replace num_plots based on monkey_group
  morisita_index_per_monkey_group <- morisita_index_per_monkey_group %>%
    left_join(default_num_plots, by = "monkey_group", suffix = c("", "_override")) %>%
    mutate(num_plots = coalesce(num_plots_override, num_plots)) %>%
    select(-num_plots_override)
  
  # Fail fast if any replacement fails
  if (any(is.na(morisita_index_per_monkey_group$num_plots))) {
    missing_groups <- morisita_index_per_monkey_group$monkey_group[is.na(morisita_index_per_monkey_group$num_plots)]
    stop(paste("Failed to replace 'num_plots' for monkey groups:", paste(missing_groups, collapse = ", ")))
  }
  
  # Quality checks
  if (!all(c("monkey_group", "num_plots", "morisita_index") %in% names(morisita_index_per_monkey_group))) {
    stop("The dataframe must contain 'monkey_group', 'num_plots', and 'morisita_index' columns.")
  }
  
  if (!is.integer(morisita_index_per_monkey_group$num_plots)) {
    stop("'num_plots' must be an integer.")
  }
  
  if (any(morisita_index_per_monkey_group$morisita_index <= 0)) {
    stop("All 'morisita_index' values must be positive.")
  }
  
  # Apply the Morisita test
  morisita_index_per_monkey_group %>%
    mutate(
      chi_squared_obs = num_plots * (morisita_index - 1),  # Calculate observed chi-squared
      df = num_plots - 1,  # Degrees of freedom
      chi_squared_critical = qchisq(1 - alpha, df),  # Calculate critical value
      reject_null = chi_squared_obs > chi_squared_critical,  # Test for randomness
      conclusion = if_else(reject_null, 
                           "Deviates from random", 
                           "Does not deviate from random")  # Final conclusion
    )
}

#
get_morisita_for_top_species(99, 5, db_con) %>% 
  test_morisita_randomness() %>%
  View()


#
# 7. Export the final result
#


# Define the parameters and sheet names
params <- list(
  list(top_n = 1, sheet_name = "Top 1 Species"),
  list(top_n = 3, sheet_name = "Top 3 Species"),
  list(top_n = "all food", sheet_name = "All Food Consumptions"),
  list(top_n = "all vegetation", sheet_name = "All Vegetation")
)

# Initialize a list to store the data for each sheet
sheets <- list()

# Loop through each parameter, calculate Morisita index, and store in the list
for (param in params) {
  top_n <- param$top_n
  sheet_name <- param$sheet_name
  
  # Calculate Morisita index and perform randomness test
  result <- get_morisita_for_top_species(top_n, db_con) %>%
    test_morisita_randomness()
  
  # Store the result in the sheets list with the corresponding sheet name
  sheets[[sheet_name]] <- result
}

# Set the working directory to the vegetation folder
setwd(VEGETATION_FOLDER)

# Export all sheets to an Excel file
write_xlsx(sheets, "morisita_index_for_top_species.xlsx")


