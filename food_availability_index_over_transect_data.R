#
#
#
#
#
#
#


#
# 0.0 research questions
#

#
# 0.1 set-up (libraries)
#

library(dplyr)
library(ggplot2)
library(ggrepel)
library(lubridate)
library(openxlsx)
library(readxl)
library(tidyr)
library(RSQLite)
library(stringr)


#
# 0.1 set-up (work environment)
#

rm(list=ls())
FOOD_AVAILABILITY_FOLDER=dirname(rstudioapi::getActiveDocumentContext()$path)
setwd(FOOD_AVAILABILITY_FOLDER)
list.files()

#
# 0.2 (custom libraries)
#

source(sprintf("%s/config.R","C:/Users/57320/Desktop/consultorias 2024/6267839/monkey_research_collaboration/monkeys"))
source(sprintf( "%s/%s", "C:/Users/57320/Desktop/consultorias 2024/6267839/monkey_research_collaboration/monkeys", "setup_db_conn.R"))
source(sprintf( "%s/%s", TRANSVERSAL_CODE_FOLDER,"resampling_dataframes.R"))
source(sprintf( "%s/%s", TRANSVERSAL_CODE_FOLDER,"dataframe_extentions.R"))
source(sprintf( "%s/%s", TRANSVERSAL_CODE_FOLDER,"kruskal_dunn_test.R"))
source(sprintf( "%s/%s", TRANSVERSAL_CODE_FOLDER,"contingency_analysis_for_group_features.R"))
dbListTables(db_con)

#
# (0.3) module configurations
#

phenology_scores=c("B", "YL", "IML", "ML", "FL", "GrFr", "RFr") 

#
# 1. load data from disk, load into db
#

# #
# list.files()
# setwd(FOOD_AVAILABILITY_FOLDER)
# phenology_data=readxl::read_excel("All groups phenology plots - scores 3.xlsx") %>%
#   dplyr::mutate(
#     Species=.[["Scientific name"]]
#   )
# #
# phenology_data %>% View()
# 
# #
# # (1) 
# #
# str(phenology_data)
# #
# # (2)
# #
# 
# # tibble [5,630 × 17] (S3: tbl_df/tbl/data.frame)
# # $ Scientific name: chr [1:5630] "Syzygium malaccensis" "Cocos nucifera" "Cocos nucifera" "Cocos nucifera" ...
# # $ Tag            : chr [1:5630] "E1" "E2" "E3" "E4" ...
# # $ DBH            : num [1:5630] 28.7 36.4 28 28.6 42.1 ...
# # $ Basal area     : num [1:5630] 647 1038 616 642 1392 ...
# # $ B              : num [1:5630] 1 1 1 1 1 1 1 1 1 1 ...
# # $ YL             : num [1:5630] 1 1 1 1 3 3 1 1 1 1 ...
# # $ IML            : num [1:5630] 1 1 1 1 3 3 1 1 1 1 ...
# # $ ML             : num [1:5630] 4 1 1 1 4 4 1 1 1 1 ...
# # $ FL             : num [1:5630] 0 1 1 0 0 0 1 1 0 1 ...
# # $ GrFr           : num [1:5630] 0 1 1 1 2 0 1 1 0 0 ...
# # $ RFr            : num [1:5630] 0 0 0 0 2 0 0 0 0 1 ...
# # $ Dead leaf      : num [1:5630] 0 0 0 0 0 0 0 0 1 0 ...
# # $ Seeds          : num [1:5630] 0 0 0 0 0 0 0 0 0 0 ...
# # $ Month          : chr [1:5630] "March, 2019" "March, 2019" "March, 2019" "March, 2019" ...
# # $ Location       : chr [1:5630] "Shamba" "Shamba" "Shamba" "Shamba" ...
# # $ Group          : chr [1:5630] "Camp" "Camp" "Camp" "Camp" ...
# # $ Species        : chr [1:5630] "Syzygium malaccensis" "Cocos nucifera" "Cocos nucifera" "Cocos nucifera"
# 
# #
# # (3)
# #
# 

#
# dbExecute(db_con, "DROP TABLE IF EXISTS phenology_data" )
# #
# sql_statement <- "
# CREATE TABLE IF NOT EXISTS phenology_data (
#     scientific_name TEXT,
#     tag TEXT,
#     dbh REAL,
#     basal_area REAL,
#     b INTEGER,
#     yl INTEGER,
#     iml INTEGER,
#     ml INTEGER,
#     fl INTEGER,
#     grfr INTEGER,
#     rfr INTEGER,
#     dead_leaf INTEGER,
#     seeds INTEGER,
#     month TEXT,
#     location TEXT,
#     monkey_group TEXT,
#     species TEXT
# );
# "
# # 
# dbExecute(db_con, sql_statement)
# 
# # 
# # #
# # # (4) Insert data into the SQLite table
# # #
# dbWriteTable(db_con,
#              "phenology_data",
#              phenology_data %>% 
#                dplyr::rename(
#                  monkey_group=Group
#                ),
#              append = FALSE,
#              overwrite =TRUE,
#              row.names = FALSE)
#
dbGetQuery(db_con, "SELECT * FROM phenology_data;") %>%
  View()

#
# 2. exploratory / profiling work
#

#
# (0) number of records
#
dbGetQuery(db_con, "SELECT COUNT(*) AS freq FROM phenology_data;") 
#
# (1)
#
dbGetQuery(db_con, 
           "
           SELECT *
           FROM phenology_data
           ORDER BY RANDOM()
           LIMIT 30
           ;") 
#
# (2)
#
dbGetQuery(db_con, 
           "
           SELECT
           monkey_group,
           
           COUNT(*) AS freq, 
           ROUND((COUNT(*) * 1.0 / SUM(COUNT(*)) OVER())*100,3) AS rel_freq
           
           FROM phenology_data
           GROUP BY monkey_group 
           ;") 
#
# (3)
#
dbGetQuery(db_con, 
           "
           WITH sdt_phenology_data AS (
           
            SELECT *,
            
            lower(species) AS species_,
            
            CASE 
                WHEN monkey_group = 'DF' THEN 'deep forest'
                ELSE lower(monkey_group)
            END AS monkey_group_
            
            FROM phenology_data
           )
           
           SELECT
           
           monkey_group_,
           species_, 
           COUNT(*) AS freq, 
           ROUND((COUNT(*) * 1.0 / SUM(COUNT(*)) OVER(PARTITION BY monkey_group_ ))*100,3) AS rel_freq,
           ROW_NUMBER() OVER (PARTITION BY monkey_group_ ORDER BY COUNT(*) DESC) AS ran
           
           FROM sdt_phenology_data
           GROUP BY monkey_group_, species_
           ORDER BY monkey_group_, ran
           ;") %>% View()

#
# 3. store the monthly phenological scores (disregarding other parameters)
#


#
# (0)
#
phenology_scores=c("B", "YL", "IML", "ML", "FL", "GrFr", "RFr") 
#
# dbExecute(db_con, "DROP VIEW IF EXISTS phenology_summary" ) 
# #
# dbExecute(db_con, 
#            "
#            CREATE VIEW phenology_summary AS
#            WITH sdt_phenology_data AS (
#              SELECT *,
#              lower(species) AS scientific_name
#              FROM phenology_data
#            )
#            SELECT 
#            --Month, 
#            scientific_name,
#            COUNT(*) AS num_observations,
#            
#            -- repeated code for the phenological scores
#            AVG(B) AS B,
#            AVG(YL) AS YL,
#            AVG(IML) AS IML,
#            AVG(ML) AS ML,
#            AVG(FL) AS FL,
#            AVG(GrFr) AS GrFr,
#            AVG(RFr) AS RFr
#            
#            FROM sdt_phenology_data
#            GROUP BY 1 --,2
#            --HAVING num_observations > 5;
# "
# ) 
#
# (1)
#
dbGetQuery(db_con, "SELECT * FROM phenology_summary;") %>%
  View()

#
# (2) how well i can learn the phenological score for the main cons
#
dbGetQuery(db_con, "SELECT * FROM species_cons_ranking;") %>%
  View()
#
dbGetQuery(db_con, "
           SELECT scr.monkey_group, 
           scr.scientific_name, 
           scr.rank_by_count, 
           scr.rel_cons, 
           scr.cum_rel_cons, 
           ps.num_observations
           FROM species_cons_ranking AS scr
           LEFT JOIN phenology_summary AS ps
           ON scr.scientific_name = ps.scientific_name
           WHERE scr.rank_by_count<=10
           ;") %>%
  View()
#
#
dbGetQuery(db_con, "
          
           WITH species_availability AS (
           SELECT LOWER(scr.monkey_group) as monkey_group, 
           LOWER(scr.scientific_name) as scientific_name, 
           scr.rank_by_count, 
           scr.rel_cons, 
           scr.cum_rel_cons, 
           ps.num_observations
           FROM species_cons_ranking AS scr
           LEFT JOIN phenology_summary AS ps
           ON scr.scientific_name = ps.scientific_name
           WHERE scr.rank_by_count<=20
           
           ),
           non_available AS (
           SELECT monkey_group, scientific_name, rank_by_count +1 AS rank_plus_one
           FROM  species_availability
           WHERE num_observations IS NULL
           ),
           available AS (
           SELECT * FROM  species_availability
           WHERE num_observations IS NOT NULL
           )
           SELECT * FROM available AS a
           LEFT JOIN non_available AS b
           ON a.monkey_group = b.monkey_group AND
              a.rank_by_count = b.rank_plus_one
           ORDER BY monkey_group, rank_by_count
           
           ;") %>%
  View()


#
# 4. designing a query for the transect data
#

#
# (0) list tables in the database
#
dbListTables(db_con)
#
# (1) load vegetation data
#
dbGetQuery(db_con,
           "SELECT * 
            FROM vegetation_data
           ;") %>%
  View()
# 
# (2) save a view of vegetation data with phenological scores
#
dbExecute(db_con,"DROP VIEW IF EXISTS  vegetation_data_with_scores;")
#
dbExecute(db_con,
      "
      CREATE VIEW vegetation_data_with_scores AS
      WITH species_availability AS (
        SELECT
        LOWER(scr.monkey_group) AS monkey_group,
        LOWER(scr.scientific_name) AS scientific_name,
        scr.rank_by_count,
        scr.rel_cons,
        scr.cum_rel_cons,
        ps.num_observations
        FROM
        species_cons_ranking AS scr
        LEFT JOIN
        phenology_summary AS ps
        ON
        scr.scientific_name = ps.scientific_name
        --WHERE scr.rank_by_count <= 20
      ),
      non_available AS (
        SELECT
        monkey_group,
        scientific_name,
        rank_by_count + 1 AS rank_plus_one
        FROM
        species_availability
        WHERE
        num_observations IS NULL
      ),
      available AS (
        SELECT *
          FROM
        species_availability
        WHERE
        num_observations IS NOT NULL
      ),
      cons_rank_avai_data AS (
        SELECT *,
        ROW_NUMBER() OVER (PARTITION BY a.monkey_group ORDER BY rank_by_count) AS surr_rank
          FROM
        available AS a
        LEFT JOIN
        non_available AS b
        ON
        a.monkey_group = b.monkey_group
        AND a.rank_by_count = b.rank_plus_one
        ORDER BY
        monkey_group, rank_by_count
      ),
      cons_rank_avai_data_scores AS (
        SELECT *
          FROM
        cons_rank_avai_data a
        LEFT JOIN
        phenology_summary b
        ON
        a.scientific_name = b.scientific_name
        ORDER BY
        monkey_group, rank_by_count
      )
      SELECT *
        FROM
      vegetation_data a
      LEFT JOIN
      cons_rank_avai_data_scores b
      ON
      a.species = b.scientific_name
      AND
      a.monkey_group = b.monkey_group
      ;
      "
)
#
dbGetQuery(
  db_con,
  "SELECT * FROM vegetation_data_with_scores"
) %>% View()
#
# (3) proptype a query of pipeline for the monhtly index
#
dbGetQuery(
  db_con,
  "
  WITH summary_per_species AS (
    SELECT monkey_group, species, month,
            sum(dbh) as total_dbh,
            --COUNT(DISTINCT species) as num_species,
            
    -- repeated code for the phenological scores
    AVG(B) AS B,
    AVG(YL) AS YL,
    AVG(IML) AS IML,
    AVG(ML) AS ML,
    AVG(FL) AS FL,
    AVG(GrFr) AS GrFr,
    AVG(RFr) AS RFr,
    
    AVG(surr_rank) AS surr_rank,
    AVG(rel_cons) AS rel_cons
    
    FROM vegetation_data_with_scores
    WHERE surr_rank<=10
    GROUP BY 1,2,3
  ),
  monthly_food_index_per_species AS (
  
    SELECT monkey_group, species, month, surr_rank,
    
    --total_dbh as basal_area,
    3.1416 * ( ( total_dbh / 2) * ( total_dbh / 2) ) / 10000 AS basal_area_m2,
    
    B + YL + IML + ML + FL + GrFr + RFr  as total_phen_score,
    
    (
      3.1416 * ( ( total_dbh / 2) * ( total_dbh / 2) ) / 10000
      
    )*(B + YL + IML + ML + FL + GrFr + RFr) as quantity_index
    
    FROM summary_per_species
  
  ),
  monthly_food_index_monkey_group AS (
      SELECT 
          monkey_group, 
          month, 
          SUM(quantity_index) AS quantity_index,
          COUNT(DISTINCT species) AS species,
          STRING_AGG(species, ', ') AS species_names
      FROM monthly_food_index_per_species
      GROUP BY 1, 2
  ),
  monthly_food_index_monkey_group AS (
      SELECT 
          monkey_group, 
          month, 
          SUM(quantity_index) AS quantity_index,
          COUNT(DISTINCT species) AS species,
          STRING_AGG(species, ', ') AS species_names
      FROM monthly_food_index_per_species
      GROUP BY 1, 2
  )
  SELECT * FROM monthly_food_index_monkey_group 
  
  ;"
) %>%
  View()

#
# 5. computing the montly index
#

#
# (1) Define the function
#
get_monthly_food_index <- function(rank_size = 99, return_cte = "monthly_food_index_monkey_group") {
  # Define valid CTEs for validation
  valid_ctes <- c(
    "summary_per_species", 
    "monthly_food_index_per_species", 
    "monthly_food_index_monkey_group",
    "annual_food_index_monkey_group"
    )
  
  # Validate rank_size to ensure it's a positive integer
  if (!is.numeric(rank_size) || rank_size <= 0 || rank_size %% 1 != 0) {
    stop("rank_size must be a positive integer.")
  }
  
  # Validate return_cte to ensure it's one of the valid CTEs
  if (!(return_cte %in% valid_ctes)) {
    stop(sprintf("Invalid return_cte: '%s'. Must be one of: %s", return_cte, paste(valid_ctes, collapse = ", ")))
  }
  
  # Construct the SQL query dynamically
  sql_query <- sprintf("
    WITH summary_per_species AS (
      SELECT monkey_group, species, month,
             sum(dbh) as total_dbh,
             AVG(B) AS B,
             AVG(YL) AS YL,
             AVG(IML) AS IML,
             AVG(ML) AS ML,
             AVG(FL) AS FL,
             AVG(GrFr) AS GrFr,
             AVG(RFr) AS RFr,
             AVG(surr_rank) AS surr_rank,
             AVG(rel_cons) AS rel_cons
      FROM vegetation_data_with_scores
      WHERE surr_rank <= %d
      GROUP BY 1, 2, 3
    ),
    monthly_food_index_per_species AS (
      SELECT monkey_group, species, month, surr_rank,
             3.1416 * ( ( total_dbh / 2) * ( total_dbh / 2) ) / 10000 AS basal_area_m2,
             B + YL + IML + ML + FL + FL + GrFr + RFr as total_phen_score,
             (3.1416 * ( ( total_dbh / 2) * ( total_dbh / 2) ) / 10000) * (B + YL + IML + ML + FL + GrFr + RFr) as quantity_index
      FROM summary_per_species
    ),
    monthly_food_index_monkey_group AS (
      SELECT monkey_group, month, 
             SUM(quantity_index) AS quantity_index,
             COUNT(DISTINCT species) AS species,
             STRING_AGG(species, ', ') AS species_names
      FROM monthly_food_index_per_species
      GROUP BY 1, 2
    ),
     annual_food_index_monkey_group AS (
      SELECT monkey_group,
             SUM(quantity_index) AS quantity_index,
             COUNT(DISTINCT species) AS species,
             STRING_AGG(species, ', ') AS species_names
      FROM monthly_food_index_per_species
      GROUP BY 1
    )
    SELECT * FROM %s;
  ", rank_size, return_cte)
  
  # Execute the SQL query and return the result
  dbGetQuery(db_con, sql_query)
}
#
get_monthly_food_index(
  rank_size = 1,
  return_cte="annual_food_index_monkey_group"
) %>%
  View()
#
get_montly_food_index(
  3
) %>%
  View()
#
get_montly_food_index(
  10
) %>%
  View()
#
get_montly_food_index(
  
) %>%
  View()

#
# 6. viz
#

#
# 6.1 All groups, all ranking sizes viz of monthly index
#

# Set global visual adjustments as parameters
visual_params <- list(
  background_color = "white",      # White background (not transparent)
  grid_color = "white",            # Remove grid lines (set to white)
  label_size = rel(0.9),           # Set text size to 90% of the default
  line_size = 0.5,                 # Set line size to 0.5
  point_size = 1,                  # Set point size to 1
  alpha_point = 0.5,               # Set transparency of points
  alpha_line = 0.5,                # Set transparency of lines
  margin = margin(15, 15, 15, 15)  # Increase plot margins
)


#
# (0)
#
month_mapping <- c(
  "April, 2019" = "2019-04-15", 
  "August, 2019" = "2019-08-15", 
  "December, 2019" = "2019-12-15", 
  "February, 2019" = "2019-02-15", 
  "February, 2020" = "2020-02-15", 
  "January, 2020" = "2020-01-15", 
  "July, 2019" = "2019-07-15", 
  "June, 2019" = "2019-06-15", 
  "March, 2019" = "2019-03-15", 
  "May, 2019" = "2019-05-15", 
  "November, 2019" = "2019-11-15", 
  "October, 2019" = "2019-10-15", 
  "September, 2019" = "2019-09-15"
)
#
# (1)
#
combined_df =
list(
  get_monthly_food_index(1) %>% 
    mutate(rank_size = "1"),
  get_monthly_food_index(3) %>% 
    mutate(rank_size = "3"),
  get_monthly_food_index(10) %>%
    mutate(rank_size = "10"),
  get_monthly_food_index(99) %>% 
    mutate(rank_size = "All")  
) %>%
  bind_rows() %>%
  mutate(
    month_datetime = as.Date(plyr::mapvalues(month, from = names(month_mapping), to = month_mapping))
  )
#
# (2)
#
overall_monhtly_trends_plot=
combined_df %>%
  ggplot(aes(
    x = month_datetime,
    y = quantity_index,
    color = as.factor(rank_size),
    group = rank_size
  )) +
  geom_line(size = .5) +          # Reduced line thickness and dashed lines
  geom_point(size = 1, alpha = 0.5) +                 # Points with 75% transparency
  facet_wrap(~monkey_group, scales = "free_y") +       # Facet by monkey group with free y-scales
  labs(
    title = "Food Quantity Over Time by Rank Size",
    x = "Month",
    y = "Food Quantity Index",
    color = "Rank Size"
  ) +
  theme_minimal() +  # Use minimal theme
  theme(
    plot.background = element_rect(fill = visual_params$background_color), # White background
    panel.background = element_rect(fill = visual_params$background_color), # White background for the panel
    legend.position = "bottom",                       # Place the legend at the bottom
    legend.justification = "center",                  # Center-align the legend
    legend.text = element_text(size = visual_params$label_size),      # Increase legend text size
    legend.title = element_text(size = visual_params$label_size),     # Increase legend title size
    axis.text.x = element_text(angle = 90, hjust = 1, size = visual_params$label_size), # Increase x-axis label size
    axis.text.y = element_text(size = visual_params$label_size),      # Increase y-axis label size
    axis.title.x = element_text(size = visual_params$label_size),     # Increase x-axis title size
    axis.title.y = element_text(size = visual_params$label_size),     # Increase y-axis title size
    strip.text = element_text(size = visual_params$label_size),       # Increase facet labels size
    plot.title = element_text(size = visual_params$label_size),       # Increase plot title size
    plot.subtitle = element_text(size = visual_params$label_size),    # Increase subtitle size
    panel.grid.major = element_line(color = visual_params$grid_color), # Remove grid lines (white)
    panel.grid.minor = element_blank(),               # Remove minor grid lines
    plot.margin = visual_params$margin               # Increase plot area margins
  ) +
  scale_x_date(
    date_breaks = "1 month", 
    date_labels = "%b %Y"                             # Format x-axis labels as "Month Year"
  ) +
  scale_y_continuous(expand = expansion(mult = c(0.15, 0.15))) # Add vertical spacing
#
overall_monhtly_trends_plot

#
# 6.2 All groups, all ranking sizes viz of annual index
#


#
# (1)
#
annual_monthly_index_plot=
get_monthly_food_index(
  99, 
  return_cte = "monthly_food_index_per_species"
  ) %>% 
  
  group_by(monkey_group, species) %>%
  summarise(quantity_index = sum(quantity_index, na.rm = TRUE)) %>%
  
  dplyr::mutate(
    monkey_group = factor(
      monkey_group, 
      levels = get_monthly_food_index(99, return_cte = "monthly_food_index_per_species") %>%
        dplyr::group_by(monkey_group) %>%
        dplyr::summarise(total_food_index = sum(quantity_index, na.rm = TRUE)) %>%
        dplyr::arrange(total_food_index) %>%
        dplyr::pull(monkey_group)
    )
  ) %>%
  
  ungroup() %>%
  group_by(monkey_group) %>%
  arrange(monkey_group, quantity_index*(-1)) %>%
  
  dplyr::mutate(
    rel_cum_cons = cumsum(quantity_index) / sum(quantity_index),
    species_label = ifelse(rel_cum_cons < 0.8, str_wrap(species, width = 10), NA) # Wrap labels to 10 characters
  ) %>% 
  
  dplyr::mutate(
    species_=paste(monkey_group, species),
    species_=factor(species_, levels= species_)
  ) %>%
  
  group_by(monkey_group) %>%
  arrange(monkey_group, quantity_index*(1)) %>% #View()
  
  dplyr::mutate(
    rev_cum_cons = cumsum(quantity_index),
    label_x_pos = (rev_cum_cons + dplyr::lag(rev_cum_cons, default = 0)) / 2
  ) %>%
  
  ggplot(aes(
    y = monkey_group,
    x = quantity_index,
    fill = species,
    group = species_, 
  )) +
  geom_col(
    alpha = 0.8,
    color = "white",
    width = 0.5
  ) +
  geom_text(
    aes(
      x=label_x_pos,
      label = species_label
      ),
    color = "black",
    size = 3,
    na.rm = TRUE
  ) +
  labs(
    title = "Annual Food Quantity Index per Group",
    y = "Group",
    x = "Food Quantity Index"
  ) +
  theme_minimal()+
  theme(
    plot.background = element_rect(fill = visual_params$background_color), # White background
    panel.background = element_rect(fill = visual_params$background_color), # White background for the panel
    legend.position = "none",
    axis.text.x = element_text(angle = 90, hjust = 1, size = visual_params$label_size),
    axis.text.y = element_text(size = visual_params$label_size),
    axis.title.x = element_text(size = visual_params$label_size),
    axis.title.y = element_text(size = visual_params$label_size),
    strip.text = element_text(size = visual_params$label_size),
    plot.title = element_text(size = visual_params$label_size),
    plot.subtitle = element_text(size = visual_params$label_size),
    panel.grid.major = element_line(color = visual_params$grid_color),
    panel.grid.minor = element_blank(),
    plot.margin = visual_params$margin
  )
#
annual_monthly_index_plot

#
# 7. export
#


# Create a new workbook
wb <- createWorkbook()

# 1. Add Annual Phenological Weights
phenology_summary <- dbGetQuery(db_con, "SELECT * FROM phenology_summary;")
addWorksheet(wb, "Annual Phenological Weights")
writeData(wb, "Annual Phenological Weights", phenology_summary)

# 2. Add Intermediate Table: monthly_food_index_per_species
monthly_food_index_per_species <- get_monthly_food_index(rank_size = 999, return_cte = "monthly_food_index_per_species")
addWorksheet(wb, "Monthly Q (Per Species)")
writeData(wb, "Monthly Q (Per Species)", monthly_food_index_per_species)
setColWidths(wb, sheet = "Monthly Q (Per Species)", cols = 1:ncol(monthly_food_index_per_species), widths = "auto")

# 3. Add Monthly and Annual Food Index Tables for Each Rank Size
rank_sizes <- list(1, 3, 10, "all species")

for (rank_size in rank_sizes) {
  # Fetch monthly and annual data for the current rank size
  monthly_data <- if (rank_size == "all species") {
    get_monthly_food_index(return_cte = "monthly_food_index_monkey_group")
  } else {
    get_monthly_food_index(rank_size = rank_size, return_cte = "monthly_food_index_monkey_group")
  }
  
  annual_data <- if (rank_size == "all species") {
    get_monthly_food_index(return_cte = "annual_food_index_monkey_group")
  } else {
    get_monthly_food_index(rank_size = rank_size, return_cte = "annual_food_index_monkey_group")
  }
  
  # Add monthly data
  monthly_sheet_name <- paste0("Monthly (Rank ", rank_size, ")")
  addWorksheet(wb, monthly_sheet_name)
  writeData(wb, monthly_sheet_name, monthly_data)
  setColWidths(wb, sheet = monthly_sheet_name, cols = 1:ncol(monthly_data), widths = "auto")
  

  # Add annual data
  annual_sheet_name <- paste0("Annual (Rank ", rank_size, ")")
  addWorksheet(wb, annual_sheet_name)
  writeData(wb, annual_sheet_name, annual_data)
  setColWidths(wb, sheet = annual_sheet_name, cols = 1:ncol(annual_data), widths = "auto")
}

# Create the final sheet for graphical reports
addWorksheet(wb, "Graphical Reports")

setwd(FOOD_AVAILABILITY_FOLDER)
# Save the plot in high resolution
ggsave("overall_monthly_trends_plot.png", plot = overall_monhtly_trends_plot, path = FOOD_AVAILABILITY_FOLDER, dpi = 300, width = 10, height = 6)
# Insert the plot into the "Graphical Reports" sheet
insertImage(wb, "Graphical Reports", file = "overall_monthly_trends_plot.png", width = 10, height = 6, startRow = 1, startCol = 1)

setwd(FOOD_AVAILABILITY_FOLDER)
# Save the plot in high resolution
ggsave("annual_monthly_index_plot.png", plot = annual_monthly_index_plot, path = FOOD_AVAILABILITY_FOLDER, dpi = 300, width = 10, height = 6)
# Insert the plot into the "Graphical Reports" sheet
insertImage(wb, "Graphical Reports", file = "annual_monthly_index_plot.png", width = 10, height = 6, startRow = 15, startCol = 1)

# Save the workbook
setwd(FOOD_AVAILABILITY_FOLDER)
saveWorkbook(wb, "food_index_with_intermediate_and_ranked.xlsx", overwrite = TRUE)
