#'
#' title: Geometric Modelling for Nutrition Data
#' description: Applies Geometric Modelling based on dimensionality reduction to analize nutritional preferences of the monkey species.
#' debrief: This module performs a visualization devide to facilitate the analisis of
#'          the main nutritional profiles for the monkey groups under study.
#' image_path: gm_per_group.png
#'

#
# 0.0 research questions
#

#
# 0.1 set-up (libraries)
#

library(dplyr)       # Data manipulation and transformation (e.g., filtering, summarizing, and arranging)
library(ggcorrplot)  # Visualizing correlation matrices with customized color schemes and text labels
library(ggplot2)     # Data visualization with a flexible grammar for creating a variety of plots
library(ggrepel)     # Improves label placement in ggplot2 by preventing overlap of text labels
library(readxl)      # Reading Excel files (.xls and .xlsx) into R for data analysis
library(reshape2)    # Reshaping data (wide to long format and vice versa) for tidy analysis


#
# 0.1 set-up (work environment)
#

rm(list=ls())
NUTRITION_HOME=dirname(rstudioapi::getActiveDocumentContext()$path)
setwd(NUTRITION_HOME)
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
source(sprintf( "%s/%s", TRANSVERSAL_CODE_FOLDER,"db_tools.R"))
source(sprintf( "%s/%s", NUTRITION_HOME,"geometric_modelling_tools.R"))
dbListTables(db_con)

#
# 0.3 set-up (configuration)
#

#
# This defines the nutritional paramaters that are the target for this analysis,
# as parameters available in lab sample which were preselectd by the main researched
#
NUTRITIONAL_PARAMETERS = c(
  "total_dry_matter",        # Total dry matter content
  "total_moisture",          # Total moisture content
  "crude_fat_dup_av_dm",     # Crude fat (duplicate available, dry matter basis)
  "sugars_dup_av_dm",        # Sugars (duplicate available, dry matter basis)
  "tnc",                     # Total non-structural carbohydrates
  "me_kl_g_1",               # Metabolizable energy (kJ/g)
  
  # "crude_protein_dup_av_dm", # Crude protein (duplicate available, dry matter basis)
  # "cp_of_adin_residue_dup_av_dm",
  
  "available_protein_dup_av_dm", # Available protein (duplicate available, dry matter basis)
  "crude_fat_dup_av_dm",         # (Potential redundancy with previous crude fat entry)
  
  # "adl_dup_av_dm",
  
  "ndf_dup_av_dm",           # Neutral detergent fiber (duplicate available, dry matter basis)
  "adf_dup_av_dm",           # Acid detergent fiber (duplicate available, dry matter basis)
  "pro_fib",                 # Probable fiber content
  
  # "ash_minerals_dup_av",     # Ash/mineral content (duplicate available)
  "ash"                      # Total ash content
)



#
# 1. Exploratory query on the table of nutritional values: The main data assets for
#    this module is the table of nutritional values, which holds lab measurements of
#    a set of nutritional parameters for species found in each of the home ranges.
#


#
# (0)
#
dbGetQuery(db_con, 
           "SELECT * FROM nutritional_values"
) %>% 
  dplyr::mutate(

    available_protein_dup_av_dm=available_protein_dup_av_dm %>% as.numeric(),
    crude_fat_dup_av_dm=crude_fat_dup_av_dm %>% as.numeric()
  ) %>% #names()
  overwrite_table(db_con, "nutritional_values") %>%
  View()
# #
# NUTRIONAL_PARAMTERS=dbGetQuery(db_con,
#            "SELECT * FROM nutritional_values LIMIT 6"
# ) %>%  sapply(function(x){class(x)=="numeric"}) %>% .[.] %>% names()


#
# 2. We summarize the nutritional by averaging lab measurements on the nutritional
#    parameters at the species level. For simplicity and methodological consistency,
#    summaries are taken regardless of the precise location where each sample was taken.
#

#
# (0) Prototyping a query to summarize nutritional value at the species level
#
sql_to_standadize_nutritional_value_at_species_level=
"
  WITH 
-- Step 1: Compute the average values per species for each nutritional parameter
SpeciesSummary AS (
  SELECT species_name, 
  COUNT(*) AS num_obs,
  AVG(crude_protein_dup_av_dm) AS avg_crude_protein,
  AVG(crude_fat_dup_av_dm) AS avg_crude_fat,
  AVG(sugars_dup_av_dm) AS avg_sugars,
  AVG(tnc) AS avg_tnc,
  AVG(me_kl_g_1) AS avg_me_kl_g_1,
  AVG(ndf_dup_av_dm) AS avg_ndf,
  AVG(adf_dup_av_dm) AS avg_adf,
  AVG(pro_fib) AS avg_pro_fib,
  AVG(ash_minerals_dup_av) AS avg_ash_minerals,
  AVG(ash) AS avg_ash,
  AVG(available_protein_dup_av_dm) AS avg_available_protein
  FROM nutritional_values
  GROUP BY species_name
),

-- Step 2: Compute global means for each nutritional parameter
GlobalMeans AS (
  SELECT AVG(avg_crude_protein) AS mean_crude_protein,
  AVG(avg_crude_fat) AS mean_crude_fat,
  AVG(avg_sugars) AS mean_sugars,
  AVG(avg_tnc) AS mean_tnc,
  AVG(avg_me_kl_g_1) AS mean_me_kl_g_1,
  AVG(avg_ndf) AS mean_ndf,
  AVG(avg_adf) AS mean_adf,
  AVG(avg_pro_fib) AS mean_pro_fib,
  AVG(avg_ash_minerals) AS mean_ash_minerals,
  AVG(avg_ash) AS mean_ash,
  AVG(avg_available_protein) AS mean_available_protein
  FROM SpeciesSummary
),

-- Step 3: Compute variances for each nutritional parameter (for standard deviation calculation)
NutritionalVariances AS (
  SELECT 
  SUM(POWER(avg_crude_protein - (SELECT mean_crude_protein FROM GlobalMeans), 2)) / COUNT(*) AS variance_crude_protein,
  SUM(POWER(avg_crude_fat - (SELECT mean_crude_fat FROM GlobalMeans), 2)) / COUNT(*) AS variance_crude_fat,
  SUM(POWER(avg_sugars - (SELECT mean_sugars FROM GlobalMeans), 2)) / COUNT(*) AS variance_sugars,
  SUM(POWER(avg_tnc - (SELECT mean_tnc FROM GlobalMeans), 2)) / COUNT(*) AS variance_tnc,
  SUM(POWER(avg_me_kl_g_1 - (SELECT mean_me_kl_g_1 FROM GlobalMeans), 2)) / COUNT(*) AS variance_me_kl_g_1,
  SUM(POWER(avg_ndf - (SELECT mean_ndf FROM GlobalMeans), 2)) / COUNT(*) AS variance_ndf,
  SUM(POWER(avg_adf - (SELECT mean_adf FROM GlobalMeans), 2)) / COUNT(*) AS variance_adf,
  SUM(POWER(avg_pro_fib - (SELECT mean_pro_fib FROM GlobalMeans), 2)) / COUNT(*) AS variance_pro_fib,
  SUM(POWER(avg_ash_minerals - (SELECT mean_ash_minerals FROM GlobalMeans), 2)) / COUNT(*) AS variance_ash_minerals,
  SUM(POWER(avg_ash - (SELECT mean_ash FROM GlobalMeans), 2)) / COUNT(*) AS variance_ash,
  SUM(POWER(avg_available_protein - (SELECT mean_available_protein FROM GlobalMeans), 2)) / COUNT(*) AS variance_available_protein
  FROM SpeciesSummary
),

-- Step 4: Compute standardized values (Z-scores) for each nutritional parameter
StandardizedNutrients AS (
  SELECT 
  s.species_name,
  s.num_obs,
  --COALESCE((s.avg_crude_protein - g.mean_crude_protein) / SQRT(v.variance_crude_protein), 0) AS z_avg_crude_protein,
  COALESCE((s.avg_crude_fat - g.mean_crude_fat) / SQRT(v.variance_crude_fat), 0) AS z_avg_crude_fat,
  COALESCE((s.avg_sugars - g.mean_sugars) / SQRT(v.variance_sugars), 0) AS z_avg_sugars,
  COALESCE((s.avg_tnc - g.mean_tnc) / SQRT(v.variance_tnc), 0) AS z_avg_tnc,
  COALESCE((s.avg_me_kl_g_1 - g.mean_me_kl_g_1) / SQRT(v.variance_me_kl_g_1), 0) AS z_avg_me_kl_g_1,
  COALESCE((s.avg_ndf - g.mean_ndf) / SQRT(v.variance_ndf), 0) AS z_avg_ndf,
  COALESCE((s.avg_adf - g.mean_adf) / SQRT(v.variance_adf), 0) AS z_avg_adf,
  COALESCE((s.avg_pro_fib - g.mean_pro_fib) / SQRT(v.variance_pro_fib), 0) AS z_avg_pro_fib,
  COALESCE((s.avg_ash - g.mean_ash) / SQRT(v.variance_ash), 0) AS z_avg_ash,
  COALESCE((s.avg_available_protein - g.mean_available_protein) / SQRT(v.variance_available_protein), 0) AS z_avg_available_protein
  FROM SpeciesSummary s
  CROSS JOIN GlobalMeans g
  CROSS JOIN NutritionalVariances v
)

-- Step 5: Return final table with standardized values
SELECT * FROM StandardizedNutrients;
"
#
# (1) summarize nutritional value, and persist the table at the database.
#
dbGetQuery(
  db_con, 
  sql_to_standadize_nutritional_value_at_species_level
) %>%
  overwrite_table(db_con, "std_nutr_val_at_spec", as_view =TRUE) %>%
  View()

#
# 3. We create a view which contrains only the standardized nutritional scores
#    per species
#

#
# (0) fetch the table of nutritional values
#

dbGetQuery(
  db_con, 
  "SELECT * FROM std_nutr_val_at_spec"
) %>% View()

#
# (1) creating a view with standardized scores only
#

#
concat_var_expr(
  c(
    "species_name AS species",
    get_vars_by_regex(db_con, "std_nutr_val_at_spec", "z_%")
  )
)
#
dbGetQuery(
  db_con,
  sprintf(
  "
  SELECT %s 
  FROM std_nutr_val_at_spec
  ", 
    concat_var_expr(
      c(
        "species_name AS species",
        get_vars_by_regex(db_con, "std_nutr_val_at_spec", "z_%")
      )
    )
  )) %>%
  overwrite_table(db_con, "only_std_nutr_val_at_spec", as_view = TRUE) %>%
  View()


#
# 4. On the basis of the species level standardized data, we perform the first
#    exercide of Geometric Modelling by projecting the measurements on latent 2d
#    psace


#
dbGetQuery(db_con, 
           "
           SELECT *
           FROM only_std_nutr_val_at_spec;
           ") %>% 
  regex_on_col_names(
    "(species)|(?<=^z_avg_).*"
    ) %>%
  visualize_main_patterns(
    label="species", 
    threshold = 0.1
  )


#
# 5. In order to complete the empirical approach, we need to merge the entity table
#    of nutritional value per species with the event table of groupped food intakes.
#    The source for the former dataset are feeding records with daily informations
#    of plant species consumed at the daily level, which we summarize at year-month
#    partitions for easiness of vizualization
#

#
# (1) check again the schema of the entity table
#
dbGetQuery(db_con, 
           "
           SELECT *
           FROM only_std_nutr_val_at_spec;
           ") %>% names()
#
# [1] "species"             "z_avg_crude_protein" "z_avg_crude_fat"     "z_avg_sugars"        "z_avg_tnc"           "z_avg_me_kl_g_1"    
# [7] "z_avg_ndf"           "z_avg_adf"           "z_avg_pro_fib"       "z_avg_ash_minerals"  "z_avg_ash" 

#
# (2) check again the content of the event level table of species take-in
#

sample_query(
   "
   SELECT monkey_group, 
          CONCAT(year_, '-', month_) AS year_month, 
          scientific_name_ AS species
   FROM feeding_records;
  ",
  6
)
#
dbGetQuery(db_con, 
             sample_query(
             "
             SELECT monkey_group, 
                    CONCAT(year_, '-', month_) AS year_month, 
                    scientific_name_ AS species
             FROM feeding_records
             ")
           ) #%>% View()
# monkey_group year_month             species
# 1     Wangwani    2019-09 Terminalia boivinii
# 2        Joshi    2020-01  Bridelia micrantha
# 3     Wangwani    2019-07 Terminalia boivinii
# 4        Joshi    2019-06  Bridelia micrantha
# 5        Joshi    2019-09     Ficus sycomorus
# 6        Joshi    2019-12     Flueggea virosa

#
# 6. Development of main function to merge the data, visualize in simplified 2d 
#    space


#
#  (1) main signature parameters
#
query_for_nutrition_table="SELECT * FROM only_std_nutr_val_at_spec"
query_for_intakes_table=
"
SELECT monkey_group, sex, species, reproductive_state, month as year_month
FROM species_cons_view
"
group="monkey_group"
replicating_at="year_month"


#
#  (2) inferred parameters
#


nutrition_params=get_vars_by_regex(db_con, nutrition_table, "z_%")
nutrition_params


#
#  dynamic query rendering
#
query_to_left_join_intakes_with_nutritional_value=
  sprintf(
    "
    WITH fr AS (
      %s
    ), 
    nt AS(
      %s
    )
    SELECT * FROM fr
    LEFT JOIN nt ON fr.species=nt.species
    ",
    query_for_intakes_table,
    query_for_nutrition_table
  )
#
query_to_model_merged_table = 
  concat_var_expr(
  c(
    group,
    replicating_at,
    "COUNT(*) AS num_obs", 
    "COUNT(DISTINCT species) AS num_dist_species",
    sprintf(
      "AVG(IFNULL(%s, 0)) AS %s", nutrition_params, nutrition_params
    )
  )
)
#
query_to_summarize_merged_data_at_group_level=
  sprintf(
    "
    WITH merged_data AS (
      %s
    )
    
    SELECT %s
    FROM merged_data
    GROUP BY 1,2",
    
    query_to_left_join_intakes_with_nutritional_value,
    query_to_model_merged_table
  )  


# #
# dbGetQuery(
#   db_con, 
#   query_for_intakes_table
# ) %>%
#   View()
# #
# dbGetQuery(
#   db_con, 
#   query_to_left_join_intakes_with_nutritional_value
# ) %>%
#   View()
#

#
#  (3) perform the left-join, model and summarize nutritional value
#      of consumptions
#

dbGetQuery(
  db_con, 
  query_to_summarize_merged_data_at_group_level
) %>% #View()
  dplyr::select(
    grep(names(.), pattern="(monkey)|(avg)", value=TRUE)
  ) %>% #View()
  clean_column_names() %>% 
  regex_on_col_names("(monkey_group)|(?<=^z_avg_).*") %>%
  overwrite_table(db_con, "nutr_val_month_intake", as_view = TRUE )


#
#  (4) Serving the main plot in 2d space
#

#
dbGetQuery(
  db_con, 
  "SELECT * FROM nutr_val_month_intake"
) %>%
visualize_main_patterns(
  group="monkey_group", 
  threshold = 0.1
) 

#
# (1.2) collecting as a single function
#

nutrition_table="only_std_nutr_val_at_spec"
plot_nutritional_analysis_by_group <- function(db_con, 
                                         query_for_nutrition_table = "SELECT * FROM only_std_nutr_val_at_spec", 
                                         query_for_intakes_table = "
                                           SELECT monkey_group, sex, species, reproductive_state, month as year_month
                                           FROM species_cons_view
                                         ", 
                                         group = "monkey_group", 
                                         replicating_at = "year_month") {
  
  # (1) Main Signature Parameters
  # query_for_nutrition_table = "SELECT * FROM only_std_nutr_val_at_spec"
  # query_for_intakes_table = "
  #   SELECT monkey_group, sex, species, reproductive_state, month as year_month
  #   FROM species_cons_view
  # "
  # group = "monkey_group"
  # replicating_at = "year_month"
  
  # (2) Inferred Parameters
  nutrition_params = get_vars_by_regex(db_con, nutrition_table, "z_%")
  print(nutrition_params)
  
  # (3) Dynamic Query Rendering
  query_to_left_join_intakes_with_nutritional_value = sprintf(
    "
    WITH fr AS (
      %s
    ), 
    nt AS(
      %s
    )
    SELECT * FROM fr
    LEFT JOIN nt ON fr.species = nt.species
    ",
    query_for_intakes_table,
    query_for_nutrition_table
  )
  
  query_to_model_merged_table = concat_var_expr(
    c(
      group,
      replicating_at,
      "COUNT(*) AS num_obs", 
      "COUNT(DISTINCT species) AS num_dist_species",
      sprintf("AVG(IFNULL(%s, 0)) AS %s", nutrition_params, nutrition_params)
    )
  )
  
  query_to_summarize_merged_data_at_group_level = sprintf(
    "
    WITH merged_data AS (
      %s
    )
    
    SELECT %s
    FROM merged_data
    GROUP BY 1,2",
    
    query_to_left_join_intakes_with_nutritional_value,
    query_to_model_merged_table
  )
  
  # (4) Perform the Left-Join, Model and Summarize Nutritional Value
  dbGetQuery(
    db_con, 
    query_to_summarize_merged_data_at_group_level
  ) %>%
    dplyr::select(
      grep(names(.), pattern = sprintf("(%s)|(avg)",group), value = TRUE)
    ) %>%
    clean_column_names() %>%
    regex_on_col_names(sprintf("(%s)|(?<=^z_avg_).*",group)) %>%
    overwrite_table(db_con, "nutr_val_month_intake", as_view = TRUE)
  
  # (5) Serving the Main Plot in 2D Space
  dbGetQuery(
    db_con, 
    "SELECT * FROM nutr_val_month_intake"
  ) %>%
    visualize_main_patterns(
      group = group, 
      threshold = 0.1
    )
}

#
# (1)
#
plot_nutritional_analysis_by_group(
  db_con, 
  query_for_nutrition_table = "SELECT * FROM only_std_nutr_val_at_spec", 
  query_for_intakes_table = "
                             SELECT monkey_group, sex, species, reproductive_state, month as year_month
                             FROM species_cons_view
                            ", 
  group = "monkey_group", 
  replicating_at = "year_month"
)
#
# (2)
#
plot_nutritional_analysis_by_group(
  db_con, 
  query_for_nutrition_table = "SELECT * FROM only_std_nutr_val_at_spec", 
  query_for_intakes_table = "
                             SELECT monkey_group, sex, species, reproductive_state, month as year_month
                             FROM species_cons_view
                            ", 
  group = "sex", 
  replicating_at = "year_month"
)
#
# (3)
#
plot_nutritional_analysis_by_group(
  db_con, 
  query_for_nutrition_table = "SELECT * FROM only_std_nutr_val_at_spec", 
  query_for_intakes_table = "
                             SELECT monkey_group, sex, species, reproductive_state, month as year_month
                             FROM species_cons_view
                            ", 
  group = "reproductive_state", 
  replicating_at = "year_month"
)

#
# variation: using a different source table for event data
#

#
# shoudl move this to nutrition
#
dbGetQuery(
  db_con,
  "SELECT * FROM parts_cons_view"
) %>% 
  dplyr::mutate(
    plant_part=plyr::mapvalues(plant_part, 
                               c("Green Fruit","Green barg"), 
                               c("Unripe Fruit","Young bark") )
  ) %>%
  overwrite_table(
    db_con,
    "parts_cons_view",
    as_view = TRUE
  ) %>%
  View()
  
  
#
plot_nutritional_analysis_by_group(
  db_con, 
  query_for_nutrition_table = "SELECT * FROM only_std_nutr_val_at_spec", 
  query_for_intakes_table = 
  "
  SELECT plant_part, species, month as year_month
  FROM parts_cons_view
  ", 
  group = "plant_part", 
  replicating_at = "year_month"
)

#
#
#

#
# 7. main method to export the results
#


