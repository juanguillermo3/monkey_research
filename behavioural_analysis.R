#'
#' title: Behavioural Analysis
#' description: This modules performs several comparisons to detect behavioral 
#'              differences between monkey groups defined by species, sex, and 
#'              reproductive status. It utilizes an engine of inferential statistics 
#'              powered by the dunn test and a custom implementation of blocked 
#'              boostrapping algorithm
#' report_url: https://docs.google.com/spreadsheets/d/1JxmBCHIgdH5HYbL0C6YYk3DgW5ZwhmKc/edit?gid=1502020711#gid=1502020711


        
#
# 0.0 research questions
#

# (1) Is there an overall significant difference in behaviours and feeding between males and females (overall and by month - regardless of group)?
# (2) Is there a difference between males and females between each group (overall and by month)?
# (3) Did behaviour or diet of the of the individuals who were followed once a month differ over the course of the 12 months (– there’s a male and a female in each group who were followed regularly. These are: Camp group – Batini (F) and Droopy (M), Joshi group – Marylin (F) and Taj (M), Deep Forest group – Mersey (F) and Pablo (M), Wangwani group – BBF (F) and Goliath (M).
# (4) Did behaviour or diet vary depending on the reproductive state of the females?

#
# 0.1 set-up (libraries)
#

library(dplyr)
library(tidyr)
library(magrittr)
library(ggplot2)
library(purrr)
library(openxlsx)

#
# 0.2 set-up (work environment)
#

rm(list=ls())
INDIVIDUAL_ANALYSIS_HOME=dirname(rstudioapi::getActiveDocumentContext()$path)
setwd(INDIVIDUAL_ANALYSIS_HOME)
list.files()

#
# 0.3 set-up (custom libraries)
#

source(sprintf("%s/config.R","C:/Users/57320/Desktop/consultorias 2024/6267839/monkey_research_collaboration/monkeys"))
source(sprintf( "%s/%s", "C:/Users/57320/Desktop/consultorias 2024/6267839/monkey_research_collaboration/monkeys", "setup_db_conn.R"))
dbListTables(db_con)
#
source(sprintf( "%s/%s", TRANSVERSAL_CODE_FOLDER,"resampling_dataframes.R"))
source(sprintf( "%s/%s", TRANSVERSAL_CODE_FOLDER,"dataframe_extentions.R"))
source(sprintf( "%s/%s", TRANSVERSAL_CODE_FOLDER,"kruskal_dunn_test.R"))
source(sprintf( "%s/%s", TRANSVERSAL_CODE_FOLDER,"contingency_analysis_for_group_features.R"))
source(sprintf( "%s/%s", TRANSVERSAL_CODE_FOLDER,"flexible_excel_reports.R"))
source(sprintf( "%s/%s", TRANSVERSAL_CODE_FOLDER,"recover_entity_table_from_df.R"))
#
source(sprintf( "%s/%s", TRANSVERSAL_CODE_FOLDER,"flexible_test_across_groups.R"))
source(sprintf("%s/%s", TRANSVERSAL_CODE_FOLDER, "bootstrapping.R" ))

#
# (0.4) set-up (configuration)
#

NUM_BOOTSTRAPS=100
BEHAVIOURAL_PLOTS_HEIGHT=12
BEHAVIOURAL_PLOTS_WIDTH=10

#
# 1. load individual data
#

# #
# setwd(INDIVIDUAL_BEHAVIOUR_FOLDER)
# list.files()
# #
# curated_individual_data=openxlsx::read.xlsx("curated_individual_records_per_sex.xlsx")  #View()
# curated_individual_data$behaviour=
#  stringr::str_replace_all(
#   curated_individual_data$behaviour,
#   "Copulation Given",
#   "Copulation"
# )
# #
# curated_individual_data %>%
#   View()
# 
# 
# curated_individual_data %>% 
#   dplyr::rename(
#     monkey_group=group
#     ) %>% 
#   dplyr::select(-"reproductive.state") %>% 
#   clean_column_names() %>% 
#   overwrite_table(
#     db_con, # avoid the spetial work from lite
#     "individual_behaviour"
#   ) %>%
#   View()

behaviour_data=dbGetQuery(db_con, "SELECT * FROM individual_behaviour")
behaviour_data %>% sample(6)
  
#
# 2. exploratory queries on individual records
#


#
# (0)
#
dbGetQuery(db_con, "SELECT COUNT(*) AS num_obs FROM individual_behaviour")
#
# (1)
#
dbGetQuery(db_con, "SELECT * 
           FROM individual_behaviour
           ORDER BY RANDOM()
           LIMIT 6
           ")
#
# (2)
#
dbGetQuery(db_con, 
           "
           WITH per_behav_summary AS (
            SELECT behaviour, COUNT(*) AS num_obs
            FROM individual_behaviour
            GROUP BY 1
            ORDER BY num_obs DESC
           )
           SELECT *,
                  ROUND(num_obs * 100.0 / SUM(num_obs) OVER (),3) AS rel_freq
           FROM per_behav_summary
           "
)
#
# (3)
#
dbGetQuery(db_con, 
           "
           WITH per_behav_summary AS (
               SELECT behaviour, 
                      COUNT(*) AS num_obs
               FROM individual_behaviour
               GROUP BY behaviour
           ),
           with_rel_freq AS (
               SELECT *,
                      ROUND(num_obs * 100.0 / SUM(num_obs) OVER (), 3) AS rel_freq
               FROM per_behav_summary
               ORDER BY rel_freq DESC
           ),
           with_cum_freq AS (
               SELECT *,
                      SUM(rel_freq) OVER (ORDER BY rel_freq DESC ROWS BETWEEN UNBOUNDED PRECEDING AND CURRENT ROW) AS cum_freq
               FROM with_rel_freq
           ),
           categorized AS (
               SELECT *,
                      CASE 
                          WHEN cum_freq <= 95 THEN behaviour
                          ELSE 'Infrequent'
                      END AS behaviour_category
               FROM with_cum_freq
           )
           SELECT behaviour_category, 
                  SUM(num_obs) AS freq,
                  SUM(rel_freq) AS rel_freq
           FROM categorized
           GROUP BY behaviour_category
           ORDER BY rel_freq DESC
           "
)


#
# 2. some summarizing programs
#


#
# (1)
#
Behaviour_data.XGroups.Duration <- function(
    Behaviour_data = dbGetQuery(db_con, "SELECT * FROM individual_behaviour"),
    groupings = c("monkey_group", "sex"), 
    behaviour_var = "behaviour"
) {
  
  #
  # (0) set out of all unique behaviours
  #
  all_behaviors <- unique(Behaviour_data[[behaviour_var]]) %>% na.omit()
  
  #
  # (1.1) outter cycle over the groups
  #
  Behaviour_data %>%
    
    dplyr::filter(
      !is.na(.[[behaviour_var]])
    ) %>%
    split(., Behaviour_data[groupings]) %>% #names()
    
    lapply(function(sub_df) {
      
      #
      # (1.1) inner cycle over the unique behaviours
      #
      
      #
      # fast skip if no data for the particular group
      #
      if (nrow(sub_df) == 0) {
        # Construct a message with the first value of each grouping variable
        grouping_values = sapply(groupings, function(g) sub_df[[g]][1])
        grouping_values = paste(grouping_values, collapse = ", ")
        message(paste("Skipping group with empty data:", grouping_values))
        return(NULL)  # Skip this iteration
      }
      
      
      new_df=all_behaviors %>%
        lapply(function(behavior) {
          
          behavior_df <- sub_df[sub_df[[behaviour_var]] == behavior, ]
          
          data_frame_result <- 
            data.frame(
              behavior = behavior,
              total_duration = sum(behavior_df$duration, na.rm = TRUE)
            ) %>%
            dplyr::mutate(
              relative_duration = total_duration / sum(sub_df$duration, na.rm = TRUE),
              relative_duration = round(relative_duration * 100, 3)
            )
          names(data_frame_result)[names(data_frame_result) == "behavior"] <- behaviour_var
          return(data_frame_result)
          
        }) %>%
        bind_rows() %>%
        
        #
        # duration, relative duration, and ranks at teh group level
        #

        dplyr::arrange(relative_duration*(-1)) %>%
        dplyr::arrange(.[[behaviour_var]]=="Rare") %>% # push sparse data down the rank
        dplyr::mutate(
          rank=1:nrow(.)
        ) %>%
        mutate(cumulative_relative_duration = cumsum(relative_duration))
      
      #
      # add a new column per groupping factor,single col with the group label
      #
      
      # Adding columns for groupings
      for (col in groupings) {
        #print(unique(sub_df[[col]]))
        new_df[[col]] = unique(sub_df[[col]])
      }
      # Create the new 'Group' column by concatenating values of columns in 'groupings'
      if (length(groupings) > 1) {
        new_df$group <- apply(new_df[, groupings], 1, paste, collapse = "_")
      } else {
        new_df$group <- new_df[, groupings]
      }
      new_df 
      
      
    }) %>%
    bind_rows() %>%
    wipe_out_row_names() 
  
}

behaviour_data=dbGetQuery(db_con, "SELECT * FROM individual_behaviour")
#
# (1.1) (all sample) summary of behaviour
#
behaviour_data %>% 
  dplyr::mutate(sample="all_monkeys") %>%
  Behaviour_data.XGroups.Duration(
  groupings = c("sample"), 
  behaviour_var = "behaviour"
) %>%
  View()
#
# (1.2) summary of duration for each feeding log per species
#
main_species_summary=behaviour_data %>%
  dplyr::filter(behaviour=="Feeding") %>%
  Behaviour_data.XGroups.Duration(
    #Behaviour_data = Feeding_records ,
    groupings = c("all_sample"), 
    behaviour_var = "species"
  )
#
# - check summary on duration from curated feeding records
#
main_species_summary %>%
  View()

#
# (1.3) summary of duration for each part of the plant
#
behaviour_data  %>% 
  dplyr::mutate(sample="all_monkeys") %>%
  dplyr::filter(behaviour=="Feeding") %>%  #names()
  explode_and_standardize(
    nested_column="plant_part", 
  ) %>% #View()
  Behaviour_data.XGroups.Duration(
    groupings = c("sample"), 
    behaviour_var = "plant_part"
  ) %>%
  View()
#
# - check summary on duration from curated feeding records
#
main_species_summary %>%
  View()


#
# (2)
#
base_behavioural_frequencies <-
  function(
    Behaviour_data,
    behaviour_var
  ) {
    
    
    Behaviour_data %>% 
      dplyr::mutate(sample="all_monkeys") %>%
      Behaviour_data.XGroups.Duration(
        groupings = c("sample"), 
        behaviour_var = behaviour_var
      ) %>%
        dplyr::mutate(
          quarter_sample = ntile(relative_duration, 4)  # Create quartiles based on relative_duration
        ) %>%
        dplyr::mutate(
          frequency = case_when(
            quarter_sample == 4 ~ "c. High frequency (percentiles 100-75%)",
            quarter_sample == 3 ~ "d. Above-average frequency (percentiles 75-50%)",
            quarter_sample == 2 ~ "e. Below-average frequency (percentiles 50-25%)",
            quarter_sample == 1 ~ "f. Low frequency (percentiles 25-0%)"
          )
        )  %>%
        dplyr::select(
          behavior = !!rlang::sym(behaviour_var),
          frequency
        ) %>%
        dplyr::mutate(
          frequency_tier = case_when(
            row_number() == 1 ~ "a. Most frequent (percentiles 100-75%)",
            row_number() == 2 ~ "b. Second Most frequent (percentiles 100-75%)",
            TRUE ~ frequency  # Keep existing frequency labels for other ranks
          )
        ) %>%
        dplyr::select(
          -frequency
        )
  }
#
# (2.1) sample wide base frequencies (behaviour)
# 
behaviour_data %>%
  base_behavioural_frequencies(
    behaviour_var = "behaviour"
    ) %>%
  View()
#
# (2.2) sample wide base frequencies (species)
#
behaviour_data %>%
  base_behavioural_frequencies(
    behaviour_var = "species"
  ) %>%
  View()

#
# 3.  inferential statistics to detect differences between groups
#

#
# (0)
#
setwd(TRANSVERSAL_CODE_FOLDER)
list.files()
#source(sprintf( "%s/%s", TRANSVERSAL_CODE_FOLDER,"flexible_test_across_groups.R"))
#
# (1) source data
#
behaviour_data %>%
  View()
#
# (2) source data + summaries
#
behaviour_data %>%
  Behaviour_data.XGroups.Duration(
    groupings = c("sex", "month"), 
    behaviour_var = "behaviour"
  ) %>%
  View()
#
# (3.1)  source data + summaries + detecting behavioural differences between groups
#
behaviour_target_var="behaviour"
explanatory_var="sex"
#
behaviour_data %>%
  
  Behaviour_data.XGroups.Duration(
    groupings = c(explanatory_var, "month"), 
    behaviour_var = behaviour_target_var
    
  ) %>% 
  
  flexible_comparison_across_groups(
    dependent_var = "relative_duration", 
    independent_var = explanatory_var,
    item_var=behaviour_target_var, 
    alpha = 0.05,
    post_hoc = TRUE
  ) %>%
  View()
  
#
# (3.2) source data + summaries + detecting behavioural differences between groups
#       (new_example)
#

behaviour_target_var="behaviour"
explanatory_var="monkey_group"
#
behaviour_data %>%
  
  Behaviour_data.XGroups.Duration(
    groupings = c(explanatory_var, "month"), 
    behaviour_var = behaviour_target_var
    
  ) %>% 
  
  flexible_comparison_across_groups(
    dependent_var = "relative_duration", 
    independent_var = explanatory_var,
    item_var=behaviour_target_var, 
    alpha = 0.05,
    post_hoc = TRUE
  ) %>%
  View()


#
# 4.  Blocked boostrapping statistics to detect differences between groups
#

#
# (0)
#
setwd(TRANSVERSAL_CODE_FOLDER)
#source(sprintf("%s/%s", TRANSVERSAL_CODE_FOLDER, "bootstrapping.R" ))

#
# (1)
#
behaviour_target_var="behaviour"
explanatory_var="sex"
#
behaviour_data %>%
  bootstrap_parameter(
    parameter_col="duration",
    group_cols=c(explanatory_var),
    block_col= "individual",           
    behavior_col = behaviour_target_var,           
    normalize = TRUE,      
    alpha = 0.05,       
    num_bootstrap = 100,    
    cache_path = NULL   
  ) %>%
    View()


#
# (2) visualization for the boostrapping parameters
#
plot_bootstrap_results <- function(
    bootstrap_results,
    n_dodge = 3,
    call_parameters = list(),  # Optional parameter for additional info
    file_path = NULL,  # Optional file path for exporting the image
    width = BEHAVIOURAL_PLOTS_WIDTH,  # Default width of the exported image
    height = BEHAVIOURAL_PLOTS_HEIGHT  # Default height of the exported image
) {
  
  # Extract optional parameters safely
  flattened_groupings <- ifelse("groupings" %in% names(call_parameters),
                                paste(call_parameters$groupings, collapse = ", "),
                                "")
  
  behaviour_var <- ifelse("behaviour_var" %in% names(call_parameters),
                          call_parameters$behaviour_var,
                          "")
  
  num_samples <- ifelse("num_samples" %in% names(call_parameters),
                        call_parameters$num_samples,
                        "")
  
  sex_dist <- ifelse("sex_dist" %in% names(call_parameters),
                     call_parameters$sex_dist,
                     "")
  
  monkey_dist <- ifelse("monkey_dist" %in% names(call_parameters),
                        call_parameters$monkey_dist,
                        "")
  
  num_replicas <- ifelse("num_replicas" %in% names(call_parameters),
                         call_parameters$num_replicas,
                         "")
  
  # Generate the subtitle dynamically
  subtitle <- ifelse(behaviour_var != "" & flattened_groupings != "",
                     paste("Suggested interpretation: Is there a difference in", behaviour_var,
                           "by groups defined by", flattened_groupings, "?"),
                     "")
  
  # Generate the caption
  caption_text <- paste(
    "Number of samples:", num_samples, "\n",
    "Distribution per sex:", sex_dist, "\n",
    "Distribution per monkey species:", monkey_dist, "\n",
    "Number of bootstrapping replicas:", num_replicas, "\n",
    "Variable defining sample blocks cluster:", "individual",
    sep = " "
  )
  
  # Generate the legend title
  legend_title <- flattened_groupings
  
  # Plot the bootstrap results
  plot <- bootstrap_results %>% 
    ggplot(aes(x = behavior, 
               y = point_estimate, 
               ymin = lower_ci,
               ymax = upper_ci, 
               color = group, 
               group = group)) +
    geom_point(position = position_dodge(0.6)) + 
    geom_errorbar(position = position_dodge(0.6), width = 0.25) +
    labs(title = "Behavioural Estimates per Group",
         subtitle = subtitle,
         y = "Point Estimate", 
         x = "Behaviour",
         caption = caption_text) +
    theme_minimal() +
    theme(legend.position = "bottom") +
    facet_wrap(~ frequency_tier, ncol = 2, scales = "free") +
    scale_x_discrete(labels = function(x) gsub(" ", "\n", x),
                     guide = guide_axis(n.dodge = n_dodge)) +
    scale_color_discrete(name = legend_title) 
  
  # Save the plot if file_path is provided
  if (!is.null(file_path)) {
    ggsave(file_path, plot, width = width, height = height, dpi = 300)
  }
  
  return(plot)
}



#
# 5. Integrated behavioural anaolysis (inferential statistics + boostrapping)
#

integrated_behavioral_analysis <- function(
    
    data=dbGetQuery(db_con, "SELECT * FROM individual_behaviour"),
    
    explanatory_vars = c("sex"),
    
    behaviour_var = "behaviour",
    
    boostrap_parameters=list(num_bootstrap= 25, block_id = "individual"),
    
    replication_col="month",
    
    n_dodge=3
    
) {
    #
    # (-1)
    #
    required_columns <- c(explanatory_vars, behaviour_var, "duration", boostrap_parameters$block_id, replication_col)
    data <- drop_incomplete_rows(data, required_columns)

    #
    # (0) 
    #
    base_frequencies= data %>% 
      base_behavioural_frequencies(
        behaviour_var = behaviour_var
      ) 
    #
    # (1) 
    #
    boostrap_results=data %>% 
        bootstrap_parameter(
          parameter_col = "duration",
          behavior_col=behaviour_var, 
          group_cols = explanatory_vars,
          normalize = TRUE,
          block_col = boostrap_parameters$block_id, 
          num_bootstrap =  boostrap_parameters$num_bootstrap
        )
    #
    # (2) 
    #
    boostrap_results <- dplyr::left_join(
      boostrap_results,
      base_frequencies,
      by = "behavior") 
    #
    boostrap_results= boostrap_results %>%
      dplyr::mutate(
        behavior=factor(behavior, levels = base_frequencies$behavior, ordered = TRUE)
      )
    #
    # (3) 
    #
    get_distribution_string <- function(dataframe, column_name) {
      num_samples=nrow(dataframe)
      dataframe %>%
        dplyr::group_by({{column_name}}) %>%
        dplyr::summarise(count = n()) %>%
        dplyr::mutate(percentage = count / num_samples * 100) %>%
        dplyr::arrange(-count) %>%
        dplyr::summarise(dist_string = paste0({{column_name}}, ": ", count, " (", sprintf("%.3f", percentage), "%)")) %>%
        dplyr::pull(dist_string) %>%
        paste(collapse = ", ")
    }
    # 
    # Define export path using INDIVIDUAL_ANALYSIS_HOME and behaviour_var
    file_name <- paste0(explanatory_vars[1], "_", behaviour_var, ".png")
    file_path <- file.path(INDIVIDUAL_ANALYSIS_HOME, file_name)
    # Generate and export the plot
    boostrap_plot <- boostrap_results %>% 
      plot_bootstrap_results(
        n_dodge = n_dodge,
        call_parameters = list(
          groupings = explanatory_vars,
          behaviour_var = behaviour_var,
          num_samples = data %>% nrow(),
          sex_dist = get_distribution_string(data, sex),
          monkey_dist = get_distribution_string(data, monkey_group),
          num_replicas = boostrap_parameters$num_bootstrap
        ),
        file_path = file_path,  # Pass the generated file path
      )
    #
    message("Plot saved to: ", file_path)
    #
    # (4) 
    #
    test_results <- data %>%
      
        Behaviour_data.XGroups.Duration(
          groupings = c(explanatory_vars[1], replication_col), 
          behaviour_var = behaviour_target_var
        ) %>% #View()
        flexible_comparison_across_groups(
          dependent_var = "relative_duration", 
          independent_var = explanatory_vars[1],
          item_var=behaviour_target_var, 
          alpha = 0.05,
          post_hoc = TRUE
        ) 
  
    result <- list(
      #base_frequencies = base_frequencies,
      boostrap_results = boostrap_results,
      boostrap_plot = boostrap_plot,
      test_results = test_results
    )
}
#
# (example 1)
#
integrated_analysis_results=
  integrated_behavioral_analysis(
    
  data=dbGetQuery(db_con, "SELECT * FROM individual_behaviour"),
  
  explanatory_vars = c("sex"),
  
  behaviour_var = "behaviour",

  boostrap_parameters=list(num_bootstrap= NUM_BOOTSTRAPS, block_id = "individual"),
  
  replication_col="month"
  
)
#
integrated_analysis_results$boostrap_results %>% View()
integrated_analysis_results$boostrap_plot
integrated_analysis_results$test_results
#
# (example 2)
#
integrated_analysis_results=
  integrated_behavioral_analysis(
    
    data=dbGetQuery(db_con, "SELECT * FROM individual_behaviour"),
    
    explanatory_vars = c("reproductive_state"), # only defined on woman
    
    behaviour_var = "behaviour",
    
    boostrap_parameters=list(num_bootstrap= 25, block_id = "individual"),
    
    replication_col="month"
    
  )
#
integrated_analysis_results$boostrap_results %>% View()
integrated_analysis_results$boostrap_plot
integrated_analysis_results$test_results

#
# 6. main to perform multiple comparisons
#

#
# (0) 
#
BEHAVIOUR_DATA=dbGetQuery(db_con, "SELECT * FROM individual_behaviour")
BOOTSTRAP_PARAMETERS=list(num_bootstrap= NUM_BOOTSTRAPS, block_id = "individual")
REPLICATION_COL="month"
#
# (1)
#
intended_comparisons=list(
    "comparison_1"=list(
      explanatory_vars = c("sex"),
      behaviour_var = "behaviour"
    ),
    "comparison_2"=list(
      explanatory_vars = c("monkey_group"),
      behaviour_var = "behaviour"
    ),
    "comparison_3"=list(
      explanatory_vars = c("reproductive_state"),
      behaviour_var = "behaviour"
    )) 
#
# (2)
#
results_per_comparison=intended_comparisons %>% 
  lapply(function(parameters){
    integrated_analysis_results=
      integrated_behavioral_analysis(
        
        data=BEHAVIOUR_DATA,
        
        explanatory_vars = parameters$explanatory_vars,
        
        behaviour_var = parameters$behaviour_var,
        
        boostrap_parameters=BOOTSTRAP_PARAMETERS,
        
        replication_col=REPLICATION_COL
      )
  })

#
# 7.
#

#
setwd(INDIVIDUAL_ANALYSIS_HOME)
results_per_comparison %>% excel_report(
    "new_behavioral_results.xlsx",
    width = BEHAVIOURAL_PLOTS_WIDTH, 
    height = BEHAVIOURAL_PLOTS_HEIGHT
  )

