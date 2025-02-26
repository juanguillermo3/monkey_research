#'
#' title: Dataframe extentions
#' description: Provide several enancements to regular R dataframe, wich can be used in dplyr pipelines
#' 

library(dplyr)
library(stringr)

#
rename_by_regex <- function(Dataframe, patterns) {
  #' Rename columns by regex patterns
  #'
  #' @param Dataframe A data frame.
  #' @param patterns A list of regular expression patterns.
  #' @return A dataframe with selected columns renamed based on matching patterns.
  
  # Concatenate regex patterns using OR (|)
  combined_pattern <- paste(patterns, collapse = "|")
  
  # Select matching columns
  selected_df <- cols_by_regex(Dataframe, combined_pattern)
  
  # Rename columns based on the sequence of patterns they match
  new_colnames <- purrr::map_chr(names(selected_df), function(col) {
    matches <- patterns[stringr::str_detect(col, patterns)]
    if (length(matches) > 0) {
      paste(matches, collapse = "_")  # Concatenate matched patterns
    } else {
      col  # Keep original if no match (shouldn't happen since we filter first)
    }
  })
  
  # Apply new column names
  colnames(selected_df) <- new_colnames
  
  return(selected_df)
}

#
cols_by_regex <- function(Dataframe, pattern) {
  #' Fetch columns from a dataframe matching a regex pattern.
  #'
  #' @param Dataframe A data frame.
  #' @param pattern A regular expression pattern to match column names.
  #' @return A dataframe with only the columns that match the pattern.
  
  matched_cols <- names(Dataframe)[stringr::str_detect(names(Dataframe), pattern)]
  message("Matched columns: ", paste(matched_cols, collapse = ", "))
  dplyr::select(Dataframe, dplyr::all_of(matched_cols))
}

#
postfix_duplicate_columns <- function(dataframe) {
  #' Append an index to duplicate column names to ensure uniqueness.
  #'
  #' This function scans the column names of the given dataframe and, if any 
  #' duplicate names exist, appends an index (e.g., `_2`, `_3`) to differentiate them. 
  #' The function maintains the original order of columns and only modifies duplicates.
  #' A message is displayed if any column names are changed.
  #'
  #' @param dataframe A dataframe with potentially duplicate column names.
  #' @return A dataframe with unique column names, indexed where necessary.
  
  original_colnames <- colnames(dataframe)  # Store the original column names
  seen_cols <- list()  # Dictionary to track occurrences of column names
  new_colnames <- c()  # List to store updated column names
  
  # Iterate through each column name
  for (col_name in original_colnames) {
    if (col_name %in% names(seen_cols)) {
      # If the column name has been seen before, increment its counter
      seen_cols[[col_name]] <- seen_cols[[col_name]] + 1
      # Append an index to differentiate duplicate names
      new_colnames <- c(new_colnames, paste0(col_name, "_", seen_cols[[col_name]]))
    } else {
      # First occurrence of the column name, initialize it in the dictionary
      seen_cols[[col_name]] <- 1
      new_colnames <- c(new_colnames, col_name)
    }
  }
  
  # If any renaming was performed, display a message
  if (!identical(original_colnames, new_colnames)) {
    message("Column renaming applied due to duplicates: ", 
            paste(setdiff(new_colnames, original_colnames), collapse = ", "))
  }
  
  # Assign the modified column names back to the dataframe
  colnames(dataframe) <- new_colnames
  return(dataframe)
}

#
merge_header_rows <- function(.data, n = 2, sep = "_", default_value = "") {
  #' Concatenate multiple header rows into a single dataframe header
  #'
  #' @description Sometimes, dataframes are provided in source files where column headers 
  #' are scattered over multiple rows. This function enhances an R dataframe by merging 
  #' the text content of the first `n` rows (parametrizable) into a single dataframe header.
  #' It integrates seamlessly with `dplyr` pipelines.
  #'
  #' @param .data A `data.frame` or `tibble` where headers are scattered across multiple rows.
  #' @param n An integer specifying the number of rows to merge into the header.
  #' @param sep A character string used to concatenate header fragments (default: "_").
  #' @param default_value A character string used to replace missing header values (default: "").
  #' @return A cleaned `tibble` with a single header row.
  
  # Extract first `n` rows as headers, replacing NA or empty values with default_value
  header <- .data[1:n, ] %>%
    apply(2, function(x) {
      x[is.na(x) | x == ""] <- default_value  # Replace missing values
      clean_name <- gsub(paste0(sep, "+"), sep, paste(x, collapse = sep))  # Collapse and clean
      return(clean_name)
    })
  
  # Remove the first `n` rows and set new column names
  .data <- .data[-(1:n), ]
  colnames(.data) <- header
  
  return(.data)
}

#
drop_incomplete_rows <- function(Dataframe, cols) {
  
  # Ensure required columns exist
  check_required_columns(Dataframe, cols)
  
  # Compute missing row statistics per column
  missing_stats <- Dataframe %>%
    dplyr::summarise(across(all_of(cols), ~ sum(is.na(.)))) %>%
    tidyr::pivot_longer(cols = everything(), names_to = "Column", values_to = "Missing_Rows") %>%
    dplyr::mutate(Percentage = round((Missing_Rows / nrow(Dataframe)) * 100, 2))
  
  # Print summary of missing values per column
  message("\n🛑 Missing Data Analysis:")
  print(missing_stats, row.names = FALSE)
  
  # Compute how many rows will be dropped
  rows_before <- nrow(Dataframe)
  Dataframe <- Dataframe %>% dplyr::filter(complete.cases(dplyr::select(., all_of(cols))))
  rows_after <- nrow(Dataframe)
  
  # Print summary of dropped rows
  message("\n✅ Rows before: ", rows_before, 
          "\n✅ Rows after: ", rows_after, 
          "\n🚨 Total rows removed: ", rows_before - rows_after, 
          " (", round(((rows_before - rows_after) / rows_before) * 100, 2), "% of data)\n")
  
  return(Dataframe)
}

#
# (1)
#
explode_and_standardize <- function(
    Dataframe, 
    nested_column, 
    split_regex = ",", 
    index_col = "row_index"
) {
  #' Expands a column containing nested values into multiple rows while standardizing.
  #'
  #' @param Dataframe Data frame containing the data.
  #' @param nested_column Column name with nested values to be exploded.
  #' @param split_regex Regular expression to split the column values.
  #' @param index_col Name of the new column storing the original row index.
  #' @return A data frame where the specified column has been exploded into multiple rows and standardized.
  
  old_n_rows <- nrow(Dataframe)  # Store original row count
  
  expanded_df <- Dataframe %>%
    dplyr::mutate(!!index_col := dplyr::row_number()) %>%  # Assign row index
    tidyr::separate_rows(!!rlang::sym(nested_column), sep = split_regex) %>%  # Explode rows
    dplyr::mutate(
      !!nested_column := stringr::str_squish(stringr::str_trim(stringr::str_to_lower(!!rlang::sym(nested_column))))
    )  # Standardize column values
  
  new_n_rows <- nrow(expanded_df)  # Count new rows
  
  # Log transformation summary
  message(
    sprintf(
      "Column '%s' expanded: %d → %d rows.", 
      nested_column, old_n_rows, new_n_rows
    )
  )
  
  return(expanded_df)
}
#
# (2)
#
rollback_exploded_records <- function(
    Dataframe, 
    expanded_column, 
    index_col = "row_index", 
    join_string = ","
) {
  #' Rolls back an exploded column by aggregating values back into a single row per index.
  #'
  #' @param Dataframe Data frame containing the expanded records.
  #' @param expanded_column Column that was previously exploded and needs to be collapsed.
  #' @param index_col Column storing the original row index.
  #' @param join_string Character used to concatenate values when collapsing.
  #' @return A data frame where the expanded column has been collapsed back into a single row per original index.
  
  old_n_rows <- nrow(Dataframe)  # Store original row count
  
  collapsed_df <- Dataframe %>%
    dplyr::group_by(!!rlang::sym(index_col)) %>%
    dplyr::summarise(
      dplyr::across(-!!rlang::sym(expanded_column), dplyr::first),  # Retain first value of non-expanded columns
      !!expanded_column := stringr::str_c(
        stringr::str_squish(stringr::str_trim(stringr::str_to_lower(!!rlang::sym(expanded_column)))), 
        collapse = join_string
      ),  # Collapse and standardize
      .groups = "drop"
    ) %>%
    dplyr::arrange(!!rlang::sym(index_col))  # Ensure ordering
  
  new_n_rows <- nrow(collapsed_df)  # Count new rows
  
  # Log transformation summary
  message(
    sprintf(
      "Column '%s' collapsed: %d → %d rows.", 
      expanded_column, old_n_rows, new_n_rows
    )
  )
  
  return(collapsed_df)
}

#
wipe_out_row_names <- function(Dataframe) {
  #' Remove row names from a dataframe.
  #'
  #' @param Dataframe A data frame.
  #' @return The dataframe without row names.
  
  message("Removing row names from dataframe.")
  row.names(Dataframe)=NULL
  Dataframe
}

#
rowwise_sum <- function(Dataframe, sum_columns, new_col, na.rm = TRUE) {
  #' Perform a row-wise sum of selected columns and add the result as a new column.
  #'
  #' @param Dataframe The input dataframe.
  #' @param sum_columns Character vector of columns to sum.
  #' @param new_col Name of the new column to store the sum.
  #' @param na.rm Logical value indicating whether to remove NA values (default: TRUE).
  #' @return A dataframe with the new column containing row-wise sums.
  
  message("Summing columns: ", paste(sum_columns, collapse = ", "))
  message("Creating new column: ", new_col)
  
  Dataframe %>%
    dplyr::rowwise() %>%
    dplyr::mutate(
      {{ new_col }} := sum(dplyr::c_across(all_of(sum_columns)), na.rm = na.rm)
    ) %>%
    dplyr::ungroup()
}

#
check_required_columns <- function(Dataframe, mandatory_vars) {
  #' Check for the presence of required columns in a dataframe.
  #'
  #' @param Dataframe A dataframe to check.
  #' @param mandatory_vars A character vector of required column names.
  #' @return The original dataframe if validation passes; otherwise, an error is thrown.
  
  missing_vars <- setdiff(mandatory_vars, names(Dataframe))
  
  if (length(missing_vars) > 0) {
    stop(
      "The following mandatory columns are missing from the dataframe:\n",
      paste(missing_vars, collapse = ", "), 
      "\nTotal missing: ", length(missing_vars)
    )
  } else {
    message("All required columns are present.")
  }
  
  return(Dataframe)
}

#
prefix_dataframe_columns <- function(Dataframe, prefix, cols = NULL) {
  #' Prefix selected column names of a dataframe with a specified string.
  #'
  #' @param Dataframe A dataframe whose columns will be renamed.
  #' @param prefix A string to prepend to selected column names.
  #' @param cols A character vector of column names to prefix (default is all columns).
  #' @return A dataframe with updated column names.
  
  if (is.null(cols)) {
    cols <- names(Dataframe)
  }
  
  message("Adding prefix to selected column names")
  original_names <- names(Dataframe)
  
  updated_names <- original_names
  updated_names[original_names %in% cols] <- paste0(prefix, original_names[original_names %in% cols])
  
  message("Original vs. Updated Names:")
  print(data.frame(Original = original_names, Updated = updated_names))
  
  names(Dataframe) <- updated_names
  return(Dataframe)
}

#
curate_names_for_stata <- function(Dataframe) {
  #' Curate column names of a dataframe for Stata.
  #'
  #' @param Dataframe A dataframe with columns to be curated.
  #' @return A dataframe with curated column names.
  
  original_names <- names(Dataframe)
  
  # Step 1: Convert to lowercase
  message("Step 1: Converting names to lowercase")
  curated_names <- tolower(original_names)
  
  # Step 2: Remove accents and special characters
  message("Step 2: Removing accents and special characters")
  curated_names <- stringi::stri_trans_general(curated_names, "Latin-ASCII")
  
  # Step 3: Replace non-alphanumeric characters with underscores
  message("Step 3: Replacing non-alphanumeric characters with underscores")
  curated_names <- stringr::str_replace_all(curated_names, "[^a-z0-9]", "_")
  
  # Step 4: Shorten names to a maximum of 25 characters
  message("Step 4: Trimming names to 25 characters")
  curated_names <- substr(curated_names, 1, 25)
  
  # Step 5: Prefix names starting with a number with 'v_'
  message("Step 5: Prefixing numeric-starting names with 'v_'")
  curated_names <- ifelse(stringr::str_detect(curated_names, "^[0-9]"), paste0("v_", curated_names), curated_names)
  
  message("Original vs. Curated Names:")
  print(data.frame(Original = original_names, Curated = curated_names))
  
  # Assign curated names to dataframe and return
  names(Dataframe) <- curated_names
  return(Dataframe)
}


#
map_NAS <- function(Dataframe, mapped_to = 0, columns = NULL) {
  #' Replace NA values in specified dataframe columns with a given value, with logging and type safety.
  #'
  #' @param Dataframe Data frame containing the data.
  #' @param mapped_to Value to replace NAs with (default is 0).
  #' @param columns Optional character vector of column names to impute. If NULL, all columns are used.
  #' @return A dataframe with NA values replaced.
  
  # Filter columns to impute
  target_columns <- if (is.null(columns)) colnames(Dataframe) else columns
  
  # Count NAs in target columns
  na_count <- sum(is.na(Dataframe[target_columns]))
  message("NA values are defined by: is.na()")
  message("Number of NA values matched (in selected columns): ", na_count)
  message("Replacing NA values with: ", mapped_to)
  
  # Replace NAs using vectorized ifelse
  Dataframe %>%
    dplyr::mutate(across(
      all_of(target_columns),
      ~ ifelse(is.na(.), mapped_to, .)
    ))
}

#
frequency_counts <- function(Dataframe, counted, normalize = FALSE, sort = TRUE, exclude_na = FALSE) {
  #' Compute value frequencies for a dataframe column and output wide format.
  #'
  #' @param Dataframe Data frame containing the data.
  #' @param counted Column name (string) to count frequencies from.
  #' @param normalize Logical. If TRUE, returns percentages instead of counts.
  #' @param sort Logical. If TRUE, sorts by count in descending order.
  #' @param exclude_na Logical. If TRUE, excludes NA values from counts.
  #' @return A data frame in wide format with frequencies or percentages.
  
  # Filter out NA values if requested
  if (exclude_na) {
    Dataframe <- Dataframe[!is.na(Dataframe[[counted]]), ]
  }
  
  # Count frequencies
  counts <- Dataframe %>%
    dplyr::count(.data[[counted]], name = "count")
  
  # Normalize counts if requested
  if (normalize) {
    counts <- counts %>%
      dplyr::mutate(count = round(count / sum(count) * 100, 3))
  }
  
  # Sort counts if requested
  if (sort) {
    counts <- counts %>%
      dplyr::arrange(dplyr::desc(count))
  }
  
  # Reshape the data into wide format
  counts %>%
    tidyr::pivot_wider(
      names_from = .data[[counted]],
      values_from = count,
      values_fill = 0
    )
}

#
regex_on_col_names <- function(df, pattern) {
  # Store the original column names for reference
  original_cols <- colnames(df)
  
  # Apply the regex pattern using str_extract to match the desired portion of each column name
  matched_cols <- str_extract(original_cols, pattern)
  
  # Detect and warn about columns with no match (NA results from str_extract)
  no_match <- is.na(matched_cols)
  if(any(no_match)) {
    warning(paste("No match found for columns:", paste(original_cols[no_match], collapse = ", ")))
  }
  
  # Detect and warn about repeated matched names
  duplicates <- matched_cols[duplicated(matched_cols)]
  if(length(duplicates) > 0) {
    warning(paste("Repeated matched column names:", paste(unique(duplicates), collapse = ", ")))
  }
  
  # Return the modified dataframe with the matched column names
  colnames(df) <- matched_cols
  return(df)
}

#
regex_on_col_names <- function(df, pattern) {
  # Store the original column names for reference
  original_cols <- colnames(df)
  
  # Apply the regex pattern using str_extract to match the desired portion of each column name
  matched_cols <- str_extract(original_cols, pattern)
  
  # Detect and warn about columns with no match (NA results from str_extract)
  no_match <- is.na(matched_cols)
  if(any(no_match)) {
    warning(paste("No match found for columns:", paste(original_cols[no_match], collapse = ", ")))
  }
  
  # Detect and warn about repeated matched names
  duplicates <- matched_cols[duplicated(matched_cols)]
  if(length(duplicates) > 0) {
    warning(paste("Repeated matched column names:", paste(unique(duplicates), collapse = ", ")))
  }
  
  # Return the modified dataframe with the matched column names
  colnames(df) <- matched_cols
  return(df)
}

#
parse_numeric_columns <- function(df, threshold = 0.99) {
  df <- df %>%
    dplyr::mutate(across(everything(), ~ {
      valid_numeric <- sum(!is.na(suppressWarnings(as.numeric(.)))) / length(.)
      if (valid_numeric >= threshold) {
        message(sprintf("Column '%s' converted to numeric (%.2f%% valid values).", cur_column(), valid_numeric * 100))
        return(suppressWarnings(as.numeric(.)))
      }
      return(.)
    }))
  
  return(df)
}

#
# older versions (most of them will be replaced by the new syntx)
#

#
# (0)
#

Richdf.Nullify_row_names <- function(df) {
  row.names(df)=NULL;df 
}


#
# (1)
#
Richdf.asNumeric <- function(df) {
  # This function will return TRUE if x can be coerced to a number, otherwise FALSE
  is_coercible <- function(x) {
    !is.na(suppressWarnings(as.numeric(x)))
  }
  
  df %>%
    dplyr::mutate(across(everything(), 
                         ~ if (mean(is_coercible(.)) >= 0.8) as.numeric(as.character(.)) else .
    ))
}


#
# (2)
#
Richdf_Summarize <- function(df) {
  summary_rows <- list()
  
  for (col_name in names(df)) {
    col_data <- df[[col_name]]
    
    # Check if the column can be converted to Date
    is_date <- FALSE
    first_non_na <- col_data[!is.na(col_data)][1]
    if (!is.null(first_non_na)) {
      is_date <- !inherits(try(as.Date(first_non_na), silent = TRUE), "try-error")
    }
    
    if (is_date) {
      col_data <- as.numeric(as.Date(col_data))
    }
    
    if (is.numeric(col_data)) {
      # Compute numeric summaries
      quantiles_vals <- quantile(col_data, na.rm = TRUE)
      min_val <- min(col_data, na.rm = TRUE)
      max_val <- max(col_data, na.rm = TRUE)
      mean_val <- mean(col_data, na.rm = TRUE)
      sd_val <- sd(col_data, na.rm = TRUE)
      
      summary_row <- data.frame(
        Min = min_val,
        `1st_Quartile` = quantiles_vals["25%"],
        Median = quantiles_vals["50%"],
        Mean = mean_val,
        `3rd_Quartile` = quantiles_vals["75%"],
        Max = max_val,
        SD = sd_val
      )
      rownames(summary_row)=NULL
      
    } else {
      # Compute categorical summaries
      tab <- table(col_data)
      freqs <- prop.table(tab)
      freq_quantiles <- quantile(freqs)
      
      summary_row <- data.frame(
        NumUniqueValues = length(unique(col_data)),
        Mode = names(tab)[which.max(tab)],
        Freq_1st_Quartile = freq_quantiles["25%"],
        Freq_Median = freq_quantiles["50%"],
        Freq_3rd_Quartile = freq_quantiles["75%"]
      )
      rownames(summary_row)=NULL
    }
    
    
    
    # Add the summary_row to the list of summary rows, keyed by the original column name
    summary_rows[[col_name]] <- summary_row
  }
  
  return(summary_rows)
}

#
# (3)
#
Richdf_Summarize.filter_out_constant <- function(df) {
  # Find constant columns
  constant_cols <- names(df)[sapply(df, function(col) length(unique(col)) == 1)]
  
  # Print the dropped variables
  if (length(constant_cols) > 0) {
    cat("Dropping constant columns:", constant_cols, "\n")
  } else {
    cat("No constant columns found.\n")
  }
  
  # Return dataframe without the constant columns
  df %>% select(-one_of(constant_cols))
}

#
# (4)
#
Richdf.DropFieldsWithMissingValues <- function(df, threshold = 0.5, rescue_cols = c()) {
  # Ensure rescue_cols is a character vector
  rescue_cols <- as.character(rescue_cols)
  
  # Calculate the proportion of missing values for each column
  prop_missing <- sapply(df, function(x) mean(is.na(x)))
  
  # Identify columns where the proportion of missing values is greater than the threshold
  # and are not in the rescue_cols list
  cols_to_drop <- names(prop_missing[prop_missing > threshold & !names(prop_missing) %in% rescue_cols])
  
  # Print the dropped columns
  if (length(cols_to_drop) > 0) {
    cat("Dropping columns with more than", threshold * 100, "% missing values:", cols_to_drop, "\n")
  } else {
    cat("No columns with more than", threshold * 100, "% missing values found.\n")
  }
  
  # Drop the identified columns and return the modified dataframe
  return(df[, !(names(df) %in% cols_to_drop)])
}

#
# (5)
#
dataframe_as_frequency_counts <- function(df, column) {
  df %>%
    dplyr::count(!!sym(column), name = "Absolute_Frequency") %>%
    arrange(Absolute_Frequency*(-1)) %>%
    mutate(Relative_Frequency = round(Absolute_Frequency / sum(Absolute_Frequency) * 100, 3),
           Cumulative_Absolute_Frequency = cumsum(Absolute_Frequency),
           Cumulative_Relative_Frequency = cumsum(Relative_Frequency)) 
}

#
# (6)
#
dataframe_cap_low_frequency <- function(df, 
                                        column, 
                                        max_cumulative_frequency = 95, 
                                        min_absolute_frequency = 10,
                                        other_label = "Other",
                                        force_other_values = NULL) { # Add a parameter for values to force into "Other"
  # Perform frequency analysis
  freq_analysis <- dataframe_as_frequency_counts(df, column)
  
  # Identify low-frequency values based on cumulative and absolute frequencies
  low_freq_values <- freq_analysis %>%
    filter(Cumulative_Relative_Frequency > max_cumulative_frequency | Absolute_Frequency < min_absolute_frequency) %>%
    pull(!!rlang::sym(column))
  
  # Combine forced values with those identified by frequency analysis
  if (!is.null(force_other_values)) {
    low_freq_values <- unique(c(low_freq_values, force_other_values))
  }
  
  # Map low-frequency and forced values to 'Other'
  df[[column]] <- ifelse(df[[column]] %in% low_freq_values, other_label, df[[column]])
  
  return(df)
}

#
# (7)
#
Dataframe.order=function(
    Dataframe=db.contrataciones_normalizadas(),
    order= c("tipo_contrato", "salario") 
){
  #
  # map Dataframe to a Dataframe that counts values for counted data column
  #
  
  Dataframe %>%
    {
      .[, 
        intersect( c(order, setdiff(names(.), order) ), names(.)) 
      ]
    }
}

#
# (8)
#
Dataframe.vars_as_character=function(
    Dataframe
){
  Dataframe %>% 
    lapply(function(data_col){
      as.character(data_col)
    }) %>%
    as.data.frame()
}






#
Dataframe.totalize=function(
    Dataframe,
    i_am_not_totalizable=NaN,
    Na.rm=TRUE,
    group_name_col=1
){
  #
  an=
    rbind(Dataframe,
          sapply(names(Dataframe),function(data_col){
            ifelse(
              !(data_col %in% i_am_not_totalizable),
              sum(Dataframe[[data_col]]  %>% as.numeric(), na.rm=Na.rm),
              "-"
            )})) 
  if(is.numeric( group_name_col)){
    an[nrow(an),group_name_col]="Totales"  
  }
  an
}
#
#Dataframe.totalize()

#
Dataframe.prefix=function(
    Dataframe,
    prefix="var_"
){
  names(Dataframe)=paste(prefix, names(Dataframe), sep="")
  Dataframe
}

#
Dataframe.new_names=function(
    Dataframe,
    new_names
){
  names(Dataframe)=new_names;Dataframe
}

#
Dataframe.count_values=function(
    Dataframe,
    counted
){
  new_df=
    table(Dataframe[[counted]]) %>%
    as.data.frame()
  new_df[[1]]=as.character(new_df[[1]])
  new_df
}

#
Dataframe.insert=function(
    Dataframe,
    row_of_values
){
  rbind(Dataframe, row_of_values)
}

Dataframe.apply_treshold=function(
    Dataframe,
    treshold=1
){
  Dataframe[Dataframe>=treshold]=1
  Dataframe[Dataframe<treshold]=0
  return(Dataframe)
}

#
Dataframe.alter_table_Dataframe_add_primary_key=function(
    Dataframe,
    primary_key_components
){
  new_df=Dataframe
  new_df$primary_key=""
  
  for (component in primary_key_components){
    new_df=
      new_df %>%
      dplyr::mutate(
        primary_key=paste(primary_key, .[[component]], sep="+" )
      )
  }
  
  new_df %>%
    dplyr::filter(!duplicated(primary_key)) %>%
    return(new_df)
}

#
Dataframe.select_on_regex=function(
    Dataframe,
    selecting_regex
){
  Dataframe %>%
    dplyr::select(
      grep(names(.), pattern=selecting_regex, value=TRUE)
    ) %>%
    return()
}


#
Dataframe.delimite_dates=function(
    Dataframe,
    lower_date,
    upper_date
){
  Dataframe %>%
    
    dplyr::filter(
      fecha  > lower_date
    ) %>%
    dplyr::filter(
      fecha  <  upper_date
    ) %>%
    
    return()
}


#
# regex associated behavour
# --------

#
Textual_feature.basic_standardization=function(
    names
){
  #
  tolower(names) %>%
    iconv(to="ASCII//TRANSLIT")
  #
}
#
i_am_my_name=function(x){
  names(x)=x;x
}
#
Dataframe.expand_regex_features=function(
    Dataframe,
    text_source,
    features=table(Dataframe[[text_source]]) %>% names() %>% i_am_my_name(),
    standardization=Textual_feature.basic_standardization,
    name_prefix
    
){
  #
  state_df= 
    Dataframe %>%
    dplyr::mutate(
      z_textual_source=standardization(.[[text_source]])
    )
  #
  for (feature_name in names(features)){
    state_df[[sprintf("%s%s", name_prefix, feature_name)]]=
      ifelse(
        stringr::str_detect(state_df$z_textual_source, pattern=features[[feature_name]]),1,0)
  }
  state_df %>%
    dplyr::select(-"z_textual_source")
}
#
#
Dataframe.extract_regex_fields=function(
    Dataframe,
    text_source,
    features=table(Dataframe[[text_source]]) %>% names() %>% i_am_my_name(),
    standardization=Textual_feature.basic_standardization,
    name_prefix
){
  #
  state_df= 
    Dataframe %>%
    dplyr::mutate(
      z_textual_source=standardization(.[[text_source]])
    )
  #
  for (feature_name in names(features)){
    state_df[[sprintf("%s%s", name_prefix, feature_name)]]=
      stringr::str_extract(state_df$z_textual_source, pattern=features[[feature_name]])
  }
  state_df %>%
    dplyr::select(-"z_textual_source")
}

#
Dataframe.fetch=function(
    Dataframe,
    fetched
){
  Dataframe %>% 
    {
      Dataframe[[fetched]]
    }
}


#
list.keep_dataframes=function(
    list
){
  list[sapply(list, function(l_){"data.frame" %in% class(l_)})]
}
#
Dataframe.round_numbers=
  function(Dataframe, digits=4){
    
    Dataframe %>%
      lapply(function(data_col){
        tryCatch({
          round(data_col, digits)
        }, error=function(e){return(data_col)})
      })  %>%
      as.data.frame()
  }

