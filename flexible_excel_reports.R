#' title: Generate Comprehensive Excel Reports
#' description: A flexible function to create Excel reports with dataframes, ggplots, leaflet maps, and nested lists of content.
#' report_url: Generates structured Excel reports with support for mixed content types.


library(openxlsx)
library(ggplot2)
library(leaflet)
library(png)
library(grid)
library(gridExtra)
library(webshot)

excel_report <- function(report_items, file_name = "report.xlsx", width = 10, height = 6) {
  #' Generate Comprehensive Excel Reports
  #'
  #' This function creates an Excel report containing various types of content, including dataframes, ggplots, leaflet maps, 
  #' and nested lists. Each content item is added to a separate sheet in the Excel file.
  #'
  #' Args:
  #'   report_items: A list of items to include in the report. Supported types include:
  #'                 - dataframes: Written as Excel tables.
  #'                 - ggplots: Saved as PNG images and inserted into sheets.
  #'                 - leaflet maps: Screenshotted and added as images.
  #'                 - lists: Processed recursively, creating sub-sheets.
  #'   file_name: The name of the output Excel file (default: "report.xlsx").
  #'   width: The width of the image sheets for ggplots and maps in inches (default: 10).
  #'   height: The height of the image sheets for ggplots and maps in inches (default: 6).
  #'
  #' Returns:
  #'   Saves an Excel file with structured content.
  
  # Create a new Excel workbook
  wb <- createWorkbook()
  message("Created new Excel workbook.")
  
  # Recursive helper function to process each item
  process_item <- function(item, sheet_name = "Sheet 1") {
    if (inherits(item, "data.frame")) {
      message(sprintf("Adding dataframe to sheet: '%s'.", sheet_name))
      addWorksheet(wb, sheet_name)
      writeData(wb, sheet_name, item)
    } else if (inherits(item, "gg")) {
      message(sprintf("Adding ggplot to sheet: '%s'.", sheet_name))
      # Save ggplot to temporary PNG with white background and insert as image
      temp_file <- tempfile(fileext = ".png")
      ggsave(temp_file, plot = item, width = width, height = height, bg = "white")
      addWorksheet(wb, sheet_name)
      insertImage(wb, sheet_name, temp_file, width = width, height = height)
    } else if (inherits(item, "leaflet")) {
      message(sprintf("Adding leaflet map to sheet: '%s'.", sheet_name))
      # Save leaflet map to temporary HTML, then use webshot to take a screenshot
      temp_html <- tempfile(fileext = ".html")
      saveWidget(item, temp_html)  # Save the leaflet map as an HTML file
      temp_file <- tempfile(fileext = ".png")
      webshot(temp_html, file = temp_file, vwidth = width * 100, vheight = height * 100)  # Screenshot as PNG
      addWorksheet(wb, sheet_name)
      insertImage(wb, sheet_name, temp_file, width = width, height = height)
    } else if (is.list(item)) {
      message(sprintf("Processing nested list for sheet: '%s'.", sheet_name))
      # Process each item in the list recursively
      for (i in seq_along(item)) {
        process_item(item[[i]], sheet_name = paste0(sheet_name, "_", i))
      }
    } else {
      warning(sprintf("Unsupported item type: '%s' for sheet: '%s'.", class(item), sheet_name))
    }
  }
  
  # Iterate over each report item
  for (i in seq_along(report_items)) {
    message(sprintf("Processing report item %d.", i))
    process_item(report_items[[i]], sheet_name = paste0("Item_", i))
  }
  
  # Save the workbook with the specified file name
  saveWorkbook(wb, file_name, overwrite = TRUE)
  message(sprintf("Excel report saved as '%s'.", file_name))
}
