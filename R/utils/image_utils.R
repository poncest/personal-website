# Saving normal images (no patchwork) ----
save_plot <- function(plot, 
                      type = c("tidytuesday", "swd", "standalone", "makeovermonday", "30daychartchallenge"),
                      year = format(Sys.Date(), "%Y"),
                      week = NULL,
                      day = NULL,
                      month = NULL,
                      date = NULL,
                      name = NULL, 
                      exercise = NULL,
                      height = NULL, 
                      width = NULL) {
  
  # Match argument and error handling
  type <- match.arg(type)
  
  # Base paths setup
  base_paths <- list(
    tidytuesday = here::here("data_visualizations/TidyTuesday", as.character(year)),  # Ensure year is character
    swd = here::here("data_visualizations/SWD Challenge", as.character(year)),
    makeovermonday = here::here("data_visualizations/MakeoverMonday", as.character(year)),
    `30daychartchallenge` = here::here("data_visualizations/30DayChartChallenge", as.character(year)),
    standalone = here::here("projects/standalone_visualizations")
  )
  
  # Input validation
  if (type == "tidytuesday" && is.null(week)) {
    stop("Week parameter is required for TidyTuesday plots")
  }
  if (type == "makeovermonday" && is.null(week)) {
    stop("Week parameter is required for MakeoverMonday plots")
  }
  if (type == "30daychartchallenge" && is.null(day)) {
    stop("Day parameter is required for 30DayChartChallenge plots")
  }
  if (type == "swd" && is.null(month)) {
    warning("Month not specified for SWD plot, using current month")
  }
  
  # Construct file name based on type
  file_name <- switch(
    type,
    tidytuesday = sprintf("tt_%d_%02d.png", year, week),
    makeovermonday = sprintf("mm_%d_%02d.png", year, week),
    `30daychartchallenge` = sprintf("30dcc_%d_%02d.png", year, day),
    swd = if (!is.null(exercise)) {
      sprintf("swd_%d_%02d-Ex_%04d.png", year, month %||% as.numeric(format(Sys.Date(), "%m")), exercise)
    } else {
      sprintf("swd_%d_%02d.png", year, month %||% as.numeric(format(Sys.Date(), "%m")))
    },
    standalone = if (!is.null(name)) {
      paste0(name, ".png")
    } else {
      sprintf("sa_%d-%02d-%02d.png", 
              year,
              month %||% as.numeric(format(Sys.Date(), "%m")),
              date %||% as.numeric(format(Sys.Date(), "%d")))
    }
  )
  
  # Set up paths
  base_path <- base_paths[[type]]
  main_file <- file.path(base_path, file_name)
  thumb_file <- file.path(base_path, "thumbnails", file_name)
  
  # Create directories if they don't exist
  dir.create(dirname(main_file), recursive = TRUE, showWarnings = FALSE)
  dir.create(dirname(thumb_file), recursive = TRUE, showWarnings = FALSE)
  
  # Save main plot
  ggsave(
    filename = main_file,
    plot = plot,
    width = width,
    height = height,
    units = "in",
    dpi = 320
  )
  
  # Create thumbnail using magick
  magick::image_read(main_file) |> 
    magick::image_resize("400") |> 
    magick::image_write(thumb_file)
  
  # Return the paths invisibly
  invisible(list(main = main_file, thumbnail = thumb_file))
}


# Saving more complex images (when using patchwork) ----
save_plot_patchwork <- function(plot, 
                                type = c("tidytuesday", "swd", "standalone", "makeovermonday", "30daychartchallenge"),
                                year = format(Sys.Date(), "%Y"),
                                week = NULL,
                                day = NULL,
                                month = NULL,
                                date = NULL,
                                name = NULL, 
                                exercise = NULL, 
                                height = NULL, 
                                width = NULL) {
  
  # Match argument and error handling
  type <- match.arg(type)
  
  # Check required packages all at once
  required_packages <- c("ggplotify", "showtext", "sysfonts", "grid")
  missing_packages <- required_packages[!sapply(required_packages, requireNamespace, quietly = TRUE)]
  
  if (length(missing_packages) > 0) {
    install.packages(missing_packages)
  }
  
  # Base paths setup
  base_paths <- list(
    tidytuesday = here::here("data_visualizations/TidyTuesday", as.character(year)),  # Ensure year is character
    swd = here::here("data_visualizations/SWD Challenge", as.character(year)),
    `30daychartchallenge` = here::here("data_visualizations/30DayChartChallenge", as.character(year)),
    standalone = here::here("projects/standalone_visualizations"),
    makeovermonday = here::here("data_visualizations/MakeoverMonday", as.character(year))
  )
  
  # Input validation
  if (type == "tidytuesday" && is.null(week)) {
    stop("Week parameter is required for TidyTuesday plots")
  }
  if (type == "makeovermonday" && is.null(week)) {
    stop("Week parameter is required for MakeoverMonday plots")
  }
  if (type == "30daychartchallenge" && is.null(day)) {
    stop("Day parameter is required for 30DayChartChallenge plots")
  }
  if (type == "swd" && is.null(month)) {
    warning("Month not specified for SWD plot, using current month")
  }
  if (!is.null(week) && (!is.numeric(week) || week < 1 || week > 53)) {
    stop("Week must be a number between 1 and 53")
  }
  
  # Construct file name based on type
  file_name <- switch(
    type,
    tidytuesday = sprintf("tt_%d_%02d.png", year, week),
    makeovermonday = sprintf("mm_%d_%02d.png", year, week),
    `30daychartchallenge` = sprintf("30dcc_%d_%02d.png", year, day),
    swd = if (!is.null(exercise)) {
      sprintf("swd_%d_%02d-Ex_%04d.png", year, month %||% as.numeric(format(Sys.Date(), "%m")), exercise)
    } else {
      sprintf("swd_%d_%02d.png", year, month %||% as.numeric(format(Sys.Date(), "%m")))
    },
    standalone = if (!is.null(name)) {
      paste0(name, ".png")
    } else {
      sprintf("sa_%d-%02d-%02d.png", 
              year,
              month %||% as.numeric(format(Sys.Date(), "%m")),
              date %||% as.numeric(format(Sys.Date(), "%d")))
    }
  )
  
  # Error handling for font loading
  tryCatch({
    # Font setup
    windowsFonts(Arial = windowsFont("Arial"))
    font_path <- here::here("fonts/6.6.0/Font Awesome 6 Brands-Regular-400.otf")
    
    if (!file.exists(font_path)) {
      warning("Font Awesome file not found at specified path")
    } else {
      sysfonts::font_add("fa6-brands", font_path)
    }
  }, error = function(e) {
    warning("Error loading fonts: ", e$message)
  })
  
  # Set up paths
  base_path <- base_paths[[type]]
  main_file <- file.path(base_path, file_name)
  thumb_file <- file.path(base_path, "thumbnails", file_name)
  
  # Create directories if they don't exist
  dir.create(dirname(main_file), recursive = TRUE, showWarnings = FALSE)
  dir.create(dirname(thumb_file), recursive = TRUE, showWarnings = FALSE)
  
  # Save main plot
  tryCatch({
    plot_grob <- ggplotify::as.grob(plot)
    png(
      filename = main_file, 
      width = width, 
      height = height, 
      units = "in", 
      res = 320, 
      type = "cairo"
    )
    
    showtext::showtext_begin()
    showtext::showtext_opts(dpi = 320, regular.wt = 300, bold.wt = 800)
    
    # Draw plot
    grid::grid.draw(plot_grob)
    
    # Cleanup
    showtext::showtext_end()
    dev.off()
    
    # Create thumbnail using magick
    magick::image_read(main_file) |> 
      magick::image_resize("400") |> 
      magick::image_write(thumb_file)
    
  }, error = function(e) {
    dev.off()
    stop("Error saving plot: ", e$message)
  })
  
  # Return the paths invisibly
  invisible(list(
    main = main_file, 
    thumbnail = thumb_file,
    type = type,
    date_saved = Sys.time()
  ))
}

# Helper function for NULL coalescing
`%||%` <- function(x, y) if (is.null(x)) y else x

# Usage examples:
# 
# # TidyTuesday plot
# save_plot(
#   plot = combined_plot,
#   type = "tidytuesday", 
#   week = 48
# )
#
# # SWD Challenge plot
# save_plot(
#   plot = cumulative_line_chart,
#   type = "swd",
#   month = 11
# )
#
# # Standalone plot with automatic date
# save_plot(
#   plot = combined_plot,
#   type = "standalone"
# )
#
# # Standalone plot with custom name
# save_plot(
#   plot = combined_plot,
#   type = "standalone",
#   name = "sa_2024-11-13"
# )
#
# # Same patterns work for save_plot_patchwork()