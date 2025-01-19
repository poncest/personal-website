# Saving normal images (no patchwork)
save_plot <- function(plot, 
                      type = c("tidytuesday", "swd", "standalone"),
                      year = format(Sys.Date(), "%Y"),
                      week = NULL,
                      month = NULL,
                      date = NULL,
                      name = NULL, 
                      height = 8, 
                      width = 10) {
  
  # Match argument and error handling
  type <- match.arg(type)
  
  # Base paths setup
  base_paths <- list(
    tidytuesday = here::here("data_visualizations/TidyTuesday", year),
    swd = here::here("data_visualizations/SWD Challenge", year),
    standalone = here::here("projects/standalone_visualizations")
  )
  
  # Construct file name based on type
  file_name <- switch(
    type,
    tidytuesday = sprintf("tt_%d_%02d.png", year, week),
    swd = sprintf("swd_%d_%02d.png", year, month %||% as.numeric(format(Sys.Date(), "%m"))),
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
  
  # Input validation
  if (type == "tidytuesday" && is.null(week)) {
    stop("Week parameter is required for TidyTuesday plots")
  }
  if (type == "swd" && is.null(month)) {
    warning("Month not specified for SWD plot, using current month")
  }
  
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

# Saving more complex images (when using patchwork)
save_plot_patchwork <- function(plot, 
                                type = c("tidytuesday", "swd", "standalone"),
                                year = format(Sys.Date(), "%Y"),
                                week = NULL,
                                month = NULL,
                                date = NULL,
                                name = NULL, 
                                height = 10, 
                                width = 16) {
  
  # Match argument and error handling
  type <- match.arg(type)
  
  # Required packages
  if (!requireNamespace("ggplotify", quietly = TRUE)) {
    install.packages("ggplotify")
  }
  if (!requireNamespace("showtext", quietly = TRUE)) {
    install.packages("showtext")
  }
  
  # Base paths setup
  base_paths <- list(
    tidytuesday = here::here("data_visualizations/TidyTuesday", year),
    swd = here::here("data_visualizations/SWD Challenge", year),
    standalone = here::here("projects/standalone_visualizations")
  )
  
  # Construct file name based on type
  file_name <- switch(
    type,
    tidytuesday = sprintf("tt_%d_%02d.png", year, week),
    swd = sprintf("swd_%d_%02d.png", year, month %||% as.numeric(format(Sys.Date(), "%m"))),
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
  
  # Input validation
  if (type == "tidytuesday" && is.null(week)) {
    stop("Week parameter is required for TidyTuesday plots")
  }
  if (type == "swd" && is.null(month)) {
    warning("Month not specified for SWD plot, using current month")
  }
  
  # Save main plot
  plot_grob <- ggplotify::as.grob(plot)
  png(
    filename = main_file, 
    width = width, 
    height = height, 
    units = "in", 
    res = 320, 
    type = "cairo"
  )
  
  # Font setup
  windowsFonts(Arial = windowsFont("Arial"))
  sysfonts::font_add("fa6-brands", here::here("fonts/6.6.0/Font Awesome 6 Brands-Regular-400.otf"))
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
  
  # Return the paths invisibly
  invisible(list(main = main_file, thumbnail = thumb_file))
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