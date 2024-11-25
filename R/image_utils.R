
# Saving normal images (no patchwoprk)
save_plot <- function(plot, type = "tidytuesday", year = 2024, week = NULL, name = NULL, height = 8, width = 10) {
  # Base paths setup
  base_paths <- list(
    tidytuesday = here::here("data_visualizations/TidyTuesday", year),
    swd = here::here("data_visualizations/SWD Challenge", year),
    standalone = here::here("projects/standalone_visualizations")
  )
  
  file_name <- switch(type,
                      tidytuesday = sprintf("tt_%d_%02d.png", year, week),
                      swd = sprintf("swd_%d_%02d.png", year, week),
                      standalone = paste0(name, ".png")
  )
  
  base_path <- base_paths[[type]]
  main_file <- file.path(base_path, file_name)
  thumb_file <- file.path(base_path, "thumbnails", file_name)
  
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
}

# Usage
# TidyTuesday plot
# save_plot(
#   plot = combined_plot,
#   type = "tidytuesday", 
#   week = 48,
#   year = 2024,
#   width = 16,
#   height = 10
# )

# # SWD Challenge plot
# save_plot(
#   plot = cumulative_line_chart,
#   type = "swd",
#   week = 11,
#   year = 2024,
#   width = 10,
#   height = 8
# )

# # Standalone plot
# save_plot(
#   plot = combined_plot,
#   type = "standalone",
#   name = "sa_2024-11-13",
#   width = 9,
#   height = 10
# )


# Saving more complex images (when using patchwork)
save_plot_patchwork <- function(plot, type = "tidytuesday", year = 2024, week = NULL, name = NULL, height = 10, width = 16) {
  
  # Required packages
  if (!require("ggplotify")) install.packages("ggplotify")
  require(ggplotify)  
  
  # Base paths setup
  base_paths <- list(
    tidytuesday = here::here("data_visualizations/TidyTuesday", year),
    swd = here::here("data_visualizations/SWD Challenge", year),
    standalone = here::here("projects/standalone_visualizations")
  )
  
  file_name <- switch(type,
                      tidytuesday = sprintf("tt_%d_%02d.png", year, week),
                      swd = sprintf("swd_%d_%02d.png", year, week),
                      standalone = paste0(name, ".png")
  )
  
  base_path <- base_paths[[type]]
  main_file <- file.path(base_path, file_name)
  thumb_file <- file.path(base_path, "thumbnails", file_name)
  
  # Save main plot
  plot_grob <- as.grob(plot)
  png(filename = main_file, width = width, height = height, units = "in", res = 320, type = "cairo")
  windowsFonts(Arial = windowsFont("Arial"))
  font_add("fa6-brands", here::here("fonts/6.4.2/Font Awesome 6 Brands-Regular-400.otf"))
  showtext::showtext_begin()
  showtext::showtext_opts(dpi = 320, regular.wt = 300, bold.wt = 800)
  grid::grid.draw(plot_grob)
  showtext::showtext_end()
  dev.off()
  
  # Create thumbnail using magick
  magick::image_read(main_file) |> 
    magick::image_resize("400") |> 
    magick::image_write(thumb_file)
}


# # TidyTuesday patchwork plot
# save_plot_patchwork(combined_plot, type = "tidytuesday", week = 48)
# 
# # SWD Challenge patchwork plot 
# save_plot_patchwork(combined_plot, type = "swd", week = 11)
# 
# # Standalone patchwork plot
# save_plot_patchwork(combined_plot, type = "standalone", name = "sa_2024-11-13")


