# image_utils.R
# Reconciled from the single uploaded copy. Three fixes applied throughout:
#
# 1. ERROR-MASKING BUG (the actual root cause of the recurring
#    "cannot shut down device 1 (the null device)" error): the original
#    tryCatch()'s error handler called dev.off() unconditionally. If the
#    try block's OWN dev.off() had already run successfully (e.g. the
#    failure was actually in the thumbnail-generation step that runs
#    AFTER dev.off()), the catch handler's dev.off() call fails because
#    there's no device left to close — and because that failure happens
#    before stop("Error saving plot: ", e$message) is reached, the real
#    original error was never actually shown. Every "same error" we saw
#    while debugging this could have been masking a different underlying
#    problem each time. Fixed by checking grDevices::dev.cur() > 1 before
#    calling dev.off() in the handler, and by printing e$message FIRST.
#
# 2. FRAGILE DEVICE PIPELINE: ggplotify::as.grob() + manual png() ->
#    grid.draw() -> dev.off() replaced with a direct ggsave() call.
#    ggsave() supports patchwork objects natively, manages its own
#    device lifecycle correctly, and removes the ggplotify dependency
#    and the Windows-only windowsFonts()/windowsFont() call entirely.
#
# 3. DUPLICATION: save_plot_patchwork() and save_plot_patchwork_map()
#    were ~150 lines of near-identical code differing only in the `bg`
#    argument. Consolidated into one internal engine
#    (.save_patchwork_engine()) that both public functions call, so a
#    fix only needs to happen in one place going forward.

# Helper function for NULL coalescing ----
`%||%` <- function(x, y) if (is.null(x)) y else x

# Shared filename/path builder (used by all three save_* functions) ----
.build_plot_paths <- function(type, year, week, day, month, date, name, exercise) {
  
  year_num <- suppressWarnings(as.numeric(year))
  if (is.na(year_num)) stop("`year` could not be interpreted as numeric: ", year)
  
  base_paths <- list(
    tidytuesday          = here::here("data_visualizations/TidyTuesday", as.character(year)),
    swd                  = here::here("data_visualizations/SWD Challenge", as.character(year)),
    makeovermonday       = here::here("data_visualizations/MakeoverMonday", as.character(year)),
    `30daychartchallenge` = here::here("data_visualizations/30DayChartChallenge", as.character(year)),
    standalone           = here::here("projects/standalone_visualizations")
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
  
  # Construct file name based on type — year_num used everywhere %d appears
  file_name <- switch(
    type,
    tidytuesday = sprintf("tt_%d_%02d.png", year_num, week),
    makeovermonday = sprintf("mm_%d_%02d.png", year_num, week),
    `30daychartchallenge` = sprintf("30dcc_%d_%02d.png", year_num, day),
    swd = if (!is.null(exercise)) {
      sprintf("swd_%d_%02d-Ex_%04d.png", year_num, month %||% as.numeric(format(Sys.Date(), "%m")), exercise)
    } else {
      sprintf("swd_%d_%02d.png", year_num, month %||% as.numeric(format(Sys.Date(), "%m")))
    },
    standalone = if (!is.null(name)) {
      paste0(name, ".png")
    } else {
      sprintf("sa_%d-%02d-%02d.png",
              year_num,
              month %||% as.numeric(format(Sys.Date(), "%m")),
              date %||% as.numeric(format(Sys.Date(), "%d")))
    }
  )
  
  base_path <- base_paths[[type]]
  main_file <- file.path(base_path, file_name)
  thumb_file <- file.path(base_path, "thumbnails", file_name)
  
  dir.create(dirname(main_file), recursive = TRUE, showWarnings = FALSE)
  dir.create(dirname(thumb_file), recursive = TRUE, showWarnings = FALSE)
  
  list(main = main_file, thumb = thumb_file)
}

# Shared font registration (used by both patchwork-saving paths) ----
.register_plot_fonts <- function() {
  font_path <- here::here("fonts/6.6.0/Font Awesome 6 Brands-Regular-400.otf")
  if (!file.exists(font_path)) {
    warning("Font Awesome file not found at: ", font_path)
    return(invisible(FALSE))
  }
  if (!("fa6-brands" %in% sysfonts::font_families())) {
    sysfonts::font_add("fa6-brands", font_path)
  }
  invisible(TRUE)
}


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
  
  type <- match.arg(type)
  paths <- .build_plot_paths(type, year, week, day, month, date, name, exercise)
  
  ggsave(
    filename = paths$main,
    plot = plot,
    width = width,
    height = height,
    units = "in",
    dpi = 320
  )
  
  magick::image_read(paths$main) |>
    magick::image_resize("400") |>
    magick::image_write(paths$thumb)
  
  invisible(list(main = paths$main, thumbnail = paths$thumb))
}


# Internal engine shared by save_plot_patchwork() and save_plot_patchwork_map() ----
.save_patchwork_engine <- function(plot, type, year, week, day, month, date,
                                   name, exercise, height, width, bg) {
  
  type <- match.arg(type, c("tidytuesday", "swd", "standalone", "makeovermonday", "30daychartchallenge"))
  
  required_packages <- c("showtext", "sysfonts", "magick")
  missing_packages <- required_packages[!sapply(required_packages, requireNamespace, quietly = TRUE)]
  if (length(missing_packages) > 0) {
    install.packages(missing_packages)
  }
  
  paths <- .build_plot_paths(type, year, week, day, month, date, name, exercise)
  
  fonts_ok <- tryCatch(
    .register_plot_fonts(),
    error = function(e) {
      warning("Error loading fonts: ", e$message)
      FALSE
    }
  )
  
  result <- tryCatch({
    showtext::showtext_auto()
    showtext::showtext_opts(dpi = 320)
    
    # ggsave() handles patchwork objects natively — no ggplotify::as.grob(),
    # no manual png()/grid.draw()/dev.off(), no Windows-only windowsFonts().
    ggsave(
      filename = paths$main,
      plot = plot,
      width = width,
      height = height,
      units = "in",
      dpi = 320,
      bg = bg %||% "white"
    )
    
    showtext::showtext_auto(FALSE)
    
    magick::image_read(paths$main) |>
      magick::image_resize("400") |>
      magick::image_write(paths$thumb)
    
    TRUE
  }, error = function(e) {
    # Print the REAL error before attempting any cleanup — this is the
    # fix for the masking bug described at the top of this file.
    message("Original error while saving plot: ", conditionMessage(e))
    # Only close a device if one is actually open (dev.cur() > 1 means
    # something other than the null device is current). Calling dev.off()
    # unconditionally here was what masked the real error previously.
    if (grDevices::dev.cur() > 1) {
      grDevices::dev.off()
    }
    showtext::showtext_auto(FALSE)
    stop("Error saving plot: ", conditionMessage(e), call. = FALSE)
  })
  
  invisible(list(
    main = paths$main,
    thumbnail = paths$thumb,
    type = type,
    date_saved = Sys.time()
  ))
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
  .save_patchwork_engine(
    plot = plot, type = type, year = year, week = week, day = day,
    month = month, date = date, name = name, exercise = exercise,
    height = height, width = width, bg = "white"
  )
}

# Saving patchwork images with a custom background (e.g. maps) ----
save_plot_patchwork_map <- function(plot,
                                    type = c("tidytuesday", "swd", "standalone", "makeovermonday", "30daychartchallenge"),
                                    year = format(Sys.Date(), "%Y"),
                                    week = NULL,
                                    day = NULL,
                                    month = NULL,
                                    date = NULL,
                                    name = NULL,
                                    exercise = NULL,
                                    height = NULL,
                                    width = NULL,
                                    bg = NULL) {
  .save_patchwork_engine(
    plot = plot, type = type, year = year, week = week, day = day,
    month = month, date = date, name = name, exercise = exercise,
    height = height, width = width, bg = bg %||% "white"
  )
}

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
# # Patchwork plot (same call signature as before — no script changes needed)
# save_plot_patchwork(
#   plot = p,
#   type = "tidytuesday",
#   year = 2026,
#   week = 30,
#   width = 11,
#   height = 6.5
# )
#
# # Patchwork plot with a custom background (e.g. maps)
# save_plot_patchwork_map(
#   plot = p,
#   type = "tidytuesday",
#   year = 2026,
#   week = 30,
#   width = 11,
#   height = 6.5,
#   bg = "aliceblue"
# )