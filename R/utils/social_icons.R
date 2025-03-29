## social_icons.R

# Social media icons configuration
get_social_icons <- function() {
    list(
        linkedin = str_glue("<span style='font-family:fa6-brands'>&#xf08c;</span>"),
        github   = str_glue("<span style='font-family:fa6-brands'>&#xf09b;</span>"),
        bluesky  = str_glue("<span style='font-family:fa6-brands'>&#xe671;</span>")      # Current
        # bluesky = str_glue("<span style='font-family:fa6-brands'>\ue671</span>")       # Alternative
    )
}

# Create social media caption (tidytuesday)
create_social_caption <- function(tt_year, tt_week, source_text) {
    icons <- get_social_icons()
    
    tt_text <- str_glue("#TidyTuesday: {tt_year} Week {tt_week} &bull; Source: {source_text}<br>")
    social_text <- str_glue("{icons$linkedin} stevenponce &bull; {icons$bluesky} sponce1 &bull; {icons$github} poncest &bull; #rstats #ggplot2")
    
    str_glue("{tt_text} {social_text}")
}

# Create SWD social media caption
create_swd_caption <- function(year, month, source_text) {
  # Get icons from existing function
  icons <- get_social_icons()
  
  # Create SWD challenge header
  swd_text <- str_glue("#SWDchallenge: {month} {year} &bull; Source: {source_text}<br>")
  
  # Create social media handles
  social_text <- str_glue("{icons$linkedin} stevenponce &bull; {icons$bluesky} sponce1 &bull; {icons$github} poncest &bull; #rstats #ggplot2")
  
  # Combine texts
  str_glue("{swd_text} {social_text}")
}

# Create SWD (Exercise) social media caption
create_swd_exe_caption <- function(year, month, source_text) {
  # Get icons from existing function
  icons <- get_social_icons()
  
  # Create SWD challenge header
  swd_text <- str_glue("#SWDchallenge: {month} {year} Exercise &bull; Source: {source_text}<br>")
  
  # Create social media handles
  social_text <- str_glue("{icons$linkedin} stevenponce &bull; {icons$bluesky} sponce1 &bull; {icons$github} poncest &bull; #rstats #ggplot2")
  
  # Combine texts
  str_glue("{swd_text} {social_text}")
}

# Create social media caption (MakeoverMonday)
create_mm_caption <- function(mm_year, mm_week, source_text) {
  icons <- get_social_icons()
  
  mm_text <- str_glue("#MakeoverMonday: {mm_year} Week {mm_week} &bull; Source: {source_text}<br>")
  social_text <- str_glue("{icons$linkedin} stevenponce &bull; {icons$bluesky} sponce1 &bull; {icons$github} poncest &bull; #rstats #ggplot2")
  
  str_glue("{mm_text} {social_text}")
}

# Create social media caption (#30DayChartChallenge)
create_dcc_caption <- function(dcc_year, dcc_day, source_text) {
  icons <- get_social_icons()
  
  dcc_text <- str_glue("#30DayChartChallenge: {dcc_year} Day {dcc_day} &bull; Source: {source_text}<br>")
  social_text <- str_glue("{icons$linkedin} stevenponce &bull; {icons$bluesky} sponce1 &bull; {icons$github} poncest &bull; #rstats #ggplot2")
  
  str_glue("{dcc_text} {social_text}")
}

# # Example usage:
# caption_text <- create_swd_caption(
#   year = 2025,
#   month = "Jan",
#   source_text = "Source: Scrapped from goodreads & librarthing"
# )