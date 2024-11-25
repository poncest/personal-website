library(showtext)
library(here)

setup_fonts <- function() {
  font_add("fa6-brands", here::here("fonts/6.6.0/Font Awesome 6 Brands-Regular-400.otf"))
  font_add_google("Oswald", regular.wt = 400, family = "title")
  font_add_google("Source Sans Pro", family = "text")  
  font_add_google("Roboto Mono", family = "numbers")   
  font_add_google("Noto Sans", regular.wt = 400, family = "caption")
  showtext_auto(enable = TRUE)
}

renv::status()
renv::snapshot()