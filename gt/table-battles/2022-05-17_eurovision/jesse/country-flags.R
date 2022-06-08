#' modified from Dr. Liam Bailey's blog post on beautiful tables with {gt}
#' https://www.liamdbailey.com/post/making-beautiful-tables-with-gt/

library(countrycode)
library(pins)
library(readr)
library(here)

flag_db <- read_csv(here("2022-05-17_eurovision", "Country_Flags.csv")) %>% 
  #Convert country names into 3-letter country codes
  mutate(
    code = countrycode(sourcevar = Country, 
      origin = "country.name", destination = "iso3c", warn = FALSE)) %>% 
  select(
    code, 
    flag_URL = ImageURL,
    country = Country)

glimpse(flag_db)

board <- board_folder(here("2022-05-17_eurovision"))
board %>% 
  pin_write(flag_db)

# flag_data <- emissions_data %>% 
#   left_join(flag_db, by = "Code") %>% 
#   select(flag_URL, Entity, everything())

emissions_table %>% 
  gt::text_transform(
    #Apply a function to a column
    locations = cells_body(c(flag_URL)),
    fn = function(x) {
      #Return an image of set dimensions
      web_image(
        url = x,
        height = 12
      )
    }
  ) %>% 
  #Hide column header flag_URL and reduce width
  cols_width(c(flag_URL) ~ px(30)) %>% 
  cols_label(flag_URL = "")