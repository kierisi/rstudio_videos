# set up ----
library(tidyverse)
library(countrycode)
library(gt)
library(pins)
library(here)

# board initialization ----
eurovision_data <- board_folder(here("2022-05-17_eurovision"))

# data import ----
eurovision_raw <- read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-05-17/eurovision.csv')

# create tibble for {gt} ----
## eurovision dataset: create + pin ----
finals <- c("final", "grand-final")
eurovision_raw %>% 
  filter(
    section %in% finals,
    rank <= 3) %>% 
  mutate(event = str_remove_all(event, pattern = "[:digit:]")) %>% 
  select(host_country, year, event, artist_country, artist, song, rank, total_points) %>% 
  pin_write(board = eurovision_data, name = "eurovision_table_tibble")

## flag dataset: create + pin ----
read_csv(here("2022-05-17_eurovision", "Country_Flags.csv")) %>% 
  #Convert country names into 3-letter country codes
  mutate(
    code = countrycode(sourcevar = Country, 
      origin = "country.name", destination = "iso3c", warn = FALSE)) %>% 
  select(
    code, 
    flag_URL = ImageURL,
    country = Country) %>% 
  pin_write(board = eurovision_data, name = "flag_table")

## final dataset: merge eurovision with flags + pin ----
flags <- pin_read(
  board = eurovision_data,
  name = "flag_table"
)
eurovision_tbl <- pin_read(
  board = eurovision_data,
  name = "eurovision_table_tibble"
)

eurovision_tbl %>% 
  left_join(flags, by = c("host_country" = "country")) %>% 
  select(-code) %>% 
  select(flag_URL, host_country, event, year, artist_country, everything()) %>% 
  group_by(artist_country) %>% 
  arrange(desc(year), rank, .by_group = TRUE) %>% 
  ungroup() %>% 
  pin_write(board = eurovision_data, name = "eurovision_and_flags")

evfl <- pin_read(
  board = eurovision_data,
  name = "eurovision_and_flags"
)

glimpse(evfl)

# sorting out issue with pin boards
eurovision_tbl %>% 
  left_join(flags, by = c("host_country" = "country")) %>% 
  select(-code) %>% 
  select(flag_URL, host_country, event, year, artist_country, everything()) %>% 
  group_by(artist_country) %>% 
  arrange(desc(year), rank, .by_group = TRUE) %>% 
  ungroup() %>% 
  write_csv(here("2022-05-17_eurovision", "eurovision_and_flags.csv"))

# gt table time ----
evfl %>% 
  gt(
    # create grouping row by country ----
    groupname_col = "artist_country"
  ) %>% 
  tab_style(
    style = cell_text(
      font = google_font(name = "Source Serif 4"),
      weight = "bold"
    ),
    locations = cells_row_groups()
  ) %>% 
  
  # title and subtitle ----
tab_header(
  title = md(c("Euro", emo::ji("black heart"), "ision Song Contest Winners"))
) %>% 
  tab_style(
    style = cell_text(
      font = google_font(name = "Rock Salt"),
      weight = "bold"
    ),
    locations = cells_title(groups = "title")
  ) %>% 
  
  # clean up column labels ----
cols_label(
  flag_URL = "",
  host_country = "Host Country",
  event = "City",
  year = "Year",
  artist = "Artist",
  song = "Song",
  rank = "Rank",
  total_points = "Total points"
) %>% 
  tab_style(
    style = cell_text(
      font = google_font(name = "Montserrat"),
      size = "13px",
      align = "center",
      weight = "500"
    ),
    locations = cells_column_labels()
  ) %>% 
  
  # stylize cell text in body of table ----
tab_style(
  style = cell_text(
    font = google_font(name = "Montserrat"),
    size = "14px",
    align = "left"
  ),
  locations = cells_body()
) %>% 
  
  # bold artist names ----
tab_style(
  style = cell_text(
    weight = "600",
  ),
  locations = cells_body("artist")
) %>% 
  
  # italicize song names ----
tab_style(
  style = cell_text(
    style = "italic",
    weight = "500"
  ),
  locations = cells_body("song")
) %>% 
  
  # add host country flag images ----
text_transform(
  locations = cells_body(flag_URL),
  fn = function(x) {
    web_image(url = x, height = 12)
  }
) %>% 
  
  # add source notes with links ----
tab_source_note(
  source_note = md("Data from the [Eurovision Song Contest](https://eurovision.tv/), published by [TidyTuesday](https://github.com/rfordatascience/tidytuesday/blob/master/data/2022/2022-05-17/readme.md).")
) %>% 
  tab_style(
    style = cell_text(
      size = "10px",
      style = "italic",
      color = "#63666A"),
    locations = cells_source_notes()
  )

# pinned table ----
evfl %>% 
  gt(
    # create grouping row by country ----
    groupname_col = "artist_country"
  ) %>% 
  tab_style(
    style = cell_text(
      font = google_font(name = "Source Serif 4"),
      weight = "bold"
    ),
    locations = cells_row_groups()
  ) %>% 
  
  # title and subtitle ----
tab_header(
  title = md(c("Euro", emo::ji("black heart"), "ision Song Contest Winners"))
) %>% 
  tab_style(
    style = cell_text(
      font = google_font(name = "Rock Salt"),
      weight = "bold"
    ),
    locations = cells_title(groups = "title")
  ) %>% 
  
  # clean up column labels ----
cols_label(
  flag_URL = "",
  host_country = "Host Country",
  event = "City",
  year = "Year",
  artist = "Artist",
  song = "Song",
  rank = "Rank",
  total_points = "Total points"
) %>% 
  tab_style(
    style = cell_text(
      font = google_font(name = "Montserrat"),
      size = "13px",
      align = "center",
      weight = "500"
    ),
    locations = cells_column_labels()
  ) %>% 
  
  # stylize cell text in body of table ----
tab_style(
  style = cell_text(
    font = google_font(name = "Montserrat"),
    size = "14px",
    align = "left"
  ),
  locations = cells_body()
) %>% 
  
  # bold artist names ----
tab_style(
  style = cell_text(
    weight = "600",
  ),
  locations = cells_body("artist")
) %>% 
  
  # italicize song names ----
tab_style(
  style = cell_text(
    style = "italic",
    weight = "500"
  ),
  locations = cells_body("song")
) %>% 
  
  # add host country flag images ----
text_transform(
  locations = cells_body(flag_URL),
  fn = function(x) {
    web_image(url = x, height = 12)
  }
) %>% 
  
  # add source notes with links ----
tab_source_note(
  source_note = md("Data from the [Eurovision Song Contest](https://eurovision.tv/), published by [TidyTuesday](https://github.com/rfordatascience/tidytuesday/blob/master/data/2022/2022-05-17/readme.md).")
) %>% 
  tab_style(
    style = cell_text(
      size = "10px",
      style = "italic",
      color = "#63666A"),
    locations = cells_source_notes()
  ) %>% 
  pin_write(board = eurovision_data, name = "nonreactive_gt_table")

