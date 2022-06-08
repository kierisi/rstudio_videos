# notes ----
# notes ----
#' final:       1956 - 2003
#' grand final: 2004 - 2022

# setup ----
library(tidyverse)
library(pointblank)
library(pins)
library(emo)  # may not need
library(gt)

# import ----
eurovision <- read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-05-17/eurovision.csv')

# initial eda ----
glimpse(eurovision)
scan_data(eurovision)
names(eurovision)

eurovision %>% 
  select(event, year, section:rank, winner) %>% 
  scan_data()

eurovision %>% 
  group_by(section) %>% 
  summarise(n = n())

finals <- c("final", "grand-final")
eurovision %>% 
  filter(section %in% finals) %>% 
  glimpse()

eurovision %>% 
  filter(section %in% finals) %>% 
  group_by(section, year) %>% 
  summarise(n = n()) %>% 
  print(n = Inf)

eurovision %>% 
  filter(section %in% finals) %>% 
  mutate(section = replace(section, section == "final", "grand-final")) %>% 
  count(section)
  
eurovision %>% 
  filter(section %in% finals) %>% 
  mutate(section = replace(section, section == "final", "grand-final")) %>% 
  select(event, year, section:rank, winner)

evdf <- eurovision %>% 
  filter(section %in% finals) %>% 
  mutate(section = replace(section, section == "final", "grand-final")) %>% 
  select(host_country, event, year, section:rank, winner)

evdf %>% glimpse()
evdf %>% 
  filter(rank <= 3) %>% 
  glimpse()

evdf %>% 
  filter(rank == 1) %>% 
  glimpse()

names(evdf)
evdf %>% 
  filter(rank <= 3) %>% 
  select(country_emoji, host_country, year, event, country_emoji, 
    artist_country, artist, song, rank, total_points) %>% 
  mutate(event = str_remove_all(event, pattern = "[:digit:]")) %>% 
  gt()

evdf %>% 
  filter(rank <= 3) %>% 
  select(country_emoji, host_country, year, event, country_emoji, 
    artist_country, artist, song, rank, total_points) %>% 
  mutate(
    event = str_remove_all(event, pattern = "[:digit:]"),
    country_emoji = str_remove_all(country_emoji, pattern = "[:\\::]")) %>% 
  gt()

evdf %>% 
  filter(rank <= 3) %>% 
  select(country_emoji, host_country, year, event, country_emoji, 
    artist_country, artist, song, rank, total_points) %>% 
  mutate(
    event = str_remove_all(event, pattern = "[:digit:]"),
    country_emoji = str_remove_all(country_emoji, pattern = "[:\\::]")) %>% 
  mutate(country_emoji = map_chr(., ~emo::ji)) %>% 
  gt()

evdf %>% 
  select(country_emoji)

board
flags <- board %>% 
  pin_read("flag_db")
head(flags)

evdf %>% 
  left_join(flags, by = c("artist_country" = "country")) %>% 
  glimpse()

evdf %>% 
  left_join(flags, by = c("artist_country" = "country")) %>% 
  gt() %>% 
  text_transform(
    #Apply a function to a column
    locations = cells_body(c(flag_URL)),
    fn = function(x) {
      web_image(
        url = x,
        height = 12
      )
    }
  )

#  re-factored tibble creation ----
library(tidyverse)
library(gt) 
library(pins)
library(here)

eurovision <- read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-05-17/eurovision.csv')

# returns a tibble with only the columns needed for gt table
finals <- c("final", "grand-final")
evdf <- eurovision %>% 
  filter(
    section %in% finals,
    rank <= 3) %>% 
  mutate(event = str_remove_all(event, pattern = "[:digit:]")) %>% 
  select(host_country, year, event, artist_country, artist, song, rank, total_points)

# returns a tibble with flag information for emoji embedding
board <- board_folder(here("2022-05-17_eurovision"))
flags <- board %>% 
  pin_read("flag_db")

# pins final tibble to board for use in creating gt table
eurovision_flag_df <- evdf %>% 
  left_join(flags, by = c("artist_country" = "country"))  

board %>% 
  pin_write(eurovision_flag_df)

df <- board %>% 
  pin_read("eurovision_flag_df")

glimpse(df)

df_gt <- df %>% 
  select(-code) %>% 
  select(year, host_country, event, flag_URL, artist_country, everything())

# making final table ----
## can't get flag in header ----
df_gt %>% 
  gt(
    groupname_col = "artist_country"
  ) %>% 
  text_transform(
    locations = cells_body(c(flag_URL)),
    fn = function(x) {
      web_image(url = x, height = 12)
    }
  ) 

df_gt %>% 
  group_by(flag_URL, artist_country) %>% 
  gt() %>% 
  text_transform(
    locations = cells_column_labels(flag_URL),
    fn = function(x) {
      web_image(url = x, height = 12)
    }
  )

## flag by host_country instead ----
glimpse(df_gt)

evfl_df <- evdf %>% 
  left_join(flags, by = c("host_country" = "country")) %>% 
  select(-code) %>% 
  select(flag_URL, host_country, event, year, artist_country, everything()) %>% 
  group_by(artist_country) %>% 
  arrange(desc(year), rank, .by_group = TRUE) %>% 
  ungroup()

write_csv(evfl_df, "evfl_df.csv")

board %>% 
  pin_write(evfl_df)

df <- board %>% 
  pin_read("evfl_df")

df %>% 
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

