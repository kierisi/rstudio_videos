# set up ----
library(tidyverse)
library(gt)

data <- read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-04-05/news_orgs.csv')

ca_data <- data %>% 
  filter(state == "CA")

ca_data %>% 
  select(city, publication_name, tax_status_current,  year_founded, total_employees) %>% 
  mutate(
    year_founded = as.numeric(year_founded),
    years_in_operation = 2022 - year_founded,
    total_employees = as.factor(total_employees)) %>% 
  group_by(tax_status_current) %>% 
  arrange(
    city, publication_name, 
    desc(years_in_operation), desc(year_founded), desc(total_employees)) %>%
  gt() %>% 
  tab_header(
    title = md("**Tax Status of California Newspapers**"),
    subtitle = md("_Using data from [Project Oasis](https://www.projectnewsoasis.com/publications) by way of [Data is Plural](https://www.data-is-plural.com/archive/2022-03-30-edition/), as part of [TidyTuesday](https://github.com/rfordatascience/tidytuesday/blob/master/data/2022/2022-04-05/readme.md)_")
  ) %>% 
  cols_label(
    city = md("**City**"),
    publication_name = md("**Publication**"),
    year_founded = md("**Founded**"),
    years_in_operation = md("**Years of Operation**"),
    total_employees = md("**Employees**")) %>% 
  cols_move_to_end(total_employees) %>% 
  cols_align(
    align = "center",
    columns = c(year_founded, years_in_operation, total_employees)
  ) %>% 
  fmt_missing(
    columns = everything(),
    rows = everything(),
    missing_text = "---"
  ) %>% 
  tab_footnote(
    footnote = "--- indicates missing data",
    locations = cells_column_labels(c(city, total_employees))
  ) %>% 
  opt_table_font(
    font = google_font("DotGothic16")
  ) %>% 
  data_color(
    columns = years_in_operation,
    colors = scales::col_bin(
      palette = c("#ff4040", "#ffb8b8", "#f9ff9b", "#44dbdf", "#b5ff9f"),
      domain = c(0, 45),
      na.color = "#e1faf0"
    )
  ) %>% 
  data_color(
    columns = year_founded,
    colors = scales::col_bin(
      palette = c("#ff4040", "#ffb8b8", "#f9ff9b", "#44dbdf", "#b5ff9f"),
      domain = c(1977, 2022),
      na.color = "#e1faf0"
    )
  ) %>% 
  data_color(
    columns = total_employees,
    colors = scales::col_factor(
      palette = c("#ff4040", "#ffb8b8", "#f9ff9b", "#44dbdf", "#b5ff9f"),
      domain = NULL,
      na.color = "#e1faf0"
    )
  ) %>% 
  tab_style(
    style = cell_fill(color = "#ff8b83"),
    locations = cells_column_labels()
  ) %>% 
  tab_style(
    style = cell_fill(color = "#84e9c5", alpha = 0.25),
    locations = cells_body(
      columns = c(city, publication_name)
    )
  ) %>% 
  tab_style(
    style = list(
      cell_fill(color = "#44dbdf"),
      cell_text(weight = "bold")
    ),
    locations = cells_row_groups()
  ) %>% 
  tab_options(
    heading.background.color = "#84e9c5"
  )



