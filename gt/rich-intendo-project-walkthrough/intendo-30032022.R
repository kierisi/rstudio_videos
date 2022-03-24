library(gt)
library(intendo)
library(tidyverse)
library(lubridate)

# Get a `small`-sized version of the `all_revenue` table
all_the_revenue <- intendo::all_revenue()

# Make a vector for subsetting countries to consider
countries_considered <- c("US", "GB", "DE", "CH", "NO", "CA", "IN", "ES")

# Get the month names as a list (for later)
month_names <- as.list(substr(month.name, 1, 3))
names(month_names) <- as.character(1:12)

# Make the gt table! Yeah!
all_the_revenue %>%
  mutate(month = lubridate::month(time)) %>%
  filter(item_type == "iap") %>%
  left_join(
    gt::countrypops %>% select(country_name, country_code_2) %>%
      distinct(),
    by = c("country" = "country_name")
  ) %>%
  mutate(iap_type = gsub("[0-9]", "", item_name)) %>%
  select(
    country = country_code_2,
    month, iap_type, item_revenue
  ) %>%
  filter(country %in% countries_considered) %>%
  group_by(month, iap_type, country) %>%
  summarize(
    total_rev = sum(item_revenue),
    .groups = "drop"
  ) %>%
  arrange(month, country, iap_type) %>%
  pivot_wider(names_from = month, values_from = total_rev) %>%
  gt(rowname_col = "iap_type", groupname_col = "country") %>%
  row_group_order(groups = countries_considered) %>%
  fmt_currency(columns = everything(), use_subunits = FALSE) %>%
  fmt_missing(columns = everything()) %>%
  tab_options(row_group.as_column = TRUE) %>%
  cols_label(.list = month_names) %>%
  tab_spanner(label = "2015", columns = everything()) %>%
  tab_spanner(label = "v1.0", columns = c(`1`, `2`, `3`)) %>%
  tab_spanner(label = "v1.2", columns = as.character(4:12)) %>%
  tab_header(
    title = "Revenue Amounts by Country and IAP Type",
    subtitle = md("Considers **Top 8** Countries by Overall Revenue<br /><br />")
  ) %>%
  opt_align_table_header(align = "left") %>%
  tab_stubhead(label = md("Region and <br />IAP Type")) %>%
  tab_style(
    style = cell_text(size = "smaller"),
    locations = cells_body()
  ) %>%
  data_color(
    columns = everything(), 
    colors = scales::col_numeric(
      palette = c("#66FF0000", "#66FF00FF"),
      alpha = TRUE,
      na.color = "#00BFFF66",
      domain = NULL
    )
  ) %>%
  tab_source_note(
    source_note = md(
      "All revenue figures obtained from daily retrievals from the *App Annie* API."
    )) %>%
  tab_source_note(
    source_note = md(
      "All reporting shown here was created by the **_Central DS Team_**."
    )) %>%
  tab_footnote(
    footnote = "These figures are $0 or less (due to refunds).",
    locations = list(
      cells_body(columns = `1`, rows = is.na(`1`)),
      cells_body(columns = `3`, rows = is.na(`3`))
    )
  ) %>%
  tab_footnote(
    footnote = md("A surprising jump in **gems** revenue from the previous month."),
    locations = cells_body(columns = `12`, rows = round(`12`) == 6278)
  ) %>%
  tab_footnote(
    footnote = md("The `v1.0` build used the first version of the offer agent SDK."),
    locations = cells_column_spanners(spanners = "v1.0")
  ) %>%
  tab_footnote(
    footnote = md("The `v1.2` build added more missions and had an updated offer agent SDK."),
    locations = cells_column_spanners(spanners = "v1.2")
  ) %>%
  cols_width(
    1 ~ px(50),
    everything() ~ px(75)
  ) %>%
  opt_horizontal_padding(scale = 2.5) %>%
  opt_vertical_padding(scale = 0.5) %>%
  tab_style(
    style = list(
      cell_text(color = "snow", weight = "bold", align = "right"),
      cell_fill(color = "SlateGray", alpha = 0.75)
    ),
    locations = cells_row_groups()
  ) %>%
  opt_all_caps() %>%
  opt_table_font(font = google_font(name = "Karla")) %>%
  tab_options(
    table.background.color = adjust_luminance("LemonChiffon", steps = 2),
    heading.background.color = "LemonChiffon",
    footnotes.background.color = adjust_luminance("LemonChiffon", steps = -0.5),
    source_notes.background.color = adjust_luminance("LemonChiffon", steps = -0.5),
    table.border.top.width = px(4),
    table.border.bottom.width = px(4),
    footnotes.multiline = FALSE,
    table_body.hlines.style = "dotted",
    heading.title.font.size = px(30),
    heading.subtitle.font.size = px(18)
  )