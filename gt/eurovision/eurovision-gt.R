library(gt)
library(tidyverse)

eurovision <- readr::read_rds(file = "eurovision/eurovision.rds")

eurovision_svg <-
  readr::read_file("eurovision/eurovision_logo.svg") %>%
  gsub("^..xml.*?<svg", "<svg", .)

eurovision %>%
  filter(rank == 1 & winner == TRUE) %>%
  select(
    year, host_city, host_country, artist,
    country_emoji, artist_country, song, total_points
  ) %>%
  group_by(year) %>%
  arrange(desc(total_points)) %>%
  slice_head(n = 1) %>%
  ungroup() %>%
  arrange(desc(year)) %>%
  distinct() %>%
  filter(year >= 2012) %>%
  select(-total_points) %>%
  mutate(
    country_emoji =
      gsub("(^:flag_|:$)", "", country_emoji) %>%
      paste0(., ".svg"),
    host_country = toupper(host_country)
  ) %>%
  mutate(video = paste0(year, ".gif")) %>%
  gt(rowname_col = "year", id = "eurovision") %>%
  text_transform(
    locations = cells_body(columns = country_emoji),
    fn = function(x) {
      local_image(
        filename = paste0("eurovision/flags/4x3/", x)
      )
    }
  ) %>%
  text_transform(
    locations = cells_body(columns = video),
    fn = function(x) {
      local_image(
        filename = paste0("eurovision/videos/", x)
      )
    }
  ) %>%
  cols_move(columns = video, after = artist_country) %>%
  cols_merge(
    columns = c(host_city, host_country),
    pattern = "{1}<br>{2}"
  ) %>%
  cols_label(
    host_city = "location",
    country_emoji = "country",
    artist_country = "",
    video = "Song",
    song = ""
  ) %>%
  opt_all_caps() %>%
  opt_table_font(font = google_font("Karla")) %>%
  tab_options(
    table.background.color = "#6c4775",
    table.border.top.color = "#4f3455",
    table.border.top.width = px(3),
    table.border.bottom.color = "#4f3455",
    column_labels.border.top.style = "hidden"
  ) %>%
  tab_style(
    style = cell_text(size = "smaller"),
    locations = cells_body(columns = 2)
  ) %>%
  cols_width(
    1 ~ px(50),
    host_city ~ px(120),
    artist ~ px(170),
    country_emoji ~ px(45),
    artist_country ~ px(120),
    video ~ px(70),
    song ~ px(140)
  ) %>%
  tab_style(
    style = "overflow-x: visible;",
    locations = cells_column_labels(columns = country_emoji)
  ) %>%
  tab_style(
    style = cell_text(
      font = google_font("Square Peg"),
      size = "x-large"
    ),
    locations = cells_body(columns = song)
  ) %>%
  opt_vertical_padding(scale = 0.5) %>%
  tab_header(
    html(
      paste0(
        eurovision_svg, "<br>",
        "<div id='title-text'>",
        "The Winners Through the Years",
        "</div>"
      )
    )
  ) %>%
  tab_source_note(
    source_note = md(
      "Data available from the [*TidyTuesday* **GitHub** repository](
            https://github.com/rfordatascience/tidytuesday/tree/master/data/2022/2022-05-17)."
    )
  ) %>%
  opt_css(
    css = "
#eurovision svg {
  width: 600px;
}

#eurovision img {
  margin-top: 4px;
}

#eurovision a {
  color: white;
  text-underline-position: under;
}

#eurovision .gt_sourcenote {
  padding-top: 5px;
  padding-bottom: 6px;
}

#eurovision #title-text {
  font-size: x-large;
  padding-bottom: 10px;
  margin-top: -18px;
}

#eurovision .gt_heading {
    background: linear-gradient(295deg, #c6c3e1, #abe41e, #eaa7ea, #d9e8b5);
    background-size: 800% 800%;

    -webkit-animation: eurovision-backround 30s ease infinite;
    -moz-animation: eurovision-backround 30s ease infinite;
    animation: eurovision-backround 30s ease infinite;
}

@-webkit-keyframes eurovision-backround {
    0%{background-position:0% 50%}
    50%{background-position:100% 50%}
    100%{background-position:0% 50%}
}
@-moz-keyframes eurovision-backround {
    0%{background-position:0% 50%}
    50%{background-position:100% 50%}
    100%{background-position:0% 50%}
}
@keyframes eurovision-backround {
    0%{background-position:0% 50%}
    50%{background-position:100% 50%}
    100%{background-position:0% 50%}
}
"
  )

