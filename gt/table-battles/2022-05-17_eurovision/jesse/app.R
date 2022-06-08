library(shiny)
library(dplyr)
library(pins)
library(gt)

## data import ----
# fix issue with boards
# eurovision_data <- board_local(here::here("2022-05-17_eurovision"))
# evfl <- pin_read(
#   board = eurovision_data,
#   name = "eurovision_and_flags"
# )
# 
# gt_table_pin <- pin_read(
#   board = eurovision_data,
#   name = "nonreactive_gt_table"
# )
evfl <- readr::read_csv(
  here::here("2022-05-17_eurovision", "eurovision_and_flags.csv"))

# ui ----
ui <- fluidPage(
  selectizeInput(
    inputId = "artist_country",
    label = "Choose a country",
    choices = evfl$artist_country,
    multiple = TRUE
  ),
  
  selectizeInput(
    inputId = "rank",
    label = "Choose a rank from 1 - 3",
    choices = 1:3,
    multiple = TRUE,
    selected = 1:3
  ),
  
  gt_output(outputId = "table")
)

# server ----
server <- function(input, output, session) {
  reactive_evfl <- reactive({
    evfl %>%
      filter(
        artist_country %in% input$artist_country,
        rank %in% input$rank)
  })
  
  # reactive_evfl <- reactive({
  #   evfl %>% 
  #     filter(artist_country %in% input$artist_country)
  # })
  
  output$table <- render_gt({
    reactive_evfl() %>% 
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
  })
  #output$table <- render_gt(gt_table)
}

shinyApp(ui, server)