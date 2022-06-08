library(htmltools)

source("news-orgs-gt.R")

table_gt_layout <-
  htmltools::div(
    # This div has the header and the column labels
    htmltools::tags$div(
      news_orgs_gt_topp %>%
        tab_header(
          title = md("Select Publications from the TidyTuesday **Publications List** Dataset"),
          subtitle = "Publications are grouped by region and sorted by state"
        ) %>%
        opt_align_table_header(align = "left") %>%
        as_raw_html()
    ),
    # This div has the table body (hiding the column labels)
    htmltools::tags$div(
      news_orgs_gt_table %>%
        tab_options(column_labels.hidden = TRUE) %>%
        as_raw_html(inline_css = FALSE),
      style = htmltools::css(
        `overflow-x` = "hidden",
        `overflow-y` = "auto",
        height = "500px"
      )
    ),
    # This div has the table footer (reuses the heading code and adds source notes)
    htmltools::tags$div(
      news_orgs_gt_topp %>%
        tab_options(column_labels.hidden = TRUE) %>%
        tab_source_note(
          source_note = md(
            "Data available from the [*TidyTuesday* **GitHub** repository](
            https://github.com/rfordatascience/tidytuesday/tree/master/data/2022/2022-04-05)."
          )
        ) %>%
        tab_source_note(
          source_note = "*Includes S corps, partnerships, 501c(3) entities, and sole proprietors."
        ) %>%
        as_raw_html(inline_css = FALSE)
    ),
    style = htmltools::css(
      `padding` = "10px",
      border = "solid",
      `border-color` = gt::adjust_luminance("DarkTurquoise", steps = -0.5),
      `border-width` = "2px",
      `background-color` = gt::adjust_luminance("Snow", steps = -1)
    )
  )

# Preview the finalized table
table_gt_layout %>% html_print()
