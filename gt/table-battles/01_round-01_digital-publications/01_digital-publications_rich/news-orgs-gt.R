library(gt)
library(tidyverse)
library(curl)

# Table Reading from a separate .R file (yields `news_orgs` tibble)
source("news-orgs-read.R")

#
# Region Groupings (for grouping within the gt table)
#

west <-
  c(
    "WA", "OR", "CA", "NV", "ID", "MT", "WY",
    "CO", "UT", "AZ", "NM", "AK", "HI"
  )

midwest <-
  c(
    "ND", "SD", "NE", "KS", "MN", "IA", "MO",
    "WI", "IL", "MI", "IN", "OH"
  )

south <-
  c(
    "TX", "OK", "AR", "LA", "MS", "TN", "AL",
    "GA", "FL", "SC", "NC", "KY", "WV", "VA",
    "MD", "DC", "DE", "PR"
  )

northeast <- c("PA", "NY", "NJ", "VT", "MA", "CT", "RI", "NH", "ME")

canada <-
  c(
    "BC", "AB", "SK", "MB", "ON", "QC", "NB",
    "NS", "NT", "NL", "NU", "PE", "YT"
  )

all_states <- fct_inorder(c(west, midwest, south, northeast, canada))


# URL checking function
is_valid_url <- function(url) {

  # Perform check
  fetched <-
    suppressWarnings(
      try(curl::curl_fetch_memory(url = url), silent = TRUE)
    )

  res <-
    !inherits(fetched, "try-error") &&
    !is.null(fetched$status_code) &&
    fetched$status_code == "200"

  cat(paste0("The URL `", url, "` is ", ifelse(res, "", "NOT "), "valid.\n"))

  invisible(res)
}

#
# Initial filtering and checking of URLs (This takes awhile! Made an RDS checkpoint here)
#

# news_orgs_filtered <-
#   news_orgs %>%
#   filter(!is.na(url)) %>%
#   filter(!is.na(state)) %>%
#   filter(!grepl("facebook", url)) %>%
#   filter(vapply(url, FUN.VALUE = logical(1), USE.NAMES = TRUE, FUN = is_valid_url))
#
# readr::write_rds(news_orgs_filtered, "news_orgs_filtered.rds")
news_orgs_filtered <- readr::read_rds("news_orgs_filtered.rds")

news_orgs_summary <-
  news_orgs_filtered %>%
  mutate(
    url = gsub("https?://", "", url),
    url = tolower(url),
    url = gsub("^www.", "", url),
    url = gsub("(default.*|\\?.*|/about.*|/contact|/content.*|/main|/wordpress|/donate)", "", url),
    url = gsub("/$", "", url)
  ) %>%
  select(
    publication_name, url, state, city,
    year_founded, tax_status_current, primary_language
  ) %>%
  arrange(factor(state, levels = all_states), city) %>%
  mutate(region = case_when(
    state %in% west ~ "West",
    state %in% midwest ~ "Midwest",
    state %in% northeast ~ "Northeast",
    state %in% south ~ "South",
    state %in% canada ~ "Canada"
  )) %>%
  mutate(tax_status_current = case_when(
    tax_status_current == "For Profit" ~ "FP",
    tax_status_current == "Not for Profit" ~ "NFP",
    tax_status_current == "LLC" ~ "LLC",
    grepl("benefit", tax_status_current) ~ "PBC",
    TRUE ~ "Other"
  )) %>%
  mutate(primary_language = case_when(
    is.na(primary_language) ~ "EN",
    primary_language == "English" ~ "EN",
    primary_language == "Spanish" ~ "ES",
    grepl("English", primary_language) &
      grepl("Spanish", primary_language) ~ "EN/ES",
    TRUE ~ primary_language
  ))

# The gt table
news_orgs_gt_table <-
  news_orgs_summary %>%
  gt(groupname_col = "region", id = "pubs-table") %>%
  cols_merge(
    columns = c(publication_name, url),
    pattern = "{1}<br><a href='https://{2}' target='_blank' style='font-size:smaller'>{2}</span>"
  ) %>%
  cols_label(
    publication_name = "Publication",
    primary_language = "Lang",
    tax_status_current = md("Type of<br>Org."),
    year_founded = md("Started<br>in")
  ) %>%
  cols_width(
    publication_name ~ px(430),
    city ~ px(180),
    state ~ px(50),
    primary_language ~ px(60),
    everything() ~ px(75)) %>%
  opt_all_caps() %>%
  opt_table_font(font = google_font(name = "IBM Plex Sans")) %>%
  cols_align(
    align = "center",
    columns = c(state, primary_language, year_founded, tax_status_current)
  ) %>%
  text_transform(
    locations = cells_body(
      columns = tax_status_current,
      rows = tax_status_current == "Other"
    ),
    fn = function(x) paste0(x, "*")
  ) %>%
  tab_style(
    style = cell_text(size = "smaller"),
    locations = cells_body(columns = city)
  ) %>%
  fmt_missing(columns = city, missing_text = "") %>%
  opt_footnote_marks(marks = "standard") %>%
  tab_style(
    style = cell_text(font = google_font(name = "IBM Plex Mono")),
    locations = cells_body(year_founded)
  ) %>%
  tab_style(
    style = cell_fill(color = "oldlace"),
    locations = cells_row_groups()
  ) %>%
  tab_style(
    style = "letter-spacing:-0.6px;",
    locations = cells_body(columns = publication_name, rows = nchar(publication_name) > 30)
  ) %>%
  tab_style(
    style = "letter-spacing:-0.8px;",
    locations = cells_body(columns = url, rows = nchar(url) > 75)
  ) %>%
  tab_options(
    table.background.color = "gray99",
    table_body.hlines.style = "dotted",
    row_group.border.bottom.style = "hidden",
    column_labels.background.color = "mintcream"
  ) %>%
  opt_css(
    css = "
    #pubs-table a {
      color: DarkCyan;
      text-underline-position: under;
    }
    "
  )

# The gt table top part
news_orgs_gt_topp <-
  news_orgs_summary[0, ] %>%
  gt(groupname_col = "region", id = "pubs-table") %>%
  cols_merge(
    columns = c(publication_name, url),
    pattern = "{1}<br><a href='{2}' style='font-size:smaller'>{2}</span>"
  ) %>%
  cols_label(
    publication_name = "Publication",
    primary_language = "Lang",
    tax_status_current = md("Type of<br>Org."),
    year_founded = md("Started<br>in")
  ) %>%
  cols_width(
    publication_name ~ px(430),
    city ~ px(180),
    state ~ px(50),
    primary_language ~ px(60),
    everything() ~ px(75)) %>%
  opt_all_caps() %>%
  opt_table_font(font = google_font(name = "IBM Plex Sans")) %>%
  cols_align(
    align = "center",
    columns = c(state, primary_language, year_founded, tax_status_current)
  )


