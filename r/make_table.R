# default settings ----------
today <- as.Date("2021-05-16")
repo  <- here("data", today)

# pull state abbreviations ----------
abrev <- vroom(paste0(repo, "/covid19india_data.csv"), col_types = cols()) %>%
  group_by(State) %>%
  filter(Date == max(Date) & State != "un") %>%
  ungroup() %>%
  top_n(20, Cases)
abrev = abrev$State

# pull national prediction estimates ----------
tmp <- read_tsv(paste0(repo, "/prediction_", "tt", ".txt"), col_types = cols()) %>%
  select(
    state, date, section, pred, value = mean
  ) %>%
  pivot_wider(
    names_from = "section",
    values_from = "value",
    id_cols = c("date", "pred", "state")
  ) %>%
  rename(
    case_daily_reported = positive_daily_reported
  ) %>%
  mutate(
    death_daily_unreported = death_unreported - lag(death_unreported),
    case_daily_unreported = unreported_daily 
  ) %>%
  select(
    state, date, pred, case_daily_reported, case_daily_unreported, death_daily_reported, death_daily_unreported
  ) %>%
  filter(pred == 1) %>% 
  mutate(wk = ifelse(date == min(date) + 7, "one", ""),
         wk = ifelse(date == min(date) + 14, "two", wk),
         wk = ifelse(date == min(date) + 21, "three", wk),
         wk = ifelse(date == min(date) + 28, "four", wk)) %>% 
  filter(wk == "one" | wk == "two" | wk == "three" | wk == "four") %>% 
  pivot_wider(id_cols = c(state), names_from = wk,
              values_from = c(case_daily_reported, case_daily_unreported,
                              death_daily_reported, death_daily_unreported))

for (state in abrev) {
  if(state == "tt") {next}
  
  tmp2 <- read_tsv(paste0(repo, "/prediction_", state, ".txt"), col_types = cols()) %>%
    select(
      state, date, section, pred, value = mean
    ) %>%
    pivot_wider(
      names_from = "section",
      values_from = "value",
      id_cols = c("date", "pred", "state")
    ) %>%
    rename(
      case_daily_reported = positive_daily_reported
    ) %>%
    mutate(
      death_daily_unreported = death_unreported - lag(death_unreported),
      case_daily_unreported = unreported_daily 
    ) %>%
    select(
      state, date, pred, case_daily_reported, case_daily_unreported, death_daily_reported, death_daily_unreported
    ) %>%
    filter(pred == 1) %>% 
    mutate(wk = ifelse(date == min(date) + 7, "one", ""),
           wk = ifelse(date == min(date) + 14, "two", wk),
           wk = ifelse(date == min(date) + 21, "three", wk),
           wk = ifelse(date == min(date) + 28, "four", wk)) %>% 
    filter(wk == "one" | wk == "two" | wk == "three" | wk == "four") %>% 
    pivot_wider(id_cols = c(state), names_from = wk,
                values_from = c(case_daily_reported, case_daily_unreported,
                                death_daily_reported, death_daily_unreported))
  
  tmp = bind_rows(tmp, tmp2)
}

tmp %>% 
  gt() %>% 
  tab_style(
    style     = cell_text(size = px(14), font = "helvetica"),
    locations = cells_body()
  ) %>%
  tab_style(
    style     = cell_text(weight = "bold"),
    locations = cells_body(vars(state))
  ) %>%
  # format column names
  tab_style(
    style = cell_text(
      size      = px(12),
      color     = "#999",
      font      = "helvetica",
      transform = "uppercase"
    ),
    locations = cells_column_labels(everything())
  ) %>%
  # random formatting
  tab_options(
    column_labels.border.top.style    = "none",
    column_labels.border.bottom.width = 1,
    column_labels.border.bottom.color = "#334422",
    table_body.border.bottom.color    = "#0000001A",
    data_row.padding                  = px(4)
  ) %>%
  # column widths
  cols_width(
    vars(state) ~ px(150),
    everything() ~ px(100)
  ) %>%
  cols_align(
    align   = "center",
    columns = everything()
  ) %>%
  # title
  tab_header(
    title    = md("**Assessing COVID-19 Underreporting**"),
    subtitle = glue("as of {format(today, '%B %e')}")
  ) %>%
  # caption
  tab_source_note(
    source_note = md(glue(
      "**\uA9 COV-IND-19 Study Group**<br>**Source data:** covid19india.org<br>
      **Notes:** Daily reported and unreported cases and deaths from SEIR as of {format(today, '%B %e')}."
    ))
  ) %>% 
  # add and format column spanners
  tab_spanner(
    label   = "1 weeks out",
    columns = vars(case_daily_reported_one, case_daily_unreported_one, death_daily_reported_one,
                   death_daily_unreported_one)
  ) %>%
  tab_spanner(
    label   = "2 weeks out",
    columns = vars(case_daily_reported_two, case_daily_unreported_two, death_daily_reported_two,
                   death_daily_unreported_two)
  ) %>%
  tab_spanner(
    label   = "3 weeks out",
    columns = vars(case_daily_reported_three, case_daily_unreported_three, death_daily_reported_three,
                   death_daily_unreported_three)
  ) %>%
  tab_spanner(
    label   = "4 weeks out",
    columns = vars(case_daily_reported_four, case_daily_unreported_four, death_daily_reported_four,
                   death_daily_unreported_four)
  ) %>% 
  cols_move_to_start(vars(state)) %>%
  tab_style(
    style = cell_text(
      size      = px(14),
      color     = "#999",
      font      = "helvetica",
      transform = "uppercase"
    ),
    locations = cells_column_spanners(spanners = c("1 weeks out", "2 weeks out", "3 weeks out", "4 weeks out"))
  ) %>%
  # format numbers
  fmt_number(
    columns  = vars(case_daily_reported_one, case_daily_unreported_one, death_daily_reported_one,
                    death_daily_unreported_one, case_daily_reported_two, case_daily_unreported_two, death_daily_reported_two,
                    death_daily_unreported_two, case_daily_reported_three, case_daily_unreported_three, death_daily_reported_three,
                    death_daily_unreported_three, case_daily_reported_four, case_daily_unreported_four, death_daily_reported_four,
                    death_daily_unreported_four),
    decimals = 0
  ) %>%
  # adjust title font
  tab_style(
    style     = list(cell_text(font = "helvetica", size = px(24))),
    locations = list(cells_title(groups = "title"))
  ) %>%
  # adjust subtitle font
  tab_style(
    style     = list(cell_text(font = "helvetica", size = px(18))),
    locations = list(cells_title(groups = "subtitle"))
  ) %>%
  # highlight national estimate
  tab_style(
    style = cell_fill(color = "#fcf8d4"),
    locations = cells_body(
      rows = state == "India")
  ) %>% 
  tab_style(
    style = cell_borders(sides = "left"),
    locations = cells_body(columns = vars(case_daily_reported_one, case_daily_reported_two, case_daily_reported_three,
                                          case_daily_reported_four))
  ) %>% 
  tab_style(
    style = cell_borders(sides = "left"),
    locations = cells_column_spanners(vars("1 weeks out", "2 weeks out", "3 weeks out", "4 weeks out"))
  ) %>%
  tab_style(
    style = cell_borders(sides = "left"),
    locations = cells_column_labels(vars(case_daily_reported_one, case_daily_reported_two, case_daily_reported_three, case_daily_reported_four))
  ) %>%
  cols_label(
    case_daily_reported_one = "Daily reported cases",
    case_daily_unreported_one = "Daily unreported cases", 
    death_daily_reported_one = "Daily reported deaths",
    death_daily_unreported_one = "Daily unreported deaths",
    case_daily_reported_two = "Daily reported cases",
    case_daily_unreported_two = "Daily unreported cases", 
    death_daily_reported_two = "Daily reported deaths",
    death_daily_unreported_two = "Daily unreported deaths", 
    case_daily_reported_three = "Daily reported cases", 
    case_daily_unreported_three = "Daily unreported cases", 
    death_daily_reported_three = "Daily reported deaths",
    death_daily_unreported_three = "Daily unreported deaths", 
    case_daily_reported_four = "Daily reported cases", 
    case_daily_unreported_four= "Daily unreported cases", 
    death_daily_reported_four = "Daily reported deaths",
    death_daily_unreported_four = "Daily unreported deaths"
  )
