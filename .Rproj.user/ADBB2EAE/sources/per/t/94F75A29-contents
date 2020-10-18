library(rvest)
library(tidyverse)

url <- "http://www.netennis.com"

homepage <- read_html(url)

newsletters <- homepage %>%
  html_nodes("a") %>%
  html_attr('href') 

years <- simple %>%
  html_nodes("a") %>%
  html_text()

newsletters <-newsletters[str_detect(newsletters, "newsletter")]

years <- years[str_detect(years, "Newsletter")] %>%
  str_extract(., "[0-9]{4}")

names(newsletters) <- years

awards_year <- newsletters %>%
  map(~ read_html(paste0(url, "/", .))) %>%
  map(~ html_nodes(., "i")) %>%
  map(~ html_text(.))  %>%
  map(~ as.data.frame(.)) %>%
  bind_rows(.id = "year") %>%
  rename(award = 2) %>%
  mutate(session = str_match(award, "(.*?) Session$")[,2]) %>%
  filter(!str_detect(tolower(award), "click|note|hope|newsletter")) %>%
  mutate(award = str_trim(award)) %>%
  filter(award!="") %>%
  fill(session, .direction = "down") %>%
  filter(!str_detect(tolower(award), "session")) %>%
  mutate(is_award = TRUE)

newsletter_pages <- newsletters %>%
  map(~ read_html(paste0(url, "/", .)))

all_text <- newsletter_pages %>%
  map(~ as.character(.)) %>%
  map(~ str_replace_all(., "<br>", "#")) %>%
  map(~ as.data.frame(.)) %>%
  bind_rows(.id = "year") %>%
  rename(text = 2) %>%
  separate_rows(text, sep = "#") %>%
  mutate(text = map(text, ~ read_html(.))) %>%
  mutate(text = map(text, ~ html_text(.))) %>%
  mutate(text = str_trim(text))

winners_clean <- all_text %>%
  mutate(session = str_match(text, "(.*?) Session$")[,2]) %>%
  mutate(text = str_trim(text)) %>%
  group_by(year) %>%
  fill(session, .direction = "down") %>%
  ungroup() %>%
  left_join(awards_year, by = c("text" = "award", "year" = "year", "session" = "session")) %>%
  filter(text!="") %>%
  filter(!is.na(session)) %>%
  filter(!str_detect(tolower(text), "session")) %>%
  filter(!str_detect(tolower(text), "click|note|hope|newsletter")) %>%
  mutate(award = ifelse(is_award, text, NA)) %>%
  group_by(year, session) %>%
  fill(award, .direction = "down") %>%
  ungroup() %>%
  mutate(award = ifelse(str_detect(text, "Team Winners"), "Team Winners",
                        ifelse(str_detect(text, "Team Runners-up"), "Team Runners-up", award))) %>%
  rename(winners = text) %>%
  select(-is_award) %>%
  select(year, session, award, winners) %>%
  mutate(winners = str_remove(winners, award)) %>%
  mutate(winners = str_trim(winners)) %>%
  filter(winners!="") %>%
  mutate(winners = str_trim(winners)) %>%
  mutate(winners = str_remove(winners, "^– ")) %>%
  mutate(winners = str_trim(winners)) %>%
  separate_rows(winners, sep = "&|,") %>%
  mutate(position = str_extract(winners, "[0-9]+")) %>%
  mutate(winners = str_remove_all(winners, "[0-9]|\\.")) %>%
  mutate(winners = str_trim(winners)) %>%
  mutate(level = str_match(award, "^([A, B, C]{1}) ")[,2]) %>%
  mutate(winners = str_remove_all(winners, "–|\n|\r"))  

winners_clean %>%
  filter(winners == 'Andy') %>%
  View()

write_csv(winners_clean,here::here("Data", "winners_clean.csv"))
