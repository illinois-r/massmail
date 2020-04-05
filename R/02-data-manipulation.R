## ---- show-preview ----
id = massmail_data %>%
  filter(url == "https://massmail.illinois.edu/massmail/1930327.html")

id %>% select(-content) %>% knitr::kable()

## ---- show-email ----
id %>% pull(content) %>% cat()

## ---- data-load ----

# Retrieve data ----

# Read data in from CSV
massmail_data = readr::read_csv(
  "https://raw.githubusercontent.com/coatless/massmail/master/data/massmail_data.csv"
)

# Detection for covid19 string in text corpus
detect_covid19_emails = function(x) { 
  stringr::str_detect(x, regex("(covid-19|coronavirus)", ignore_case = TRUE))
}

# Restrict massmail archive to 2020 and add indicator variable for covid19
massmail_data_covid =
  massmail_data %>%
  filter(year(`date`) > 2019) %>% # Restrict to AY 2020 - Present
  mutate(covid19 = 
           detect_covid19_emails(subject) | detect_covid19_emails(content),
         weekday = wday(`date`, label=TRUE)
  )