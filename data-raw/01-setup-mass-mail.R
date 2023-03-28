# Load dependencies ----
library("rvest")
library("tidyverse")
library("lubridate")

# Non-tabular massmail parse rules ---- 

# General table rule
massmail_table_col = function(column_id, massmail_page) {
  massmail_page %>%
    html_nodes(glue::glue(".col{column_id}")) %>%
    html_text()
}

# Email column specific rule
massmail_table_col_email = function(massmail_page) {
  massmail_page %>%
    html_nodes(".col4 a") %>%
    html_attr("href")
}


# Massmail contents retrieval and caching ----
massmail_download_email = function(massmail_email_url,
                                   save_dir = "data-raw/massmail-emails") {
  dir.create(save_dir, showWarnings = FALSE)
  
  # More control
  for (url in massmail_email_url) {
    file_loc = file.path(save_dir, basename(url))
    
    if (file.exists(file_loc))
      next
    
    download.file(url, file_loc)
  }
}

massmail_read_email = function(massmail_email_url,
                               save_dir = "data-raw/massmail-emails") {
  
  file_loc = file.path(save_dir, basename(massmail_email_url))
  
  # There really are about 4 templates across a 10 year span.
  massmail_email_body = function(path) {
    email = read_html(path) 
    
    message("Attempting approach one on ", path, " ...")
    approach_one = email %>%
      html_node("#wrapper > tbody > tr:nth-child(2) > td:nth-child(3)") %>%
      html_text(trim = TRUE)
    
    if(!is.na(approach_one) && !identical(approach_one, "")) return(approach_one)
    message("Attempting approach two on ", path, " ...")
    
    # Based off of 27144.html
    approach_two = email %>%
      html_node("#wrapper > tbody > tr:nth-child(2) > td:nth-child(2)") %>%
      html_text(trim = TRUE)
    
    if(!is.na(approach_two) && !identical(approach_two, "")) return(approach_two)
    
    message("Attempting approach three on ", path, " ...")
    
    # Based off of 26844.html
    approach_three = email %>%
      html_node("table:nth-child(2) > tr:nth-child(1) ") %>%
      html_text(trim = TRUE)
    
    if(!is.na(approach_three) && !identical(approach_three, "")) return(approach_three)
    
    message("Attempting approach four on ", path, " ...")
    
    # Based off of 26839.html
    approach_four = email %>%
      html_node("table:nth-child(3) tr:nth-child(1)") %>%
      html_text(trim = TRUE)
    
    approach_four
    
   }
  
  map_chr(file_loc, massmail_email_body)
}


# Construct the massmail table ----
massmail_meta_data = function(massmail_page) {
  purrr::map_dfc(
    c(
      "datetime" = 1,
      "sent" = 2,
      "subject" = 4
    ),
    massmail_table_col,
    massmail_page = massmail_page
  ) %>%
    mutate(`url` = massmail_table_col_email(massmail_page),
           `sent` = readr::parse_number(`sent`),
           `datetime` = lubridate::mdy_hm(`datetime`),
           `date` = lubridate::date(`datetime`),
           `time` = paste(hour(datetime), minute(datetime), sep=":"))
}

# Handle non-table semantics
massmail_table = function(massmail_page) { 
  
  # Form overview of data
  massmail_meta_table = massmail_meta_data(massmail_page)
  
  # Download
  massmail_meta_table %>% 
    pull(url) %>%
    massmail_download_email()
  
  # Pull e-mail contents 
  contents = massmail_meta_table %>% 
    pull(url) %>%
    massmail_read_email()
  
  # Release table with emails attached
  massmail_meta_table %>%
    mutate(content = contents) %>%
    select(`datetime`, `date`, `time`, `sent`, `subject`, `url`, `content`)
}


# Build the massmail data table ----
# Retrieve the massmail page
massmail_page = read_html(httr::GET("https://massmail.illinois.edu/massmailArchive"))

# Check to see if the archive is down.
page_contents = massmail_page %>% html_text()

if(grepl("Error occured.", page_contents)) {
  message("Archive is not available.")
  quit(save = "no", status = 0)
}

# Build an updated version of the massmail archive
massmail_data = massmail_table(massmail_page)

# Preview data inside of RStudio
# View(massmail_data)

# Export data ----
#usethis::use_data(massmail_data, overwrite = TRUE)
save(massmail_data, file = "data/massmail_data.rda")
readr::write_csv(massmail_data, file = "data/massmail_data.csv")
