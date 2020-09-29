options(tidyverse.quiet = TRUE)
library(tidyverse)
library(rvest)
library(stringi)
library(httr)



#########Get List of All MPs########
# Read random MP's Page
x <-
  read_html("http://loksabhaph.nic.in/Members/MemberHomePage.aspx?mpsno=4704")

# Get all names and format as "First Last"
name_list <-
  x %>%
  html_nodes("#left_block a") %>%
  html_text() %>%
  stri_split_fixed(", ", simplify = T) %>%
  as_tibble() %>%
  mutate(last = stri_trim(V1),
         first = stri_trim(V2)) %>%
  mutate(full_name = paste(first, last) %>% stri_trim()) %>%
  .$full_name %>%
  str_squish()

# Add the original names as reference
ref_names <-
  x %>%
  html_nodes("#left_block a") %>%
  html_text()

# Get URLs 
links_list <-
  x %>%
  html_nodes("#left_block a") %>%
  html_attr("href") %>%
  paste0("http://loksabhaph.nic.in/Members/", .)

# Bring together to make tibble
lok_sabha <-
  tibble(name = ref_names,
         mp_names = name_list,
         mp_url = links_list)

############Function to get details from MP's page##########
mp_details_getter <- function(mp_url) {
  
  # mp_url ==> is the MP's page link
  # Read link
  
  page_source <- GET(mp_url) %>% read_html()
  
  
  
  # Extract Relevant Details. Variable names are self explanatory
  # Format of the page forces splitting the Data Getting Process
  details_df <-
    page_source %>%
    html_nodes("#mid_block td") %>%
    html_text() %>%
    str_squish() %>%
    t() %>%
    as_tibble() %>%
    select(
      name = V1,
      constituency = V4,
      party = V7,
      email = V10
    ) %>%
    mutate(email = stri_replace_all_fixed(email, "[DOT]", "."),
           email = stri_replace_all_fixed(email, "[AT]", "@"))  # Email needs to be cleaned. 
                                                                # Each has two official and personal. No spaces.
  
  # Phones need to be cleaned. Too many pecularities.
  phones <-
    page_source %>%
    html_nodes(".adress_ul li") %>%
    html_text() %>%
    stri_trim() %>%
    str_squish() %>%
    stri_flatten()
  
  present_address <-
    page_source %>%
    html_node(".present_adress p") %>%
    html_text() %>%
    str_squish() %>%
    stri_trim()
  
  permanent_address <-
    page_source %>%
    html_node(".permanent_adress p") %>%
    html_text() %>%
    str_squish() %>%
    stri_trim()
  
  # Logs and Tacking
  log_entry <- paste0("Completed  ",details_df$name,"  at  ",Sys.time())
  write_lines(log_entry, "Parliament Tweets/ls_log.txt", append = T)
  write_lines(details_df$name, "Parliament Tweets/completed.txt", append = T)
  
  # Return collected details with reference name
  details_df <- 
    details_df %>%
    mutate(
      phone = phones,
      permanent_address = permanent_address,
      present_address = present_address
    )
  
}

################ Get the Data for each MP ###########
# Create slower function. Don't know how LS website will respond.
mp_rate <- rate_delay(pause = 5,max_times = 10)
slow_mdg <- slowly(mp_details_getter,rate = mp_rate,quiet = T)


# Call the function to get the details.
# Storing as list to avoid any issues in between

ls_mp_data <- map(lok_sabha$mp_url, slow_mdg)

# Store as RDS for leisure processing
write_rds(mp_data, "Parliament Tweets/ls_mp_data.rds")

write_csv(lok_sabha, "Parliament Tweets/lok_sabha_members.csv")
