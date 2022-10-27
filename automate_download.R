#######################################
# title: "Test: import dataset using API"
# author: "Joseph Shim"
# date: "2022-09-12"
# output: html_document

########################################
# API
# fetch the data via API
#######################################
library(httr)
url <- "https://www.formstack.com/api/v2/form/4715050/submission.json"
queryString <- list( per_page = "25", data = "false", expand_data = "false" )
#queryString <- list(folders = "false")
response <- VERB("GET", url, 
                 add_headers('Authorization' = 'USE_YOUR_AUTHORIZATION'),
                 query = queryString, 
                 content_type("application/octet-stream"), 
                 accept("application/json"))
                 
#######################################
# import library
#######################################
if(!require(rjson))
  install.packages("rjson") 
library(rjson)

if(!require(tidyverse))
  install.packages("tidyverse")
library(tidyverse) 

if(!require(tidyr)) 
  install.packages("tidyr") 
library(tidyr)

#######################################
# change the format
result_json <- fromJSON( content(response, as="text") )

# change data to wide to have the right form
empty <- data.frame() 
total_sub <- length(result_json$submissions) # this is a total number of submission (currently 13 administration)
for (i in 1:total_sub){ # administration level
  for (name in (names(result_json$submissions[[i]]$data))){ # question level
    dat <- as.data.frame(result_json$submissions[[i]]$data[[name]]) %>%
      select(field, label, flat_value, type) %>%
      mutate(client_index=i,
             client_id = result_json$submissions[[i]]$id,
             time = result_json$submissions[[i]]$timestamp,
             ip = result_json$submissions[[i]]$remote_addr,
             latitude = result_json$submissions[[i]]$latitude,
             longitude = result_json$submissions[[i]]$longitude,
             status = result_json$submissions[[i]]$approval_status) 
    # row bind all the results for each individual
    empty <- rbind(empty, dat)
  }
}
####################################
# cleaning
all_data <- empty
# remove potential NA values
all_data[all_data == "N/A"| all_data == "N/a"] <- NA
all_data[all_data %in% c("N/A", "N/a", "na", "n/a", "null")] <- NA

####################################
# remove checkbox
all_data_deleted <- all_data %>%
  filter(type != "checkbox" & type != "name" )

# manipulate checkbox (single column to multiple columns)
all_data_check <- all_data %>%
  filter(type == "checkbox") %>%
  distinct() %>%
  separate_rows(flat_value, 1, sep="\n") %>%
  group_by(field, label) %>% dplyr::mutate(rn = row_number()) %>%
  dplyr::mutate(label = paste0(label, "-", rn) ) %>%
  select(-rn)

all_data_name <- all_data %>%
  filter(type == "name") %>% distinct() %>%
  separate_rows(flat_value, 1, sep="\n") %>% group_by(client_index) %>%
  dplyr::mutate(rn = row_number() ) %>%
  dplyr::mutate(label = paste0(label, "-", rn) ) %>% select(-rn) %>%
  dplyr::mutate(flat_value = str_replace(flat_value, c("first =", "last ="), '' ),
         label = ifelse(label=="Who is submitting this report?-1", 
                        "Who is submitting this report? (first_name)", 
                        "Who is submitting this report? (last_name)" ) )
# merge data
all_data <- rbind(all_data_deleted, all_data_check)
all_data <- rbind(all_data, all_data_name)

#########################################
# change format from long -> wide
all_data2 <- all_data %>%
  dplyr::group_by(client_index) %>%
  dplyr::mutate(row = row_number()) %>%
  tidyr::pivot_wider(names_from = label, 
                     values_from = c("flat_value"), 
                     values_fill = NA) %>%
  dplyr::select(-c("row","type") )%>% dplyr::ungroup() %>% 
  dplyr::group_by(client_index) %>%
  dplyr::summarise(across(everything(), ~ max(., na.rm = T)))
####################################

# save the final dataframe as csv
write.csv(all_data2, "result_clean.csv")
