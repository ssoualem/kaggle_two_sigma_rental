library(stringr)
library(quanteda)
library(lubridate)

# TODO : basic doc (in/out)

load_data <- function(f) {
  data <- fromJSON(f)
  
  # unlist every variable except `photos` and `features` and convert to tibble
  vars <- setdiff(names(data), c("photos", "features"))
  data <- map_at(data, vars, unlist) %>% tibble::as_tibble(.)
  data
}

# Removes HTML tags
strip_html <- function(txt) {
  gsub("<.*?>", "", txt)
}

count_nb_words <- function(txt) {
  txt_tmp <- strip_html(txt)
  
  # Tokenize words
  tok <- tokenize(txt_tmp, removePunct = TRUE, removeSymbols = TRUE)
  
  sapply(tok, length)
}



transform_raw_data <- function(x, type = "train") {
  ##########################################################################################
  # Basic transformations
  ##########################################################################################
  # Convert to datetime
  x$created <- strftime(x$created, tz = "EST", "%Y-%m-%d %H:%M:%S")
  
  # Convert outcome to factor
  # TODO : see if ordered factor changes something
  if(type == "train") {
    x$interest_level <- factor(x$interest_level, levels = c("low", "medium", "high"))
  }
  
  ##########################################################################################
  # Features about listing features
  ##########################################################################################
  x$nb_features <- sapply(x$features, length)
  
  ##########################################################################################
  # Photos
  ##########################################################################################
  x$nb_photos <- sapply(x$photos, length)
  
  ##########################################################################################
  # Date features
  ##########################################################################################
  x$created_mday <- as.integer(factor(mday(x$created)))
 # x$created_wday <- factor(wday(x$created)
  #                                , levels = c(1:7)
   #                               , labels = c("Sun", "Mon", "Tue", "Wed", "Thu", "Fri", "Sat"))
  x$created_wday <- as.integer(factor(wday(x$created)))
  x$created_dt <- as.Date(x$created)
  x$created_month <- as.integer(factor(month(x$created)))
  x$created_hour <- as.integer(factor(hour(x$created)))
  
  ##########################################################################################
  # Description features
  ##########################################################################################
  x$descr_n_words <- count_nb_words(x$description)
  
  ##########################################################################################
  # Price / room features
  ##########################################################################################
  
  x <- mutate(x
              ,price_per_bed=ifelse(!is.finite(price/bedrooms),-1, price/bedrooms)
              ,price_per_bath=ifelse(!is.finite(price/bathrooms),-1, price/bathrooms)
              ,price_per_room=ifelse(!is.finite(price/(bedrooms+bathrooms)),-1, price/(bedrooms+bathrooms))
              ,bed_per_bath=ifelse(!is.finite(bedrooms/bathrooms), -1, price/bathrooms)
              ,bed_bath_diff=bedrooms-bathrooms
              ,bed_bath_sum=bedrooms+bathrooms
              ,beds_perc=ifelse(!is.finite(bedrooms/(bedrooms+bathrooms)), -1, bedrooms/(bedrooms+bathrooms))
  )
  
  
  
  ##########################################################################################
  # Building and manager id
  ##########################################################################################
  # Convert building_ids and manager_ids with few observations into a separate group
  # Count occurences
  build_count <- count(x, building_id)
  manag_count <- count(x, manager_id)
  addr_count <- count(x, display_address)
  
  # Minimum number of observations needed to keep the ID as a feature
  # If number of is below the cutoff point, the IDs are replaced with "-1"
  build_count_cutoff <- 20
  manag_count_cutoff <- 20
  addr_count_cutoff <- 20
  
  # Replace building_id with few observations with "-1"
  build_1_count_idx <- which(x$building_id %in% filter(build_count, n < build_count_cutoff)$building_id)
  build_1_count <- slice(build_count, build_1_count_idx)$building_id
  
  x <- mutate(x
                , building_id = ifelse(building_id %in% build_1_count, "-1", building_id)
  )
  
  # Replace manager_id with few observations with "-1"
  manag_1_count_idx <- which(x$manager_id %in% filter(manag_count, n < manag_count_cutoff)$manager_id)
  manag_1_count <- slice(manag_count, manag_1_count_idx)$manager_id
  
  x <- mutate(x
                , manager_id = ifelse(manager_id %in% manag_1_count, "-1", manager_id)
  )
  
  # Replace display_address with few observations with "-1"
  addr_1_count_idx <- which(x$display_address %in% filter(addr_count, n < addr_count_cutoff)$display_address)
  addr_1_count <- slice(addr_count, addr_1_count_idx)$display_address
  
  x <- mutate(x
                , display_address = ifelse(display_address %in% addr_1_count, "-1", display_address)
  )
  
  # TODO : put stuff above in function and calc manager skill
  
  
  x
}