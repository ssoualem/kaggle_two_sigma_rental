
# TODO : basic doc (in/out)

load_data <- function(f) {
  data <- fromJSON(f)
  
  # unlist every variable except `photos` and `features` and convert to tibble
  vars <- setdiff(names(data), c("photos", "features"))
  data <- map_at(data, vars, unlist) %>% tibble::as_tibble(.)
  data
}


transform_raw_data <- function(x, type = "train") {
  # Convert to datetime
  x$created <- strftime(x$created, tz = "EST", "%Y-%m-%d %H:%M:%S")
  
  # factor
  # TODO : see if ordered factor changes something
  if(type == "train") {
    x$interest_level <- ordered(x$interest_level, levels = c("low", "medium", "high"))
  }
  
  x$nb_features <- sapply(x$features, length)
  x$nb_photos <- sapply(x$photos, length)
  x$created_mday <- factor(mday(x$created))
  x$created_wday <- factor(wday(x$created)
                                  , levels = c(1:7)
                                  , labels = c("Sun", "Mon", "Tue", "Wed", "Thu", "Fri", "Sat"))
  x$created_dt <- as.Date(x$created)
  x$created_month <- factor(month(x$created))
  x$created_hour <- factor(hour(x$created))
  x$description_nchar <- nchar(x$description)
  
  # Price variants
  x$log_price <- log(x$price)
  x$log_price_per_bedroom_p1 <- log(x$price / (1 + x$bedrooms))
  x$log_price_per_bathroom_p1 <- log(x$price / (1 + x$bathrooms))
  x$log_price_per_room_p2 <- log(x$price / (2 + x$bedrooms + x$bathrooms))
  
  x$logp1_nb_features <- log(1 + x$nb_features)
  x$logp1_nb_photos <- log(1 + x$nb_photos)
  
  x
}