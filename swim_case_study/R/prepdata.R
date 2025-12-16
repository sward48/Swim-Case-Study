source("R/libraries.R")


#function to clean the data
clean_data <- function(file_path) {
  data <- read.csv(file_path, header = FALSE)
  
  #add column names
  names(data) <- c(
    "year", "name", "gender", "age", "event", "date", "meet",
    "lane", "time_rounded", "time_sec")
  
  #clean up event names
  data$event <- gsub("100 BK LCM", "back_100",  data$event)
  data$event <- gsub("100 BR LCM", "breast_100", data$event)
  data$event <- gsub("100 FR LCM", "free_100",   data$event)
  data$event <- gsub("100 FL LCM", "fly_100",    data$event)
  data$event <- gsub("200 FR LCM", "free_200",   data$event)
  data$event <- gsub("50 FR LCM",  "free_50",    data$event)
  
  #create relay indicator, taper, and convert date
  data <- data %>%
    mutate(
      relay_indicator = ifelse(grepl("(Relay Split)", event), 1, 0),
      taper = ifelse(grepl("(In Season)", meet), 0, 1),
      taper = ifelse(grepl("(Championship)", meet), 1, taper),
      date  = as.Date(date)
    )
  
  #remove "(In Season)" / "(Championship)" from meet
  data$meet  <- gsub(" \\(In Season\\)",    "", data$meet)
  data$meet  <- gsub(" \\(Championship\\)", "", data$meet)
  
  #clean up event names more (remove " (Relay Split)")
  data$event <- gsub(" \\(Relay Split\\)", "", data$event)
  
  #separate stroke and distance
  data <- data %>%
    separate(event, into = c("stroke", "distance"), sep = "_")
  
  data$gender <- as.factor(data$gender)
  data$year   <- as.factor(data$year)
  #data$age    <- as.factor(data$age)
  data$lane <- as.factor(data$lane)
  
  data <- data %>%
    mutate(
      is_olympics = str_detect(meet, "Olympic Games"),
      is_olympics = as.integer(is_olympics)
    )
  
  data <- data %>%
    group_by(name, stroke, distance) %>%
    arrange(date) %>%
    mutate(pb_time_sec = lag(cummin(time_sec)),
           avg_prev_2_time_sec = (lag(time_sec, 1) + lag(time_sec, 2)) / 2
    ) %>%
    ungroup()
  
  data <- data %>%
    group_by(name, stroke, distance) %>%
    mutate(
      date_of_pb = date[which.max(cummin(time_sec) == time_sec)],
      days_since_pb = as.numeric(date - date_of_pb)
    )
  
  return(data)
}


#read in data and get a clean csv version
data <- clean_data("data/Rawdata.csv")

#save as RDS (this protects the exact integrity of the data)
saveRDS(data, 'data/Data.rds')

#read in data so it can be used
clean <- readRDS('data/Data.rds')


