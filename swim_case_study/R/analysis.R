source("R/libraries.R")
source("R/prepdata.R")




#function to run the model
run_model <- function(clean_data, style, dist, cutoff_date = "2024-06-01") {
  data <- clean_data %>% 
    filter(stroke == style, distance == dist)
  train_df <- data %>% 
    filter(date < as.Date(cutoff_date))
  train_df$age_num <- as.numeric(as.character(train_df$age))
  
  if(dist == 50 & style != 'free'){
    print('Please only use distance of 50 and 200 for free')
    stop()
  }
  
  if(dist == 200 & style != 'free'){
    print('Please only use distance of 50 and 200 for free')
    stop()
  }
  
  if(style == 'free'){
    if(dist == 100){
      model <- lmer(
        time_sec ~ taper + relay_indicator +
          ns(age_num, df = 3) + gender + avg_prev_2_time_sec + pb_time_sec +
          (1 | name) + (1 | meet),
        data = train_df, REML = TRUE
      )
    } 
    if(dist == 50){
      model <- lmer(
        time_sec ~ taper + ns(age_num, df = 3) + gender + avg_prev_2_time_sec +
          (1 | name) + (1 | meet),
        data = train_df,
        REML = TRUE
      )
    }
    if(dist == 200){
      model <- lmer(
        time_sec ~ taper + ns(age_num, df = 3) + gender + avg_prev_2_time_sec +
          (1 | name) + (1 | meet),
        data = train_df,
        REML = TRUE
      )
    }
  } else if(style == 'fly'){
    model <- lmer(
      time_sec ~ taper + relay_indicator + 
        ns(age_num, df = 3) + gender + avg_prev_2_time_sec +
        (1 | meet) + (1 | name),
      data = train_df, REML = TRUE
    )
  } else if(style == 'breast'){
    model <- lmer(
      time_sec ~ taper + relay_indicator + 
        ns(age_num, df = 3) + gender + avg_prev_2_time_sec + pb_time_sec +
        (1 | name) + (1 | meet),
      data = train_df, REML = TRUE
    )
  } else if(style == 'back'){
    model <- lmer(
      time_sec ~ taper + 
        ns(age_num, df = 3) + gender + avg_prev_2_time_sec +
        (1 | name) + (1 | meet),
      data = train_df, REML = TRUE
    )
  }
  
  return(model)
}


#function to make predictions
predict_olympics <- function(model, clean_data, style, dist, cutoff_date = "2024-06-01") {
  #filters stroke
  data <- clean_data %>%
    filter(stroke == style)
  
  #filters distance
  model_data <- data %>%
    filter(distance == dist)
  
  #training data
  train_df <- model_data %>%
    filter(date < as.Date(cutoff_date))
  
  newdata_raw <- train_df %>%
    arrange(name, date) %>%
    group_by(name) %>%
    slice_tail(n = 1) %>%
    ungroup() %>%
    filter(#!is.na(pb_time_sec), 
      !is.na(avg_prev_2_time_sec)) %>%
    mutate(age_num = as.numeric(as.character(age)))
  
  #flat start
  pred_individual <- newdata_raw %>%
    mutate(
      is_olympics     = 1,
      taper           = 1,
      relay_indicator = 0,
      prediction_type = "Individual_Pred"
    )
  
  #relay start
  pred_relay <- newdata_raw %>%
    mutate(
      is_olympics     = 1,
      taper           = 1,
      relay_indicator = 1,
      prediction_type = "Relay_Split_Pred"
    )
  
  #combine
  oly_2024_newdata <- bind_rows(pred_individual, pred_relay)
  
  oly_2024_newdata <- as.data.frame(oly_2024_newdata)
  
  grouping_vars <- names(model@flist)
  
  for(g in grouping_vars) {
    if(g %in% names(oly_2024_newdata)) {
      model_levels <- levels(model@flist[[g]])
      
      oly_2024_newdata[[g]] <- factor(oly_2024_newdata[[g]], levels = model_levels)
    }
  }
  
  
  intervals <- predictInterval(
    model, 
    newdata = oly_2024_newdata, 
    level = 0.95,
    n.sims = 1000,
    stat = "median",
    type = "linear.prediction",
    include.resid.var = TRUE 
  )
  
  oly_2024_newdata$pred_oly_time_sec_sim <- intervals$fit
  oly_2024_newdata$pred_low          <- intervals$lwr
  oly_2024_newdata$pred_high         <- intervals$upr

  
  #predictions
  oly_2024_newdata$pred_oly_time_sec_actual <- predict(
    model,
    newdata          = oly_2024_newdata,
    re.form          = NULL,
    allow.new.levels = TRUE
  )
  
  #final prediction table
  final_predictions <- oly_2024_newdata %>%
    dplyr::select(name, gender, age, stroke, distance, pred_oly_time_sec_actual,
           pred_oly_time_sec_sim, pred_low, pred_high, relay_indicator, 
           prediction_type) %>%
    arrange(pred_oly_time_sec_actual)
  
  #only olympic swims
  oly_actual_2024 <- model_data %>%
    filter(is_olympics == 1, year == "2024") %>%
    dplyr::select(
      name, gender, age, stroke, distance, relay_indicator,
      actual_oly_time_sec = time_sec,
      olympic_meet  = meet
    )
  
  #join the two
  comparison_2024 <- final_predictions %>%
    left_join(
      oly_actual_2024,
      by = c("name", "gender", "stroke", "distance", "relay_indicator"),
      suffix = c("_train", "_oly")
    ) %>%
    #keep only swimmers who actually swam the Olympic event
    filter(!is.na(actual_oly_time_sec)) %>%
    mutate(
      error_sec     = pred_oly_time_sec_actual - actual_oly_time_sec,
      abs_error_sec = abs(error_sec),
      in_interval   = ifelse(actual_oly_time_sec >= pred_low & 
                               actual_oly_time_sec <= pred_high, TRUE, FALSE)
    )
  
  #order the comparisons
  comparison_2024 %>%
    arrange(abs_error_sec) %>%
    dplyr::select(
      name, gender, 
      age = age_oly,
      stroke, distance,
      prediction_type,
      pred_oly_time_sec_actual,
      actual_oly_time_sec,
      pred_low,
      pred_oly_time_sec_sim,
      pred_high,
      in_interval,
      error_sec,
      abs_error_sec,
      olympic_meet,
      #olympic_date
    )
  
  
  return(comparison_2024)
}


#function to evaluate predictions
evaluate_pred <- function(comparison_2024){
  mae <- mean(abs(comparison_2024$error_sec))
  mse <- mean((comparison_2024$error_sec)^2)
  bias <- mean(comparison_2024$error_sec)
  rmse <- sqrt(mse)
  corr <- cor(comparison_2024$pred_oly_time_sec_actual, 
              comparison_2024$actual_oly_time_sec)
  results_df <- data.frame(
    Metric = c("MAE (Mean Absolute Error)", 
               "MSE (Mean Squared Error)", 
               "Bias (Avg Error Direction)", 
               "RMSE (Root Mean Squared Error)", 
               "Pearson Correlation"),
    Value = round(c(mae, mse, bias, rmse, corr), 4)
  )
  
  g <- ggplot(comparison_2024, aes(x = pred_oly_time_sec_actual, y = error_sec)) +
    geom_point(aes(color = gender), alpha = 0.6) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
    scale_color_manual(values = c('M' = 'blue', 'F' = 'deeppink1')) +
    labs(title = "Residuals vs. Predicted Speed", x = "Predicted Time", y = "Error", color = "Gender") +
    theme_minimal()
  
  g1 <- ggplot(comparison_2024, aes(x = actual_oly_time_sec, y = pred_oly_time_sec_actual)) +
    geom_point(aes(color = gender), alpha = 0.7, size = 3) +
    geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "red") +
    scale_color_manual(values = c('M' = 'blue', 'F' = 'deeppink1')) +
    labs(title = "Actual vs. Predicted", x = "Actual Time", y = "Predicted Time", color = "Gender") +
    theme_minimal() + coord_equal()
  
  output <- list(
    results = results_df,
    plot_resid = g,
    plot_actual = g1
  )
  return(output)
}




# evaluate_pred <- function(comparison_2024){
#   mae <- mean(abs(comparison_2024$error_sec))
#   mse <- mean((comparison_2024$error_sec)^2)
#   bias <- mean(comparison_2024$error_sec)
#   rmse <- sqrt(mse)
#   corr <- cor(comparison_2024$pred_oly_time_sec_actual, 
#               comparison_2024$actual_oly_time_sec)
#   
#   results <- list(
#     MAE = mae,
#     MSE = mse,
#     Bias = bias,
#     RMSE = rmse,
#     Correlation = corr
#   )
#   
#   rank_stat <- comparison_2024 %>%
#     group_by(gender) %>%
#     summarise(
#       Spearman_Rho = cor(pred_oly_time_sec_actual, actual_oly_time_sec, method = "spearman")
#     )
#   #print(rank_stat)
#   
#   g <- ggplot(comparison_2024, aes(x = pred_oly_time_sec_actual, y = error_sec)) +
#     geom_point(aes(color = gender), alpha = 0.6) +
#     geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
#     labs(
#       title = "Residuals vs. Predicted Speed",
#       x = "Predicted Time (sec)",
#       y = "Error (Predicted - Actual)",
#       color = "Gender"
#     ) +
#     theme_pub()
#   
#   print(g)
#   
#   g1 <- ggplot(comparison_2024, aes(x = actual_oly_time_sec, y = pred_oly_time_sec_actual)) +
#     geom_point(aes(color = gender), alpha = 0.7, size = 3) +
#     geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "red", linewidth = 1) +
#     labs(
#       title = "Actual vs. Predicted Olympic Times",
#       x = "Actual Time (seconds)",
#       y = "Predicted Time (seconds)",
#       color = "Gender"
#     ) +
#     theme_pub() +
#     coord_equal()
#   
#   print(g1)
#   
#   output <- list(rank_stat = rank_stat, results = results)
#   return(output)
# }



#example 100 free
#model_100_free <- run_model(clean, "fly", 100)
#predictions_100_free <- predict_olympics(model_100_free, clean, "fly", 100)
#evaluate_pred(predictions_100_free)




#function to simulate relays
simulate_relay <- function(clean_data, relay_type, gender_input = "mixed") {
  
  gender_input <- tolower(gender_input)
  relay_type   <- tolower(relay_type)
  
  if (grepl("free", relay_type)) {
    strokes_needed <- c("free")
    dist_needed    <- ifelse(grepl("200", relay_type), "200", "100")
  } else if (grepl("medley", relay_type)) {
    strokes_needed <- c("back", "breast", "fly", "free")
    dist_needed    <- "100"
  } else {
    stop("Unknown relay type.")
  }
  
  swimmer_pool <- data.frame()
  
  for (s in strokes_needed) {
    tryCatch({
      model <- run_model(clean_data, s, dist_needed)
      
      curr_data <- clean_data %>%
        filter(stroke == s, distance == dist_needed) %>%
        filter(!is.na(avg_prev_2_time_sec)) %>%
        group_by(name) %>%
        slice_tail(n = 1) %>% 
        ungroup() %>%
        mutate(age_num = as.numeric(as.character(age)))
      
      if (gender_input == "male")   curr_data <- curr_data %>% filter(gender == "M")
      if (gender_input == "female") curr_data <- curr_data %>% filter(gender == "F")
      
      get_pred <- function(m, df) {
        if("gender" %in% names(m@frame)) {
          df$gender <- factor(df$gender, levels = levels(m@frame$gender))
        }
        fe <- predict(m, newdata = df, re.form = NA)
        re_vals <- ranef(m)$name
        swimmer_res <- numeric(nrow(df))
        known <- intersect(as.character(df$name), rownames(re_vals))
        if(length(known) > 0) {
          lookup <- re_vals[known, "(Intercept)"]
          names(lookup) <- known
          matches <- as.character(df$name) %in% known
          swimmer_res[matches] <- lookup[as.character(df$name[matches])]
        }
        return(fe + swimmer_res)
      }
      
      df_flat <- curr_data %>% mutate(taper=1, relay_indicator=0)
      df_flat$pred_time <- get_pred(model, df_flat)
      #df_flat$pred_time <- predict(model, df_flat, allow.new.levels=TRUE)
      df_flat$start_type <- "flat"
      
      df_relay <- curr_data %>% mutate(taper=1, relay_indicator=1)
      df_relay$pred_time <- get_pred(model, df_relay)
      #df_relay$pred_time <- predict(model, df_relay, allow.new.levels = TRUE)
      df_relay$start_type <- "relay"
      
      combined <- bind_rows(df_flat, df_relay) %>%
        dplyr::select(name, gender, stroke, pred_time, start_type)
      
      swimmer_pool <- bind_rows(swimmer_pool, combined)
      
    }, error = function(e) {
      message(paste("   Error in", s, ":", e$message))
    })
  }
  
  if (nrow(swimmer_pool) == 0) stop("No swimmers generated.")
  
  #only keep few so computer doesnt crash
  swimmer_pool <- swimmer_pool %>%
    group_by(stroke, gender, start_type) %>%
    slice_min(pred_time, n = 10) %>%
    ungroup()
  
  results <- list()
  
  #free relays
  if (grepl("free", relay_type) && !grepl("medley", relay_type)) {
    pool <- swimmer_pool %>% filter(stroke == "free")
    
    if (gender_input == "male")   patterns <- list(c("M","M","M","M"))
    if (gender_input == "female") patterns <- list(c("F","F","F","F"))
    if (gender_input == "mixed") {
      patterns <- list(
        c("M","M","F","F"), c("M","F","M","F"), c("M","F","F","M"),
        c("F","M","M","F"), c("F","M","F","M"), c("F","F","M","M")
      )
    }
    
    for(p in patterns) {
      L1_pool <- pool %>% filter(start_type == "flat",  gender == p[1]) %>% transmute(L1_name=name, L1_time=pred_time)
      L2_pool <- pool %>% filter(start_type == "relay", gender == p[2]) %>% transmute(L2_name=name, L2_time=pred_time)
      L3_pool <- pool %>% filter(start_type == "relay", gender == p[3]) %>% transmute(L3_name=name, L3_time=pred_time)
      L4_pool <- pool %>% filter(start_type == "relay", gender == p[4]) %>% transmute(L4_name=name, L4_time=pred_time)
      
      if(nrow(L1_pool)>0 & nrow(L2_pool)>0 & nrow(L3_pool)>0 & nrow(L4_pool)>0) {
        sims <- tidyr::crossing(L1_pool, L2_pool, L3_pool, L4_pool) %>%
          filter(L1_name!=L2_name, L1_name!=L3_name, L1_name!=L4_name,
                 L2_name!=L3_name, L2_name!=L4_name, L3_name!=L4_name) %>%
          mutate(total_time = L1_time + L2_time + L3_time + L4_time)
        results[[paste(p, collapse="")]] <- sims
      }
    }
  }
  
  #medley relays
  if (grepl("medley", relay_type)) {
    pool_bk <- swimmer_pool %>% filter(stroke == "back",   start_type == "flat")
    pool_br <- swimmer_pool %>% filter(stroke == "breast", start_type == "relay")
    pool_fl <- swimmer_pool %>% filter(stroke == "fly",    start_type == "relay")
    pool_fr <- swimmer_pool %>% filter(stroke == "free",   start_type == "relay")
    
    if (gender_input == "male")   patterns <- list(c("M","M","M","M"))
    if (gender_input == "female") patterns <- list(c("F","F","F","F"))
    if (gender_input == "mixed") {
      patterns <- list(
        c("M","M","F","F"), c("M","F","M","F"), c("M","F","F","M"),
        c("F","M","M","F"), c("F","M","F","M"), c("F","F","M","M")
      )
    }
    
    for(p in patterns) {
      L1_pool <- pool_bk %>% filter(gender == p[1]) %>% transmute(L1_name=name, L1_time=pred_time)
      L2_pool <- pool_br %>% filter(gender == p[2]) %>% transmute(L2_name=name, L2_time=pred_time)
      L3_pool <- pool_fl %>% filter(gender == p[3]) %>% transmute(L3_name=name, L3_time=pred_time)
      L4_pool <- pool_fr %>% filter(gender == p[4]) %>% transmute(L4_name=name, L4_time=pred_time)
      
      if(nrow(L1_pool)>0 & nrow(L2_pool)>0 & nrow(L3_pool)>0 & nrow(L4_pool)>0) {
        sims <- tidyr::crossing(L1_pool, L2_pool, L3_pool, L4_pool) %>%
          filter(L1_name!=L2_name, L1_name!=L3_name, L1_name!=L4_name,
                 L2_name!=L3_name, L2_name!=L4_name, L3_name!=L4_name) %>%
          mutate(total_time = L1_time + L2_time + L3_time + L4_time)
        results[[paste(p, collapse="")]] <- sims
      }
    }
  }
  
  if(length(results) == 0) return(data.frame(Message="No valid teams found."))
  
  final_df <- bind_rows(results)
  
  #fix the abcd=adbc problem
  if (grepl("free", relay_type) && !grepl("medley", relay_type)) {
    final_df <- final_df %>%
      rowwise() %>%
      mutate(
        relay_legs_id = paste(sort(c(L2_name, L3_name, L4_name)), collapse = ", "),
        team_id = paste(L1_name, relay_legs_id)
      ) %>%
      ungroup() %>%
      group_by(team_id) %>%
      slice_min(total_time, n = 1, with_ties = FALSE) %>% 
      
      ungroup() %>%
      dplyr::select(-relay_legs_id, -team_id)
  }
  
  final_df <- final_df %>%
    arrange(total_time) %>% 
    dplyr::select(
      `1st leg` = L1_name, `1st leg time` = L1_time,
      `2nd leg` = L2_name, `2nd leg time` = L2_time,
      `3rd leg` = L3_name, `3rd leg time` = L3_time,
      `4th leg` = L4_name, `4th leg time` = L4_time,
      `total time` = total_time
    )
  
  return(final_df)
}

#sims <- simulate_relay(clean, '4x100_free', 'female')



#function to make age curves
age_curve <- function(clean_data, style, dist, gen) {
  
  data <- clean_data %>%
    filter(stroke == style)

  model_data <- data %>%
    filter(distance == dist)
  
  model_data$age_num <- as.numeric(as.character(model_data$age))
  
  age_lmer <- lmer(
    time_sec ~ ns(age_num, df = 3) * gender + taper  +
      (1 | name) + (1 | meet),
    data = model_data
  )
  
  new_ages <- data.frame(
    age_num = seq(min(model_data$age_num), max(model_data$age_num), length.out = 100),
    gender = gen,
    taper = 1,
    relay_indicator = 0
  )
  
  new_ages$pred <- predict(age_lmer, newdata = new_ages, re.form = NA)
  
  g <- ggplot(new_ages, aes(age_num, pred)) +
    geom_line() +
    labs(title = "Age Curve", x = "Age", y = "Predicted Time (sec)") +
    theme_pub()
  
  return(g)
}

#example
#age_curve(data, 'back', '100', 'F')