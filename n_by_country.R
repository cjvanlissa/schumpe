timepoints <- matrix(NA, nrow = length(unique(df_long$time)), ncol = (length(vars$dv)))
colnames(timepoints) <- vars$dv
rownames(timepoints) <- unique(df_long$time)
#  1)    How much % of the participants at baseline took part in wave 4, 5, 6, 7, 8, 9, 10, 11 respectively?
  
#  2)    I need to describe the samples at the different waves (maybe show the drop out was not systematic, i.e. predicted by any of the predictors or covariates)
  
countries <- matrix(NA, nrow = max(df_long$country, na.rm = T), ncol = (length(vars$dv)))
colnames(countries) <- vars$dv

#miss_mcar <- rep(NA, ncol(countries))
#names(miss_mcar) <- colnames(countries) 

for(thisdv in vars$dv){
  use_waves <- unique(df_long$time[df_long$variable == thisdv])
  next_waves <- use_waves[-length(use_waves)]
  names(next_waves) <- use_waves[-1]
  
  # Add next wave -----------------------------------------------------------
  
  df_nextwave <- df_long[df_long$variable == thisdv, .SD, .SDcols = c("id", "time", "variable", "value", "date")]
  df_nextwave <- df_nextwave[df_nextwave$time %in% names(next_waves), ]
  df_nextwave$time <- next_waves[as.character(df_nextwave$time)]
  df_nextwave <- df_nextwave[!time < 0, ]
  df_nextwave <- df_nextwave[!duplicated(df_nextwave[, .SD, .SDcols = names(df_nextwave)[!names(df_nextwave) == "value"]]), ]
  
  df_nextwave <- pivot_wider(df_nextwave, names_from = "variable", values_from = "value")
  df_nextwave <- df_nextwave[!is.na(df_nextwave$date), ]
  names(df_nextwave)[match(c("date", thisdv), names(df_nextwave))] <- paste0("DV_", c("date", thisdv))
  
  df_anal <- merge(df_wide, df_nextwave, by = c("id", "time"), all.x = TRUE)
  
  df_anal$Dt <- df_anal$DV_date - df_anal$date
  #table(sign(df_anal$Dt))
  
  
  thisdv_preds <- tapply(df_long$time, df_long$variable, table)
  
  pred_invar <- c(vars$predictor, vars$control)[!c(vars$predictor, vars$control) %in% longv]
  pred_invar <- pred_invar[!pred_invar == "political_view"]
  pred_time_var <- c(vars$predictor, vars$control)[c(vars$predictor, vars$control) %in% longv]
  
  
  keep_timevar <- rep(TRUE, length(pred_time_var))
  make_invar <- rep(FALSE, length(pred_time_var))
  for(v in 1:length(pred_time_var)){
    this_pred <- pred_time_var[v]
    has_waves <- as.numeric(names(thisdv_preds[[this_pred]]))
    has_waves <- has_waves[has_waves %in% use_waves[-length(use_waves)]]
    if(length(has_waves) == 0){
      keep_timevar[which(pred_time_var == this_pred)] <- FALSE
    }
    if(length(has_waves) == 1){
      keep_timevar[which(pred_time_var == this_pred)] <- FALSE
      make_invar[which(pred_time_var == this_pred)] <- TRUE
    }
  }
  becomes_invar <- pred_time_var[make_invar]
  pred_invar <- c(pred_invar, pred_time_var[make_invar])
  pred_time_var <- pred_time_var[keep_timevar]
  
  # Make plot of available data
  use_data <- t(sapply(c(thisdv, pred_time_var), function(x){
    the_waves %in% names(thisdv_preds[[x]])[names(thisdv_preds[[x]]) %in% use_waves]
  }))
  if(nrow(use_data) > 1){
    use_data[2:nrow(use_data), ncol(use_data)] <- FALSE
  }
  colnames(use_data) <- the_waves
  
  pred_time_var <- c(pred_time_var, thisdv)
  
  df_anal[pred_invar] <- lapply(df_anal[pred_invar], function(thisv){
    #thisv = df_anal[[pred_invar[1]]]
    unlist(tapply(thisv, df_anal$id, function(x){
      rep(unique(na.omit(x))[1], length(x))
    }))
  })
  
  center_these <- c(pred_invar, pred_time_var)
  center_these <- center_these[sapply(df_anal[center_these], function(x){length(unique(x))}) > 3]
  center_these <- c(center_these, "tightness", "Dt")
  
  df_anal[center_these] <-  sapply(df_anal[center_these], function(x){as.vector(scale(x, scale = TRUE))})
  
  pred_time_var <- c("date", "Dt", pred_time_var)
  
  use_these <- c(pred_invar, pred_time_var, "tightness")
  df_anal <- df_anal[!is.na(df_anal$Dt), ]
  # rename <- c("protest_containment_measures", "social_contact_friendsandfamily", "leave_house_leisure_others", "willingness_vaccinated")
  # names(rename) <- c("contain_protest", "frienfam", "leavleis", "willvacc")
  # for(i in 1:length(rename)){
  #   pred_time_var <- gsub(rename[i], names(rename)[i], pred_time_var)
  #   names(df_anal) <- gsub(rename[i], names(rename)[i], names(df_anal))
  #   thisdv <- gsub(rename[i], names(rename)[i], thisdv)
  #   use_these <- gsub(rename[i], names(rename)[i], use_these)
  # }
  
  # Make Mplus model object
  n_by_wave <- tapply(df_anal$id, df_anal$time, function(x){length(unique(x))})
  
  timepoints[match(names(n_by_wave), rownames(timepoints)), match(thisdv, colnames(timepoints))] <- n_by_wave
  #tmp <- MissMech::TestMCARNormality(df_anal[c(paste0("DV_", thisdv), use_these)])
  #miss_mcar[thisdv] <- tmp$pvalcomb
  
  
  tmp <- df_anal[c("id", "country", paste0("DV_", thisdv), use_these)]
  tmp <- tmp[complete.cases(tmp[,use_these]), ]
  
  n_by_country <- tapply(tmp$id, tmp$country, function(x){length(unique(x))})
  countries[as.numeric(names(n_by_country)), match(thisdv, colnames(countries))] <- n_by_country
}

countries <- cbind(country_number = 1:nrow(countries), countries)
countries[,1] <- names(char_dict$country)[countries[,1]]
write.csv(countries, "n_by_country.csv", row.names = FALSE)

timepoints <- cbind(wave_number = rownames(timepoints), timepoints)
write.csv(timepoints, "timepoints.csv", row.names = FALSE)

#saveRDS(miss_mcar, "miss_mcar.RData")