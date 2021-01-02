#options(scipen=999)
library(foreign)
library(worcs)
library(lme4)
library(lmerTest)
library(effects)
library(ggplot2)
library(readxl)
library(lavaan)
library(tidyr)
library(data.table)
library(foreign)
library(tidySEM)
library(multilevel)
library(MplusAutomation)
if(!require(MissMech)){
  library(remotes)
  install_github("cran/MissMech")
  library(MissMech)
}
run_everything <- FALSE
# Load data
df <- read.spss("RMD10_Birga new_data_final4.sav", to.data.frame = TRUE, use.value.labels = TRUE)
df <- df[df$representative == "Yes", ]
names(df) <- tolower(names(df))
names(df) <- gsub("predicyot", "predictor", names(df))

# Rename political dimensions
names(df) <- gsub("political_view(?=[xy])", "polit", names(df), perl = TRUE)

# Use only harmonized
vnames <- gsub("_harmonized$", "", names(df))
vnames <- gsub("(_b)?_harmonized(?=_)", "", vnames, perl = TRUE)

vnames <- table(vnames)[table(vnames) == 2]
df[names(vnames)] <- NULL
names(df) <- gsub("_harmonized$", "", names(df))
names(df) <- gsub("(_b)?_harmonized(?=_)", "", names(df), perl = TRUE)

# Make list with types of variables
vars <- lapply(c("predictor", "control", "dv"), function(x) names(df)[startsWith(names(df), x)])
names(vars) <- c("predictor", "control", "dv")

# Rename variables
rename <- names(df)
names(rename) <- rename
rename <- gsub("^(predictor|control|dv)_", "", rename)
names(df) <- rename
vars <- lapply(vars, function(x){ unname(rename[x]) })

# Plot variable distributions
# df_plot <- df[, unlist(vars)]
# df_plot <- pivot_longer(df_plot, cols = names(df_plot))
# p <- ggplot(df_plot, aes(x = value)) + geom_histogram() +facet_wrap(~name, scales = "free") + theme_bw() + labs(x = "Value", y = "Frequency")
# ggsave("density.png", p, "png")
# ggsave("density.svg", p, "svg")
# 
# # Remove living situation
# #df$control_living_situation <- NULL
# tmp <- df[[6]]
# desc <- descriptives(df[6])
# write.csv(desc, "descriptives.csv")
# plot(density(df$DV_wash_hands, na.rm=T))

# Remove cols / rows with too many missing 
miss <- colSums(is.na(df))
df <- df[, !miss == nrow(df)]

miss <- rowSums(is.na(df))
df <- df[!miss == ncol(df), ]



# Check time intervals
dat <- df[grep("start", names(df), value = TRUE)]
first_time <- min(dat, na.rm = TRUE)
dat <- t(apply(dat, 1, function(x){diff(as.numeric(x))}))
range(dat, na.rm = T)
length(which(apply(dat, 1, function(x){any(x<0)})))

# Convert everything to numeric for reshape
df[c("responseid", "representative")] <- NULL
names(df) <- gsub("multilevel_", "", names(df))
df$country <- trimws(df$country)

dummy_vars <- sapply(df, function(x){length(unique(x)) < 5})
#names(df)[dummy_vars]
#sapply(df[dummy_vars], table)
df[grep("leave_house_leisure_others", names(df))] <- lapply(df[grep("leave_house_leisure_others", names(df))], function(x){as.numeric(x)-1})
levels(df$gender_b) <- c("0", "1", "NA")
df$gender_b <- as.numeric(as.character(df$gender_b))
df$religious_b <- as.numeric(df$religious_b)-1

char_vars <- sapply(df, function(x){is.character(x)})
char_dict <- lapply(df[char_vars], function(x){
  nams <- unique(x)
  out <- 1:length(nams)
  names(out) <- nams
  out
})
df[char_vars] <- lapply(names(df)[char_vars], function(vname){
  out <- char_dict[[vname]][df[[vname]]]
})
fac_vars <- sapply(df, function(x){is.factor(x)})
fac_dict <- lapply(df[fac_vars], function(x){
  out <- as.numeric(unique(x))
  names(out) <- unique(x)
  out
})
df[fac_vars] <- lapply(df[fac_vars], function(x){
  as.numeric(x)
})

for(i in 1:2){
  vars <- lapply(vars, function(x){unique(gsub(pattern = "(^start_|_([bw]\\d{0,}$|harmonized)|_survey_taken$)", replacement = "", x))})
}


# Check employment status -------------------------------------------------

dat <- df[, grep("employment_status", names(df))]
#table(apply(dat, 1, function(x){length(unique(na.omit(x)))}))
these <- apply(dat, 1, function(x){length(unique(na.omit(x)))}) > 1
tmp <- dat[these, ]

# Reshape
df$id <- 1:nrow(df)
long_vars <- gsub("_[bw]\\d{0,}$", "", names(df))
long_vars <- table(long_vars)
id_vars <- names(long_vars)[long_vars == 1]
id_vars <- unname(unlist(sapply(id_vars, function(x){names(df)[startsWith(names(df), x)]})))
long_vars <- names(long_vars)[long_vars > 1]
long_vars <- unname(unlist(sapply(long_vars, function(x){names(df)[startsWith(names(df), x)]})))
df_long <- pivot_longer(df, cols = all_of(long_vars), names_to = "variable")
for(i in 1:2){
  long_vars <- unique(gsub(pattern = "(^start_|_([bw]\\d{0,}$|harmonized)|_survey_taken$)", replacement = "", long_vars))
}
names(df_long) <- c("country", "tightness", "age", "gender", "religious", "politx", "polity", "political_view", "education", "id", "variable", "value")
df_long$time <- gsub("^.+?_([bw]\\d{0,})$", "\\1", df_long$variable)
df_long$time[df_long$time == "b"] <- "0"
df_long$time <- gsub("w", "", df_long$time)
df_long$time <- as.numeric(df_long$time)

df_long$variable <- gsub("^(.+?)_([bw]\\d{0,})$", "\\1", df_long$variable)

df_dates <- df_long[df_long$variable == "start_date_survey_taken", c("id", "time", "value")]
# Set 0 to start of data collection
df_dates$value <- df_dates$value - min(df_dates$value, na.rm = TRUE)
# Convert to days
df_dates$value <- df_dates$value / 86400
df_long <- data.table(df_long)
df_dates <- data.table(df_dates)
names(df_dates)[3] <- "date"

df_long <- merge(df_long, df_dates, by = c("id", "time"), all.x = TRUE)
df_long <- df_long[!df_long$variable == "start_date_survey_taken", ]
df_long <- df_long[!is.na("date"), ]

if(any(duplicated(df_long[, .SD, .SDcols = names(df_long)[!names(df_long) == "value"]]))){
  browser()
  df_long <- df_long[!duplicated(df_long[, .SD, .SDcols = names(df_long)[!names(df_long) == "value"]]), ]
}

# Descriptives ------------------------------------------------------------

desc <- descriptives(df)
df_wide <- pivot_wider(df_long, names_from = "variable", values_from = "value")
df_wide <- df_wide[!is.na(df_wide$date), ]
longv <- unique(c("start_date_survey_taken", long_vars))[-1]
longv[!longv %in% names(df_wide)]
if(run_everything){
  ICCs <- mult.icc(data.frame(df_wide[, longv]), grpid = df_wide$id)
  names(ICCs)[1] <- "id"
  desc$id <- gsub("_[bw]\\d{0,}$", "", desc$name)
  
  desc <- merge(desc, ICCs, by = "id", all.x = TRUE)
  desc <- desc[!startsWith(desc$name, "DV_"), ]
  write.csv(desc, "descriptives.csv")
}

# Get the names of the waves
the_waves <- unique(df_long$time)

if(run_everything){
  write.table(t(c("Title", "LL", "Parameters", "AIC", "BIC", "RMSEA_Estimate", 
                  "CFI", "TLI")), "model_fits.csv", sep = "\t", row.names = FALSE, col.names = FALSE)
}

# Prepare for N by country and by timepoint
timepoints <- list()
countries <- list()
miss_mcar <- list()

# Run analyses for each DV
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
  df_plot <- as.data.frame.table(use_data)
  df_plot[1:2] <- lapply(df_plot[1:2], ordered)
  df_plot$Var1 <- ordered(df_plot$Var1, levels = rev(levels(df_plot$Var1)))
  ggsave(paste0("used_waves_", thisdv, ".png"), 
         ggplot(df_plot, aes(x = Var2, y = Var1, fill = Freq)) + geom_raster() + scale_fill_manual(values = c("TRUE" = "grey50", "FALSE" = "white")) + labs(x = "Wave", y = "Variable") + theme(legend.position = "none"),
         device = "png")
  
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
  
  # Continue with Mplus models
  rename <- c("protest_containment_measures", "social_contact_friendsandfamily", "leave_house_leisure_others", "willingness_vaccinated")
  names(rename) <- c("contain_protest", "frienfam", "leavleis", "willvacc")
  for(i in 1:length(rename)){
    pred_time_var <- gsub(rename[i], names(rename)[i], pred_time_var)
    names(df_anal) <- gsub(rename[i], names(rename)[i], names(df_anal))
    thisdv_orig <- thisdv
    thisdv <- gsub(rename[i], names(rename)[i], thisdv)
    use_these <- gsub(rename[i], names(rename)[i], use_these)
  }
  
# Make Mplus model object
  r_data <- df_anal[c("id", "time", "country", paste0("DV_", thisdv), use_these)]
  if(run_everything){
    mod <- mplusObject(
      TITLE = thisdv,
      VARIABLE = paste0(c(
        paste0("USEVARIABLES =\n", paste0(names(r_data)[!names(r_data) %in% c("id", "time", "country")], collapse = "\n"), ";"),
        "CLUSTER = country id;",
        "AUXILIARY = time;",
        paste0(c("WITHIN = ", pred_time_var, pred_invar, ";"), collapse = "\n"),
        "BETWEEN = tightness;"), collapse = "\n"),
      ANALYSIS = "TYPE = COMPLEX TWOLEVEL RANDOM;",
      MODEL = c(
        "%WITHIN%",
        paste0("s", 1:length(c(pred_time_var, pred_invar)), " | ", paste0("DV_", thisdv), " ON ", c(c(pred_time_var, pred_invar)), ";"),
        "%BETWEEN%",
        paste0(paste0("DV_", thisdv), " ON ", "tightness", ";")),
      OUTPUT = "TECH1 TECH8 stdyx;",
      SAVEDATA = paste0('file is "save_', thisdv, '.dat";'),
      usevariables = names(r_data),
      rdata = r_data
    )
    # Estimate Mplus model
    tryCatch({
      res <- mplusModeler(mod, modelout = paste0(thisdv, ".inp"), run = 1L)
    }, error = function(e){
      message("Model failed for variable ", thisdv)
    })
    fit <- SummaryTable(res, keepCols = c("Title", "LL", "Parameters", "AIC", "BIC", "RMSEA_Estimate", "CFI", "TLI"), type = "none")
    write.table(fit, "model_fits.csv", append = TRUE, sep = "\t", row.names = FALSE, col.names = FALSE)
    fit <- structure(list(Title = thisdv, LL = NA, Parameters = NA, 
                          AIC = NA, BIC = NA), row.names = c(NA, -1L), class = c("data.frame", 
                                                                                 "mplus.summaries"))
    write.table(fit, "model_fits.csv", append = TRUE, sep = "\t", row.names = FALSE, col.names = FALSE)
  }
  res <- readModels(paste0(thisdv, ".out"))
  save_data <- res$savedata
  # Check missingness
  tmp <- MissMech::TestMCARNormality(r_data[c(paste0("DV_", thisdv), use_these)])
  miss_mcar[[thisdv]] <- tmp$pvalcomb
  # Check N by country and wave
  n_by_country <- rep(NA, length(char_dict$country))
  tmp <- tapply(save_data$ID, save_data$COUNTRY, function(x){length(unique(x))})
  n_by_country[as.numeric(names(tmp))] <- tmp
  countries[[thisdv]] <- n_by_country
  tmz <- unique(df_long$time)
  n_by_wave <- rep(NA, length(tmz))
  names(n_by_wave) <- tmz
  tmp <- tapply(save_data$ID, save_data$TIME, function(x){length(unique(x))})
  n_by_wave[names(tmp)] <- tmp
  timepoints[[thisdv]] <- n_by_wave
  
  if(isTRUE(file.exists("long_id.RData"))){
    long_id <- readRDS("long_id.RData")
  } else {
    long_id <- NULL
  }
  long_id <- unique(c(long_id, unique(save_data$ID)))
  saveRDS(long_id, "long_id.RData")
  
  tab <- table_results(res, columns = NULL)
  if(!is.null(tab)){
    tab <- tab[grepl("^(T|S)", tab$param), ]
    for(i in 1:length(c(pred_time_var, pred_invar))){
      tab$param <- gsub(paste0("^S", i, "$"), c(pred_time_var, pred_invar)[i], tab$param)
    }
    tab <- tab[, c("paramheader", "param", "est_sig", "pval", "confint")]
    write.csv(tab, paste0("results_", thisdv, ".csv"), row.names = FALSE)
  }
}

# Table N by country and timepoint

countries <- data.frame(do.call(cbind, countries))
countries <- cbind(Country = names(char_dict$country)[as.numeric(rownames(countries))], countries)
write.csv(countries, "n_by_country.csv", row.names = FALSE)

timepoints <- do.call(cbind, timepoints)
timepoints <- cbind(wave_number = rownames(timepoints), timepoints)
write.csv(timepoints, "timepoints.csv", row.names = FALSE)

# Table descriptives by country
ids <- readRDS("long_id.RData")
df_desc <- df
df_desc$country <- names(char_dict$country)[df_desc$country]
df_desc$id <- 1:nrow(df_desc)
df_desc <- df_desc[df_desc$id %in% ids, ]

percs <- function(x){
  tb <- table(x)
  paste0(names(tb), " = ", formatC(prop.table(tb), digits = 2, format = "f"), "%")
}
max_n = max(sapply(df_desc[c("age_b", "education_b")], function(x){length(unique(x))}))

tab <- lapply(names(char_dict$country), function(thiscount){
  # thiscount = names(char_dict$country)[1]
  tmp <- df_desc[df_desc$country == thiscount, ]
  if(nrow(tmp) == 0) return(NULL)
  out <- list(
    country = thiscount,
    N = nrow(tmp),
    age = percs(tmp$age_b),
    gender = percs(tmp$gender_b),
    education = percs(tmp$education_b),
    religious = percs(tmp$religious_b)
  )
  sapply(out, `length<-`, 9)
})

tab <- do.call(rbind, tab)
write.csv(tab, "table1_descriptives.csv", row.names = FALSE)


f <- list.files(pattern = "^results.+?csv$")
f <- f[!f == "results_all.csv"]
out <- lapply(f, function(filenam){
  x <- read.csv(filenam, stringsAsFactors = FALSE)
  varname <- gsub("^results_(.+?)\\.csv$", "\\1", filenam)
  vs <- x[x$paramheader == "Variances", ]
  x <- merge(x[!x$paramheader == "Variances", ], vs, by = "param", all.x = TRUE)
  x[grep("paramheader", names(x))] <- NULL
  names(x) <- gsub("\\.x", "_mean", names(x))
  names(x) <- gsub("\\.y", "_var", names(x))
  names(x)[-1] <- paste0(varname, "_", names(x)[-1])
  x
})
f <- out
out <- f[[1]]
for(i in seq_along(f)[-1]){
  out <- merge(out, f[[i]], by = "param", all = TRUE)
}

write.csv(out, "results_all.csv", row.names = FALSE)
