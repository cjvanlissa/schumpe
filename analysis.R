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

# Load data
df <- read.spss("RMD10_Birga new_data_final4.sav", to.data.frame = TRUE, use.value.labels = TRUE)
df <- df[df$representative == "Yes", ]
names(df) <- tolower(names(df))
names(df) <- gsub("predicyot", "predictor", names(df))

# Rename political dimensions
names(df) <- gsub("political_view(?=[xy])", "polit", names(df), perl = TRUE)

# Use only harmonized
grep("harmoniz", names(df), value = TRUE)
vnames <- gsub("_harmonized$", "", names(df))
vnames <- gsub("(_b)?_harmonized(?=_)", "", vnames, perl = TRUE)
vnames
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
names(df)[dummy_vars]
sapply(df[dummy_vars], table)
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
table(apply(dat, 1, function(x){length(unique(na.omit(x)))}))
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
ICCs <- mult.icc(data.frame(df_wide[, longv]), grpid = df_wide$id)
names(ICCs)[1] <- "id"
desc$id <- gsub("_[bw]\\d{0,}$", "", desc$name)

desc <- merge(desc, ICCs, by = "id", all.x = TRUE)
desc <- desc[!startsWith(desc$name, "DV_"), ]
write.csv(desc, "descriptives.csv")

# Get the names of the waves
the_waves <- unique(df_long$time)

write.table(t(c("Title", "LL", "Parameters", "AIC", "BIC", "RMSEA_Estimate", 
              "CFI", "TLI")), "model_fits.csv", sep = "\t", row.names = FALSE, col.names = FALSE)


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
  
  pred_invar <- c(pred_invar, pred_time_var[make_invar])
  pred_time_var <- pred_time_var[keep_timevar]
  
  # Make plot of available data
  use_data <- t(sapply(c(thisdv, pred_time_var), function(x){
    the_waves %in% names(thisdv_preds[[x]])[names(thisdv_preds[[x]]) %in% use_waves]
  }))
  use_data[2:nrow(use_data), ncol(use_data)] <- FALSE
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
  
  df_anal[paste0("int_", pred_time_var)] <- lapply(df_anal[pred_time_var], function(x){x*df_anal$Dt})
  pred_time_var <- c("date", "Dt", pred_time_var, paste0("int_", pred_time_var))

  use_these <- c(pred_invar, pred_time_var, "tightness")
  use_these <- use_these[!use_these == "int_date"]
  df_anal <- df_anal[!is.na(df_anal$Dt), ]
  rename <- c("protest_containment_measures")
  names(rename) <- "contain_protest"
  for(i in 1:length(rename)){
    pred_time_var <- gsub(rename[i], names(rename)[i], pred_time_var)
    names(df_anal) <- gsub(rename[i], names(rename)[i], names(df_anal))
    thisdv <- gsub(rename[i], names(rename)[i], thisdv)
    use_these <- gsub(rename[i], names(rename)[i], use_these)
  }
  
# Make Mplus model object
  mod <- mplusObject(
    TITLE = thisdv,
    VARIABLE = paste0(c(
      "CLUSTER = country id;",
      paste0(c("WITHIN = ", pred_time_var, ";"), collapse = "\n"),
      paste0(c("BETWEEN = (country) tightness", paste0("(id) ", pred_invar)), collapse = "\n"), ";"), collapse = "\n"),
    ANALYSIS = "TYPE = THREELEVEL RANDOM;",
    MODEL = c(
      "%WITHIN%",
      paste0(paste0("DV_", thisdv), " ON ", c(pred_time_var, thisdv), ";"),
      "%BETWEEN id%",
      paste0(paste0("DV_", thisdv), " ON ", pred_invar, ";"),
      "%BETWEEN country%",
      paste0(paste0("DV_", thisdv), " ON ", "tightness", ";")),
    OUTPUT = "TECH1 TECH8 stdyx;",
    rdata = df_anal[c("id", "country", paste0("DV_", thisdv), use_these)]
  )
  # Estimate Mplus model
  res <- mplusModeler(mod, modelout = paste0(thisdv, ".inp"), run = 1L)
  
  tab <- table_results(res, columns = NULL)
  tab <- tab[tab$op == "~", c("param", "level", "est_sig", "pval", "confint")]
  tab$param <- c(pred_time_var, pred_invar, thisdv, "Dt", "tightness")[pmatch(tolower(tab$param), c(pred_time_var, pred_invar, thisdv, "Dt", "tightness"))]
  write.csv(tab, paste0("results_", thisdv, ".csv"), row.names = FALSE)
  
  fit <- SummaryTable(res, keepCols = c("Title", "LL", "Parameters", "AIC", "BIC", "RMSEA_Estimate", "CFI", "TLI"), type = "none")
  write.table(fit, "model_fits.csv", append = TRUE, sep = "\t", row.names = FALSE, col.names = FALSE)
}


}
# Tot hier


# DV_wash_hands
# /METHOD=ENTER Zcontrol_t1_measurement Zcontrol_gender Zcontrol_edu Zcontrol_employment 
# Zcontrol_religious Zcontrol_exposure
# /METHOD=ENTER Zpredictor_risk_perception Zpredictor_trust_gov Zpredictor_clear_message 
# Zpredictor_social_norm Zpredictor_age Zpredictor_preference_tightness.

# Drop cases with missing cluster 
df <- df[!is.na(df$country), ]

run_mod <- function(dv){
  # model <- paste('level: individual',
  #                paste0(dv, " ~ ", paste0(vars$predictor[!grepl("country", vars$predictor)], collapse = " + ")),
  #                'level: country',
  #                paste0(dv, " ~ ", paste0(vars$predictor[grepl("country", vars$predictor)], collapse = " + ")),
  #                sep = "\n"
  # )
  model_control <- paste('level: individual',
                         paste0(dv, " ~ ", paste0(
                           c(vars$predictor[!grepl("country", vars$predictor)], 
                             vars$control[-c(1:3)]), collapse = " + ")),
                         'level: country',
                         paste0(dv, " ~ ", paste0(vars$predictor[grepl("country", vars$predictor)], collapse = " + ")),
                         sep = "\n"
  )
  #res <- sem(model, data = df, cluster = "country")
  res_control <- sem(model_control, data = df, cluster = "country")
  write.csv(tidySEM::table_results(res_control, columns = NULL), file = paste0("results_", dv, ".csv"), row.names = FALSE)
}

out <- lapply(grep("t1$", vars$dv, value = TRUE), run_mod)

names(out) <- grep("t1$", vars$dv, value = TRUE)
fits <- sapply(out, `[[`, "fit")
tab <- lapply(names(out), function(i){
  tmp <- out[[i]][["res"]]
  names(tmp) <- paste0(i, "_", names(tmp))
  tmp
})
  
  do.call(cbind, lapply(names(out), function(i){
  tmp <- out[[i]]
  names(tmp) <- paste0(i, "_", names(tmp))
  tmp
}))
fit <- sem(model = model, data = df, cluster = "country")
summary(fit)

load("final24_04.Rda") #### LOAD THE ALREADY CLEANED DATA, If you load this you can skip to line 188

#merge the additional exploratory variables downloaded from Caspar's metadata

preparedness_indicators <- read.csv("additional datasets/preparedness_indicators.csv")
wdi <- read.csv("additional datasets/wdi.csv")
WHO_OECD_health_infrastructure <- read.csv("additional datasets/WHO_OECD_health_infrastructure.csv")

preparedness_indicators <- preparedness_indicators %>% select(Q4.1.2a, country, countryiso3)
wdi <- wdi %>% select(SI.POV.GINI_2018, NY.GDP.PCAP.PP.CD_2018, NY.GDP.PCAP.CD_2018, country, countryiso3)
WHO_OECD_health_infrastructure <- WHO_OECD_health_infrastructure %>% select(total_n_of_hospital_beds,	hospital_beds_per_1000, hospitals_per_1mil, country, countryiso3)

wdi <- wdi[!is.na(wdi$countryiso3), ]

final <- merge(x = final, y = preparedness_indicators, by = "countryiso3", all.x =TRUE)
final <- merge(x = final, y = wdi, by = "countryiso3", all.x = TRUE)
final <- merge(x = final, y = WHO_OECD_health_infrastructure, by = "countryiso3", all.x = TRUE)


summary(dt$coded_country)
cdata <- indi_full_names2 
cdata$country <- as.factor(cdata$country)
dt$country <- dt$coded_country  

mymergedata1 <- merge(x = dt, y = cdata, by = "country", all = TRUE)

final <- mymergedata1

final$trust_GPS_zscore <- as.numeric(final$trust_GPS_zscore)
final$Cooperation42s <- as.numeric(scale(final$Cooperation42, center = TRUE, scale = TRUE))
final$Belief42z <- as.numeric(scale(final$Beliefs42, center = TRUE, scale = TRUE))

#reverse code

final$avg_conf_armed_forcesR <- (5-final$avg_conf_armed_forces)
final$avg_conf_courtsR <- (5-final$avg_conf_courts)
final$avg_conf_governmentR <- (5-final$avg_conf_government)
final$avg_conf_parliamentR <- (5-final$avg_conf_parliament)
final$avg_conf_policeR <- (5-final$avg_conf_police)
final$avg_religious_attendanceR <- (8-final$avg_religious_attendance)
final$avg_importance_religionR <- (4-final$avg_importance_religion)

#zscore indicators

final$avg_conf_armed_forcesR_zscor <- as.numeric(scale(final$avg_conf_armed_forcesR, center = TRUE, scale = TRUE))
final$avg_conf_courtsR_zscor <- as.numeric(scale(final$avg_conf_courtsR, center = TRUE, scale = TRUE))
final$avg_conf_governmentR_zscor <- as.numeric(scale(final$avg_conf_governmentR, center = TRUE, scale = TRUE))
final$avg_conf_parliamentR_zscor <- as.numeric(scale(final$avg_conf_parliamentR, center = TRUE, scale = TRUE))
final$avg_conf_policeR_zscor <- as.numeric(scale(final$avg_conf_policeR, center = TRUE, scale = TRUE))

final$avg_importance_religionR_zscore <- as.numeric(scale(final$avg_importance_religionR, center = TRUE, scale = TRUE))
final$avg_religious_attendanceR_zscore <- as.numeric(scale(final$avg_religious_attendanceR, center = TRUE, scale = TRUE))
final$government_effectiveness_zscore <- as.numeric(scale(final$government_effectiveness, center = TRUE, scale = TRUE))
final$rule_of_law_zscore <- as.numeric(scale(final$rule_of_law, center = TRUE, scale = TRUE))

#z-scoring of additional indicators

final$prepa_hospitals_z <- as.numeric(scale(final$Q4.1.2a, center = TRUE, scale = TRUE))
final$GINI_2018_z <- as.numeric(scale(final$SI.POV.GINI_2018, center = TRUE, scale = TRUE))
final$GDP.PPP_2018_z <- as.numeric(scale(final$NY.GDP.PCAP.PP.CD_2018, center = TRUE, scale = TRUE))
final$GDP.CD_2018_z <- as.numeric(scale(final$NY.GDP.PCAP.CD_2018, center = TRUE, scale = TRUE))
final$total_n_of_hospital_beds_z <- as.numeric(scale(final$total_n_of_hospital_beds, center = TRUE, scale = TRUE))
final$hospital_beds_per_1000_z <- as.numeric(scale(final$hospital_beds_per_1000, center = TRUE, scale = TRUE))
final$hospitals_per_1mil_z <- as.numeric(scale(final$hospitals_per_1mil, center = TRUE, scale = TRUE))

#re-coding PsyCorona data (to range 1 to 7)

final$c19ProSo01 <- recode(final$c19ProSo01, '-3' = "1", '-2' = "2", '-1' = "3", '0' = "4", '1' = "5", '2' = "6", '3' = "7")
final$c19ProSo01 <- as.numeric(final$c19ProSo01)

final$c19ProSo02 <- recode(final$c19ProSo02, '-3' = "1", '-2' = "2", '-1' = "3", '0' = "4", '1' = "5", '2' = "6", '3' = "7")
final$c19ProSo02 <- as.numeric(final$c19ProSo02)

final$c19ProSo03 <- recode(final$c19ProSo03, '-3' = "1", '-2' = "2", '-1' = "3", '0' = "4", '1' = "5", '2' = "6", '3' = "7")
final$c19ProSo03 <- as.numeric(final$c19ProSo03)

final$c19ProSo04 <- recode(final$c19ProSo04, '-3' = "1", '-2' = "2", '-1' = "3", '0' = "4", '1' = "5", '2' = "6", '3' = "7")
final$c19ProSo04 <- as.numeric(final$c19ProSo04)


final$c19perBeh01 <- recode(final$c19perBeh01, '-3' = "1", '-2' = "2", '-1' = "3", '0' = "4", '1' = "5", '2' = "6", '3' = "7")
final$c19perBeh01 <- as.numeric(final$c19perBeh01)

final$c19perBeh02 <- recode(final$c19perBeh02, '-3' = "1", '-2' = "2", '-1' = "3", '0' = "4", '1' = "5", '2' = "6", '3' = "7")
final$c19perBeh02 <- as.numeric(final$c19perBeh02)

final$c19perBeh03 <- recode(final$c19perBeh03, '-3' = "1", '-2' = "2", '-1' = "3", '0' = "4", '1' = "5", '2' = "6", '3' = "7")
final$c19perBeh03 <- as.numeric(final$c19perBeh03)

final$c19RCA01 <- recode(final$c19RCA01, '-3' = "1", '-2' = "2", '-1' = "3", '0' = "4", '1' = "5", '2' = "6", '3' = "7")
final$c19RCA01 <- as.numeric(final$c19RCA01) 

final$c19RCA02 <- recode(final$c19RCA02, '-3' = "1", '-2' = "2", '-1' = "3", '0' = "4", '1' = "5", '2' = "6", '3' = "7")
final$c19RCA02 <- as.numeric(final$c19RCA02) 

final$c19RCA03 <- recode(final$c19RCA03, '-3' = "1", '-2' = "2", '-1' = "3", '0' = "4", '1' = "5", '2' = "6", '3' = "7")
final$c19RCA03 <- as.numeric(final$c19RCA03)

summary(final$c19ProSo03)

final$houseLeave <- as.numeric(final$houseLeave) #the higher the score greater number of times person left home

final$pros_mot <- (final$c19ProSo01+final$c19ProSo02+final$c19ProSo03+final$c19ProSo04)/4
final$pros_beh <- (final$c19perBeh01+final$c19perBeh02+final$c19perBeh03)/3
final$support <- (final$c19RCA01+final$c19RCA02+final$c19RCA03)/3

alpha(final$c19ProSo01,final$c19ProSo02,final$c19ProSo03,final$c19ProSo04)

#GOOGLE MOBILITY DATA
final$retail_recreation_2020.02.15
final$grocery_pharmacy_2020.02.15
final$grocery_pharmacy_2020.04.11
final$parks_2020.02.15
final$parks_2020.04.11
final$transit_stations_2020.02.15
final$transit_stations_2020.04.11

final$residential_2020.02.15
final$residential_2020.04.11


which( colnames(final)=="retail_recreation_2020.02.15" )
which( colnames(final)=="retail_recreation_2020.04.11" )

which( colnames(final)=="grocery_pharmacy_2020.02.15" )
which( colnames(final)=="grocery_pharmacy_2020.04.11" )

which( colnames(final)=="parks_2020.02.15" )
which( colnames(final)=="parks_2020.04.11" )

which( colnames(final)=="transit_stations_2020.02.15" )
which( colnames(final)=="transit_stations_2020.04.11" )

which( colnames(final)=="workplaces_2020.02.15" )
which( colnames(final)=="workplaces_2020.04.11" )

which( colnames(final)=="residential_2020.02.15" )
which( colnames(final)=="residential_2020.04.11" )

final$retail_diff <- (apply(final[,2895:2951], 1, FUN=min))  # difference between the least mobility minus 0
final$grocery_pharm_diff <- (apply(final[,2952:3008], 1, FUN=min))  # difference between the least mobility minus 0
final$park_diff <- (apply(final[,3009:3065], 1, FUN=min))  # difference between the least mobility minus 0
final$transit_diff <- (apply(final[,3066:3122], 1, FUN=min))  # difference between the least mobility minus 0
final$workplace_diff <- (apply(final[,3123:3179], 1, FUN=min))  # difference between the least mobility minus 0
final$residential <- (apply(final[,3180:3236], 1, FUN=min))  # difference between the highest residential increase minus 0





summary(final$retail_diff)

summary(final$pros_mot)
summary(final$pros_beh)
summary(final$support)

#models

final$country <- final$country.x
df <- as.data.frame.table(table(final$country))
tmp <- as.data.frame.table(tapply(final$pros_mot, factor(final$country), function(x){sd(x, na.rm = T)}))
df <- merge(df, tmp, by =  "Var1")
df <- df[df$Freq.x > 50, ]
final <- final[final$country %in% df$Var1, ]
tmp <- tapply(final$pros_mot, factor(final$country), function(x){sd(x, na.rm = T)})
plot(density(tmp))
df[df$Freq.y > .2, ]
cor(df[2:3])
### clean for stringency analysis

final$max_stringencyz <-as.numeric(scale(final$max_stringency))
final$string_ratioz <- scale(final$max_stringency/(final$max_cases/1000000),center = TRUE, scale = TRUE)

final_short <- final[!duplicated(final[3]),] ##create a country-level dataset
final$housestay <- 5- final$houseLeave

###STRINGENCY
##PLEASE NOTE: FINAL SHORT IS A DATABASE WHERE THERE IS ONE ROW PER COUNTRY: WE USED THIS WHEN WE HAD COUNTRY-LEVEL AS BOTH DV and IV
final_short <- final[!duplicated(final[3]),] ##create a country-level dataset

final_short$string_ratio <- final_short$max_stringency/(final_short$max_cases/1000000)
final_short$string_ratioz <- as.numeric(scale(final_short$max_stringency/(final_short$max_cases/1000000),center = TRUE, scale = TRUE))

m0_coop <- lm(string_ratioz~Cooperation42s,  data= final_short)
m0_trust <- lm(string_ratioz~trust_GPS_zscore, data= final_short)
m0_belief <- lm(string_ratioz~Belief42z, data= final_short) 

m0_coop_ <- lm(max_stringencyz~Cooperation42s,  data= final_short)
m0_trust_ <- lm(max_stringencyz~trust_GPS_zscore, data= final_short)
m0_belief_ <- lm(max_stringencyz~Belief42z, data= final_short) 

#### PROSOCIAL MOTIVATIONS RESULTS. IN ALL MODELS WE SELECT CASES WHERE THERE ARE AT LEAST 30 OBSERVATIONS PER COUNTRY

###no interaction models
m1_coop_noint <- lmer(pros_mot~Cooperation42s+severityz  +(1|country),  data= final[ final$country %in%  names(table(final$country))[table(final$country) >30] , ])
summary(m1_coop_noint)

df <- final[ final$country %in%  names(table(final$country))[table(final$country) >30] , c("pros_mot", "Cooperation42s", "severityz", "country")]

model <- ' 
  level: individual
    pros_mot ~1
  level: country
    pros_mot ~ Cooperation42s + severityz
'
fit <- sem(model = model, data = df, cluster = "country")
summary(fit)

m1_trust_noint <- lmer(pros_mot~ trust_GPS_zscore+severityz +(1|country), data= final[ final$country %in%  names(table(final$country))[table(final$country) >30] , ])
summary(m1_trust_noint)
df <- final[ final$country %in%  names(table(final$country))[table(final$country) >30] , c("pros_mot", "trust_GPS_zscore", "severityz", "country")]

model <- ' 
  level: individual
    pros_mot ~1
  level: country
    pros_mot ~ trust_GPS_zscore + severityz
'
fit <- sem(model = model, data = df, cluster = "country")
summary(fit)


m1_belief_noint <- lmer(pros_mot~ Beliefs42+severityz +(1|country), data= final[ final$country %in%  names(table(final$country))[table(final$country) >30] , ])
summary(m1_belief_noint)
df <- final[ final$country %in%  names(table(final$country))[table(final$country) >30] , c("pros_mot", "Beliefs42", "severityz", "country")]

model <- ' 
  level: individual
    pros_mot ~1
  level: country
    pros_mot ~ Beliefs42 + severityz
'
fit <- sem(model = model, data = df, cluster = "country")
summary(fit)

tapply(data$pros_mot, factor(data$country), sd)

## models with interactions with interaction
m1_coop <- lmer(pros_mot~Cooperation42s*max_stringencyz+severityz  +(1|country),  data= final[ final$country %in%  names(table(final$country))[table(final$country) >30] , ])
m1_trust <- lmer(pros_mot~ trust_GPS_zscore*max_stringencyz+severityz +(1|country), data= final[ final$country %in%  names(table(final$country))[table(final$country) >30] , ])
m1_belief <- lmer(pros_mot~ Belief42z*max_stringencyz+severityz +(1|country), data= final[ final$country %in%  names(table(final$country))[table(final$country) >30] , ])

m1_gov <- lmer(pros_mot~government_effectiveness_zscore*max_stringencyz+severityz +(1|country), data= final[ final$country %in%  names(table(final$country))[table(final$country) >30] , ])
m1_rol <- lmer(pros_mot~rule_of_law_zscore*max_stringencyz+severityz +(1|country), data= final[ final$country %in%  names(table(final$country))[table(final$country) >30] , ])

m1_conf_armed <- lmer(pros_mot~avg_conf_armed_forcesR_zscor*max_stringencyz+severityz +(1|country), data= final[ final$country %in%  names(table(final$country))[table(final$country) >30] , ])
m1_conf_court <- lmer(pros_mot~avg_conf_courtsR_zscor*max_stringencyz+severityz +(1|country), data= final[ final$country %in%  names(table(final$country))[table(final$country) >30] , ])
m1_conf_gov <- lmer(pros_mot~avg_conf_governmentR_zscor*max_stringencyz+severityz +(1|country), data= final[ final$country %in%  names(table(final$country))[table(final$country) >30] , ])
m1_conf_parl <- lmer(pros_mot~avg_conf_parliamentR_zscor*max_stringencyz+severityz +(1|country), data= final[ final$country %in%  names(table(final$country))[table(final$country) >30] , ])
m1_conf_pol <- lmer(pros_mot~avg_conf_policeR_zscor*max_stringencyz+severityz +(1|country), data= final[ final$country %in%  names(table(final$country))[table(final$country) >30] , ])

m1_pathog <- lmer(pros_mot~historical_prevalence_pathogens_zscore*max_stringencyz+severityz +(1|country), data= final[ final$country %in%  names(table(final$country))[table(final$country) >30] , ])

m1_church <- lmer(pros_mot~ChurchExpWest_zscore*max_stringencyz+severityz +(1|country), data= final[ final$country %in%  names(table(final$country))[table(final$country) >30] , ])
m1_rel1 <- lmer(pros_mot~ avg_religious_attendanceR_zscore*max_stringencyz+severityz+(1|country), data= final[ final$country %in%  names(table(final$country))[table(final$country) >30] , ])
m1_rel2 <- lmer(pros_mot~ avg_importance_religionR_zscore*max_stringencyz+severityz+(1|country), data= final[ final$country %in%  names(table(final$country))[table(final$country) >30] , ])

### PROSOCIAL BEHAVIORS 

##no interaction models

m2_coop_noint <- lmer(pros_beh~Cooperation42s  +(1|country),  data= final[ final$country %in%  names(table(final$country))[table(final$country) >30] , ])
m2_trust_noint <- lmer(pros_beh~ trust_GPS_zscore +(1|country), data= final[ final$country %in%  names(table(final$country))[table(final$country) >30] , ])
m2_belief_noint <- lmer(pros_beh~ Beliefs42 +(1|country), data= final[ final$country %in%  names(table(final$country))[table(final$country) >30] , ])

##interaction models

m2_coop <- lmer(pros_beh~Cooperation42s*max_stringencyz  +severityz+(1|country),  data= final[ final$country %in%  names(table(final$country))[table(final$country) >30] , ])
m2_trust <- lmer(pros_beh~ trust_GPS_zscore*max_stringencyz+severityz +(1|country), data= final[ final$country %in%  names(table(final$country))[table(final$country) >30] , ])
m2_belief <- lmer(pros_beh~ Belief42z*max_stringencyz+severityz +(1|country), data= final[ final$country %in%  names(table(final$country))[table(final$country) >30] , ])
m2_church <- lmer(pros_beh~ChurchExpWest_zscore*max_stringencyz+severityz +(1|country), data= final[ final$country %in%  names(table(final$country))[table(final$country) >30] , ])
m2_gov <- lmer(pros_beh~government_effectiveness_zscore*max_stringencyz+severityz +(1|country), data= final[ final$country %in%  names(table(final$country))[table(final$country) >30] , ])
m2_rol <- lmer(pros_beh~rule_of_law_zscore*max_stringencyz+severityz +(1|country), data= final[ final$country %in%  names(table(final$country))[table(final$country) >30] , ])
m2_conf_armed <- lmer(pros_beh~avg_conf_armed_forcesR_zscor*max_stringencyz+severityz +(1|country), data= final[ final$country %in%  names(table(final$country))[table(final$country) >30] , ])
m2_conf_court <- lmer(pros_beh~avg_conf_courtsR_zscor*max_stringencyz+severityz +(1|country), data= final[ final$country %in%  names(table(final$country))[table(final$country) >30] , ])
m2_conf_gov <- lmer(pros_beh~avg_conf_governmentR_zscor*max_stringencyz+severityz +(1|country), data= final[ final$country %in%  names(table(final$country))[table(final$country) >30] , ])
m2_conf_parl <- lmer(pros_beh~avg_conf_parliamentR_zscor*max_stringencyz+severityz +(1|country), data= final[ final$country %in%  names(table(final$country))[table(final$country) >30] , ])
m2_conf_pol <- lmer(pros_beh~avg_conf_policeR_zscor*max_stringencyz+severityz +(1|country), data= final[ final$country %in%  names(table(final$country))[table(final$country) >30] , ])
m2_pathog <- lmer(pros_beh~historical_prevalence_pathogens_zscore*max_stringencyz+severityz +(1|country), data= final[ final$country %in%  names(table(final$country))[table(final$country) >30] , ])
m2_rel1 <- lmer(pros_beh~ avg_religious_attendanceR_zscore*max_stringencyz+severityz+(1|country), data= final[ final$country %in%  names(table(final$country))[table(final$country) >30] , ])
m2_rel2 <- lmer(pros_beh~ avg_importance_religionR_zscore*max_stringencyz+severityz+(1|country), data= final[ final$country %in%  names(table(final$country))[table(final$country) >30] , ])

#### BEHAVIORS THAT SUPPORT MONITORING SYSTEMS 

## no interaction model
m3_coop_noint <- lmer(support~Cooperation42s  +(1|country),  data= final[ final$country %in%  names(table(final$country))[table(final$country) >30] , ])
m3_trust_noint <- lmer(support~ trust_GPS_zscore +(1|country), data= final[ final$country %in%  names(table(final$country))[table(final$country) >30] , ])
m3_belief_noint <- lmer(support~ Beliefs42 +(1|country), data= final[ final$country %in%  names(table(final$country))[table(final$country) >30] , ])

## model with interaction
m3_coop <- lmer(support~Cooperation42s*max_stringencyz+severityz  +(1|country),  data= final[ final$country %in%  names(table(final$country))[table(final$country) >30] , ])
m3_trust <- lmer(support~ trust_GPS_zscore*max_stringencyz+severityz +(1|country), data= final[ final$country %in%  names(table(final$country))[table(final$country) >30] , ])
m3_belief <- lmer(support~ Belief42z*max_stringencyz +severityz+(1|country), data= final[ final$country %in%  names(table(final$country))[table(final$country) >30] , ])
m3_church <- lmer(support~ChurchExpWest_zscore*max_stringencyz+severityz +(1|country), data= final[ final$country %in%  names(table(final$country))[table(final$country) >30] , ])
m3_gov <- lmer(support~government_effectiveness_zscore*max_stringencyz+severityz +(1|country), data= final[ final$country %in%  names(table(final$country))[table(final$country) >30] , ])
m3_rol <- lmer(support~rule_of_law_zscore*max_stringencyz+severityz +(1|country), data= final[ final$country %in%  names(table(final$country))[table(final$country) >30] , ])
m3_conf_armed <- lmer(support~avg_conf_armed_forcesR_zscor*max_stringencyz+severityz +(1|country), data= final[ final$country %in%  names(table(final$country))[table(final$country) >30] , ])
m3_conf_court <- lmer(support~avg_conf_courtsR_zscor*max_stringencyz+severityz +(1|country), data= final[ final$country %in%  names(table(final$country))[table(final$country) >30] , ])
m3_conf_gov <- lmer(support~avg_conf_governmentR_zscor*max_stringencyz+severityz +(1|country), data= final[ final$country %in%  names(table(final$country))[table(final$country) >30] , ])
m3_conf_parl <- lmer(support~avg_conf_parliamentR_zscor*max_stringencyz+severityz +(1|country), data= final[ final$country %in%  names(table(final$country))[table(final$country) >30] , ])
m3_conf_pol <- lmer(support~avg_conf_policeR_zscor*max_stringencyz+severityz +(1|country), data= final[ final$country %in%  names(table(final$country))[table(final$country) >30] , ])
m3_pathog <- lmer(support~historical_prevalence_pathogens_zscore*max_stringencyz+severityz +(1|country), data= final[ final$country %in%  names(table(final$country))[table(final$country) >30] , ])
m3_rel1 <- lmer(support~ avg_religious_attendanceR_zscore*max_stringencyz+severityz+(1|country), data= final[ final$country %in%  names(table(final$country))[table(final$country) >30] , ])
m3_rel2 <- lmer(support~ avg_importance_religionR_zscore*max_stringencyz+severityz+(1|country), data= final[ final$country %in%  names(table(final$country))[table(final$country) >30] , ])

##### HOUSE STAY VARIABLE

#nointeraction models
m4_coop_noint <- lmer(housestay~Cooperation42s  +(1|country),  data= final[ final$country %in%  names(table(final$country))[table(final$country) >30] , ])
m4_trust_noint <- lmer(housestay~ trust_GPS_zscore +(1|country), data= final[ final$country %in%  names(table(final$country))[table(final$country) >30] , ])
m4_belief_noint <- lmer(housestay~ Belief42z +(1|country), data= final[ final$country %in%  names(table(final$country))[table(final$country) >30] , ])
summary(m4_belief_noint)

######################


m4_coop <- lmer(housestay~Cooperation42s*max_stringencyz+severityz  +(1|country),  data= final[ final$country %in%  names(table(final$country))[table(final$country) >30] , ])
m4_trust <- lmer(housestay~ trust_GPS_zscore*max_stringencyz+severityz +(1|country), data= final[ final$country %in%  names(table(final$country))[table(final$country) >30] , ])
m4_belief <- lmer(housestay~ Belief42z*max_stringencyz+severityz +(1|country), data= final[ final$country %in%  names(table(final$country))[table(final$country) >30] , ])
m4_church <- lmer(housestay~ChurchExpWest_zscore*max_stringencyz+severityz +(1|country), data= final[ final$country %in%  names(table(final$country))[table(final$country) >30] , ])
m4_gov <- lmer(housestay~government_effectiveness_zscore*max_stringencyz+severityz +(1|country), data= final[ final$country %in%  names(table(final$country))[table(final$country) >30] , ])
m4_rol <- lmer(housestay~rule_of_law_zscore*max_stringencyz+severityz +(1|country), data= final[ final$country %in%  names(table(final$country))[table(final$country) >30] , ])
m4_conf_armed <- lmer(housestay~avg_conf_armed_forcesR_zscor*max_stringencyz+severityz +(1|country), data= final[ final$country %in%  names(table(final$country))[table(final$country) >30] , ])
m4_conf_court <- lmer(housestay~avg_conf_courtsR_zscor*max_stringencyz+severityz +(1|country), data= final[ final$country %in%  names(table(final$country))[table(final$country) >30] , ])
m4_conf_gov <- lmer(housestay~avg_conf_governmentR_zscor*max_stringencyz+severityz +(1|country), data= final[ final$country %in%  names(table(final$country))[table(final$country) >30] , ])
m4_conf_parl <- lmer(housestay~avg_conf_parliamentR_zscor*max_stringencyz+severityz +(1|country), data= final[ final$country %in%  names(table(final$country))[table(final$country) >30] , ])
m4_conf_pol <- lmer(housestay~avg_conf_policeR_zscor*max_stringencyz+severityz +(1|country), data= final[ final$country %in%  names(table(final$country))[table(final$country) >30] , ])
m4_pathog <- lmer(housestay~historical_prevalence_pathogens_zscore*max_stringencyz+severityz +(1|country), data= final[ final$country %in%  names(table(final$country))[table(final$country) >30] , ])
m4_rel1 <- lmer(housestay~ avg_religious_attendanceR_zscore*max_stringencyz+severityz+(1|country), data= final[ final$country %in%  names(table(final$country))[table(final$country) >30] , ])
m4_rel2 <- lmer(housestay~ avg_importance_religionR_zscore*max_stringencyz+severityz+(1|country), data= final[ final$country %in%  names(table(final$country))[table(final$country) >30] , ])




###################################################
############## ADDITIONAL ANALYSES ################
###################################################
#final$country <- final$country.x


##############   What is the association between stringency and prosocial motivations and behaviors? 
#final$max_stringencyz <- as.numeric(scale(final$max_stringency))

m5_1 <- lmer(housestay~string_ratioz  +(1|country),  data= final[ final$country %in%  names(table(final$country))[table(final$country) >30] , ])
m5_2 <- lmer(support~string_ratioz +(1|country), data= final[ final$country %in%  names(table(final$country))[table(final$country) >30] , ])
m5_3 <- lmer(pros_beh~string_ratioz +(1|country), data= final[ final$country %in%  names(table(final$country))[table(final$country) >30] , ])
m5_4 <- lmer(pros_mot~string_ratioz+(1|country), data= final[ final$country %in%  names(table(final$country))[table(final$country) >30] , ])


summary(m5_1)
summary(m5_2)
summary(m5_3)
summary(m5_4)

m5_1_ <- lmer(housestay~max_stringencyz  +(1|country),  data= final[ final$country %in%  names(table(final$country))[table(final$country) >30] , ])
m5_2_ <- lmer(support~max_stringencyz +(1|country), data= final[ final$country %in%  names(table(final$country))[table(final$country) >30] , ])
m5_3_ <- lmer(pros_beh~max_stringencyz +(1|country), data= final[ final$country %in%  names(table(final$country))[table(final$country) >30] , ])
m5_4_ <- lmer(pros_mot~max_stringencyz+(1|country), data= final[ final$country %in%  names(table(final$country))[table(final$country) >30] , ])



summary(m5_1_)
summary(m5_3_)
summary(m5_4_)
summary(m5_2_) 


##############   How much variation exists across societies in motivations and behaviors?

m6_1 <- lmer(housestay~1  +(1|country),  data= final[ final$country %in%  names(table(final$country))[table(final$country) >30] , ])
m6_2 <- lmer(support~1 +(1|country), data= final[ final$country %in%  names(table(final$country))[table(final$country) >30] , ])
m6_3 <- lmer(pros_beh~1 +(1|country), data= final[ final$country %in%  names(table(final$country))[table(final$country) >30] , ])
m6_4 <- lmer(pros_mot~1+(1|country), data= final[ final$country %in%  names(table(final$country))[table(final$country) >30] , ])


summary(m6_3__)
dotplot(ranef(m6_1,TRUE))
dotplot(ranef(m6_2,TRUE))
dotplot(ranef(m6_3,TRUE))
dotplot(ranef(m6_4,TRUE))

summary(m6_4)
dotplot(ranef(m6_4,TRUE)) ### analisi effetti random, se vanno a 0, indipendenza predittori dallo 0 

##############   Can we see the sample size per country?

final2 <- final[ final$country %in%  names(table(final$country))[table(final$country) >30] , ]
summary(final2$country)

##############   Do prosocial motivations positively correlate with prosocial behaviors for participants?

m7 <- lmer(pros_beh~pros_mot +(1|country), data= final[ final$country %in%  names(table(final$country))[table(final$country) >30] , ])
cor.test(final$pros_beh,final$pros_mot)
summary(m7)


##############   GDP, GINI, hospital beds and prosocial behaviors and motivations
m8_3 <- lmer(pros_beh~pros_mot*max_stringencyz +(1|country), data= final[ final$country %in%  names(table(final$country))[table(final$country) >30] , ])

m8_1 <- lmer(housestay~Q4.1.2az  +(1|country),  data= final[ final$country %in%  names(table(final$country))[table(final$country) >30] , ])
m8_2 <- lmer(support~Q4.1.2az +(1|country), data= final[ final$country %in%  names(table(final$country))[table(final$country) >30] , ])
m8_3 <- lmer(pros_beh~Q4.1.2az +(1|country), data= final[ final$country %in%  names(table(final$country))[table(final$country) >30] , ])
m8_4 <- lmer(pros_mot~Q4.1.2az+(1|country), data= final[ final$country %in%  names(table(final$country))[table(final$country) >30] , ])

m9_1 <- lmer(housestay~NY.GDP.PCAP.PP.CD_2018z  +(1|country),  data= final[ final$country %in%  names(table(final$country))[table(final$country) >30] , ])
m9_2 <- lmer(support~NY.GDP.PCAP.PP.CD_2018z +(1|country), data= final[ final$country %in%  names(table(final$country))[table(final$country) >30] , ])
m9_3 <- lmer(pros_beh~NY.GDP.PCAP.PP.CD_2018z +(1|country), data= final[ final$country %in%  names(table(final$country))[table(final$country) >30] , ])
m9_4 <- lmer(pros_mot~NY.GDP.PCAP.PP.CD_2018z+(1|country), data= final[ final$country %in%  names(table(final$country))[table(final$country) >30] , ])

m10_1 <- lmer(housestay~SI.POV.GINI_2018z  +(1|country),  data= final[ final$country %in%  names(table(final$country))[table(final$country) >30] , ])
m10_2 <- lmer(support~SI.POV.GINI_2018z +(1|country), data= final[ final$country %in%  names(table(final$country))[table(final$country) >30] , ])
m10_3 <- lmer(pros_beh~SI.POV.GINI_2018z +(1|country), data= final[ final$country %in%  names(table(final$country))[table(final$country) >30] , ])
m10_4 <- lmer(pros_mot~SI.POV.GINI_2018z +(1|country), data= final[ final$country %in%  names(table(final$country))[table(final$country) >30] , ])

m11_1 <- lmer(housestay~hospital_beds_per_1000  +(1|country),  data= final[ final$country %in%  names(table(final$country))[table(final$country) >30] , ])
m11_2 <- lmer(support~hospital_beds_per_1000 +(1|country), data= final[ final$country %in%  names(table(final$country))[table(final$country) >30] , ])
m11_3 <- lmer(pros_beh~hospital_beds_per_1000 +(1|country), data= final[ final$country %in%  names(table(final$country))[table(final$country) >30] , ])
m11_4 <- lmer(pros_mot~hospital_beds_per_1000+(1|country), data= final[ final$country %in%  names(table(final$country))[table(final$country) >30] , ])


m12a <- lm(Q4.1.2az~government_effectiveness_zscore, data= final_short)
m12b <- lm(NY.GDP.PCAP.PP.CD_2018z~government_effectiveness_zscore, data= final_short)
m13a<- lm(Q4.1.2az~rule_of_law_zscore, data= final_short)
m13b <- lm(NY.GDP.PCAP.PP.CD_2018z~rule_of_law_zscore, data= final_short)
m14a<- lm(SI.POV.GINI_2018z~government_effectiveness_zscore, data= final_short)
m14b <- lm(SI.POV.GINI_2018z~rule_of_law_zscore, data= final_short)


#final$SI.POV.GINI_2018z <- as.numeric(scale(final$SI.POV.GINI_2018))
#final$NY.GDP.PCAP.PP.CD_2018z <- as.numeric(scale(final$NY.GDP.PCAP.PP.CD_2018))
#final$Q4.1.2az <- as.numeric(scale(final$Q4.1.2a))

m15a <- lm(max_stringencyz~SI.POV.GINI_2018z, data= final_short)
m15b <- lm(max_stringencyz~Q4.1.2az, data= final_short)
m15c <- lm(max_stringencyz~NY.GDP.PCAP.PP.CD_2018z, data= final_short)



#################GOOGLE MOBILITY

#z-scoring

final_short$retail_diff_Z <- as.numeric(scale(final_short$retail_diff,center = TRUE, scale = TRUE))
final_short$grocery_pharm_diff_Z  <- as.numeric(scale(final_short$grocery_pharm_diff,center = TRUE, scale = TRUE))
final_short$park_diff_Z <- as.numeric(scale(final_short$park_diff,center = TRUE, scale = TRUE) )
final_short$transit_diff_Z <- as.numeric(scale(final_short$transit_diff,center = TRUE, scale = TRUE))
final_short$workplace_diff_Z  <- as.numeric(scale(final_short$workplace_diff,center = TRUE, scale = TRUE))
final_short$residential_Z  <- as.numeric(scale(final_short$residential,center = TRUE, scale = TRUE))

# create a mean of the google mobility items (all except of residential) (mobility mean)
final_short$mobility_mean <- (final_short$retail_diff_Z + final_short$grocery_pharm_diff_Z + final_short$park_diff_Z + final_short$transit_diff_Z + final_short$workplace_diff_Z)/5

final_short$mobility_mean <- as.numeric(final_short$mobility_mean)

#reverse-code mobility mean, so that high scores mean less mobility recorded in places of public interest
final_short$mobility_mean <- (final_short$retail_diff + final_short$grocery_pharm_diff + final_short$park_diff + final_short$transit_diff + final_short$workplace_diff)/5
final_short$mobility_meanR <- (final_short$mobility_mean*(-1))

#z-score mobility mean

final_short$mobility_meanR_Z <- as.numeric(scale(final_short$mobility_meanR, center = TRUE, scale = TRUE))

#models with the mobility data separated for places of interest

mg_coop_ret <- lm(retail_diff_Z~Cooperation42*string_ratioz,  data= final_short)
mg_trust_ret <- lm(retail_diff_Z~trust_GPS_zscore*string_ratioz, data= final_short)
mg_belief_ret <- lm(retail_diff_Z~Beliefs42*string_ratioz, data= final_short)

summary(mg_coop_ret)
summary(mg_trust_ret)
summary(mg_belief_ret)

mg_coop_groc <- lm(grocery_pharm_diff_Z~Cooperation42*string_ratioz,  data= final_short)
mg_trust_groc <- lm(grocery_pharm_diff_Z~trust_GPS_zscore*string_ratioz, data= final_short)
mg_belief_groc <- lm(grocery_pharm_diff_Z~Beliefs42*string_ratioz, data= final_short) 


summary(mg_coop_groc)
summary(mg_trust_groc)
summary(mg_belief_groc)

mg_coop_park <- lm(park_diff_Z~Cooperation42*string_ratioz,  data= final_short)
mg_trust_park <- lm(park_diff_Z~trust_GPS_zscore*string_ratioz, data= final_short)
mg_belief_park <- lm(park_diff_Z~Beliefs42*string_ratioz, data= final_short) 

summary(mg_coop_park)
summary(mg_trust_park)
summary(mg_belief_park)


mg_coop_trans <- lm(transit_diff_Z~Cooperation42*string_ratioz,  data= final_short)
mg_trust_trans <- lm(transit_diff_Z~trust_GPS_zscore*string_ratioz, data= final_short)
mg_belief_trans <- lm(transit_diff_Z~Beliefs42*string_ratioz, data= final_short) ###  

summary(mg_coop_trans)
summary(mg_trust_trans)
summary(mg_belief_trans)

mg_coop_work <- lm(workplace_diff_Z~Cooperation42*string_ratioz,  data= final_short)
mg_trust_work <- lm(workplace_diff_Z~trust_GPS_zscore*string_ratioz, data= final_short)
mg_belief_work <- lm(workplace_diff_Z~Beliefs42*string_ratioz, data= final_short) 

summary(mg_coop_work)
summary(mg_trust_work)
summary(mg_belief_work)

#models using residential traffic as DV (Mobility trends for places of residence)

mg_coop_resnoint <- lm(residential_Z~Cooperation42s,  data= final_short) ### RESIDENTIAL MOBILITY
mg_trust_resnoint <- lm(residential_Z~trust_GPS_zscore,  data= final_short) ### RESIDENTIAL MOBILITY
mg_belief_resnoint <- lm(residential_Z~Beliefs42,  data= final_short) ### RESIDENTIAL MOBILITY

final$Cooperation42 <- as.numeric(final$Cooperation42)
summary(mg_coop_resnoint)
plot_model(mg_coop_resnoint, type="eff")

mg_coop_res <- lm(residential_Z~string_ratioz*Cooperation42,  data= final_short) ### RESIDENTIAL MOBILITY
mg_trust_res <- lm(residential_Z~string_ratioz*trust_GPS_zscore, data= final_short)
mg_belief_res <- lm(residential_Z~string_ratioz*Beliefs42, data= final_short) 
mg_church <- lm(residential_Z~string_ratioz*ChurchExpWest_zscore,data=final_short )
mg_gov <- lm(residential_Z~string_ratioz*government_effectiveness_zscore,data=final_short )
mg_rol <- lm(residential_Z~string_ratioz*rule_of_law_zscore, data=final_short )
mg_conf_armed <- lm(residential_Z~string_ratioz*avg_conf_armed_forcesR_zscor, data=final_short )
mg_conf_court <- lm(residential_Z~string_ratioz*avg_conf_courtsR_zscor, data=final_short )
mg_conf_gov <- lm(residential_Z~string_ratioz*avg_conf_governmentR_zscor,data=final_short )
mg_conf_parl <- lm(residential_Z~string_ratioz*avg_conf_parliamentR_zscor, data=final_short )
mg_conf_pol <- lm(residential_Z~string_ratioz*avg_conf_policeR_zscor, data=final_short )
mg_pathog <- lm(residential_Z~string_ratioz*historical_prevalence_pathogens_zscore, data=final_short)
mg_rel1 <- lm(residential_Z~string_ratioz*avg_religious_attendanceR_zscore, data=final_short)
mg_rel2 <- lm(residential_Z~string_ratioz*avg_importance_religionR_zscore,data=final_short)

summary(mg_coop_res)
summary(mg_trust_res)
summary(mg_belief_res)
summary(mg_church)
summary(mg_gov)
summary(mg_rol)
summary(mg_conf_armed)
summary(mg_pathog) ## 0.05
plot_model(mg_pathog, type="eff")$historical_prevalence_pathogens_zscore
plot_model(mg_pathog, type="int")
summary(mg_conf_court)
summary(mg_conf_gov) #####
plot_model(mg_conf_gov, type = "pred", terms = c( "string_ratioz","avg_conf_governmentR_zscor"))

summary(final_short$string_ratioz)

summary(mg_conf_parl) #####
summary(mg_conf_pol)
summary(mg_rel1)
summary(mg_rel2)
plot_model(mg_rel2, type = "pred", terms = c("avg_importance_religionR_zscore", "string_ratioz"))

mg_coop_avg_noint <- lm(mobility_meanR_Z~Cooperation42s,  data= final_short) 
mg_trust_avg_noint <- lm(mobility_meanR_Z~trust_GPS_zscore, data= final_short)
mg_belief_avg_noint <- lm(mobility_meanR_Z~Beliefs42, data= final_short) 

summary(mg_belief_avg_noint)

mg_coop_avg <- lm(mobility_meanR_Z~string_ratioz*Cooperation42,  data= final_short) 
mg_trust_avg <- lm(mobility_meanR_Z~string_ratioz*trust_GPS_zscore, data= final_short)
mg_belief_avg <- lm(mobility_meanR_Z~string_ratioz*Beliefs42, data= final_short) 
mg_church_avg <- lm(mobility_meanR_Z~string_ratioz*ChurchExpWest_zscore,data=final_short )
mg_gov_avg <- lm(mobility_meanR_Z~string_ratioz*government_effectiveness_zscore,data=final_short )
mg_rol_avg <- lm(mobility_meanR_Z~string_ratioz*rule_of_law_zscore, data=final_short )
mg_conf_armed_avg <- lm(mobility_meanR_Z~string_ratioz*avg_conf_armed_forcesR_zscor, data=final_short )
mg_conf_court_avg <- lm(mobility_meanR_Z~string_ratioz*avg_conf_courtsR_zscor, data=final_short )
mg_conf_gov_avg <- lm(mobility_meanR_Z~string_ratioz*avg_conf_governmentR_zscor,data=final_short )
mg_conf_parl_avg <- lm(mobility_meanR_Z~string_ratioz*avg_conf_parliamentR_zscor, data=final_short )
mg_conf_pol_avg <- lm(mobility_meanR_Z~string_ratioz*avg_conf_policeR_zscor, data=final_short )
mg_pathog_avg <- lm(mobility_meanR_Z~string_ratioz*historical_prevalence_pathogens_zscore, data=final_short)
mg_rel1_avg <- lm(mobility_meanR_Z~string_ratioz*avg_religious_attendanceR_zscore, data=final_short)
mg_rel2_avg <- lm(mobility_meanR_Z~string_ratioz*avg_importance_religionR_zscore,data=final_short)

summary(mg_coop_avg )
summary(mg_trust_avg ) 
summary(mg_belief_avg ) ##  7percent
summary(mg_church_avg ) # 3percent
summary(mg_gov_avg ) # six percent

plot_model(mg_gov_avg, type = "int")
summary(mg_rol_avg ) # 8 percent
plot_model(mg_rol_avg, type = "int")

summary(mg_conf_armed_avg )
summary(mg_conf_court_avg )
summary(mg_conf_gov_avg ) 
summary(mg_conf_parl_avg )
summary(mg_pathog_avg) ### 1percent
plot_model(mg_pathog_avg, type="int")
summary(mg_conf_pol_avg )
summary(mg_rel1_avg )
summary(mg_rel2_avg )


final_short$residential_Z <- as.numeric(final_short$residential_Z)
final_short$avg_conf_governmentR_zscor <- as.numeric(final_short$avg_conf_governmentR_zscor)
final_short$string_ratioz <- as.numeric(final_short$string_ratioz)
final_short$avg_importance_religionR_zscore <- as.numeric(final_short$avg_importance_religionR_zscore)



summary(mg_pathog)

plot_model(mg_coop_resnoint, type="eff")


#############################

