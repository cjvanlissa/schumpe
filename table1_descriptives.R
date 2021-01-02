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