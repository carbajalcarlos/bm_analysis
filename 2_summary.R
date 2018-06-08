# Loading df created by bibliometrix
load("H:/code/r/bm_analysis/0_process/di_df.Rdata")

# ----- basic edition ----- 
# Inverse the current order by date (descending to ascending)
di <- di[12872:1, ]
# Gererating a local unique ID
di$local.id <- paste("di", 1:nrow(di), sep = "_")

# ----- Thinkering ------
# Production per year
hist(di$PY, breaks = (max(di$PY, na.rm = TRUE) - min(di$PY, na.rm = TRUE)))

# analysis per year
years <- as.data.frame(table(di$PY), stringsAsFactors = FALSE)
years$Var1 <- as.integer(years$Var1)
years$mean <- NA

for (i in 1:nrow(years)) {
  years$mean[i] <- mean(years$Freq[i:nrow(years)])
}

mean(years$mean)
