drinking_data1 <- hh_obs[grepl("^drinking", hh_obs$item), ]
total_hours1 <- sum(hh_obs$Duration[hh_obs$Duration > 0], na.rm = TRUE) / 3600
drinking_counts1 <- table(drinking_data1$item)
drinking_rate1 <- drinking_counts1 / total_hours1

drinking_data2 <- hh_obs[grepl("^drinking", hh_obs$item), ]
total_hours2 <- sum(hh_obs$Duration[hh_obs$Duration > 0], na.rm = TRUE) / 3600
drinking_counts2 <- table(drinking_data2$item)
drinking_rate2 <- drinking_counts2 / total_hours2