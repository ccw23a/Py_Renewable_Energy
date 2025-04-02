library(dplyr)
library(ggplot2)

#
u <- read.csv("D:/Py_Renewable/NK_Wind/Offshore_nc/Dataset/Areal/Plant 2/Plant_2_u10.csv",
              header = FALSE)
v <- read.csv("D:/Py_Renewable/NK_Wind/Offshore_nc/Dataset/Areal/Plant 2/Plant_2_v10.csv",
              header = FALSE)

data_matrix_u <- as.matrix(u)
data_matrix_v <- as.matrix(v)

# def 
wind_Spd_Dir_Calculation <- function(U_Vector, V_Vector) {
  # U_Vector, V_Vector are numeric vectors (e.g., length 3)
  # Calculate wind speed
  wind_spd <- sqrt(U_Vector^2 + V_Vector^2)
  # Calculate wind direction (meteorological convention)
  wind_dir <- (270 - (atan2(U_Vector, V_Vector) * 180/pi)) %% 360
  # Return a list with two elements
  return(list(ws = wind_spd, wd = wind_dir))
}

results_a <- matrix(nrow = 366, ncol = 2)
colnames(results_a) <- c("Speed", "Direction")

results_b <- matrix(nrow = 366, ncol = 2)
colnames(results_b) <- c("Speed", "Direction")

results_c <- matrix(nrow = 366, ncol = 2)
colnames(results_c) <- c("Speed", "Direction")

results_d <- matrix(nrow = 366, ncol = 2)
colnames(results_d) <- c("Speed", "Direction")

results_e <- matrix(nrow = 366, ncol = 2)
colnames(results_e) <- c("Speed", "Direction")

results_f <- matrix(nrow = 366, ncol = 2)
colnames(results_f) <- c("Speed", "Direction")

# Step 4: Loop through each day
for (i in 1:366) {
  out_a <- wind_Spd_Dir_Calculation(data_matrix_u[2*i - 1, 1], data_matrix_v[2*i-1, 1])
  results_a[i, 1] <- out_a$ws
  results_a[i, 2] <- out_a$wd
  
  out_b <- wind_Spd_Dir_Calculation(data_matrix_u[2*i - 1, 2], data_matrix_v[2*i-1, 2])
  results_b[i, 1] <- out_b$ws
  results_b[i, 2] <- out_b$wd
  
  out_c <- wind_Spd_Dir_Calculation(data_matrix_u[2*i - 1, 3], data_matrix_v[2*i-1, 3])
  results_c[i, 1] <- out_c$ws
  results_c[i, 2] <- out_c$wd
  
  out_d <- wind_Spd_Dir_Calculation(data_matrix_u[2*i, 1], data_matrix_v[2*i, 1])
  results_d[i, 1] <- out_d$ws
  results_d[i, 2] <- out_d$wd
  
  out_e <- wind_Spd_Dir_Calculation(data_matrix_u[2*i, 2], data_matrix_v[2*i, 2])
  results_e[i, 1] <- out_e$ws
  results_e[i, 2] <- out_e$wd
  
  out_f <- wind_Spd_Dir_Calculation(data_matrix_u[2*i, 3], data_matrix_v[2*i, 3])
  results_f[i, 1] <- out_f$ws
  results_f[i, 2] <- out_f$wd
}

str(out_a)
dim(out_a)
head(out_a)

write.csv(results_a, file = "D:/Py_Renewable/NK_Wind/Offshore_nc/Dataset/Areal/375_125.csv", row.names = FALSE)
write.csv(results_b, file = "D:/Py_Renewable/NK_Wind/Offshore_nc/Dataset/Areal/375_12525.csv", row.names = FALSE)
write.csv(results_c, file = "D:/Py_Renewable/NK_Wind/Offshore_nc/Dataset/Areal/375_1255.csv", row.names = FALSE)
write.csv(results_d, file = "D:/Py_Renewable/NK_Wind/Offshore_nc/Dataset/Areal/3725_125.csv", row.names = FALSE)
write.csv(results_e, file = "D:/Py_Renewable/NK_Wind/Offshore_nc/Dataset/Areal/3725_12525.csv", row.names = FALSE)
write.csv(results_f, file = "D:/Py_Renewable/NK_Wind/Offshore_nc/Dataset/Areal/3725_1255.csv", row.names = FALSE)

# Histogram
hist(results_a[, 1],
     main = "Histogram of Wind Speeds 37.5 125 ",
     xlab = "Speed",
     breaks = 15)  # break of 1m/s

hist(results_a[, 2],
     main = "Histogram of Wind Directions 37.5 125",
     xlab = "Direction (degrees)",
     breaks = 36)  # e.g., 10° increments




hist(results_b[, 1],
     main = "Histogram of Wind Speeds 37.5 125.25",
     xlab = "Speed",
     breaks = 15)  # break of 1m/s

hist(results_b[, 2],
     main = "Histogram of Wind Directions 37.5 125.25",
     xlab = "Direction (degrees)",
     breaks = 36)  # e.g., 10° increments





hist(results_c[, 1],
     main = "Histogram of Wind Speeds 37.5 125.5",
     xlab = "Speed",
     breaks = 15)  # break of 1m/s

hist(results_c[, 2],
     main = "Histogram of Wind Directions 37.5 125.5",
     xlab = "Direction (degrees)",
     breaks = 36)  # e.g., 10° increments





hist(results_d[, 1],
     main = "Histogram of Wind Speeds 37.25 125",
     xlab = "Speed",
     breaks = 15)  # break of 1m/s

hist(results_d[, 2],
     main = "Histogram of Wind Directions 37.25 125",
     xlab = "Direction (degrees)",
     breaks = 36)  # e.g., 10° increments




hist(results_e[, 1],
     main = "Histogram of Wind Speeds 37.25 125.25",
     xlab = "Speed",
     breaks = 15)  # break of 1m/s

hist(results_e[, 2],
     main = "Histogram of Wind Directions 37.25 125.25",
     xlab = "Direction (degrees)",
     breaks = 36)  # e.g., 10° increments




hist(results_f[, 1],
     main = "Histogram of Wind Speeds 37.25 125.5",
     xlab = "Speed",
     breaks = 15)  # break of 1m/s

hist(results_f[, 2],
     main = "Histogram of Wind Directions 37.25 125.5",
     xlab = "Direction (degrees)",
     breaks = 36)  # e.g., 10° increments

#Stastical Analysis

df_scaled_WS_a <- scale(results_a[, 1])
df_scaled_WS_b <- scale(results_b[, 1])
df_scaled_WS_c <- scale(results_c[, 1])
df_scaled_WS_d <- scale(results_d[, 1])
df_scaled_WS_e <- scale(results_e[, 1])
df_scaled_WS_f <- scale(results_f[, 1])

hist(df_scaled_WS_a,
     main = "Standardized Wind Speed 37.5 125 ",
     xlab = "Z-Score")

hist(df_scaled_WS_b,
     main = "Standardized Wind Speed 37.5 125.25",
     xlab = "Z-Score")

hist(df_scaled_WS_c,
     main = "Standardized Wind Speed 37.5 125.5",
     xlab = "Z-Score")

hist(df_scaled_WS_d,
     main = "Standardized Wind Speed 37.25 125",
     xlab = "Z-Score")

hist(df_scaled_WS_e,
     main = "Standardized Wind Speed 37.25 125.25",
     xlab = "Z-Score")

hist(df_scaled_WS_f,
     main = "Standardized Wind Speed 37.25 125.5",
     xlab = "Z-Score")

# or maybe just make separate csvs decided by lat,long
# and each csv will have dir and str within separated in days -> 2x366

# and do on jupyter notebook for batch processing and easy comparison

# making numerical array for skewness, kurtosis values

# making numerical array for Normal Distribution and mean values

# making boxplot/violinplot for visualizing data distributions
# make as Horizontal or just make it separate png files

