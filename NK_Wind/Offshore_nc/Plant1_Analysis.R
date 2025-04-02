library(dplyr)
library(ggplot2)

#
u <- read.csv("D:/Py_Renewable/NK_Wind/Offshore_nc/Dataset/Areal/Plant_1/u100.csv",
              header = FALSE)
v <- read.csv("D:/Py_Renewable/NK_Wind/Offshore_nc/Dataset/Areal/Plant_1/v100.csv",
              header = FALSE)

data_matrix_u <- as.matrix(u)
data_matrix_v <- as.matrix(v)

# def 
wind_Spd_Dir_Calculation <- function(U_Vector, V_Vector) {
  wind_spd <- sqrt(U_Vector^2 + V_Vector^2)
  wind_dir <- (270 - (atan2(U_Vector, V_Vector) * 180/pi)) %% 360
  return(list(ws = wind_spd, wd = wind_dir))
}

# 63개 데이터 * 366일, 2개의 데이터 프레임으로 해결 (Dir, Spd)
# 삼중 for문으로 해결

wind_spd <- matrix(nrow = 366, ncol = 63)
wind_dir <- matrix(nrow = 366, ncol = 63)

for(i in 1:366) {
  for (j in 1:9){
    for(k in 1:7){
      out <- wind_Spd_Dir_Calculation(data_matrix_u[9*(i - 1) + j, k], data_matrix_v[9*(i - 1) + j, k])
      wind_spd[i, 7 * (j - 1) + k] <- out$ws
      wind_dir[i, 7 * (j - 1) + k] <- out$wd
    }
  }
}
lat <- matrix(nrow = 63, ncol = 1)
long <- matrix(nrow = 63, ncol = 1)
#------------------------------------------------------------------
for (i in 1:9) {
  for (j in 1:7) {
    # Select the appropriate column for the current i, j combination
    windspd_data <- wind_spd[, 7 * (i - 1) + j]
    winddir_data <- wind_dir[, 7 * (i - 1) + j]
    
    lat[7 * (i - 1) + j] <- 39.0 - 0.25 * (i - 1)
    long[7 * (i - 1) + j] <- 123.0 + 0.25 * (j - 1)
    
    wind_data <- data.frame(wind_speed = windspd_data, wind_direction = winddir_data)
    
    file_name <- paste0("wind_data_", 39.0 - 0.25 * (i - 1), ",", 123.0 + 0.25 * (j - 1), ".csv")
    
    # Write the data frame to CSV without row names
    write.csv(wind_data, file = file_name, row.names = TRUE)
    
    #------------------------------------------------------------------------------------------------------------
    # Compute the histogram and capture the counts for annotation
    #h <- hist(col_data, breaks = 36,
              #main = paste("Wind Speed above 10m of", 39.0 - 0.25 * (i - 1), ",", 123.0 + 0.25 * (j - 1)),
              #xlab = "Direction (degrees)")
    
    # Calculate the mean of the current column
    #m <- mean(col_data, na.rm = TRUE)
    
    # Add a vertical red line at the mean value
    #abline(v = m, col = "red", lwd = 2)
    
    # Annotate the mean value on the plot (rounding it to one decimal)
    #text(m, max(h$counts), labels = round(m, 1), pos = 3, col = "red")
  }
}

loc_data <- data.frame(latitude = lat, longitude = long)
write.csv(loc_data, file = "loc_data.csv", row.names = TRUE)





# or maybe just make separate csvs decided by lat,long
# and each csv will have dir and str within separated in days -> 2x366

# and do on jupyter notebook for batch processing and easy comparison

# making numerical array for skewness, kurtosis values

# making numerical array for Normal Distribution and mean values

# making boxplot/violinplot for visualizing data distributions
# make as Horizontal or just make it separate png files
