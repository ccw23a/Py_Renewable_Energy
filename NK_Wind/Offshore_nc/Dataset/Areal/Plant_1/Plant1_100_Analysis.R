library(dplyr)

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

#------------------------------------------------------------------
for (i in 1:9) {
  for (j in 1:7) {
    # 1) Open a PNG device, specifying a file name
    #png(filename = paste0("Wind Speed above 100m of_", 39.0 - 0.25 * (i - 1), "_", 123.0 + 0.25 * (j - 1), ".png"), 
        #width = 1920, height = 1080, res = 120)
    
    # Select the appropriate column for the current i, j combination
    col_data <- wind_spd[, 7 * (i - 1) + j]
    
    # Compute the histogram and capture the counts for annotation
    h <- hist(col_data, breaks = 15,
              main = paste("Wind Speed above 100m of", 39.0 - 0.25 * (i - 1), ",", 123.0 + 0.25 * (j - 1)),
              xlab = "Speed (m/s)")
    
    # Calculate the mean of the current column
    m <- mean(col_data, na.rm = TRUE)
    
    # Add a vertical red line at the mean value
    abline(v = m, col = "red", lwd = 2)
    
    # Annotate the mean value on the plot (rounding it to one decimal)
    text(m, max(h$counts), labels = round(m, 1), pos = 3, col = "red")
    
    # 3) Close the device
    #dev.off()
  }
}

