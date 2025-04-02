library(dplyr)

#
u <- read.csv("D:/Py_Renewable/NK_Wind/Offshore_nc/Dataset/Areal/comp_grid/u100.csv",
              header = FALSE)
v <- read.csv("D:/Py_Renewable/NK_Wind/Offshore_nc/Dataset/Areal/comp_grid/v100.csv",
              header = FALSE)

data_matrix_u <- as.matrix(u)
data_matrix_v <- as.matrix(v)

# def 
wind_Spd_Dir_Calculation <- function(U_Vector, V_Vector) {
  wind_spd <- sqrt(U_Vector^2 + V_Vector^2)
  wind_dir <- (270 - (atan2(U_Vector, V_Vector) * 180/pi)) %% 360
  return(list(ws = wind_spd, wd = wind_dir))
}

# 208개 데이터 * 366일, 2개의 데이터 프레임으로 해결 (Dir, Spd)
# 삼중 for문으로 해결

wind_spd <- matrix(nrow = 366, ncol = 208)
wind_dir <- matrix(nrow = 366, ncol = 208)

for(i in 1:366) {
  for (j in 1:16){
    for(k in 1:13){
      out <- wind_Spd_Dir_Calculation(data_matrix_u[16*(i - 1) + j, k], data_matrix_v[16*(i - 1) + j, k])
      wind_spd[i, 13 * (j - 1) + k] <- out$ws
      wind_dir[i, 13 * (j - 1) + k] <- out$wd
    }
  }
}

lat <- matrix(nrow = 208, ncol = 1)
long <- matrix(nrow = 208, ncol = 1)

#------------------------------------------------------------------
for (i in 1:16) {
  for (j in 1:13) {
    # Select the appropriate column for the current i, j combination
    windspd_data <- wind_spd[, 13 * (i - 1) + j]
    winddir_data <- wind_dir[, 13 * (i - 1) + j]
    
    lat[13 * (i - 1) + j] <- 37.75 - 0.25 * (i - 1)
    long[13 * (i - 1) + j] <- 123.5 + 0.25 * (j - 1)
    
    wind_data <- data.frame(wind_speed = windspd_data, wind_direction = winddir_data)
    
    file_name <- paste0("wind_data_", 37.75 - 0.25 * (i - 1), ",", 123.5 + 0.25 * (j - 1), ".csv")
    write.csv(wind_data, file = file_name, row.names = TRUE)
  }
}

loc_data <- data.frame(latitude = lat, longitude = long)
write.csv(loc_data, file = "loc_data_comp.csv", row.names = TRUE)


