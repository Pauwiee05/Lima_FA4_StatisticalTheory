# FA4 R PROGRAM
# Paul Carlos T. Lima I

# Normal
no <- c(67, 69, 70, 62, 63, 67, 65, 59, 68, 66, 60, 65, 70, 63, 64, 65, 69, 60, 61, 67, 66, 64, 65, 68, 71, 61, 62, 69, 
        66, 65, 68, 62, 64, 67, 67, 70, 62, 64, 66, 63, 65, 68, 63, 64, 66, 65, 65, 61, 63, 66 )

# Skewed-right
sr <- c( 31, 40, 43, 24, 30, 29, 30 ,24, 38, 27, 26, 35, 29, 33, 55, 75, 46, 38, 26, 34, 29, 85, 57, 29, 34, 40,
         34, 41, 36, 35, 40, 26, 28, 34, 26, 19, 66, 23, 63, 28, 30, 26, 33, 31, 24, 25, 35, 22, 34, 28)

# Skewed-left
sl <- c(102, 87, 55, 104, 70, 75, 95, 80, 73, 66, 79, 93, 60, 90, 73, 84, 89, 73, 85, 98, 72, 79, 92, 35, 76, 71, 93,
        90, 76, 71, 97, 63, 10, 58, 70, 82, 85, 72, 25, 93, 53, 44, 58, 65, 10, 77, 92, 81, 82, 77)
# Uniform
un <- c(12.1, 12.1, 12.4, 12.1, 11.6, 11.6, 12.0, 11.6, 12.1, 12.2, 12.2, 12.2, 11.6, 11.7, 12.3, 11.7, 11.9, 11.7, 12.2,
        11.7, 12.3, 11.8, 12.3, 12.5, 11.7, 11.8, 12.3, 11.8, 12.3, 11.8, 12.4, 11.9, 12.4, 11.9, 12.1, 11.9, 12.4, 12.2,
        12.4, 11.9, 12.5, 12.0, 11.8, 11.9, 12.5, 12.0, 12.5, 12.0, 12.5, 12.0)


# FOR NUMBER 1
# Calculations of Moments
cMoments <- function(data, order) {
  n <- length(data)
  moments <- sum(data^order) / n
  return(moments)
}

# Calculate moments for each of the data's (no, sr, sl , un)
noMoments <- sapply(1:4, function(order) cMoments(no, order))
srMoments <- sapply(1:4, function(order) cMoments(sr, order))
slMoments <- sapply(1:4, function(order) cMoments(sl, order))
unMoments <- sapply(1:4, function(order) cMoments(un, order))

# Display results of the moments
#THE ORDER OF THE MOMENTS OF THE RESULTS ARE AS FOLLOWS:
# First Moment, Second Moment, Third Moment, and Fourth Moment
cat("Normal Moments:", noMoments, "\n")
cat("Skewed-Right Moments:", srMoments, "\n")
cat("Skewed-Left Moments:", slMoments, "\n")
cat("Uniform Moments:", unMoments, "\n")



# FOR NUMBER 2
# Function to calculate moments about the mean
cMomentsMean <- function(data, order) {
  meanV <- mean(data)
  momentsMean <- sum((data - meanV)^order) / length(data)
  return(momentsMean)
}

# Calculate moments about the mean for each set of data
no_Moments_Mean <- sapply(1:4, function(order) cMomentsMean(no, order))
sr_Moments_Mean <- sapply(1:4, function(order) cMomentsMean(sr, order))
sl_Moments_Mean <- sapply(1:4, function(order) cMomentsMean(sl, order))
un_Moments_Mean <- sapply(1:4, function(order) cMomentsMean(un, order))

# Display results of the moments about the mean
#THE ORDER OF THE MOMENTS OF THE RESULTS ARE AS FOLLOWS:
# First Moment, Second Moment, Third Moment, and Fourth Moment
cat("Normal Moments about Mean:", no_Moments_Mean, "\n")
cat("Skewed-Right Moments about Mean:", sr_Moments_Mean, "\n")
cat("Skewed-Left Moments about Mean:", sl_Moments_Mean, "\n")
cat("Uniform Moments about Mean:", un_Moments_Mean, "\n")


# FOR NUMBER 3
# Calculate moments about number 75
cMoments75 <- function(data, order) {
  xValue <- 75
  moments75 <- sum((data - xValue)^order) / length(data)
  return(moments75)
}

noMoments75 <- sapply(1:4, function(order) cMoments75(no, order))
cat("Normal Moments about number 75:", noMoments75, "\n")

#FOR NUMBER 4 
#Using the results of 2 and 3, we verify relations between the moments
m1 <- -9.88
m2 <- 105.92
m3 <- -1211.08
m4 <- 14572.64

calculate_m2 <- m2 - (m1)**2
cat("m2 = ", calculate_m2, "\n")

calculate_m3 <- m3 - 3*m1*m2 + 2*(m1)**3
cat("m3 = ", calculate_m3, "\n")

calculate_m4 <- m4 - 4*m1*m3 + 6*((m1)**2)*m2-3*(m1)**4
cat("m4 = ", calculate_m4, "\n")

# Using results of 2 to verify and it matches.




