#determining outliers

# Calculating Q1, Q3, and IQR
Q1 <- quantile(data$reports_filled, 0.25)
Q3 <- quantile(data$reports_filled, 0.75)
IQR_value <- IQR(data$reports_filled)
