# entropy_1

# R script for calculating Information Gain based on Temperature Categories

# Initialize counts for high temperature
temp_alta_si <- 2  # Count of 'si' for high temperature
temp_alta_no <- 2  # Count of 'no' for high temperature
temp_alta_total <- temp_alta_si + temp_alta_no  # Total count for high temperature

# Initialize counts for medium temperature
temp_media_si <- 4  # Count of 'si' for medium temperature
temp_media_no <- 2  # Count of 'no' for medium temperature
temp_media_total <- temp_media_si + temp_media_no  # Total count for medium temperature

# Initialize counts for low temperature
temp_baja_si <- 3  # Count of 'si' for low temperature
temp_baja_no <- 1  # Count of 'no' for low temperature
temp_baja_total <- temp_baja_si + temp_baja_no  # Total count for low temperature

# Define a function to calculate entropy
entropy <- function(p) {
  if (p == 0 || p == 1) {
    return(0)
  } else {
    return(-p * log2(p) - (1 - p) * log2(1 - p))
  }
}

# Calculate conditional entropy for each temperature category
I_temp_alta <- entropy(temp_alta_si / temp_alta_total) + 
  entropy(temp_alta_no / temp_alta_total)
I_temp_media <- entropy(temp_media_si / temp_media_total) + 
  entropy(temp_media_no / temp_media_total)
I_temp_baja <- entropy(temp_baja_si / temp_baja_total) + 
  entropy(temp_baja_no / temp_baja_total)

# Calculate total conditional entropy for Temperature
total_ejemplos <- 14  # Total number of examples
I_temp <- (temp_alta_total / total_ejemplos) * I_temp_alta + 
  (temp_media_total / total_ejemplos) * I_temp_media + 
  (temp_baja_total / total_ejemplos) * I_temp_baja

# Calculate Information Gain for Temperature
I <- entropy(9 / 14)  # Entropy of the complete dataset
G_temp <- I - I_temp  # Information Gain for Temperature
