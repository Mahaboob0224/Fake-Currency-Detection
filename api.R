# Load required libraries
install.packages("plumber")
library(plumber)
library(imager)
library(EBImage)
install.packages("jsonlite")
library(jsonlite)

# Define multiple directories
folders <- list(
  "C:/Users/Alekya/Desktop/Fake Currency Detection/",
  "C:/Users/Alekya/Documents/Fake Currency Data/"
)

# Function to preprocess images
preprocess_image <- function(filepath) {
  img <- load.image(filepath) # Load image
  img <- grayscale(img) # Convert to grayscale
  img <- resize(img, 64, 64) # Resize image
  return(as.matrix(img)) # Convert to matrix
}

# Function to load images from multiple folders
load_images <- function(filenames) {
  images <- list()
  for (folder in folders) {
    for (filename in filenames) {
      filepath <- paste0(folder, filename)
      if (file.exists(filepath)) {
        images <- append(images, list(preprocess_image(filepath)))
      }
    }
  }
  return(images)
}

# List of images for training (Make sure these files exist in at least one folder)
real_images <- c("2000_s1.jpg", "2000_s2.jpg", "500_s1.jpg", "500_my.jpeg", "500_s4.jpg", "500_s7.jpg")
fake_images <- c("2000_f1.jpg", "2000_f2.jpg", "500_f1.jpg", "500_f2.jpg")

# Load training images from multiple folders
train_real <- load_images(real_images)
train_fake <- load_images(fake_images)

# Function to calculate correlation
calculate_correlation <- function(A, B) {
  return(cor(as.vector(A), as.vector(B)))
}

# Function to find max correlation
max_correlation <- function(train_set, test_note) {
  corrs <- sapply(train_set, function(img) calculate_correlation(img, test_note))
  return(max(corrs)) # Highest correlation
}

# Function to predict if currency is real or fake
predict_currency <- function(test_note) {
  cor_real <- max_correlation(train_real, test_note)
  cor_fake <- max_correlation(train_fake, test_note)
  
  if (cor_real > cor_fake && cor_real >= 0.5) {
    return("Real")
  } else {
    return("Fake")
  }
}

# API to handle image uploads
#* @post /upload
#* @serializer json
function(req) {
  temp_image <- tempfile(fileext = ".png")
  writeBin(req$postBody, temp_image)
  
  # Preprocess the uploaded image
  test_note <- preprocess_image(temp_image)
  
  # Predict currency type
  result <- predict_currency(test_note)
  
  # Return JSON response
  list(result = result)
}

# Start API on port 8000
pr <- plumber::plumb("api.R")
pr$run(port = 8000)
