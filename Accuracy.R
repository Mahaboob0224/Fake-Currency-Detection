# Load required libraries
library(imager)
library(EBImage)

# Preprocess image: Convert to grayscale, resize, and normalize
preprocess_image <- function(filepath) {
  img <- load.image(filepath)          # Load image
  img <- grayscale(img)                # Convert to grayscale
  img <- resize(img, 64, 64)           # Resize to 64x64 pixels
  return(as.matrix(img))               # Return as matrix
}

# Training phase: Load, preprocess, and prepare training data
train_real <- list(
  preprocess_image("/Users/Alekya/Desktop/Fake Currency Detection/2000_s1.jpg"),
  preprocess_image("/Users/Alekya/Desktop/Fake Currency Detection/2000_s2.jpg"),
  preprocess_image("/Users/Alekya/Desktop/Fake Currency Detection/500_s1.jpg"),
  preprocess_image("/Users/Alekya/Desktop/Fake/500_my.jpeg"),
  preprocess_image("/Users/Alekya/Desktop/Fake Currency Detection/500_s4.jpg"),
  preprocess_image("/Users/Alekya/Desktop/Fake Currency Detection/500_s7.jpg")
)

train_fake <- list(
  preprocess_image("/Users/Alekya/Desktop/Fake Currency Detection/2000_f1.jpg"),
  preprocess_image("/Users/Alekya/Desktop/Fake Currency Detection/2000_f2.jpg"),
  preprocess_image("/Users/Alekya/Desktop/Fake Currency Detection/500_f1.jpg"),
  preprocess_image("/Users/Alekya/Desktop/Fake Currency Detection/500_f2.jpg")
)

# Function to calculate correlation between two images
calculate_correlation <- function(A, B) {
  return(cor(as.vector(A), as.vector(B)))  # Flatten the matrices into vectors
}

# Function to find the maximum correlation within a training set
max_correlation <- function(train_set, test_note) {
  corrs <- sapply(train_set, function(img) calculate_correlation(img, test_note))
  return(max(corrs))  # Take the highest correlation
}

# Function to predict if a test note is Real or Fake and print the result
predict_currency <- function(test_note, train_real, train_fake) {
  cor_real <- max_correlation(train_real, test_note)
  cor_fake <- max_correlation(train_fake, test_note)
  if (cor_real > cor_fake && cor_real >= 0.5) {
    print("Prediction: The currency is legitimate (Real).")
    return("Real")
  } else {
    print("Prediction: The currency is counterfeit (Fake).")
    return("Fake")
  }
}

# Testing phase: Define test set with labeled test images
test_set <- list(
  list(image = preprocess_image("/Users/Alekya/Desktop/Fake Currency Detection/2000_s3.jpg"), label = "Real"),
  list(image = preprocess_image("/Users/Alekya/Desktop/Fake Currency Detection/500_s10.jpg"), label = "Real"),
  list(image = preprocess_image("/Users/Alekya/Desktop/Fake/500_s8.jpg"), label = "Real"),
  list(image = preprocess_image("/Users/Alekya/Desktop/Fake/500_s9.jpg"), label = "Real"),
  list(image = preprocess_image("/Users/Alekya/Desktop/Fake/500_my.jpeg"), label = "Real"),
  list(image = preprocess_image("/Users/Alekya/Desktop/Fake Currency Detection/2000_f4.jpg"), label = "Fake"),
  list(image = preprocess_image("/Users/Alekya/Desktop/Fake/2000_f3.jpg"), label = "Fake"),
  list(image = preprocess_image("/Users/Alekya/Desktop/Fake/2000_f5.jpg"), label = "Fake"),
  list(image = preprocess_image("/Users/Alekya/Desktop/Fake/2000_f6.jpg"), label = "Fake"),
  list(image = preprocess_image("/Users/Alekya/Desktop/Fake Currency Detection/500_f6.jpg"), label = "Fake"),
  list(image = preprocess_image("/Users/Alekya/Desktop/Fake/500_f4.jpg"), label = "Fake")
)

# Function to calculate accuracy
accuracy <- function(test_set, train_real, train_fake) {
  correct_predictions <- 0
  for (test_note in test_set) {
    prediction <- predict_currency(test_note$image, train_real, train_fake)
    print(paste("Actual Label:", test_note$label))  # Print actual label
    if (prediction == test_note$label) {
      correct_predictions <- correct_predictions + 1
    }
  }
  return(correct_predictions / length(test_set) * 100)  # Return accuracy as a percentage
}

# Compute accuracy
result_accuracy <- accuracy(test_set, train_real, train_fake)
print(paste("Accuracy of the model:", result_accuracy, "%"))

