const video = document.getElementById("video");
const canvas = document.getElementById("canvas");
const resultText = document.getElementById("result-text");

// Function to Upload Image
function uploadImage() {
    let fileInput = document.getElementById("imageUpload");
    let file = fileInput.files[0];

    if (!file) {
        alert("Please select an image!");
        return;
    }

    let formData = new FormData();
    formData.append("file", file); // Match API field name

    resultText.textContent = "Detecting... Please wait.";

    fetch("http://127.0.0.1:8000/predict", {
        method: "POST",
        body: formData
    })
    .then(response => response.json())
    .then(data => {
        if (data && data.prediction) {
            resultText.textContent = "Prediction: " + data.prediction;
        } else {
            resultText.textContent = "Error: Unexpected response from API.";
        }
    })
    .catch(error => {
        console.error("Error:", error);
        resultText.textContent = "An error occurred while detecting the currency.";
    });
}

// Function to Start Camera
function startCamera() {
    navigator.mediaDevices.getUserMedia({ video: true })
        .then(stream => {
            video.srcObject = stream;
        })
        .catch(err => {
            console.error("Error accessing camera:", err);
            alert("Unable to access your camera.");
        });
}

// Function to Capture & Detect Image
function captureImage() {
    let context = canvas.getContext("2d");
    canvas.width = video.videoWidth;
    canvas.height = video.videoHeight;
    context.drawImage(video, 0, 0, canvas.width, canvas.height);

    resultText.textContent = "Detecting... Please wait.";

    canvas.toBlob(blob => {
        let formData = new FormData();
        formData.append("file", blob, "captured.png"); // Match API field name

        fetch("http://127.0.0.1:8000/predict", {
            method: "POST",
            body: formData
        })
        .then(response => response.json())
        .then(data => {
            if (data && data.prediction) {
                resultText.textContent = "Prediction: " + data.prediction;
            } else {
                resultText.textContent = "Error: Unexpected response from API.";
            }
        })
        .catch(error => {
            console.error("Error:", error);
            resultText.textContent = "An error occurred while detecting the currency.";
        });
    }, "image/jpeg", 0.9);
}
