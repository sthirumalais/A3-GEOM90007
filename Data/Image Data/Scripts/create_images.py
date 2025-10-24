# import all necessary packages
import os
import csv
import time
import requests
from PIL import Image
from io import BytesIO
from urllib.parse import quote


input_csv = "bird_wikipedia_data.csv" 
base_dir = "Images"
os.makedirs(base_dir, exist_ok = True)

HEADERS = {
    "User-Agent": "Mozilla/5.0 (Windows NT 10.0; Win64; x64) "
                  "AppleWebKit/537.36 (KHTML, like Gecko) "
                  "Chrome/118.0 Safari/537.36",
    "Accept": "image/avif,image/webp,image/apng,image/*,*/*;q=0.8",
    "Referer": "https://en.wikipedia.org/",
}


def download_and_resize_image(url, bird_name, size = (256, 256), retries = 3):
    folder_name = bird_name.replace(" ", "_")
    subfolder = os.path.join(base_dir, folder_name)
    os.makedirs(subfolder, exist_ok = True)

    if not url or not url.startswith("http"):
        return False

    safe_url = quote(url, safe = "/:%?=&")
    output_path = os.path.join(subfolder, f"{folder_name}.jpg")

    for attempt in range(retries):
        try:
            response = requests.get(safe_url, headers = HEADERS, timeout = 15, stream = True)
            if response.status_code != 200:
                raise Exception(f"HTTP {response.status_code}")

            img = Image.open(BytesIO(response.content)).convert("RGB") # Read and resize image
            img = img.resize(size, Image.Resampling.LANCZOS)
            img.save(output_path, "JPEG", quality=90)

            return True

        except Exception as e:
            time.sleep(1.0)

    return False


failed_log = []

with open(input_csv, newline='', encoding = "utf-8") as f:
    reader = csv.DictReader(f)
    for row in reader:
        bird_name = row.get("scientific_name", "").strip()
        url = row.get("image_url", "").strip()

        ok = download_and_resize_image(url, bird_name)
        if not ok:
            failed_log.append({"scientific_name": bird_name, "image_url": url})

        time.sleep(0.3)  # prevent throttling
