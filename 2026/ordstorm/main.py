import json
import time
from playwright.sync_api import sync_playwright

def main():
    # Manually scraped from https://api.edtech.fagbokforlaget.no/contents/c609bff0-1ff6-4f86-ba5c-80d1d2ced09d/objects/8e7a7102-1b8d-463f-8334-624a995905be by inspecting in the network tab of devtools
    with open("answers.json") as file:
        data = json.load(file)
    answers = {a["question"]:a["answers"][0] for a in data["content"]["dictionary"]}

    with sync_playwright() as p:
        browser = p.chromium.launch(headless=False)
        page = browser.new_page()
        page.goto("https://ordstorm.fagbokforlaget.no/")
        page.click(".external-providers div:nth-child(2) a")
        page.wait_for_load_state("networkidle")
        page.wait_for_selector("#answer") # Game started
        print("Game started")

        start_time = time.time()
        end_time = start_time + 120
        # while page.locator("#answer").is_visible():
        while time.time() < end_time:
            query = page.locator('label[for="answer"]').inner_text()
            answer = answers[query]
            page.fill("#answer", answer)
            page.press("#answer", "Enter")

        page.wait_for_event("close", timeout=0)

if __name__ == "__main__":
    main()
