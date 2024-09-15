import { chromium } from "playwright-core";
import "dotenv/config";
import * as fs from "fs/promises";
import { execSync } from "child_process";
import { URL } from "url";

class Downloader {
  constructor() {
    this._base_url = "https://les.unibok.no";
  }

  async start() {
    this.browser = await chromium.launch({
      executablePath: execSync("which chromium").toString().trim(),
      headless: false,
    });

    this.ctx = await this.browser.newContext();
    this.page = await this.ctx.newPage();
  }

  async login() {
    await this.page.goto("https://unibok.no/feidelogin");
    await this.page.waitForSelector("input#org_selector_filter");

    await this.page.fill("input#org_selector_filter", " "); // needs to be filled before button can be pressed
    await this.page.click(`li[org_id="${process.env.FEIDE_ORG_NAME}"]`);
    await this.page.click("button#selectorg_button");

    await this.page.waitForSelector("input#username");

    await this.page.fill("input#username", process.env.FEIDE_USERNAME);
    await this.page.fill("input#password", process.env.FEIDE_PASSWORD);
    await this.page.click(`button[type="submit"]`);

    await this.page.waitForTimeout(5000); // Wait for auth to process
  }

  async download_file(url, destination) {
    const response = await this.page.context().request.get(`${url}`);
    let responseBuffer = await response.body();
    await fs.writeFile(destination, responseBuffer, "binary");
  }

  async stop() {
    await this.browser.close();
  }

  parse_url(url) {
    const parsed_url = new URL(url);
    const fragment = parsed_url.hash.slice(1);
    const data = fragment.split("/");
    const [publisher, ref, id] = data;
    return { publisher, ref, id };
  }

  async download_book(name, url) {
    const book = this.parse_url(url);
    const file_url = `${this._base_url}/bookresource/publisher/${book.publisher}/book/${book.ref}/epub/${book.id}/offline.ub`;
    const filename = `./${name}.epub`;
    await this.download_file(file_url, filename);
  }
}

(async () => {
  const downloader = new Downloader();
  await downloader.start();
  await downloader.login();
  await downloader.download_book(
    "Enchanté",
    "https://les.unibok.no/#cappelendamm/p193917/2430/1",
  );
  await downloader.stop();
})();
