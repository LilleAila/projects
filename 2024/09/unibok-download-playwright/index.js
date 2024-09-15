import { chromium } from "playwright-core";
import "dotenv/config";
import * as fs from "fs/promises";
import { execSync } from "child_process";
import { URL } from "url";

class Downloader {
  constructor() {
    this.base_url = "https://les.unibok.no";
    this.output_dir = "./output";
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
    await fs.mkdir(this.output_dir, { recursive: true });
    const book = this.parse_url(url);
    const file_url = `${this.base_url}/bookresource/publisher/${book.publisher}/book/${book.ref}/epub/${book.id}/offline.ub`;
    const filename = `${this.output_dir}/${name}.epub`;
    await this.download_file(file_url, filename);
  }

  async download_books(books) {
    for (const book of books) {
      const [name, url] = book;
      await this.download_book(name, url);
    }
  }

  async get_library() {
    const books = this.page.locator(".book");
    const book_count = await books.count();
    let book_data = [];
    for (let i = 0; i < book_count; i++) {
      const book = books.nth(i);
      const name = await book
        .locator(".wrapper .bookinfo h3.tittel")
        .textContent();
      const href = await book.locator(".wrapper a.omslag").getAttribute("href");
      book_data.push([name, `${this.base_url}/${href}`]);
    }
    return book_data;
  }

  async download_library() {
    // Done in to iterations to avoid going back and forth because of clicking the links
    const books = await this.get_library();
    let books_to_download = [];
    for (const book of books) {
      const [name, url] = book;
      await this.page.goto(url);
      await this.page.waitForLoadState("load");
      await this.page.waitForSelector(".splashscreen", { state: "detached" });
      const new_url = this.page.url();
      books_to_download.push([name, new_url]);
    }
    // download_books(books_to_download);
    console.log(books_to_download);
  }
}

(async () => {
  const downloader = new Downloader();
  await downloader.start();
  await downloader.login();
  // await downloader.download_book(
  //   "Enchanté",
  //   "https://les.unibok.no/#cappelendamm/p193917/2430/1",
  // );
  // await downloader.download_books([
  //   ["Enchanté", "https://les.unibok.no/#cappelendamm/p193917/2430/1"],
  // ]);
  await downloader.download_library();
  await downloader.stop();
})();
