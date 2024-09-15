import { chromium } from "playwright-core";
import "dotenv/config";
import * as fs from "fs/promises";

const main = async () => {
  const browser = await chromium.launch({
    executablePath:
      "/nix/store/mslmcb4ddcpvl0f65r0bxkfzgsqx2p17-ungoogled-chromium-127.0.6533.119/bin/chromium",
    headless: false,
  });

  const ctx = await browser.newContext();
  const page = await ctx.newPage();

  // Login to get authorization for downloading files
  await page.goto("https://unibok.no/feidelogin");
  await page.waitForSelector("input#org_selector_filter");

  await page.fill("input#org_selector_filter", " "); // needs to be filled before button can be pressed
  await page.click(`li[org_id="${process.env.FEIDE_ORG_NAME}"]`);
  await page.click("button#selectorg_button");

  await page.waitForSelector("input#username");

  await page.fill("input#username", process.env.FEIDE_USERNAME);
  await page.fill("input#password", process.env.FEIDE_PASSWORD);
  await page.click(`button[type="submit"]`);

  // Download the file
  await page.waitForTimeout(5000);
  const downloadUrl =
    "https://les.unibok.no/bookresource/publisher/cappelendamm/book/p193917/epub/2430/offline.ub";
  // const [download] = await Promise.all([
  //   page.waitForEvent("download"),
  //   page.goto(downloadUrl),
  // ]);
  // const downloadPath = "./output.epub";
  // await download.saveAs(downloadPath);
  // https://dev.to/ryanroselloog/use-playwright-to-download-files-from-remote-servers-23b0
  // It looks like page.goto() doesn't like direct file URLs
  const response = await page.context().request.get(`${downloadUrl}`);
  let responseBuffer = await response.body();
  await fs.writeFile(`./output.epub`, responseBuffer, "binary");

  await browser.close();
};

main();
