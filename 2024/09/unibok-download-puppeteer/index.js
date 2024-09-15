import { chromium } from "playwright-core";
import "dotenv/config";

const main = async () => {
  const browser = await chromium.launch({
    executablePath:
      "/nix/store/mslmcb4ddcpvl0f65r0bxkfzgsqx2p17-ungoogled-chromium-127.0.6533.119/bin/chromium",
    headless: false,
  });

  const ctx = await browser.newContext();
  const page = await ctx.newPage();

  await page.goto("https://unibok.no/feidelogin");
  await page.waitForSelector("input#org_selector_filter");

  // needs to be filled before button can be pressed
  await page.fill("input#org_selector_filter", " ");
  await page.click(`li[org_id="vlfk.no"]`);
  await page.click("button#selectorg_button");

  await page.waitForSelector("input#username");

  await page.fill("input#username", process.env.FEIDE_USERNAME);
  await page.fill("input#password", process.env.FEIDE_PASSWORD);
  await page.click(`button[type="submit"]`);
};

main();

/*
import puppeteer from "puppeteer-core";

const browser = await puppeteer.launch({
  executablePath:
    "/nix/store/irznr30hbryms34qhgc2g7jnap962xif-google-chrome-127.0.6533.119/bin/google-chrome-stable",
});
const page = await browser.newPage();

await page.goto("https://unibok.no/feidelogin");

await page.setViewport({ width: 1080, height: 1080 });
await page.locator("input#username").fill("e-olasol2");
await page.locator("input#password").fill("mitt passord");
// await page.locator(`.main .breathe-top button.button-primary`).click();

// await browser.close();
*/
