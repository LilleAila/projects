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
  await page.click(`li[org_id="${process.env.FEIDE_ORG_NAME}"]`);
  await page.click("button#selectorg_button");

  await page.waitForSelector("input#username");

  await page.fill("input#username", process.env.FEIDE_USERNAME);
  await page.fill("input#password", process.env.FEIDE_PASSWORD);
  await page.click(`button[type="submit"]`);
};

main();
