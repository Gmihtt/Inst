import puppeteer = require('puppeteer');

export async function createBrowser(dirPath: string): Promise<puppeteer.Browser> {
    const args = [
        '--no-sandbox',
        '--lang=en-GB',
    ];

    const browser: puppeteer.Browser = await puppeteer.launch({
        headless: false,
        userDataDir: dirPath,
        args: args,
    });

    const page: puppeteer.Page = (await browser.pages())[0];

    await page.waitForTimeout(1500);

    await page.setViewport({
        width: 1024,
        height: 768,
    });

    return browser;
}
