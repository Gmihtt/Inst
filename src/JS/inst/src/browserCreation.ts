import puppeteer = require('puppeteer');

export interface Proxy {
    ip: string,
    port_http: number,
    username: string,
    password: string,
}

export async function createBrowser(dirPath: string, proxy: Proxy): Promise<puppeteer.Browser> {
    const args = [
        '--no-sandbox',
        '--lang=en-GB',
        `--proxy-server=${proxy.ip}:${proxy.port_http}`,
    ];
    console.log('proxy: ' + `--proxy-server=${proxy.ip}:${proxy.port_http}`);

    const browser: puppeteer.Browser = await puppeteer.launch({
        headless: true,
        userDataDir: dirPath,
        args: args,
    });

    const page: puppeteer.Page = (await browser.pages())[0];

    await page.authenticate({
        username: proxy.username,
        password: proxy.password,
    });

    await page.waitForTimeout(1500);

    await page.setViewport({
        width: 1024,
        height: 768,
    });

    return browser;
}
