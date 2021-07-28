import puppeteer = require('puppeteer');
import {Logger} from "./log";

export interface Proxy {
    ip: string,
    port_http: number,
    username: string,
    password: string,
}

const proxy: Proxy = {
    ip: '185.81.147.205',
    port_http: 5986,
    username: 'user61575',
    password: 'wk5rx4'
}

export async function createBrowser(dirPath: string): Promise<puppeteer.Browser> {
    const args = [
        '--no-sandbox',
        '--lang=en-GB',
        `--proxy-server=${proxy.ip}:${proxy.port_http}`,
    ];
    Logger.info(`proxy: ${proxy.ip}:${proxy.port_http}`);


    const browser: puppeteer.Browser = await puppeteer.launch({
        headless: false,
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
