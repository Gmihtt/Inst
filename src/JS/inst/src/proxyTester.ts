import puppeteer = require('puppeteer');

const yaml = require('js-yaml');
const fs = require('fs-extra');
const path = require('path');


export let isProxy: boolean = false;
export let proxyServer: string = "";
export let isAuth: boolean = false;
export let proxyUsername: string = "";
export let proxyPassword: string = "";

interface ProxyData {
    isEnabled: boolean;
    url: string;
    isAuth: boolean;
    username: string;
    password: string;
}

interface ConfigFile {
    proxy: ProxyData
}

export async function authenticate(browser: puppeteer.Browser): Promise<void> {
    const page = await browser.newPage();
    await page.authenticate({
        username: proxyUsername,
        password: proxyPassword,
    });
    await page.waitForTimeout(10000);
}

export async function checkProxyAndSetVar(): Promise<void> {
    // Try/catch is required only for closing the browser in case of an error
    let browser: puppeteer.Browser | null = null;
    try {
        const config: ConfigFile = yaml.load(fs.readFileSync(path.resolve(__dirname, `config.yaml`)));
        if (!config.proxy.isEnabled) {
            return;
        }

        if (config.proxy.isAuth) {
            isAuth = true;
            proxyUsername = config.proxy.username;
            proxyPassword = config.proxy.password;
        }

        browser = await puppeteer.launch({
            userDataDir: path.resolve(__dirname, `loginDirs/userDirTest`),
            ignoreHTTPSErrors: true,
            headless: false,
            args: [
                '--no-sandbox',
                `--proxy-server=${config.proxy.url}`,
                '--lang=en-GB'
            ]
        });

        if (isAuth) {
            await authenticate(browser);
        }

        const page: puppeteer.Page = (await browser.pages())[0];

        let responseObject: any = await page.evaluate(async () => {
            try {
                const response: Response = await fetch('https://freegeoip.app/json/');
                return response.json();
            } catch (e) {
                return null;
            }
        });
        if (responseObject === null) {
            // noinspection ExceptionCaughtLocallyJS
            throw new Error('Fetch ip info error');
        }

        if (responseObject.country_name === 'Russia') {
            // noinspection ExceptionCaughtLocallyJS
            throw new Error("Proxy is in Russia");
        }

        isProxy = true;
        proxyServer = config.proxy.url;
    } catch (e) {
        throw e;
    } finally {
        if (browser != null) {
            await browser.close();
            await fs.remove(path.resolve(__dirname, `loginDirs/userDirTest`));
        }
    }
}