import puppeteer = require('puppeteer');

const yaml = require('js-yaml');
const fs = require('fs-extra');
const path = require('path');


export let isProxy: boolean = false;
export let proxyServer: string = "";

interface ProxyData {
    isEnabled: boolean;
    url: string;
}

interface ConfigFile {
    proxy: ProxyData
}

export async function checkProxyAndSetVar(): Promise<void> {
    // Try/catch is required only for closing the browser in case of an error
    let browser: puppeteer.Browser | null = null;
    try {
        const config: ConfigFile = yaml.load(fs.readFileSync(path.resolve(__dirname, `config.yaml`)));
        if (!config.proxy.isEnabled) {
            return;
        }

        browser = await puppeteer.launch({
            userDataDir: path.resolve(__dirname, `loginDirs/userDirTest`),
            ignoreHTTPSErrors: true,
            args: [
                '--no-sandbox',
                `--proxy-server=${config.proxy.url}`,
                '--lang=en-GB'
            ]
        });
        const page: puppeteer.Page = await browser.newPage();

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
        if (browser != null){
            await browser.close();
            await fs.remove(path.resolve(__dirname, `loginDirs/userDirTest`));
        }
    }
}