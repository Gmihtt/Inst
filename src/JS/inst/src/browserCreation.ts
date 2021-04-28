import puppeteer = require('puppeteer');

export async function createBrowser(dirPath: string): Promise<puppeteer.Browser> {
    let args = [
        '--no-sandbox',
        '--lang=en-GB'
    ];


    return await puppeteer.launch({
        headless: true,
        userDataDir: dirPath,
        args: args,
    });
}
