import * as BR from './browserCreation'
import path = require('path');

(async () =>{
    let pr: BR.Proxy = {
        ip: '109.94.219.89',
        port_http: '49465',
        username: '8sjR2Gfi',
        password: 'qgFW6UV2',
    };

    let browser = await BR.createBrowser(path.resolve(__dirname, `loginDirs/userDir${10}`), pr);

    const page = (await browser.pages())[0];

    await page.waitForTimeout(10000000);
})()