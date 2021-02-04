import puppeteer = require('puppeteer');
import path = require('path');
import {Mutex} from "async-mutex";

import fs from "fs-extra";

export interface LoginRequest {
    username: string;
    password: string;
}

export interface LoginResponse {
    status: boolean;
    username: string;
    inst_id?: string;
    errorMessage?: string;
}

const dirNumberMutex = new Mutex();

let counter: number = 0;

export async function login(username: string, password: string): Promise<LoginResponse> {
    await dirNumberMutex.acquire();
    let dirNumber = counter++;
    dirNumberMutex.release();

    const browser: puppeteer.Browser = await puppeteer.launch({
        headless: false,
        userDataDir: path.resolve(__dirname, `loginDirs/userDir${dirNumber}`),
        args: [
            '--no-sandbox',
            '--lang=en-GB'
        ]
    });

    let id: string | null = null;

    const page: puppeteer.Page = await browser.newPage();
    try {
        id = await LoginAndGetId(page, username, password);
        return {
            status: true,
            username: username,
            inst_id: id,
        }
    } catch (e) {
        return {
            status: false,
            username: username,
            errorMessage: e.message,
        }
    } finally {
        await browser.close();
        if (id != null) {
            await copyUserFolderIntoCookiesDir(dirNumber, id);
        }
        await removeUserDirFolder(dirNumber);
    }
}


async function copyUserFolderIntoCookiesDir(counter: number, id: string) {
    await fs.copy(path.resolve(__dirname, `loginDirs/userDir${counter}`), path.resolve(__dirname, `cookies/${id}`));
}


async function removeUserDirFolder(counter: number) {
    await fs.remove(path.resolve(__dirname, `loginDirs/userDir${counter}`));
}


async function LoginAndGetId(page: puppeteer.Page, username: string, password: string) {


    await page.goto('https://www.instagram.com/accounts/login/');
    await page.waitForTimeout(3000);
    //await page.screenshot({path: '1-loginPage.png'});

    if ((await page.$('[href="/accounts/activity/"]') === null)) {
        await fillInputsAndSubmit(page, username, password);
    }

    return await getId(page, username);
}


async function fillInputsAndSubmit(page: puppeteer.Page, username: string, password: string) {
    await page.type('[name=username]', username);
    await page.type('[name=password]', password);
    //await page.screenshot({path: '1-passwordTyping.png'});
    await page.click('[type=submit]');
    await page.waitForTimeout(5000);
    if (await page.$('[role="alert"]') != null) {
        //await page.screenshot({path: 'errorEnter.png'});
        // Check ${path.resolve(__dirname, 'errorEnter.png')}
        throw new Error(`Instagram exception: probably wrong password`);
    }
    await page.waitForTimeout(5000);
    //await page.screenshot({path: '1-afterLogin.png'});
}

async function getId(page: puppeteer.Page, username: string): Promise<string> {
    let responseObject: any = await page.evaluate(async (username: string) => {
        try {
            const response = await fetch(`https://www.instagram.com/${username}/?__a=1`);
            return response.json();
        } catch (e){
            return null;
        }
    }, username);
    if (responseObject === null){
        throw new Error('Error while fetching id');
    }
    return responseObject.graphql.user.id;
}

