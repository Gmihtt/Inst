import puppeteer = require('puppeteer');
import path = require('path');
import {Mutex} from "async-mutex";
import {createBrowser} from "./browserCreation";
import fs from "fs-extra";
//const fetchNode = require('node-fetch');
import * as File from './file';

export interface LoginRequest {
    type: string; // Login | DoubleAuth | Sus
    username: string;
    body: string;
}

export interface LoginResponse {
    status: boolean;
    username: string;
    is_private?: boolean;
    inst_id?: string;
    is_double_auth?: boolean;
    is_sus_login?: boolean;
    error_message?: string;
}

interface UserIdAndPrivacy {
    inst_id: string;
    is_private: boolean;
}


export interface BrowserData {
    browser: puppeteer.Browser;
    page: puppeteer.Page;
    dirNumber: number;
}


let dirNumberMutex = new Mutex();
let dirCounter: number = 0;

export class Login {
    private readonly browser: puppeteer.Browser;
    private readonly page: puppeteer.Page;
    private readonly dirNumber: number;
    private instId: string | null = null;

    //A constructor cannot be async so I created async function for getting browser data
    public static async getBrowserAndPage(): Promise<BrowserData> {
        await dirNumberMutex.acquire();
        let dirNumber = dirCounter++;
        dirNumberMutex.release();

        let browser: puppeteer.Browser = await createBrowser(path.resolve(__dirname, `loginDirs/userDir${dirNumber}`));

        let page = await browser.newPage();
        return {
            browser: browser,
            page: page,
            dirNumber: dirNumber,
        };
    }

    constructor(browserData: BrowserData) {
        this.browser = browserData.browser;
        this.page = browserData.page;
        this.dirNumber = browserData.dirNumber;
    }


    public async login(username: string, password: string): Promise<LoginResponse> {
        let isDouble: boolean = false;
        let isSus: boolean = false;
        let wasError: boolean = false;
        try {
            await this.page.goto('https://www.instagram.com/accounts/login/');

            await this.clickAcceptCookies();

            await this.fillInputsAndSubmit(username, password);

            isDouble = await this.isDoubleAuth();

            if (isDouble) {
                return {
                    status: true,
                    username: username,
                    is_double_auth: true,
                }
            }

            await this.checkUnusualPlace();

            isSus = await this.isSus();

            if (isSus){
                return {
                    status: true,
                    username: username,
                    is_double_auth: false,
                    is_sus_login: true,
                }
            }

            const userIdAndPrivacy = await this.afterAuthMenu(username);

            return {
                status: true,
                username: username,
                inst_id: userIdAndPrivacy.inst_id,
                is_double_auth: false,
                is_private: userIdAndPrivacy.is_private,
            }

        } catch (e) {
            wasError = true;
            await File.screenError(`${this.dirNumber}-login.png`, this.page);
            await File.saveHTML(`${this.dirNumber}-login.html`, this.page);
            return {
                status: false,
                username: username,
                error_message: `${e.message}, Check ${this.dirNumber}-login.{png/html}`,
            }
        } finally {

            let correctFinishing: boolean = !isDouble && !wasError && !isSus;
            let justFinishing: boolean = !isDouble || wasError || !isSus;

            if (justFinishing) {
                await this.browser.close();
            }
            if (correctFinishing) {
                await File.copyUserDirIntoCookiesDir(this.dirNumber, this.instId as string);
            }
            if (justFinishing) {
                await this.removeUserDir();
            }
        }
    }

    // I need username only for creating response object. Maybe I should get rid of this. Same for sus
    public async doubleAuth(username: string, code: string): Promise<LoginResponse> {
        let isSus: boolean = false;
        try {
            await this.page.type('[name=verificationCode]', code);
            await this.page.addScriptTag({path: require.resolve('jquery')});
            await this.page.evaluate(() => {
                $('button:contains("Confirm")').addClass('followerGettingApp');
                $('button:contains("Подтвердить")').addClass('followerGettingApp');
            });
            await this.page.click('.followerGettingApp');
            await this.page.waitForTimeout(8000);


            isSus = await this.isSus();
            if (isSus){
                return {
                    status: true,
                    username: username,
                    is_sus_login: true,
                }
            }

            const userIdAndPrivacy = await this.afterAuthMenu(username);
            await this.browser.close();
            await File.copyUserDirIntoCookiesDir(this.dirNumber, this.instId as string);
            return {
                status: true,
                username: username,
                inst_id: userIdAndPrivacy.inst_id,
                is_private: userIdAndPrivacy.is_private,

            }
        } catch (e) {
            await File.screenError(`${this.dirNumber}-doubleAuth.png`, this.page);
            await File.saveHTML(`${this.dirNumber}-doubleAuth.html`, this.page);
            await this.browser.close();
            return {
                status: false,
                username: username,
                error_message: e.message + `Check ${this.dirNumber}-doubleAuth.{png/html}`,
            }
        } finally {
            if (!isSus) {
                await this.removeUserDir();
            }
        }
    }

    public async sus(username:string, code: string) {
        try {
            await this.page.type('[aria-label="Security code"]', code);
            await this.page.addScriptTag({path: require.resolve('jquery')});
            await this.page.evaluate(() => {
                $('button:contains("Submit")').addClass('submitButton');
            });
            await this.page.click('.submitButton');
            await this.page.waitForTimeout(8000);

            const userIdAndPrivacy = await this.afterAuthMenu(username);
            await this.browser.close();
            await File.copyUserDirIntoCookiesDir(this.dirNumber, this.instId as string);
            return {
                status: true,
                username: username,
                inst_id: userIdAndPrivacy.inst_id,
                is_private: userIdAndPrivacy.is_private,

            }
        } catch (e){
            await File.screenError(`${this.dirNumber}-sus.png`, this.page);
            await File.saveHTML(`${this.dirNumber}-sus.html`, this.page);
            await this.browser.close();
            return {
                status: false,
                username: username,
                error_message: e.message + `Check ${this.dirNumber}-sus.{png/html}`,
            }
        } finally {
            await this.removeUserDir();
        }
    }

    private async clickAcceptCookies(): Promise<void> {
        await this.page.addScriptTag({path: require.resolve('jquery')});
        await this.page.evaluate(() => {
            $('button:contains("Accept")').addClass('cookiesAcceptInst');
            $('button:contains("Принять")').addClass('cookiesAcceptInst');
        });
        if (await this.page.$('.cookiesAcceptInst') != null) {
            await this.page.click('.cookiesAcceptInst');
        }
        await this.page.waitForTimeout(2000);
    }


    private async isUserLoggedInInst(): Promise<boolean> {
        try {
            let response: boolean = await this.page.evaluate(async () => {
                return (await fetch(`https://www.instagram.com/accounts/activity/?__a=1`)).redirected;
            });
            return !response;
        } catch {
            return false;
        }
    }


    private async afterAuthMenu(username: string): Promise<UserIdAndPrivacy> {
        if (!(await this.isUserLoggedInInst())) {
            throw new Error(`User wasn't logged in.`)
        }
        await this.clickSaveInfoButton();
        await this.page.waitForTimeout(3000);
        const userIdAndPrivacy = await this.getIdAndPrivacy(username);
        this.instId = userIdAndPrivacy.inst_id;
        if (await File.isUserLoggedInBot(this.instId)) {
            throw new Error('User with this inst_id already exist');
        }
        return userIdAndPrivacy;
    }

    private async removeUserDir() {
        await fs.remove(path.resolve(__dirname, `loginDirs/userDir${this.dirNumber}`));
    }

    private async isDoubleAuth() {
        return await this.page.$('#verificationCodeDescription') != null;
    }

    private async checkUnusualPlace() {
        /*
        <button name="choice" value="0" class="_5f5mN    -fzfL    KUBKM      yZn4P   ">This Was Me</button>
        */
        await this.page.addScriptTag({path: require.resolve('jquery')});
        await this.page.evaluate(() => {
            $('button:contains("This Was Me")').addClass('thisWasMe');
        });
        if (await this.page.$('.thisWasMe') != null) {
            await this.page.click('.thisWasMe');
        }
    }

    private async isSus() {
        await this.page.addScriptTag({path: require.resolve('jquery')});
        await this.page.evaluate(() => {
            $('button:contains("Send Security Code")').addClass('suuuus');
        });
        if (await this.page.$('.suuuus') == null) {
            return false;
        }
        await this.page.click('.suuuus');
        return true;
    }


    private async clickSaveInfoButton() {
        await this.page.addScriptTag({path: require.resolve('jquery')});
        await this.page.evaluate(() => {
            $('button:contains("Save Info")').addClass('saveInfoButton');
            //$('button:contains("Подтвердить")').addClass('followerGettingApp');
        });
        if (await this.page.$('.saveInfoButton') != null) {
            await this.page.click('.saveInfoButton');
        }
    }


    private async fillInputsAndSubmit(username: string, password: string) {
        await this.page.waitForSelector('[name=username]');
        await this.page.type('[name=username]', username);
        await this.page.type('[name=password]', password);
        await this.page.click('[type=submit]');
        await this.page.waitForTimeout(7000);
        if ((await this.page.$$('button[disabled=""]')).length != 0) {
            throw new Error(`Instagram exception: button is inactive`);
        }
        if (await this.page.$('[role="alert"]') != null) {
            throw new Error(`Instagram exception: probably wrong password`);
        }
    }

    private async getIdAndPrivacy(username: string): Promise<UserIdAndPrivacy> {

        let responseObject: any = await this.page.evaluate(async (username: string) => {
            try {
                const response = await fetch(`https://www.instagram.com/${username}/?__a=1`);
                return response.json();
            } catch (e) {
                return null;
            }
        }, username);
        if (responseObject === null) {
            throw new Error('Error while fetching id and privacy');
        }
        return {
            inst_id: responseObject.graphql.user.id,
            is_private: responseObject.graphql.user.is_private,
        };
    }
}

