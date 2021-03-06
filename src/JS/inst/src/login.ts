import puppeteer = require('puppeteer');
import path = require('path');
import {Mutex} from "async-mutex";
import {createBrowser} from "./browserCreation";
import fs from "fs-extra";
import * as File from './file';
import * as Random from './random'


interface LoginRequestState{
    status: 'Login'
    username: string
    body: string
}

interface OtherRequestState{
    status: 'DoubleAuth' | 'Sus' | 'PhoneCheck'
    username: string
    body: string
}

export type LoginRequest = LoginRequestState | OtherRequestState

interface SuccessResponse{
    status: 'Success'
    username: string
    is_private: boolean
    inst_id: string
}

interface ErrorResponse{
    status: 'Error'
    username: string
    error_message: string
}

interface CheckResponse{
    status: 'DoubleAuth' | 'Sus' | 'PhoneCheck'
    username: string
}

export type LoginResponse = SuccessResponse | ErrorResponse | CheckResponse


interface UserIdAndPrivacy {
    inst_id: string;
    is_private: boolean;
}

interface Checks {
    doubleAuth: boolean;
    phoneCheck: boolean;
    sus: boolean;
}

enum CheckResult {
    ok,
    doubleAuth,
    phoneCheck,
    sus,
}

interface UserData{
    username: string
    code: string
}

export interface BrowserData {
    browser: puppeteer.Browser;
    page: puppeteer.Page;
    dirNumber: number;
}

type Action = (data: UserData) => Promise<void>;



export class Login {
    private static dirNumberMutex = new Mutex();
    private static dirCounter: number = 0;

    private readonly browser: puppeteer.Browser;
    private readonly page: puppeteer.Page;
    private readonly dirNumber: number;
    private instId: string | null = null;

    public static async getNewDirNumber(): Promise<number>{
        await Login.dirNumberMutex.acquire();
        let dirNumber = Login.dirCounter++;
        Login.dirNumberMutex.release();
        return dirNumber;
    }

    // A constructor cannot be async, so I created async function for getting browser data
    public static async getBrowserAndPage(dirNumber: number): Promise<BrowserData> {

        let browser: puppeteer.Browser = await createBrowser(path.resolve(__dirname, `loginDirs/userDir${dirNumber}`));

        let page = (await browser.pages())[0]
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

    private async runAction(action: Action, username: string, code: string, checks: Checks): Promise<LoginResponse> {
        let isCheck: boolean = false;
        let wasError: boolean = false;
        try {

            await action.call(this, {
                username: username,
                code: code,
            });

            const check = await this.runChecks(checks);

            if (check != CheckResult.ok) {
                isCheck = true;
            }

            if (checks.doubleAuth && check === CheckResult.doubleAuth) {
                return {
                    status: 'DoubleAuth',
                    username: username,
                }
            }
            if (checks.phoneCheck && check === CheckResult.phoneCheck) {
                return {
                    status: 'PhoneCheck',
                    username: username,
                }
            }
            if (checks.sus && check === CheckResult.sus) {
                return {
                    status: 'Sus',
                    username: username,
                }
            }
            if (check === CheckResult.ok) {
                const userIdAndPrivacy = await this.afterAuthMenu(username);

                return {
                    status: 'Success',
                    username: username,
                    inst_id: userIdAndPrivacy.inst_id,
                    is_private: userIdAndPrivacy.is_private,
                }
            }

            wasError = true;
            await File.screenError(`${this.dirNumber}-login.png`, this.page);
            await File.saveHTML(`${this.dirNumber}-login.html`, this.page);
            return {
                status: 'Error',
                username: username,
                error_message: `THIS EXCEPTION SHOULD NEVER OCCUR! Info: checks: double:${checks.doubleAuth}, phone:${checks.phoneCheck}, sus:${checks.sus}, check: ${check}`,
            }
        } catch (e) {
            wasError = true;
            await File.screenError(`${this.dirNumber}-action.png`, this.page);
            await File.saveHTML(`${this.dirNumber}-action.html`, this.page);
            return {
                status: 'Error',
                username: username,
                error_message: `${e.message}, Check ${this.dirNumber}-action.{png/html}`,
            }
        } finally {
            let correctFinishing: boolean = !isCheck && !wasError;
            let justFinishing: boolean = !isCheck || wasError;

            if (justFinishing) {
                await this.browser.close();
            }
            if (correctFinishing) {
                await File.copyUserDirIntoCookiesDir(this.dirNumber, this.instId as string);
            }
            if (justFinishing) {
                await deleteUserDir(this.dirNumber);
            }
        }
    }


    public async login(username: string, password: string): Promise<LoginResponse> {
        return await this.runAction.call(this, this.loginAction, username, password, {
            doubleAuth: true,
            phoneCheck: true,
            sus: true,
        });
    }

    private async loginAction(data: UserData): Promise<void> {
        await this.page.goto('https://www.instagram.com/accounts/login/');

        await this.clickAcceptCookies();

        await this.fillInputsAndSubmit(data.username, data.code);

        await this.page.waitForTimeout(Random.getRandomDelay(5000, 30));
    }

    public async doubleAuth(username: string, code: string): Promise<LoginResponse> {
        return await this.runAction.call(this, this.doubleAuthAction, username, code, {
            doubleAuth: false,
            phoneCheck: true,
            sus: true,
        });
    }

    private async doubleAuthAction(data: UserData): Promise<void> {
        await this.page.type('[name=verificationCode]', data.code, {
            delay: Random.getRandomDelay(400, 30),
        });

        await this.page.addScriptTag({path: require.resolve('jquery')});
        await this.page.evaluate(() => {
            $('button:contains("Confirm")').addClass('followerGettingApp');
            $('button:contains("Подтвердить")').addClass('followerGettingApp');
        });
        await this.page.click('.followerGettingApp');

        await this.page.waitForTimeout(Random.getRandomDelay(8000, 30));
    }

    public async sus(username: string, code: string): Promise<LoginResponse> {
        return await this.runAction.call(this, this.susAction, username, code, {
            doubleAuth: false,
            phoneCheck: true,
            sus: false,
        });
    }

    private async susAction(data: UserData): Promise<void> {
        await this.page.type('[aria-label="Security code"]', data.code, {
            delay: Random.getRandomDelay(400, 30),
        });

        await this.page.addScriptTag({path: require.resolve('jquery')});
        await this.page.evaluate(() => {
            $('button:contains("Submit")').addClass('submitButton');
        });
        await this.page.click('.submitButton');

        await this.page.waitForTimeout(Random.getRandomDelay(8000, 30));
    }

    private async runChecks(checks: Checks): Promise<CheckResult> {
        if (checks.doubleAuth && await this.isDoubleAuth()) {
            return CheckResult.doubleAuth;
        }
        if (checks.phoneCheck && await this.isPhoneCheck()) {
            return CheckResult.phoneCheck;
        }
        if (checks.sus && await this.isSus()) {
            return CheckResult.sus;
        }

        await this.checkUnusualPlace();

        return CheckResult.ok;
    }

    public async phoneCheck(username: string, code: string): Promise<LoginResponse> {
        return await this.runAction.call(this, this.runPhoneCheck, username, code, {
            doubleAuth: false,
            phoneCheck: false,
            sus: true,
        });
    }

    private async runPhoneCheck(data: UserData): Promise<void> {
        await this.page.type('[name="tel"]', data.code, {
            delay: Random.getRandomDelay(400, 30),
        });

        await this.page.addScriptTag({path: require.resolve('jquery')});
        await this.page.evaluate(() => {
            $('button:contains("Submit")').addClass('submitButton1');
        });
        await this.page.click('.submitButton1');

        await this.page.waitForTimeout(Random.getRandomDelay(8000, 30));
    }


    private async isPhoneCheck(): Promise<boolean> {
        await this.page.addScriptTag({path: require.resolve('jquery')});
        await this.page.evaluate(() => {
            $('button:contains("Send Confirmation")').addClass('phoneCheck');
        });
        if (await this.page.$('.phoneCheck') == null) {
            return false;
        }
        //TEST SHIT
        await File.screenError(`${this.dirNumber}-phoneCheck.png`, this.page);
        await File.saveHTML(`${this.dirNumber}-phoneCheck.html`, this.page);
        await this.page.click('.phoneCheck');
        return true;
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

        await this.page.waitForTimeout(Random.getRandomDelay(2000, 30));
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
        await this.page.waitForTimeout(Random.getRandomDelay(3000, 30));
        const userIdAndPrivacy = await this.getIdAndPrivacy(username);
        this.instId = userIdAndPrivacy.inst_id;
        if (await File.isUserLoggedInBot(this.instId)) {
            throw new Error('User with this inst_id already exist');
        }
        return userIdAndPrivacy;
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
            $('button:contains("Send Security Code")').addClass('suspicious');
        });
        if (await this.page.$('.suspicious') == null) {
            return false;
        }
        //TEST SHIT
        await File.screenError(`${this.dirNumber}-suss.png`, this.page);
        await File.saveHTML(`${this.dirNumber}-suss.html`, this.page);
        await this.page.click('.suspicious');
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
        await this.page.type('[name=username]', username, {
            delay: Random.getRandomDelay(150, 30),
        });
        await this.page.type('[name=password]', password, {
            delay: Random.getRandomDelay(250, 30),
        });
        await this.page.click('[type=submit]');
        await this.page.waitForTimeout(Random.getRandomDelay(7000, 30));
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

export async function deleteUserDir(dirNumber: number) {
    await fs.remove(path.resolve(__dirname, `loginDirs/userDir${dirNumber}`));
}
