import puppeteer = require('puppeteer');
import path = require('path');
import {createBrowser} from "./browserCreation";
import * as File from './file'
import * as Random from './random'
import {Proxy} from "./browserCreation";

export interface StatsStart {
    status: 'Start'
    proxy: Proxy
    inst_id: string
    timeout?: number
}

export interface StatsOther {
    status: 'Stop' | 'Logout'
    inst_id: string
}

export type StatsRequest = StatsStart | StatsOther

type ErrorCode = 'USER_IS_NOT_LOGGED' | 'FETCHING_ERROR' | 'NO_USER_DIR' | 'LOGOUT_FAILURE' | 'LOGOUT_NO_USER' |
    'OTHER_ERROR_1' | 'OTHER_ERROR_2' | 'LOGIC_ERROR'

export interface Error {
    error_message: string
    error_code: ErrorCode
}

export interface StatsResponse {
    inst_id: string
    users?: Array<string>
    error?: Error
}

export interface BrowserData {
    browser: puppeteer.Browser
    page: puppeteer.Page
}

export interface BrowserState {
    state: 'browser'
    browserData: BrowserData
}

export interface ErrorState {
    state: 'error'
    errorMessage: string
    errorCode: ErrorCode
}

export type BrowserCreation = BrowserState | ErrorState;


export async function getInstPageBrowser(id: string, proxy: Proxy): Promise<BrowserCreation> {

    if (!await File.isUserLoggedInBot(id)) {
        return {
            state: 'error',
            errorMessage: "User directory doesn't exist",
            errorCode: 'NO_USER_DIR',
        };
    }

    let browser: puppeteer.Browser = await createBrowser(path.resolve(__dirname, path.resolve(__dirname, `cookies/${id}`)), proxy);

    const page: puppeteer.Page = (await browser.pages())[0];

    try {
        await page.goto('https://www.instagram.com/');
        await page.waitForTimeout(Random.getRandomDelay(8000, 30));

        if (!(await isUserLoggedInInst(page))) {
            let screenObj = await File.screenErrorStats(page);
            let htmlObj = await File.saveHTMLStats(page);
            return {
                state: 'error',
                errorMessage: `User isn't logged in. ${screenAndHtml(screenObj, htmlObj)}`,
                errorCode: 'USER_IS_NOT_LOGGED',

            }
        }

        return {
            state: 'browser',
            browserData: {
                browser: browser,
                page: page,
            },
        };

    } catch (e) {
        await browser.close();

        return {
            state: 'error',
            errorMessage: e.message,
            errorCode: 'OTHER_ERROR_1'
        }
    }

}

export async function getFollowers(id: string, browserData: BrowserData): Promise<StatsResponse> {
    const page = browserData.page;
    try {
        await page.goto('https://www.instagram.com/');
        await page.waitForTimeout(Random.getRandomDelay(5000, 30));

        if (!(await isUserLoggedInInst(page))) {
            let screenObj = await File.screenErrorStats(page);
            let htmlObj = await File.saveHTMLStats(page);
            return {
                inst_id: id,
                error: {
                    error_message: `User isn't logged in. ${screenAndHtml(screenObj, htmlObj)}`,
                    error_code: 'USER_IS_NOT_LOGGED',
                }
            }
        }

        let responseObject: any = await page.evaluate(async () => {
            try {
                const response: Response = await fetch(`https://www.instagram.com/accounts/activity/?__a=1&include_reel=true`);
                return response.json();
            } catch (e) {
                return null;
            }
        });

        if (responseObject === null) {
            const screenObj = await File.screenErrorStats(page);
            const htmlObj = await File.saveHTMLStats(page);
            return {
                inst_id: id,
                error: {
                    error_message: `Error while fetching followers. ${screenAndHtml(screenObj, htmlObj)}`,
                    error_code: 'FETCHING_ERROR',
                }
            }
        }

        let usersArray = responseObject.graphql.user.edge_follow_requests.edges;

        let IDs: Array<string> = [];

        for (let user of usersArray) {
            IDs.push(user.node.id);
        }

        return {
            inst_id: id,
            users: IDs,
        }

    } catch (e) {
        const screenObj = await File.screenErrorStats(page)
        const htmlObj = await File.saveHTMLStats(page);
        return {
            inst_id: id,
            error: {
                error_message: e.message + screenAndHtml(screenObj, htmlObj),
                error_code: 'OTHER_ERROR_1'
            }
        }
    }
}

async function isUserLoggedInInst(page: puppeteer.Page): Promise<boolean> {
    try {
        let response: boolean = await page.evaluate(async () => {
            return (await fetch(`https://www.instagram.com/accounts/activity/?__a=1`)).redirected;
        });
        return !response;
    } catch {
        return false;
    }
}

function screenAndHtml(screenObj: File.StatsScreen, htmlOjb: File.StatsScreen): string {
    return `screen: ${screenObj.message} - ${screenObj.counter}.png
html: ${htmlOjb.message} - ${htmlOjb.counter}.html`;
}