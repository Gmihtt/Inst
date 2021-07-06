import puppeteer = require('puppeteer');
import path = require('path');
import {createBrowser} from "./browserCreation";
import * as File from './file'
import * as Random from './random'
import {pageExtend} from "puppeteer-jquery";

export interface StatsStart {
    status: 'Start'
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

interface EvalState {
    ok: boolean,
    result?: any,
    error?: string,
}

export type BrowserCreation = BrowserState | ErrorState;


export async function getInstPageBrowser(id: string): Promise<BrowserCreation> {

    if (!await File.isUserLoggedInBot(id)) {
        return {
            state: 'error',
            errorMessage: "User directory doesn't exist",
            errorCode: 'NO_USER_DIR',
        };
    }

    let browser: puppeteer.Browser = await createBrowser(path.resolve(__dirname, path.resolve(__dirname, `cookies/${id}`)));

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

        const pagejQuery = pageExtend(page);

        await pagejQuery.jQuery('button:contains("Not Now")').addClass('.notNowButton');
        if (await page.$('.notNowButton') != null) {
            await page.click('.notNowButton');
        }

        await page.waitForTimeout(Random.getRandomDelay(2000, 20));

        await page.screenshot({path: '2-waitingToLoad.png'});
        await page.click('[href="/accounts/activity/"]');
        await page.waitForTimeout(10000);
        await page.screenshot({path: '2-afterClicking.png'});


        const isOkFollowButton: EvalState = await page.evaluate(() => {
            const divs = document.querySelectorAll('div');
            let followDiv: null | HTMLDivElement = null;
            for (let i = 0; i < divs.length; i++) {
                if (divs[i].innerText == 'Follow Requests') {
                    followDiv = divs[i];
                    break;
                }
            }
            if (followDiv == null) {
                return {
                    ok: false,
                    error: 'followDiv fail',
                };
            }
            const followButton = followDiv.parentElement;
            if (followButton == null) {
                return {
                    ok: false,
                    error: 'followButton fail'
                };
            }
            followButton.classList.add('theFollowRequestButton');
            return {
                ok: true,
            }
        });

        if (!isOkFollowButton.ok) {
            const screenObj = await File.screenErrorStats(page)
            const htmlObj = await File.saveHTMLStats(page);
            return {
                inst_id: id,
                error: {
                    error_message: isOkFollowButton.error + screenAndHtml(screenObj, htmlObj),
                    error_code: 'OTHER_ERROR_1'
                }
            }
        }

        await page.click('.theFollowRequestButton');
        await page.screenshot({path: '2-afterClickingFollowersButton.png'});
        await page.waitForTimeout(Random.getRandomDelay(5000, 20));



        const statsNames: EvalState = await page.evaluate(() => {
            let buttons = document.querySelectorAll('button');
            let firstButton: null | HTMLButtonElement = null;
            for (let i = 0; i < buttons.length; i++) {
                if (buttons[i].innerText == 'Confirm') {
                    firstButton = buttons[i];
                    break;
                }
            }
            if (firstButton == null){
                return {
                    ok: false,
                    error: 'firstButton fail',
                }
            }
            let block: null | HTMLElement = firstButton;
            for (let i = 0; i < 5; i++){
                block = block.parentElement;
                if (block == null){
                    return {
                        ok: false,
                        error: 'parentChain fail',
                    }
                }
            }
            let requests: Array<string> = [];
            for (let person of block.children){
                // @ts-ignore
                requests.push(person.children[1].children[0].children[0].innerText);
            }
            return {
                ok: true,
                result: requests,
            }
        });

        if (!statsNames.ok) {
            const screenObj = await File.screenErrorStats(page)
            const htmlObj = await File.saveHTMLStats(page);
            return {
                inst_id: id,
                error: {
                    error_message: statsNames.error + screenAndHtml(screenObj, htmlObj),
                    error_code: 'OTHER_ERROR_1'
                }
            }
        }

        return {
            inst_id: id,
            users: statsNames.result,
        }


        /*
        let blockDiv = await pagejQuery.jQuery('button:contains("Confirm")').first().parent().parent().parent().parent().parent();
        let userDivs = await blockDiv.children();
        let usersCount = 0;
        let requests: Array<string> = [];
        await userDivs.map((id: number, elm: HTMLElement) => {
            usersCount++;
            let usernameDiv = jQuery(elm).children().eq(1);
            let userLink = usernameDiv.children().first().children().first();
            requests.push(userLink.text());
        });

        return {
            inst_id: id,
            users: requests,
        };


*/
        /*let responseObject: any = await page.evaluate(async () => {
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
        }*/

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