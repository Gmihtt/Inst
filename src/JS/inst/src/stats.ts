import puppeteer = require('puppeteer');
import path = require('path');
import {createBrowser} from "./browserCreation";
import * as File from './file'

const fs = require('fs-extra');

const TIMEOUT: number = 5000;

export interface StatsRequest {
    status: string; //Start | Stop | Logout
    inst_id: string;
    timeout?: number;
}


export interface StatsResponse {
    inst_id: string;
    users?: Array<string>;
    error_message?: string;
    error_code?: string; // USER_IS_NOT_LOGGED | FETCHING_ERROR  | NO_USER_DIR | LOGOUT_FAILURE | LOGOUT_NO_USER | OTHER_ERROR_1 | OTHER_ERROR_2
}


export async function getFollowers(id: string): Promise<StatsResponse> {
    const isLogged: boolean = await fs.pathExists(path.resolve(__dirname, `cookies/${id}`));
    if (!isLogged) {
        return {
            inst_id: id,
            error_message: "User directory doesn't exist",
            error_code: 'NO_USER_DIR'
        }
    }

    let browser: puppeteer.Browser = await createBrowser(path.resolve(__dirname, path.resolve(__dirname, `cookies/${id}`)));

    const page: puppeteer.Page = (await browser.pages())[0];

    try {
        await page.goto('https://www.instagram.com/');
        await page.waitForTimeout(TIMEOUT);


        if (!(await isUserLoggedInInst(page))) {
            let screenNum = File.screenErrorStats(page);
            let htmlNum = File.saveHTMLStats(page);
            return {
                inst_id: id,
                error_message: `User isn't logged in. Check ${screenNum}.png, ${htmlNum}.html`,
                error_code: 'USER_IS_NOT_LOGGED',
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
            let screenNum = File.screenErrorStats(page);
            let htmlNum = File.saveHTMLStats(page);
            return {
                inst_id: id,
                error_message: `Error while fetching followers. Check ${screenNum}.png, ${htmlNum}.html`,
                error_code: 'FETCHING_ERROR',
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
        let screenNum = File.screenErrorStats(page);
        let htmlNum = File.saveHTMLStats(page);
        return {
            inst_id: id,
            error_message: e.message + `. Check ${screenNum}.png, ${htmlNum}.html`,
            error_code: 'OTHER_ERROR_1'
        }
    } finally {
        await browser.close();
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
