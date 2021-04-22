import puppeteer = require('puppeteer');
import path = require('path');
import {createBrowser} from "./browserCreation";

const fs = require('fs-extra');

const TIMEOUT: number = 5000;

export interface StatsRequest {
    action: string; //Start | Stop | Logout
    inst_id: string;
    timeout?: number;
}


export interface StatsResponse {
    inst_id: string;
    users?: Array<string>;
    error_message?: string;
}


export async function getFollowers(id: string): Promise<StatsResponse> {
    const isLogged: boolean = await fs.pathExists(path.resolve(__dirname, `cookies/${id}`));
    if (!isLogged) {
        return {
            inst_id: id,
            error_message: "User directory doesn't exist",
        }
    }

    let browser: puppeteer.Browser = await createBrowser(path.resolve(__dirname, path.resolve(__dirname, `cookies/${id}`)));

    const page: puppeteer.Page = (await browser.pages())[0];

    try {
        await page.goto('https://www.instagram.com/');
        await page.waitForTimeout(TIMEOUT);


        if (!(await isUserLoggedInInst(page))) {
            return {
                inst_id: id,
                error_message: "User isn't logged in",
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
            return {
                inst_id: id,
                error_message: 'Error while fetching followers',
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
        return {
            inst_id: id,
            error_message: e.message,
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
