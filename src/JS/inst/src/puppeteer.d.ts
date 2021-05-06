declare module "puppeteer" {
    //import {Serializable} from "puppeteer/lib/cjs/puppeteer/common/EvalTypes";

    interface Browser {
        newPage(): Promise<Page>;

        pages(): Promise<Array<Page>>;

        close(): Promise<any>;
    }

    interface Credentials {
        username: string;
        password: string;
    }

    interface Viewport{
        width: number,
        height: number,

    }

    interface Page {
        goto(url: string): Promise<HTTPResponse>;

        screenshot(options: any): Promise<any>;

        content(): Promise<string>;

        waitForTimeout(milliseconds: number): Promise<any>;

        evaluate(func: any, ...args: any): Promise<any>

        $(selector: string): Promise<any>;

        $$(selector: string): Promise<Array<any>>;

        $$eval(selector: string, pageFunction: Function, ...args: any): Promise<any>;

        addScriptTag(options: any): Promise<any>;

        type(selector: string, text: string, options: object): Promise<any>;

        authenticate(credentials: Credentials): Promise<void>;

        click(selector: string): Promise<any>;

        waitForNavigation(options?: any): Promise<HTTPResponse>;

        waitForSelector(selector: string, options?: any): Promise<any>;

        setViewport(viewport: Viewport): Promise<void>;
    }

    interface HTTPResponse {
    }

    function launch(obj: object): Promise<Browser>;
}