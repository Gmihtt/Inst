declare module "puppeteer"{
    import {Serializable} from "puppeteer/lib/cjs/puppeteer/common/EvalTypes";

    interface Browser{
        newPage(): Promise<Page>;
        close(): Promise<any>
    }
    interface Page{
        goto(url: string): Promise<HTTPResponse>;
        waitForTimeout(milliseconds: number): Promise<any>;
        evaluate(func: any, ...args: any): Promise<Serializable>
        $(selector: string): Promise<any>;
        type(selector: string, text: string): Promise<any>;
        click(selector: string): Promise<any>;
    }
    interface HTTPResponse{}
    function launch(obj: object): Promise<Browser>;
}