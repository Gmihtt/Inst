/**
 * @param delay: desired delay
 * @param spread: in percents!! result = delay +- spread%
*/
export function getRandomDelay(delay: number, spread: number): number {
    if (spread < 0 || spread > 100) {
        throw new Error('Wrong spread: should be 0-100');
    }
    let border: number = delay * (spread * 2) / 100;
    let adding: number = Math.ceil(Math.random() * border);
    let balancedAdding = adding - border / 2;
    return delay + balancedAdding;
}