import winston = require('winston');
import path = require('path');
import _ from 'lodash';



const filename = _.now().toString();

export const Logger: winston.Logger = winston.createLogger({
    format: winston.format.combine(
        winston.format.timestamp({
            format: 'YYYY-MM-DD HH:mm:ss'
        }),
        winston.format.errors({stack: true}),
        winston.format.splat(),
        winston.format.json()
    ),
    transports: [
        new winston.transports.File({filename: `${filename}_log.log`, dirname: path.resolve(__dirname)}),
    ]
});
Logger.add(new winston.transports.Console({
    format: winston.format.combine(
        winston.format.colorize(),
        winston.format.simple()
    )
}));
