"use strict";

var fs = require('fs');
var parser = require('./rofl');

var p07 = require('./p07');

// http://stackoverflow.com/a/4351548/5403184
process.argv.forEach(function (element, index) {
    if (index > 1) {
        fs.readFile(element, 'utf8', function (err, data) {
            if (err) {
                return console.log(err);
            }

            console.log(p07.process_assignments(parser.parse(data)));
        });
    }
});

