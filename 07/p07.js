module.exports = (function () {
    "use strict";
    var assert, wires;

    assert = require('assert');

    function mask(value) {
        return 0x0000ffff & value;
    }

    function bitwise_inverse(value) {
        return mask(~value);
    }

    function do_a_binop(left, op, right) {
        // console.log('trying to do', op, 'on', left, 'and', right);
        if (op === 'AND') {
            return mask(left & right);
        }

        if (op === 'OR') {
            return mask(left | right);
        }

        if (op === 'RSHIFT') {
            return mask(left >> right);
        }

        if (op === 'LSHIFT') {
            return mask(left << right);
        }

        assert(op !== op);
    }

    function evaluate_expression(expr, level) {
        var left, right, result;

        // console.log(Array(level + 1).join("    "), level, expr);

        if (expr.type === 'int') {
            return expr;
        }

        if (expr.type === 'unop') {
            assert(expr.hasOwnProperty('op'));
            assert(expr.hasOwnProperty('arg'));
            assert(expr.op === 'NOT');

            result = evaluate_expression(expr.arg, level + 1);

            return {
                type: 'int',
                value: bitwise_inverse(result.value)
            };
        }

        if (expr.type === 'binop') {
            assert(expr.hasOwnProperty('left'));
            assert(expr.hasOwnProperty('right'));
            assert(expr.hasOwnProperty('op'));

            left = evaluate_expression(expr.left, level + 1);
            right = evaluate_expression(expr.right, level + 1);

            return {
                type: 'int',
                value: do_a_binop(left.value, expr.op, right.value),
            };
        }

        if (expr.type === 'wire') {
            assert(expr.hasOwnProperty('value'));
            result = evaluate_expression(wires[expr.value], level + 1);
            // console.log('found a wire', expr.value, 'with value', result);
            return result;
        }
    }

    function evaluate_all_the_statements() {
        var wire_name, result;

        for (wire_name in wires) {
            if (wires.hasOwnProperty(wire_name)) {
                console.log('evaluating', wire_name);
                result = evaluate_expression(wires[wire_name], 0);
                console.log(wire_name, 'result', result);
                wires[wire_name] = result;
            }
        }

        return wires;
    }

    function process_assignments(parse_tree) {

        wires = {};

        // https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Array/forEach
        parse_tree.forEach(function (element) {
            // assume a lot of convenient things!
            assert(element.type === 'assignment');
            assert(element.right.type === 'wire');
            assert(!wires.hasOwnProperty(element.right.value));

            wires[element.right.value] = element.left;
        });

        evaluate_all_the_statements();

        return wires;
    }

    return {
        process_assignments: process_assignments,
        evaluate_statement: evaluate_expression
    };
}());

