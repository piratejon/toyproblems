
var p07 = require('./p07');
var assert = require('assert');
var parser = require('./rofl');

function run_all_tests() {
    var e, wires;

    wires = {a: {type: 'int', value: 22}};
    e = p07.evaluate_statement(wires.a, wires);
    assert.deepEqual(e, wires.a);

    wires = {a: {type: 'int', value: 22}, b: {type: 'unop', op: 'NOT', arg: {type: 'wire', value: 'a'}}};
    e = p07.evaluate_statement(wires.a, wires);
    assert.deepEqual(e, wires.a);
    e = p07.evaluate_statement(wires.b, wires);
    assert.deepEqual(e, {type: 'int', value: 0xffe9});

    wires = {a: {type: 'int', value: 22}, b: {type: 'binop', op: 'OR', left: {type: 'wire', value: 'a'}, right: {type: 'int', value: 154} } };
    e = p07.evaluate_statement(wires.a, wires);
    assert.deepEqual(e, wires.a);
    e = p07.evaluate_statement(wires.b, wires);
    assert.deepEqual(e, {type: 'int', value: 0x009e});

    wires = {a: {type: 'int', value: 22}, b: {type: 'binop', op: 'AND', left: {type: 'wire', value: 'a'}, right: {type: 'int', value: 154} } };
    e = p07.evaluate_statement(wires.a, wires);
    assert.deepEqual(e, wires.a);
    e = p07.evaluate_statement(wires.b, wires);
    assert.deepEqual(e, {type: 'int', value: 0x0012});

    e = p07.process_assignments(parser.parse('9 RSHIFT 2 -> a\n99999 LSHIFT 16 -> c'));
    assert.deepEqual(e, {
        a: { type: 'int', value: 2 },
        c: { type: 'int', value: 0 }
    });

    e = p07.process_assignments(parser.parse('9 LSHIFT 2 -> a\n1235 LSHIFT 15 -> b'));
    assert.deepEqual(e, {
        a: { type: 'int', value: 36 },
        b: { type: 'int', value: 0x8000 },
    });

    e = p07.process_assignments(parser.parse('97 -> x'));
    assert.deepEqual(e, {x: {type: 'int', value: 97}});

    e = p07.process_assignments(parser.parse('02 -> y\ny -> x'));
    assert.deepEqual(e, {y: {type: 'int', value: 2}, x: {type: 'int', value: 2}});

    e = p07.process_assignments(parser.parse('3 -> y\n4 -> x\nx AND y -> z'));
    assert.deepEqual(e, {
        y: { type: 'int', value: 3 },
        x: { type: 'int', value: 4 },
        z: { type: 'int', value: 0 },
    });

    e = p07.process_assignments(parser.parse('3 -> y\n4 -> x\nNOT y -> z'));
    assert.deepEqual(e, {
        y: { type: 'int', value: 3 },
        x: { type: 'int', value: 4 },
        z: { type: 'int', value: 0xfffc },
    });

    e = p07.process_assignments(parser.parse('3 -> y\n4 -> x\ny AND 19 -> z'));
    assert.deepEqual(e, {
        y: { type: 'int', value: 3 },
        x: { type: 'int', value: 4 },
        z: { type: 'int', value: 3 },
    });

    e = p07.process_assignments(parser.parse('3 -> y\n177 -> x\nx AND y -> z\nz OR 77 -> b'));
    assert.deepEqual(e, {
        y: { type: 'int', value: 3 },
        x: { type: 'int', value: 177 },
        z: { type: 'int', value: 1 },
        b: { type: 'int', value: 77 },
    });

    e = p07.process_assignments(parser.parse('3 -> y\n177 -> x\nx AND y -> z\nz OR 77 -> b\nx AND 1024 -> c\nc OR 2 -> d\n'));
    assert.deepEqual(e, {
        y: { type: 'int', value: 3 },
        x: { type: 'int', value: 177 },
        z: { type: 'int', value: 1 },
        b: { type: 'int', value: 77 },
        c: { type: 'int', value: 0 },
        d: { type: 'int', value: 2 },
    });

    e = p07.process_assignments(parser.parse('3 -> y\n177 -> x\nx AND y -> z\nz OR 77 -> b\nx AND 1024 -> c\nc OR 2 -> d\nx -> a\n'));
    assert.deepEqual(e, {
        y: { type: 'int', value: 3 },
        x: { type: 'int', value: 177 },
        z: { type: 'int', value: 1 },
        b: { type: 'int', value: 77 },
        c: { type: 'int', value: 0 },
        d: { type: 'int', value: 2 },
        a: { type: 'int', value: 177 },
    });
}

run_all_tests();

