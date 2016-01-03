#!/usr/bin/python3

import fileinput

def parse_instruction(line):
    token = line.split()
    if token[0][:2] == 'ji':
        token[1] = token[1][:-1]
    try:
        return {
            'insn': token[0],
            'arg0': token[1],
            'arg1': token[2]
        }
    except IndexError:
        return {
            'insn': token[0],
            'arg0': token[1]
        }

def execute(pgrm):
    reg = {'a': 1, 'b': 0}
    ip = 0
    while ip >= 0 and ip < len(pgrm):
        print(ip, pgrm[ip], reg)
        if pgrm[ip]['insn'] == 'hlf':
            reg[pgrm[ip]['arg0']] /= 2
            ip += 1
        elif pgrm[ip]['insn'] == 'tpl':
            reg[pgrm[ip]['arg0']] *= 3
            ip += 1
        elif pgrm[ip]['insn'] == 'inc':
            reg[pgrm[ip]['arg0']] += 1
            ip += 1
        elif pgrm[ip]['insn'] == 'jmp':
            ip += int(pgrm[ip]['arg0'])
        elif pgrm[ip]['insn'] == 'jie':
            if reg[pgrm[ip]['arg0']] % 2 == 0:
                ip += int(pgrm[ip]['arg1'])
            else:
                ip += 1
        elif pgrm[ip]['insn'] == 'jio':
            if reg[pgrm[ip]['arg0']] == 1:
                ip += int(pgrm[ip]['arg1'])
            else:
                ip += 1
        else:
            raise ValueError(pgrm[ip]['insn'])

    print(ip, reg)
    return reg

def main():
    code_segment = []
    for line in fileinput.input():
        code_segment.append(parse_instruction(line))

    registers = execute(code_segment)

    print(registers['b'])

if __name__ == '__main__':
    main()

