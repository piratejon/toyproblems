#!/usr/bin/python3

PART1INPUT='vzbxkghb'

ALPHABET='abcdefghjkmnpqrstuvwxyz'
ALPHABET_ORD = [ord(_) for _ in ALPHABET]
ALPHA_MAP_NEXT =  {ALPHABET[_ - 1]: ALPHABET[_] for _ in range(1, len(ALPHABET))}
ALPHA_ORD_MAP_NEXT = {ord(k): ord(v) for k, v in ALPHA_MAP_NEXT.items()}

def alphafy(ords):
    return ''.join([chr(_) for _ in ords])

def list_compare(a, b):
    for l, r in zip(a, b):
        if a > b:
            return 1
        elif a < b:
            return -1

    return 0

class PasswordIncrementer:

    BADCHARS = frozenset(['i', 'o', 'l'])
    BADCHARS_ORD = frozenset({ord(_) for _ in BADCHARS})

    @staticmethod
    def test_has_straight(string):
        return PasswordIncrementer.test_has_straight_ords([ord(_) for _ in string])

    @staticmethod
    def test_has_straight_ords(ords):
        def straight_from(i):
            return ords[i - 2] + 1 == ords[i - 1] and ords[i - 1] + 1 == ords[i]

        for i in range(2, len(ords)):
            if straight_from(i):
                return True

        return False

    @staticmethod
    def test_has_two_doubles(string):
        return PasswordIncrementer.test_has_two_doubles_ords([ord(_) for _ in string])

    @staticmethod
    def test_has_two_doubles_ords(ords):
        for i in range(len(ords) - 1):
            if ords[i] == ords[i + 1]:
                for j in range(i + 2, len(ords) - 1):
                    if ords[j] == ords[j + 1]:
                        return True
                return False
        return False

    @staticmethod
    def test_no_uggos(string):
        for c in string:
            if c in PasswordIncrementer.BADCHARS:
                return False

        return True

    @staticmethod
    def test_no_uggos_ord(string):
        for c in string:
            if c in PasswordIncrementer.BADCHARS_ORD:
                return False

        return True
        
    @staticmethod
    def simple_increment(ords, end = 0):
        if end < 8:
            try:
                ords[-1 - end] = ALPHA_ORD_MAP_NEXT[ords[-1 - end]]
                return 0
            except KeyError:
                r = PasswordIncrementer.simple_increment(ords, end + 1)
                if r == 0:
                    ords[-1 - end] = ALPHABET_ORD[0]
                    return 0
                elif r == 1:
                    ords[-1 - end] = ALPHABET_ORD[0]
                    return 1
                elif r == 2:
                    ords[-1 - end] = ALPHABET_ORD[0]
                    return 1
        else:
            return 2

    @staticmethod
    def increment(string):
        s2 = string
        while True:
            # lol this may never terminate!
            PasswordIncrementer.simple_increment(s2)
            if PasswordIncrementer.test_has_straight_ords(s2) and PasswordIncrementer.test_has_two_doubles_ords(s2) and PasswordIncrementer.test_no_uggos_ord(s2):
                break
        return s2

    def __init__(self, current):
        self.current = [ord(_) for _ in current]

    def __iter__(self, current):
        if current is not None:
            self.current = current
        return self

    def __next__(self):
        trial = PasswordIncrementer.increment(self.current[:])
        if list_compare(trial, self.current) > 0:
            self.current = trial
            return alphafy(self.current)
        else:
            raise StopIteration

def part1lol(inputstring):
    pass

if __name__ == '__main__':
    part1lol(PART1INPUT)

