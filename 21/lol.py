#!/usr/bin/python3

class Player:
    playercount = 0
    def __init__(self, name=None, hp=0, dmg=0, ap=0):
        Player.playercount += 1

        if name is None:
            self.name = 'Player {}'.format(Player.playercount)
        else:
            self.name = name

        self.hp = hp
        self.dmg = dmg
        self.ap = ap

class Game:
    def __init__(self, p1, p2):
        self.p1 = p1
        self.p2 = p2

    def has_winner(self):
        return self.p1.hp <= 0 or self.p2.hp <= 0

    def attack(self, attacker, target):
        dmg = max(1, attacker.dmg - target.ap)
        target.hp -= dmg
        return {
            'dmg': dmg
        }

