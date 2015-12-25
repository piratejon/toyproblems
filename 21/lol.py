#!/usr/bin/python3

import fileinput

weapon_choices = [
    {'cost': 8, 'dmg': 4, 'ap': 0},
    {'cost': 10, 'dmg': 5, 'ap': 0},
    {'cost': 25, 'dmg': 6, 'ap': 0},
    {'cost': 40, 'dmg': 7, 'ap': 0},
    {'cost': 74, 'dmg': 8, 'ap': 0}
]
armor_choices = [
    {'cost': 0, 'dmg': 0, 'ap': 0},
    {'cost': 13, 'dmg': 0, 'ap': 1},
    {'cost': 31, 'dmg': 0, 'ap': 2},
    {'cost': 53, 'dmg': 0, 'ap': 3},
    {'cost': 75, 'dmg': 0, 'ap': 4},
    {'cost': 102, 'dmg': 0, 'ap': 5},
]
ring_choices = [
    {'cost': 25, 'dmg': 1, 'ap': 0},
    {'cost': 50, 'dmg': 2, 'ap': 0},
    {'cost': 100, 'dmg': 3, 'ap': 0},
    {'cost': 20, 'dmg': 0, 'ap': 1},
    {'cost': 40, 'dmg': 0, 'ap': 2},
    {'cost': 80, 'dmg': 0, 'ap': 3},
]

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

    def __str__(self):
        return '{} ({}/{}/{}) vs {} ({}/{}/{})'.format(self.p1.name, self.p1.hp, self.p1.dmg, self.p1.ap, self.p2.name, self.p2.hp, self.p2.dmg, self.p2.ap)

def play_to_end(game):
#    print('new game', game)
    while not game.has_winner():
        game.attack(attacker=game.p1, target=game.p2)
#        print(game)
        if game.has_winner():
              return 'player'
        game.attack(attacker=game.p2, target=game.p1)
#        print(game)
        if game.has_winner():
              return 'boss'

def part1():
    cheapest_win = 999999999

    for weapon in weapon_choices:
        for armor in armor_choices:
            # try with no rings
            current_cost = weapon['cost'] + armor['cost']
            boss = Player(name='boss', hp=109, dmg=8, ap=2)
            player = Player(name='Player', hp=100, dmg=weapon['dmg'], ap=armor['ap'])
            if play_to_end(Game(p1=player, p2=boss)) == 'player' and current_cost < cheapest_win:
                print('found better winning cost', current_cost, 'with', weapon, 'and', armor)
                cheapest_win = current_cost

            # try with 1 ring
            for r1 in ring_choices:
                current_cost = weapon['cost'] + armor['cost'] + r1['cost']
                boss = Player(name='boss', hp=109, dmg=8, ap=2)
                player = Player(name='Player', hp=100, dmg=weapon['dmg'] + r1['dmg'], ap=armor['ap'] + r1['ap'])
                if play_to_end(Game(p1=player, p2=boss)) == 'player' and current_cost < cheapest_win:
                    print('found better winning cost', current_cost, 'with', weapon, 'and', armor, 'and', r1)
                    cheapest_win = current_cost

            # try with 2 rings
            for i_r1 in range(len(ring_choices)):
                for i_r2 in range(i_r1 + 1, len(ring_choices)):
                    r1 = ring_choices[i_r1]
                    r2 = ring_choices[i_r2]
                    current_cost = weapon['cost'] + armor['cost'] + r1['cost'] + r2['cost']
                    boss = Player(name='boss', hp=109, dmg=8, ap=2)
                    player = Player(name='Player', hp=100, dmg=weapon['dmg'] + r1['dmg'] + r2['dmg'], ap=armor['ap'] + r1['ap'] + r2['ap'])
                    if play_to_end(Game(p1=player, p2=boss)) == 'player' and current_cost < cheapest_win:
                        print('found better winning cost', current_cost, 'with', weapon, 'and', armor, 'and', r1, 'and', r2)
                        cheapest_win = current_cost

def part2():
    expensivest_loss = 0

    for weapon in weapon_choices:
        for armor in armor_choices:
            # try with no rings
            current_cost = weapon['cost'] + armor['cost']
            boss = Player(name='boss', hp=109, dmg=8, ap=2)
            player = Player(name='Player', hp=100, dmg=weapon['dmg'], ap=armor['ap'])
            if play_to_end(Game(p1=player, p2=boss)) == 'boss' and current_cost > expensivest_loss:
                print('found better winning cost', current_cost, 'with', weapon, 'and', armor)
                expensivest_loss = current_cost

            # try with 1 ring
            for r1 in ring_choices:
                current_cost = weapon['cost'] + armor['cost'] + r1['cost']
                boss = Player(name='boss', hp=109, dmg=8, ap=2)
                player = Player(name='Player', hp=100, dmg=weapon['dmg'] + r1['dmg'], ap=armor['ap'] + r1['ap'])
                if play_to_end(Game(p1=player, p2=boss)) == 'boss' and current_cost > expensivest_loss:
                    print('found better winning cost', current_cost, 'with', weapon, 'and', armor, 'and', r1)
                    expensivest_loss = current_cost

            # try with 2 rings
            for i_r1 in range(len(ring_choices)):
                for i_r2 in range(i_r1 + 1, len(ring_choices)):
                    r1 = ring_choices[i_r1]
                    r2 = ring_choices[i_r2]
                    current_cost = weapon['cost'] + armor['cost'] + r1['cost'] + r2['cost']
                    boss = Player(name='boss', hp=109, dmg=8, ap=2)
                    player = Player(name='Player', hp=100, dmg=weapon['dmg'] + r1['dmg'] + r2['dmg'], ap=armor['ap'] + r1['ap'] + r2['ap'])
                    if play_to_end(Game(p1=player, p2=boss)) == 'boss' and current_cost > expensivest_loss:
                        print('found better winning cost', current_cost, 'with', weapon, 'and', armor, 'and', r1, 'and', r2)
                        expensivest_loss = current_cost


def main():
    part2()

if __name__ == '__main__':
    main()

