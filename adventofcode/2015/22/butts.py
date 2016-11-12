#!/usr/bin/python3

import copy

REALLY_HIGH_MANA = 999999999

SPELLS = {
    'Magic Missile': {
        'cost': 53,
        'type': 'instant',
        'impact': [
            {
                'player': 'boss',
                'stat': 'hp',
                'type': 'real',
                'delta': -4
            }
        ]
    },
    'Drain': {
        'cost': 73,
        'type': 'instant',
        'impact': [
            {
                'player': 'boss',
                'stat': 'hp',
                'type': 'real',
                'delta': -2
            },
            {
                'player': 'player',
                'stat': 'hp',
                'type': 'real',
                'delta': 2
            }
        ]
    },
    'Shield': {
        'cost': 113,
        'type': 'effect',
        'duration': 6,
        'impact': [
            {
                'player': 'player',
                'stat': 'armor',
                'type': 'effective',
                'delta': 7
            }
        ]
    },
    'Poison': {
        'cost': 173,
        'type': 'effect',
        'duration': 6,
        'impact': [
            {
                'player': 'boss',
                'stat': 'hp',
                'type': 'real',
                'delta': -3
            }
        ]
    },
    'Recharge': {
        'cost': 229,
        'type': 'effect',
        'duration': 5,
        'impact': [
            {
                'player': 'player',
                'stat': 'mana',
                'type': 'real',
                'delta': 101
            }
        ]
    }
}

EXAMPLE1 = {
    'player': {
        'hp': 10,
        'mana': 250,
        'armor': 0,
    }, 'boss': {
        'hp': 13,
        'dmg': 9
    }
}

EXAMPLE2 = {
    'player': {
        'hp': 10,
        'mana': 250,
        'armor': 0,
    }, 'boss': {
        'hp': 14,
        'dmg': 9
    }
}

REAL_GAME = {
    'player': {
        'hp': 50,
        'mana': 500,
        'armor': 0,
    }, 'boss': {
        'hp': 51,
        'dmg': 9
    }
}

GAMES_PLAYED = {}

def state_hash(state, turn):
    return tuple([turn % 4] + [(k0, k1, v1) for k0, v0 in sorted(state.items()) for k1, v1 in sorted(v0.items())])

def reverse_spell(state, spell_name):
    print('reversing spell', spell_name)
    for impact in SPELLS[spell_name]['impact']:
        if impact['type'] == 'effective':
            state[impact['player']][impact['stat']] -= impact['delta']

def apply_spell(state, spell_name, ttl=0):
    for impact in SPELLS[spell_name]['impact']:
        if impact['type'] == 'real' or (impact['type'] == 'effective' and ttl == SPELLS[spell_name]['duration']):
            state[impact['player']][impact['stat']] += impact['delta']

def cast(state, spell_name):
    spell = SPELLS[spell_name]
    if spell['type'] == 'instant':
        print(spell)
        apply_spell(state, spell_name)
    elif spell['type'] == 'effect':
        state['effects'][spell_name] = spell['duration']
    else:
        raise ValueError(spell['type'])

    state['player']['mana'] -= spell['cost']

def apply_effects(state):
    new_effects = {}
    for spell_name, ttl in state['effects'].items():
        apply_spell(state, spell_name, ttl)
        if ttl > 0:
            new_effects[spell_name] = ttl - 1
        else:
            if SPELLS[spell_name]['type'] == 'effect':
                reverse_spell(state, spell_name)
    state['effects'] = new_effects

def boss_attack(state):
    state['player']['hp'] -= max(1, state['boss']['dmg'] - state['player']['armor'])

def play_next_round(state, spent_mana=0, level=0):
    best_mana = REALLY_HIGH_MANA

    header = (' '*level)

    statehash = state_hash(state, level)
    if statehash in GAMES_PLAYED:
        print(header, level, 'already played state', statehash, GAMES_PLAYED[statehash])
        return GAMES_PLAYED[statehash]
    
    print(header, level, 'playing', state_hash(state, level))

    if level % 4 == 0:
        apply_effects(state)
        print(header, level, 'apply effects pre player cast', state['player'], state['boss'])
    elif level % 4 == 1:
        print(header, level, 'try player spells')
        for spell_name in SPELLS:
            spell_state = copy.deepcopy(state)
            spell = SPELLS[spell_name]
            if spell_name in spell_state['effects']:
                print(header, level, 'cannot cast', spell, 'while in effect')
                continue
            if spell['cost'] > spell_state['player']['mana']:
                print(header, level, 'insufficient mana', spell_state['player']['mana'], 'for spell', spell, spell['cost'])
                continue

            print(header, level, 'casting', spell_name)
            cast(spell_state, spell_name)

            if spell_state['boss']['hp'] <= 0:
                best_mana = min(best_mana, spent_mana + spell['cost'])
                print(header, level, spell, 'killed boss, new best mana', best_mana)
            else:
                print(header, level, spell, 'did not kill boss, proceeding...')
                best_mana = min(best_mana, play_next_round(spell_state, spent_mana + spell['cost'], level + 1))
                print(header, level, spell, '... best mana was', best_mana)

        print(header, level, 'best mana among spells was', best_mana)
        GAMES_PLAYED[statehash] = best_mana
        return GAMES_PLAYED[statehash]

    elif level % 4 == 2:
        print(header, level, 'apply effects pre boss attack', state['player'], state['boss'])
        apply_effects(state)

    elif level % 4 == 3:
        boss_attack(state)
        print(header, level, 'boss attacks', state['player'], state['boss'])

    if state['player']['hp'] <= 0:
        print(header, level, 'player dies')
        GAMES_PLAYED[statehash] = REALLY_HIGH_MANA
        return GAMES_PLAYED[statehash]

    if state['boss']['hp'] <= 0:
        print(header, level, 'boss dies', spent_mana)
        GAMES_PLAYED[statehash] = spent_mana
        return GAMES_PLAYED[statehash]

    GAMES_PLAYED[statehash] = play_next_round(state, spent_mana, level + 1)
    return GAMES_PLAYED[statehash]

def play_game(player, boss):
    return play_next_round({'player': player, 'boss': boss, 'effects': {}})

def main(game):
    print(play_game(player=game['player'], boss=game['boss']))

if __name__ == '__main__':
#    main(EXAMPLE1)
    main(REAL_GAME)

