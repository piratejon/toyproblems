#!/usr/bin/python3

SPELLS = {
  "Magic Missile": {
    "cost": 53,
    "type": "instant",
    "impacts": [
      {"player": "opponent", "dmg": -4}
    ]
  }, "Drain": {
    "cost": 73,
    "type": "instant",
    "impacts": [
      {"player": "opponent", "hp": 2},
      {"player": "caster", "dmg": -2}
    ]
  }, "Shield": {
    "cost": 113,
    "type": "effect",
    "duration": 6,
    "impacts": [
      {"player": "caster", "armor": 2}
    ]
  }, "Poison": {
    "cost": 173,
    "type": "effect",
    "duration": 6,
    "impacts": [
      {"player": "opponent", "dmg": 2}
    ]
  }, "Recharge": {
    "cost": 229,
    "type": "effect",
    "duration": 5,
    "impacts": [
      {"player": "caster", "mana": 101}
    ]
  }
}

class Player:
    def __init__(self, hp=0, mana=0, dmg=0, armor=0):
        self.hp = hp
        self.mana = mana
        self.dmg = dmg
        self.armor = armor

class Game:
    def __init__(self, p1, p2):
        self.p1 = p1
        self.p2 = p2
        self.effects = []

    def apply_effects(self):
        for 

    def cast(self, player, spell, target=None):
        spellname = spell
        spell = SPELLS[spellname]
        if spell['type'] == 'instant':
            pass
        elif spell['type'] == 'effect':
            self.effects.append({
                'name': spellname,
                'timer': spell['duration'],
                'caster': player,
                'target': target
                })
        else:
            raise ValueError('spell {} has unexpected type {}'.format(spellname, spell['type']))

        return True

