#!/usr/bin/python3

import fileinput

def parse_line(line):
    w = line.split()
    return {
        w[0]: {
            'speed': int(w[3]),
            'total_stamina': int(w[6]),
            'recovery_time': int(w[-2]),
            'stamina': int(w[6]),
            'recovering': 0,
            'distance': 0,
            'points': 0
        }
    }

def off_to_the_races(config, n):
    for _ in range(n):
        for reindeer in config:
            if config[reindeer]['stamina'] > 0:
                config[reindeer]['distance'] += config[reindeer]['speed']
                config[reindeer]['stamina'] -= 1
            elif config[reindeer]['stamina'] == 0:
                config[reindeer]['recovering'] += 1
                if config[reindeer]['recovering'] == config[reindeer]['recovery_time']:
                    config[reindeer]['stamina'] = config[reindeer]['total_stamina']
                    config[reindeer]['recovering'] = 0

        max_dist = max([config[r]['distance'] for r in config])

        for reindeer in config:
            if config[reindeer]['distance'] == max_dist:
                config[reindeer]['points'] += 1

    return config

def main():
    config = {}
    for line in fileinput.input():
        config.update(parse_line(line))

    results = off_to_the_races(config, 2503)

    print(sorted({results[r]['points']: r for r in results}))

if __name__ == '__main__':
    main()

