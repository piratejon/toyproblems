#!/bin/sh
erlc solve.erl && erl -noshell -s solve test -s init stop
erlc solve.erl && erl -noshell -s solve part1 -s init stop
erlc solve.erl && erl -noshell -s solve part2 -s init stop
