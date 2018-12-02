#!/bin/bash

psql <<EOF
create schema if not exists aoc2018;
drop table if exists aoc2018.input01;
create table aoc2018.input01 (rowid serial, a int);
EOF

psql -c 'copy aoc2018.input01(a) from stdin;' < input1.txt

psql -c 'select sum(a) part_1 from aoc2018.input01;'

psql <<EOF
with recursive running_freq as (
  select
    a
    , rowid
    , pass
    , sum(a) over (order by pass, rowid) running_freq
  from with_initial_row
)
, repeat_input as (
  select
     a
     , rowid
     , 0 pass
  from aoc2018.input01

  union all

  select
    a
    , rowid
    , pass + 1
  from repeat_input
  where pass < 1000
)
, with_initial_row as (
  select
    0     a
    , 0   rowid
    , 0   pass
  union all
  select
    a
    , rowid
    , pass
  from repeat_input
)
select
  running_freq part_2
from (
  select
    pass
    , rowid
    , a
    , running_freq
    , count(*) over (partition by running_freq order by pass, rowid) freq_recurrence
  from running_freq
  -- order by pass , rowid
) x
where freq_recurrence = 2
order by pass, rowid
limit 1
;
EOF
