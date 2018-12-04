#!/bin/bash

psql <<EOF
create schema if not exists aoc2018;
drop table if exists aoc2018.input04;
create table aoc2018.input04 (rowid serial, a text);
EOF

psql -c 'copy aoc2018.input04(a) from stdin;' < sample0.txt
#psql -c 'copy aoc2018.input04(a) from stdin;' < input

psql <<EOF
set time zone UTC;

select
  *
  , sum(case when _what = 'falls' then dur else 0 end) over (partition by _who) sleeptime
from (
  select
    a, t, _mm, _what, _who, shift_id
    , age(lead(t) over (partition by shift_id order by rowid), t) d
    , extract(epoch from age(lead(t) over (partition by shift_id order by rowid), t)) / 60 dur
  from (
    select
      a, _y, _m, _d, _h, _mm, t, _what, shift_id, rowid
      , first_value(_who) over (partition by shift_id order by rowid) _who
    from (
      select
        rowid
        , a
        , b
        , b[2]::int _y
        , b[3]::int _m
        , b[4]::int _d
        , b[5]::int _h
        , b[6]::int _mm
        , to_timestamp(b[2] || b[3] || b[4] || b[5] || b[6] || '00', 'YYYYMMDDhh24miss')::timestamp without time zone at time zone 'Etc/UTC' t
        , b[8] _what
        , case when b[8] = 'Guard' then b[10] end _who
        , sum(case when b[8] = 'Guard' then 1 else 0 end) over (order by rowid) shift_id
      from (
        select
          rowid
          , a
          , regexp_split_to_array(a, '[-# ,:\[\]]') b
        from aoc2018.input04
      ) x
    ) x
  ) x
) x
;
EOF

