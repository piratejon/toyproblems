#!/bin/bash

psql <<EOF
create schema if not exists aoc2018;
drop table if exists aoc2018.input04;
create table aoc2018.input04 (rowid serial, a text);
EOF

#psql -c 'copy aoc2018.input04(a) from stdin;' < sample0.txt
psql -c 'copy aoc2018.input04(a) from stdin;' < input

psql <<EOF
set time zone UTC;

with parse_inputs as (
  select
    a, _what, _who, shift
    , t window_start
    , lead(t) over (order by t) window_stop
    , age(lead(t) over (order by t), t) d
    , extract(epoch from age(lead(t) over (partition by shift order by t), t)) / 60 dur
  from (
    select
      a, t, _what, shift, rowid
      , first_value(_who) over (partition by shift order by t) _who
    from (
      select
        rowid
        , a
        , b
        , to_timestamp(b[2] || b[3] || b[4] || b[5] || b[6] || '00', 'YYYYMMDDhh24miss')::timestamp without time zone at time zone 'Etc/UTC' t
        , b[8] _what
        , case when b[8] = 'Guard' then b[10] end _who
        , sum(case when b[8] = 'Guard' then 1 else 0 end) over (order by a) shift
      from (
        select
          rowid
          , a
          , regexp_split_to_array(a, '[-# ,:\[\]]') b
        from aoc2018.input04
      ) x
    ) x
  ) x
)
select
  _who, sleeptime, extract(minute from gsm) _gsm_m
  , count(*)
  , extract(minute from gsm)::int * _who::int part_1
from (
  select
    a, _what, _who, shift, window_start, window_stop
    , sum(case when _what = 'falls' then dur else 0 end) over (partition by _who) sleeptime
    , generate_series(window_start, coalesce(window_stop - interval'1'minute, window_start), interval'1' minute) gsm
  from parse_inputs
) x
where _what = 'falls'
group by _who, sleeptime, _gsm_m
order by sleeptime desc, count(*) desc
;
EOF

