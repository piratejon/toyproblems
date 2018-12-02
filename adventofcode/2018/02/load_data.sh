#!/bin/bash

psql <<EOF
create schema if not exists aoc2018;
drop table if exists aoc2018.input02;
create table aoc2018.input02 (rowid serial, a text);
EOF

psql -c 'copy aoc2018.input02(a) from stdin;' < input

psql <<EOF
select
  sum(twos) * sum(threes) part_1
from (
  select
    rowid
    , a
    , max(case when occurrences = 2 then 1 else 0 end) twos
    , max(case when occurrences = 3 then 1 else 0 end) threes
  from (
    select
      rowid
      , a
      , regexp_split_to_table(a, '') chr
      , count(*) occurrences
    from aoc2018.input02
    group by
      rowid
      , a
      , chr
  ) x
  group by
    rowid
    , a
  -- order by rowid
)
x
;
EOF

psql <<EOF
with input_chars as (
  select
    rowid
    , a
    , chr
    , row_number() over (partition by rowid) rn
  from (
    select
      rowid
      , a
      , regexp_split_to_table(a, '') chr
    from aoc2018.input02
  ) x
)
select
  a_rowid
  , a
  , b_rowid
  , b
  , string_agg(a_chr, '' order by rn) part_2
from (
  select
    *
  from (
    select
      a.a
      , a.rowid a_rowid
      , b.a b
      , b.rowid b_rowid
      , a.chr a_chr
      , b.chr b_chr
      , a.rn
      , case when a.chr = b.chr then 0 else 1 end diff
      , sum(case when a.chr = b.chr then 0 else 1 end) over (partition by a.a, b.a) diffs
    from input_chars a
    inner join input_chars b
      on a.rowid < b.rowid
      and a.rn = b.rn
  ) x
  where 1 = diffs
) x
where diff = 0
group by
  a_rowid
  , a
  , b_rowid
  , b
;
EOF

