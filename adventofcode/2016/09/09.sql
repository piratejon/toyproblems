
drop table if exists tmp_2016_09
;

create table tmp_2016_09 (input text)
;

\copy tmp_2016_09 from './test2'
;

with recursive input_stream as (
  select
    line
    , input
    , generate_subscripts(a, 1) i
    , unnest(a) c
  from (
    select
      row_number() over () as line
      , input
      , string_to_array(input, NULL) a
    from tmp_2016_09
  ) z
)
, parse as (
  select
    line
    , input
    , i
    , c
    , 1 l
    , case
      when c = '(' then 'begin length'
      else 'pass'
    end state
    , 0 length
    , 0 len_cnt
    , 0 repeat
    , 0 rpt_cnt
    , case when c = '(' then 0 else 1 end decompress_length
  from input_stream
  where i = 1

  union all

  select
    i.line
    , i.input
    , i.i
    , i.c
    , p.l + 1

    , case
    when p.state = 'pass' and i.c = '(' then 'begin length'
    when p.state in ('begin length', 'collect length') and i.c <> 'x' then 'collect length'
    when p.state = 'collect length' and i.c = 'x' then 'begin repeat'
    when p.state in ('begin repeat', 'collect repeat') and i.c <> ')' then 'collect repeat'
    when p.state = 'collect repeat' and i.c = ')' then 'begin marker'
    when p.state = 'begin marker' then 'apply marker'
    when p.state = 'apply marker' and p.len_cnt > 1 then 'apply marker'
    when p.state = 'apply marker' and p.len_cnt = 1 and i.c = '(' then 'begin length'
    else 'pass'
    end

    , case
    when p.state = 'begin length' then i.c::int
    when p.state = 'collect length' and i.c <> 'x' then (10 * p.length) + i.c::int else p.length
    end length
    , case
    when p.state = 'begin length' then i.c::int
    when p.state = 'collect length' and i.c <> 'x' then (10 * p.length) + i.c::int
    when p.state = 'apply marker' and p.len_cnt > 0 then p.len_cnt - 1
    when p.state = 'apply marker' and p.len_cnt = 0 then 0
    else p.length
    end len_cnt

    , case
    when p.state = 'begin repeat' then i.c::int
    when p.state = 'collect repeat' and i.c <> ')' then (10 * p.repeat) + i.c::int else p.repeat
    end repeat
    , case
    when p.state = 'begin repeat' then i.c::int
    when p.state = 'collect repeat' and i.c <> ')' then (10 * p.length) + i.c::int else p.repeat
    end rpt_cnt

    , case
    when p.state = 'pass' and i.c <> '(' then 1
    when p.state = 'apply marker' and p.len_cnt > 1 then repeat
    when p.state = 'apply marker' and p.len_cnt = 1 and i.c <> '(' then 1
    when p.state = 'begin marker' then repeat
    else 0
    end decompress_length

  from parse p
  inner join input_stream i
    on 1=1
    and p.line = i.line
    and p.i + 1 = i.i
)
-- 136291 is too low
-- 138752 is too high
, part_1 as (
  select
    line
    , input
    , sum(decompress_length) decompress_length
  from parse
  group by
    line
    , input
)
, parse2 as (
  select
    line
    , i
    , c
    , case when c = '(' then 'collect length' else '' end state
    , case when c <> '(' then 1 else 0 end a
    , 0 lenb
    , 0 rptb
    , array[]::integer[] len
    , array[]::integer[] rpt
    , array[]::integer[] mrk
  from input_stream
  where i = 1

  union all

  select
    i.line
    , i.i
    , i.c

    , case
    when state = 'collect length' and i.c <> 'x' then 'collect length'
    when state = 'collect length' and i.c = 'x' then 'collect repeat'
    when state = 'collect repeat' and i.c <> ')' then 'collect repeat'
    when state = 'collect repeat' and i.c = ')' then 'end marker'
    when state in ('','end marker') and i.c = '(' then 'collect length'
    when state in ('','end marker') and i.c <> '(' then ''
    end state

    , case
    when state in ('','end marker') and i.c <> '(' then 1
    else 0
    end a

    , case
    when state = 'collect length' and i.c <> 'x' then (10 * lenb) + i.c::int
    else 0
    end lenb

    , case
    when state = 'collect repeat' and i.c <> ')' then (10 * rptb) + i.c::int
    else 0
    end rptb

    , case
    when state = 'collect length' and i.c = 'x' then array_append(len, lenb)
    else len
    end len

    , case
    when state = 'collect repeat' and i.c = ')' then array_append(rpt, rptb)
    else rpt
    end rpt

    , case
    when state = 'collect repeat' and i.c = ')' then array_append(mrk, i.i)
    else mrk
    end mrk

  from parse2 n
  inner join input_stream i
    on i.line = n.line
    and i.i = n.i + 1
)
select
  line l
  , i
  , c
  , state
  , a
  , len
  , rpt
  , mrk
from parse2
where line = 4
order by
  line
  , i
-- select * from part_1
;

--X(8x2)(3x3)ABCY
--X(3x3)ABC(3x3)ABCY
--XABCABCABCABCABCABCY

