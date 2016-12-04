
drop table if exists tmp_2016_04
;

create table tmp_2016_04 (input text)
;

\copy tmp_2016_04 from './input'
;

drop table if exists tmp_2016_04b
;

drop table if exists tmp_2016_04c
;

drop table if exists tmp_2016_04d
;

select
  x
  , i
  , line
  , row_number() over (partition by line order by i desc) j
into tmp_2016_04b
from (
  select
    unnest(arr) x
    , generate_subscripts(arr, 1) as i
    , line
  from (
    select
      string_to_array(input, '-') arr
      , row_number() over () as line
    from tmp_2016_04
  ) z
) y
;

select
  room
  , cksum
  , line
into tmp_2016_04c
from (
  select
    x
    , i
    , cast(case when j = 1 then regexp_replace(x, '^(\d+).*', '\1') end as int) room
    , case when j = 1 then regexp_replace(x, '^\d+\[(.*)\]', '\1') end cksum
    , line
    , j
  from tmp_2016_04b
) w
where j = 1
;

select
  x
  , line
  , c
  , row_number() over (partition by line order by c desc, x asc) o
into tmp_2016_04d
from (
  select
    x
    , line
    , count(*) c
  from (
    select
      unnest(arr) x
      , i
      , line
    from (
      select
        string_to_array(x, NULL) arr
        , i
        , line
      from tmp_2016_04b
      where j <> 1
    ) z
  ) y
  group by x, line
) x
;

--select * from tmp_2016_04d;
/*
 x | line | c | o
---+------+---+---
 a |    1 | 5 | 1
 b |    1 | 3 | 2
 x |    1 | 1 | 3
 y |    1 | 1 | 4
 z |    1 | 1 | 5
 a |    2 | 1 | 1
 b |    2 | 1 | 2
 c |    2 | 1 | 3
 d |    2 | 1 | 4
 e |    2 | 1 | 5
*/
with recursive items as (
  select
    r.line
    , c1 || c2 || c3 || c4 || c5 computed_cksum
    , r.cksum
    , r.room
  from crosstab('select line, o, x from tmp_2016_04d where o between 1 and 5 order by 1,2;')
  as t(line bigint, "c1" text, "c2" text, "c3" text, "c4" text, "c5" text)
  inner join tmp_2016_04c r
    on r.line = t.line
)
, valid_items as (
  select
  *
  from items
  where computed_cksum = cksum
)
, part_1 as (
  select
    sum(room) part_1
  from valid_items
)
, ciphertext as (
select
  v.line, v.cksum, v.room orig_room, v.room % 26 room, unnest(arr) i
from valid_items v
inner join (
    select
      string_to_array(input, '') arr
      , row_number() over () as line
    from tmp_2016_04
) x
on v.line = x.line
)
, decoder as (
  select
    line
    , cksum
    , orig_room
    , room
    , i
  from ciphertext

  union all

  select
    line
    , cksum
    , orig_room
    , room - 1
    , translate(i, 'abcdefghijklmnopqrstuvwxyz', 'bcdefghijklmnopqrstuvwxyza') i
  from decoder
  where room > 0
)
select
*
from decoder
where room = 0
and i like '%north%'
;

