
drop table if exists tmp_2016_03
;

create table tmp_2016_03 (input text)
;

\copy tmp_2016_03 from './input'
;

drop table if exists tmp_2016_03b
;

with tmp as (
  select
    cast(s[1] as int) s1
    , cast(s[2] as int) s2
    , cast(s[3] as int) s3
    , line
    , (line - 1) / 3 vgroup
  -- into tmp_2016_03b
  from (
    select
      string_to_array(regexp_replace(input, '[ ]+(\d+)[ ]+(\d+)[ ]+(\d+)', '\1,\2,\3'), ',') s
      , row_number() over () as line
    from tmp_2016_03
  ) y
)
, part1 as (
  select
    sum(case when
      s1 + s2 > s3
      and s2 + s3 > s1
      and s3 + s1 > s2
      then 1
    else 0
    end) answer
  , cast('1' as text) as part
  from tmp
)
, part2_rotate as (
  select
  *
  from (
    select
      s1
      , lead(s1, 1) over (partition by vgroup order by line) s2
      , lead(s1, 2) over (partition by vgroup order by line) s3
      , (line % 3) - 1 g
      , line
      , vgroup
    from tmp

    union all

    select
      s2
      , lead(s2, 1) over (partition by vgroup order by line) s2
      , lead(s2, 2) over (partition by vgroup order by line) s3
      , (line % 3) - 1
      , line
      , vgroup
    from tmp

    union all

    select
      s3
      , lead(s3, 1) over (partition by vgroup order by line) s2
      , lead(s3, 2) over (partition by vgroup order by line) s3
      , (line % 3) - 1
      , line
      , vgroup
    from tmp
  ) x
  where g = 0 -- and s1 is not null and s2 is not null and s3 is not null
)
-- 1096 is too low
, part2 as (
  select
    sum(case when
      s1 + s2 > s3
      and s2 + s3 > s1
      and s3 + s1 > s2
      then 1
    else 0
    end) answer
  , cast('2' as text) as part
  from part2_rotate
  where g = 0
)
-- select * from part2_rotate
select answer, part from part1
union all
select answer, part from part2
;

