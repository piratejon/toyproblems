
drop table if exists tmp_03
;

create table tmp_03 (input text)
;

\copy tmp_03 from './input'
;

drop table if exists tmp_03_b
;

select
  unnest(arr) c
  , generate_subscripts(arr, 1) r
into tmp_03_b
from (
    select
      string_to_array(input, NULL) arr
    from tmp_03
  ) x
;

select count(*) part_1 from (
  select distinct x,y from (
    select
      0 x, 0 y
    union all
    select
      sum(case c when '<' then -1 when '>' then 1 else 0 end) over (order by r) x
      , sum(case c when '^' then 1 when 'v' then -1 else 0 end) over (order by r) y
    from tmp_03_b
    ) x
  ) y
;

select count(*) from (
  select distinct x, y from (
    select
      0 x, 0 y
    union all
    select
      sum(case c when '<' then -1 when '>' then 1 else 0 end) over (order by r) x
      , sum(case c when '^' then 1 when 'v' then -1 else 0 end) over (order by r) y
    from (
        select c, r from tmp_03_b where r % 2 = 0
    ) santa_1
    union all
    select
      sum(case c when '<' then -1 when '>' then 1 else 0 end) over (order by r) x
      , sum(case c when '^' then 1 when 'v' then -1 else 0 end) over (order by r) y
    from (
        select c, r from tmp_03_b where r % 2 = 1
    ) santa_2
  ) x
) y
;

