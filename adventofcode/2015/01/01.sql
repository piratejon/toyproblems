
drop table if exists tmp_01
;

create table tmp_01 (input text)
;

\copy tmp_01 from './input'
;

drop table if exists tmp_01_b
;

select
  unnest(arr) c
  , generate_subscripts(arr, 1) r
into tmp_01_b
from (
    select
      string_to_array(input, NULL) arr
    from tmp_01
  ) x
;

select sum(case when c = '(' then 1 else -1 end) part_1 from tmp_01_b
;

select
  min(r) part_2
from (
select
  r
  , sum(case when c = '(' then 1 else -1 end) over (order by r) f
from tmp_01_b
) x
where f = -1
;

