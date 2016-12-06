
drop table if exists tmp_2016_06
;

create table tmp_2016_06 (input text)
;

\copy tmp_2016_06 from './input'
;

drop table if exists tmp_2016_06b
;

with tmp as (
  select
    unnest(s) k
    , generate_subscripts(s, 1) pos
    , line
  from (
    select
      string_to_array(input, NULL) s
      , row_number() over () as line
    from tmp_2016_06
  ) y
)

select
  *
from (
  select
    k
    , pos
    , c
    , row_number() over (partition by pos order by c) r
  from (
    select
      k
      , pos
      , count(*) c
    from tmp
    group by k, pos
    order by pos, count(*) desc
  ) z
) y
where r = 1
order by pos
;

