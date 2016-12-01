
with recursive bf as (
  select
    'ckczppom' s
    , md5('ckczppom') m
    , null t
    , 0 n
  union all
  select
    s
    , md5(s || n + 1) m
    , s || n + 1
    , n + 1
  from bf
  where left(m, 6) <> '000000'
)
select
  n, part
from (
select n, 'part_1' part, row_number() over (order by n) r from bf where left(m, 5) = '00000'
union all
select n, 'part_2' part, row_number() over (order by n) r from bf where left(m, 6) = '000000'
) x
where r = 1
;

