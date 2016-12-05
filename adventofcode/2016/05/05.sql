
/*
with recursive bf as (
  select
    'ffykfhsq' s
    , md5('ffykfhsq') m
    , null t
    , 0 n
    , 5 zc
    , 0 matches
  union all
  select
    s
    , md5(s || n + 1) m
    , s || n + 1
    , n + 1
    , zc
    , case when left(m, zc) = repeat('0', zc) then matches + 1 else matches end
  from bf
  where 1=1
  and matches < 8
  -- and left(m, zc) <> repeat('0', zc)
)
select
  *
from bf
where left(m, zc) = repeat('0', zc)
;
*/

with recursive bf as (
  select
    'ffykfhsq' s
    , md5('ffykfhsq') m
    , null t
    , 0 n
    , 5 zc
    , null "0"
    , null "1"
    , null "2"
    , null "3"
    , null "4"
    , null "5"
    , null "6"
    , null "7"
  union all
  select
    s
    , md5(s || n + 1) m
    , s || n + 1
    , n + 1
    , zc
    , case when "0" is null and left(m, zc) = repeat('0', zc)
        and substring(m, zc + 1, 1) = '0' then substring(m, zc + 2, 1)
      else "0" end
    , case when "1" is null and left(m, zc) = repeat('0', zc)
        and substring(m, zc + 1, 1) = '1' then substring(m, zc + 2, 1)
      else "1" end
    , case when "2" is null and left(m, zc) = repeat('0', zc)
        and substring(m, zc + 1, 1) = '2' then substring(m, zc + 2, 1)
      else "2" end
    , case when "3" is null and left(m, zc) = repeat('0', zc)
        and substring(m, zc + 1, 1) = '3' then substring(m, zc + 2, 1)
      else "3" end
    , case when "4" is null and left(m, zc) = repeat('0', zc)
        and substring(m, zc + 1, 1) = '4' then substring(m, zc + 2, 1)
      else "4" end
    , case when "5" is null and left(m, zc) = repeat('0', zc)
        and substring(m, zc + 1, 1) = '5' then substring(m, zc + 2, 1)
      else "5" end
    , case when "6" is null and left(m, zc) = repeat('0', zc)
        and substring(m, zc + 1, 1) = '6' then substring(m, zc + 2, 1)
      else "6" end
    , case when "7" is null and left(m, zc) = repeat('0', zc)
        and substring(m, zc + 1, 1) = '7' then substring(m, zc + 2, 1)
      else "7" end
  from bf
  where 1=1
  and (
    1=0
    or "0" is null
    or "1" is null
    or "2" is null
    or "3" is null
    or "4" is null
    or "5" is null
    or "6" is null
    or "7" is null
  )

)
select
  *
  , row_number() over (order by n) rn
from bf
where 
  "0" is not null
  and "1" is not null
  and "2" is not null
  and "3" is not null
  and "4" is not null
  and "5" is not null
  and "6" is not null
  and "7" is not null
;

