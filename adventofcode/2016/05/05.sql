
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
    -- , case when left(m, zc) = repeat('0', zc) then zc + 1 else zc end
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

