
drop table if exists tmp_2016_07
;

create table tmp_2016_07 (input text)
;

\copy tmp_2016_07 from './test'
;

drop table if exists tmp_2016_07b
;

-- 65 is too low
select
  line
from (
  select
    line
    , case when a[1] = a[2] then 'n' else 'y' end
    , case when b[1] = b[2] then 'n' else 'y' end
  from (
    select
      input
      , regexp_matches(input, '[^[]?([^\]])([^\]])\2\1', 'g') a
      , regexp_matches(input, '[^\]]?([^\]])([^\]])\2\1', 'g') b
      , row_number() over () as line
    from tmp_2016_07
  ) z
  where a[1] <> a[2] or b[1] <> b[2]
) x
except
select
  line
from (
  select
    line
    , case when c[1] = c[2] then 'n' else 'y' end
  from (
    select
      input
      , regexp_matches(input, '\[.*([^\]])([^\]])\2\1.*\]', 'g') c
      , row_number() over () as line
    from tmp_2016_07
  ) z
  where c[1] <> c[2]
) x
