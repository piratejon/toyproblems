
create or replace function p10(somestr text, l int)
returns table (nextstr text, l int) as
$$
with recursive
pre_init (
  a
) as (
  select somestr
)

, init as (
  select
    0 as l
    , string_to_array(a, NULL, '') as a
  from pre_init
)

, with_subscripts (
  a
  , i
  , l
) AS (
  select
    a
    , generate_subscripts(a, 1)
    , l
  from init
)

, indexed (
  a
  , i
  , r
  , l
) AS (
  select
  a[i]
  , i
  , row_number() over (partition by a[i] order by i asc) - row_number() over (order by i asc)
  , l
  from with_subscripts
)

, sequenced (
  a
  , i
  , r_asc
  , r_desc
  , l
) AS (
  select
    a
    , i
    , row_number() over (partition by a, r order by i asc)
    , row_number() over (partition by a, r order by i desc)
    , l
  from indexed
)

, filtered (
  a
  , i
  , r
  , l
) AS (
  select
    a
    , i
    , r_desc
    , l
  from sequenced
  where
    r_asc = 1
)

, arrayed (
  a
  , l
) AS (
  select distinct
    array_to_string(
        array(
        select
          r || a
        from filtered
        order by i asc
      )
      , ''
    ) as a
    , l
  from filtered
)

select
*
from arrayed

$$
language sql;

create or replace function p10driver(somestr text, maxlevel int)
returns table (oldstr text, oldlen int, newstr text, newlen int, l int) as $$
with recursive loopme as (
  select
    somestr as oldstr
    , char_length(somestr) as oldlen
    , p.nextstr as newstr
    , char_length(p.nextstr) as newlen
    , p.l + 1 as l
  from p10(somestr, 0) as p

  union all

  select
    base.newstr
    , char_length(base.newstr)
    , fun.nextstr
    , char_length(fun.nextstr)
    , base.l + 1
  from loopme as base
  cross join p10(base.newstr, base.l + 1) as fun
  where base.l < maxlevel
)

select
*
from loopme
order by l asc
$$ language sql;

