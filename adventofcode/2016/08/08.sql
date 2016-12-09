
drop table if exists tmp_2016_08
;

drop table if exists tmp_2016_08_final_state
;

create table tmp_2016_08 (input text)
;

\copy tmp_2016_08 from './input'
;

-- 19 is too low
with recursive input_stream as (
  select
    line
    , input
    , w
    , h
    , a[1] cmd
    , case
      when a[1] = 'rect'
        then string_to_array(a[2], 'x')
      when a[1] = 'rotate'
        then a[2:8]
      end a
  from (
    select
      row_number() over () as line
      , input
      , regexp_split_to_array(input, E' \|=') a
      , 50 w
      , 6 h
    from tmp_2016_08
  ) z
)
, prepare_grid as (
  select
    x
    , y
  from (
    select distinct w, h from input_stream
  ) i
  cross join generate_series(0, w - 1) x
  cross join generate_series(0, h - 1) y
)
, work_input_stream as (
  select
    i.line
    , i.input
    , i.w
    , i.h
    , i.cmd
    , i.a
    , p.x
    , p.y
    , case when i.cmd = 'rect' and p.x < i.a[1]::int and p.y < i.a[2]::int then 1 else 0 end v
    , row_number() over (partition by i.line, p.y order by p.x) xn
    , row_number() over (partition by i.line, p.x order by p.y) yn
  from input_stream i
  cross join prepare_grid p
  where i.line = 1

  union all

  select
    i.line
    , i.input
    , i.w
    , i.h
    , i.cmd
    , i.a
    , w.x
    , w.y
    , case
    when i.cmd = 'rect'
      and w.x < i.a[1]::int
      and w.y < i.a[2]::int
      then 1

    when i.cmd = 'rotate'
      and i.a[1] = 'column'
      and i.a[2] = 'x'
      and i.a[3]::int = w.x
      and w.y >= i.a[5]::int
      then lag(w.v, i.a[5]::int) over (partition by i.line, w.x order by w.y)

    when i.cmd = 'rotate'
      and i.a[1] = 'column'
      and i.a[2] = 'x'
      and i.a[3]::int = w.x
      and w.y < i.a[5]::int
      then lead(w.v, i.h - i.a[5]::int) over (partition by i.line, w.x order by w.y)

    when i.cmd = 'rotate'
      and i.a[1] = 'row'
      and i.a[2] = 'y'
      and i.a[3]::int = w.y
      and w.x >= i.a[5]::int
      then lag(w.v, i.a[5]::int) over (partition by i.line, w.y order by w.x)

    when i.cmd = 'rotate'
      and i.a[1] = 'row'
      and i.a[2] = 'y'
      and i.a[3]::int = w.y
      and w.x < i.a[5]::int
      then lead(w.v, i.w - i.a[5]::int) over (partition by i.line, w.y order by w.x)

    else v
    end v
    , row_number() over (partition by i.line, w.y order by w.x) xn
    , row_number() over (partition by i.line, w.x order by w.y) yn
  from input_stream i
  inner join work_input_stream w
    on i.line = w.line + 1
)
, final_state as (
  select
    *
  from (
    select
      *
      , row_number() over (partition by x, y order by line desc) l
    from work_input_stream
    -- order by line, x, y
  )
  x where l = 1
)
select
  *
into tmp_2016_08_final_state
from final_state
;

select
  sum(v) part1
from tmp_2016_08_final_state
;

select
  *
from crosstab('select y, x, case v when 1 then ''X'' end v from tmp_2016_08_final_state order by y, x')
as ct(
  y int
  , "0" text
  , "1" text
  , "2" text
  , "3" text
  , "4" text
  , "5" text
  , "6" text
  , "7" text
  , "8" text
  , "9" text
  , "10" text
  , "11" text
  , "12" text
  , "13" text
  , "14" text
  , "15" text
  , "16" text
  , "17" text
  , "18" text
  , "19" text
  , "20" text
  , "21" text
  , "22" text
  , "23" text
  , "24" text
  , "25" text
  , "26" text
  , "27" text
  , "28" text
  , "29" text
  , "30" text
  , "31" text
  , "32" text
  , "33" text
  , "34" text
  , "35" text
  , "36" text
  , "37" text
  , "38" text
  , "39" text
  , "40" text
  , "41" text
  , "42" text
  , "43" text
  , "44" text
  , "45" text
  , "46" text
  , "47" text
  , "48" text
  , "49" text
)
;

