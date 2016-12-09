
drop table if exists tmp_2016_08
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
, update_state as (
  select
    line
    , input
    , w
    , h
    , cmd
    , a
    -- rotation as the first instruction shouldn't do anything
    , null::int ox
    , null::int oy
    , case when cmd = 'rect' then generate_series(0, a[1]::int - 1) end x
    , case when cmd = 'rect' then generate_series(0, a[2]::int - 1) end y
    , row_number() over (order by line) r
  from input_stream
  where line = 1

  union all

  select -- distinct
    n.line
    , n.input
    , n.w
    , n.h
    , n.cmd
    , n.a
    , u.x ox
    , u.y oy
    , case
    when n.cmd = 'rect' and n.line = u.line + 2 then u.x
    when n.cmd = 'rect' then generate_series(0, n.a[1]::int - 1)
    when n.cmd = 'rotate' and n.a[1] = 'row' and n.a[2] = 'y' and u.y = n.a[3]::int then
      -- rotating a row increases x values
      (u.x + n.a[5]::int) % n.w
    else u.x
    end x
    , case
    when n.cmd = 'rect' and n.line = u.line + 2 then u.y
    when n.cmd = 'rect' then generate_series(0, n.a[2]::int - 1)
    when n.cmd = 'rotate' and n.a[1] = 'column' and n.a[2] = 'x' and u.x = n.a[3]::int then
      -- rotating a column moves y values down
      (u.y + n.a[5]::int) % n.h
    else u.y
    end y
    , row_number() over (order by n.line) r
  from input_stream n
  inner join update_state u
    -- normal increment
    on n.line = u.line + 1
    -- generate_series overwrites the values so we have to repeat them
    or n.cmd = 'rect' and n.line = u.line + 2
)
select distinct
line , input , cmd , a , x , y
from update_state
-- group by line order by line desc
order by line, x, y -- , ox, oy, r
;

