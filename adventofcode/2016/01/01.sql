
drop table if exists tmp_2016_01
;

create table tmp_2016_01 (input text)
;

\copy tmp_2016_01 from './input'
;

-- 371 is too high
select
  a
  , turn
  , steps
  , bearing
  , dir
  , n
  , sum(case dir when 'E' then steps when 'W' then -steps else 0 end) over (order by n) x
  , sum(case dir when 'N' then steps when 'S' then -steps else 0 end) over (order by n) y
  , abs(sum(case dir when 'E' then steps when 'W' then -steps else 0 end) over (order by n))
  + abs(sum(case dir when 'N' then steps when 'S' then -steps else 0 end) over (order by n)) total
into tmp_2016_01b
from (
  select
    a
    , turn
    , steps
    , bearing
    , case bearing % 360
      when 0 then 'N'
      when 90 then 'E'
      when 180 then 'S'
      when 270 then 'W'
      when -90 then 'W'
      when -180 then 'S'
      when -270 then 'E'
    end dir
    , n
  from (
    select
      a
      , turn
      , sum(case turn when 'L' then -90 when 'R' then 90 end) over (order by n) bearing
      , steps
      , n
    from (
      select
        a
        , substring(a for 1) turn
        , cast(substring(a from 2 for length(a)) as int) steps
        , n
      from (
        select
          unnest(arr) a
          , generate_subscripts(arr, 1) n
        from (
          select
            string_to_array(replace(input, ' ', ''), ',') arr
          from tmp_2016_01
        ) y
      ) x
    ) z
  ) w
) v
;

-- 257 is too high
with recursive fill_in_steps as (
  select
    a
    , turn
    , steps - 1 steps
    , bearing
    , dir
    , n
    , case dir when 'E' then 1 when 'W' then -1 else 0 end x
    , case dir when 'N' then 1 when 'S' then -1 else 0 end y
  from tmp_2016_01b
  union all
  select
    s.a
    , s.turn
    , s.steps - 1
    , s.bearing
    , s.dir
    , s.n
    , case s.dir when 'E' then 1 when 'W' then -1 else 0 end
    , case s.dir when 'N' then 1 when 'S' then -1 else 0 end
  from fill_in_steps s
  inner join tmp_2016_01b e
    on s.n + 1 = e.n
    and s.steps > 0
)
select
  *
from (
  select
    a
    , turn
    , steps
    , bearing
    , dir
    , n
    , x
    , y
    , abs(x) + abs(y) total
    , row_number() over (partition by x, y order by n, steps desc) rn
  from (
    select
      a
      , turn
      , steps
      , bearing
      , dir
      , n
      , sum(x) over (order by n, steps desc) x
      , sum(y) over (order by n, steps desc) y
    from fill_in_steps
  ) x
) y
where rn > 1
order by n, steps desc
;

