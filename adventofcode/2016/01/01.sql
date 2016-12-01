
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

