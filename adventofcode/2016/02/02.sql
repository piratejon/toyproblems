
drop table if exists tmp_2016_02
;

create table tmp_2016_02 (input text)
;

\copy tmp_2016_02 from './input'
;

drop table if exists tmp_2016_02b
;

select
  direction
  , index
  , row_number() over (partition by position order by index desc) rindex
  , position
into tmp_2016_02b
from (
  select
    unnest(arr) direction
    , generate_subscripts(arr, 1) as index
    , position
  from (
    select
      string_to_array(input, NULL) arr
      , row_number() over () as position
    from tmp_2016_02
  ) y
) x
;

with recursive press as (
  select
    direction
    , index
    , rindex
    , position
    , 5 as start
    , case
    when direction = 'U' then 2
    when direction = 'D' then 8
    when direction = 'R' then 6
    when direction = 'L' then 4
    end as end
  from tmp_2016_02b
  where index = 1 and position = 1

  union all

  select
    n.direction
    , n.index
    , n.rindex
    , n.position
    , p.end
    , case
    when n.direction = 'U' and p.end not in (1,2,3) then p.end - 3
    when n.direction = 'D' and p.end not in (7,8,9) then p.end + 3
    when n.direction = 'R' and p.end not in (3,6,9) then p.end + 1
    when n.direction = 'L' and p.end not in (1,4,7) then p.end - 1
    else p.end
  end as end
  from press p
  inner join tmp_2016_02b n
    on (
        p.position = n.position
        and p.index + 1 = n.index
        and p.rindex <> 1
      ) or (
        p.position + 1 = n.position
        and p.rindex = 1
        and n.index = 1
    )
)
select
*
from press
where rindex = 1
order by position
;

with recursive press as (
  select
    direction
    , index
    , rindex
    , position
    , '5' as start
    , case direction
    when 'U' then '5'
    when 'D' then '5'
    when 'R' then '6'
    when 'L' then '5'
    end as end
  from tmp_2016_02b
  where index = 1 and position = 1

  union all

  select
    n.direction
    , n.index
    , n.rindex
    , n.position
    , p.end
    , case
    when n.direction = 'U' then
      case p.end
      when '3' then '1'
      when '6' then '2'
      when '7' then '3'
      when '8' then '4'
      when 'A' then '6'
      when 'B' then '7'
      when 'C' then '8'
      when 'D' then 'B'
      else p.end
    end
    when n.direction = 'D' then
      case p.end
      when '1' then '3'
      when '2' then '6'
      when '3' then '7'
      when '4' then '8'
      when '6' then 'A'
      when '7' then 'B'
      when '8' then 'C'
      when 'B' then 'D'
      else p.end
    end
    when n.direction = 'R' then
      case p.end
      when '2' then '3'
      when '3' then '4'
      when '5' then '6'
      when '6' then '7'
      when '7' then '8'
      when '8' then '9'
      when 'A' then 'B'
      when 'B' then 'C'
      else p.end
    end
    when n.direction = 'L' then
      case p.end
      when '3' then '2'
      when '4' then '3'
      when '6' then '5'
      when '7' then '6'
      when '8' then '7'
      when '9' then '8'
      when 'B' then 'A'
      when 'C' then 'B'
      else p.end
    end
  end as end
  from press p
  inner join tmp_2016_02b n
    on (
        p.position = n.position
        and p.index + 1 = n.index
        and p.rindex <> 1
      ) or (
        p.position + 1 = n.position
        and p.rindex = 1
        and n.index = 1
    )
)
select
*
from press
where rindex = 1
order by position
;

