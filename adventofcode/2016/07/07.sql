
drop table if exists tmp_2016_07
;

create table tmp_2016_07 (input text)
;

\copy tmp_2016_07 from './input'
;

drop table if exists tmp_2016_07_ssl_check
;

-- 65 is too low
select
  line
  , input
from (
  select
    line
    , input
    , sum(case when h = 1 and abba = 1 then 1 else 0 end) h
    , sum(abba) abba
  from (
    select
      line
      , input
      , c
      , i
      , sum(case c when '[' then 1 when ']' then -1 else 0 end) over (partition by line order by i) - case when c = '[' then 1 else 0 end h
      , case
        when lag(c, 3) over (partition by line order by i) = c
          and lag(c, 2) over (partition by line order by i) = lag(c, 1) over (partition by line order by i)
          and c <> lag(c, 1) over (partition by line order by i)
        then 1
      else 0
      end abba
    from (
      select
        line
        , input
        , unnest(a) c
        , generate_subscripts(a, 1) i
      from (
        select
          row_number() over () as line
          , input
          , string_to_array(input, NULL) a
        from tmp_2016_07
      ) z
    ) y
  ) x
  group by 1,2
) w
where h = 0 and abba > 0 -- part 1
;

with ssl_check as (
  select
    line
    , input
    , c
    , i
    , h
    , case when aba_flag = 1 then aba end aba
    , case when aba_flag = 1 then bab end bab
    , aba_flag
  from (
    select
      line
      , input
      , c
      , i
      , sum(case c when '[' then 1 when ']' then -1 else 0 end) over (partition by line order by i) - case when c = '[' then 1 else 0 end h
      , lag(c, 2) over (partition by line order by i)
        || lag(c, 1) over (partition by line order by i)
        || c aba
      , lag(c, 1) over (partition by line order by i) || c || lag(c, 1) over (partition by line order by i) bab
      , case
        when lag(c, 2) over (partition by line order by i) = c
        and lag(c, 1) over (partition by line order by i) <> c
        then 1 else 0
      end aba_flag
    from (
      select
        line
        , input
        , unnest(a) c
        , generate_subscripts(a, 1) i
      from (
        select
          row_number() over () as line
          , input
          , string_to_array(input, NULL) a
        from tmp_2016_07
      ) z
    ) y
  ) x
)
select
*
into tmp_2016_07_ssl_check
from ssl_check
-- where aba_flag = 1
;

create index cix_tmp_2016_07_ssl_check on tmp_2016_07_ssl_check (
  line
  , aba_flag
  , aba
  , bab
  , h
)
;

select
  distinct aba.line
from tmp_2016_07_ssl_check aba
inner join tmp_2016_07_ssl_check bab
  on 1=1
  and aba.line = bab.line
  and aba.aba_flag = 1
  and bab.aba_flag = 1
  and aba.bab = bab.aba
  and aba.h = 0 and bab.h = 1
;

