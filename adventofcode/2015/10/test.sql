
with recursive

lol (
  n
) as (
  select
    1
  union all

  select
    n + 1
  from lol
  where n < 100
)

select
*
from lol

