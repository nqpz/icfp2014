nop(x)     = x
max_int(x) = x

libstd_init(x) =
   max_int = 2147483647 ;
   0

rev_cum(xs, res) =
  if atom xs
  then res
  else rev_cum(snd xs, (fst xs, res))

rev(xs) = rev_cum(xs, 0)

len_cum(xs, res) =
  if atom xs
  then res
  else len_cum(snd xs, res+1)

len(xs) = len_cum(xs, 0)

map(f, xs) =
  if atom xs
  then 0
  else (f(fst xs), map(f, snd xs))

map_idx(idx, f, xs) =
  if atom xs
  then 0
  else (f(idx, fst xs), map_idx(idx+1, f, snd xs))

map_all_helper(idx, f, left, cur, xs) =
  if atom xs
  then (f(idx, left, cur, cur), 0)
  else (f(idx, left, cur, fst xs), map_all_helper(idx+1, f, cur, fst xs, snd xs))

map_all(f, xs) =
  if atom xs
  then 0
  else if atom snd xs
       then (f(0, fst xs, fst xs, fst xs), 0)
       else map_all_helper(0, f, fst xs, fst xs, snd xs)

map_cum(f, xs, res) =
  if atom xs
  then res
  else map_cum(f, snd xs, (f(fst xs), res))

map_idx_cum(idx, f, xs, res) =
  if atom xs
  then res
  else map_idx_cum(idx+1, f, snd xs, (f(idx, fst xs), res))

map_rev(f, xs) = map_cum(f, xs, 0)
map_idx_rev(idx, f, xs) = map_idx_cum(idx, f, xs, 0)

drop(xs, n) =
  if n == 0
  then xs
  else drop(snd xs, n - 1)

take(xs, n) =
  if n == 0
  then 0
  else (fst xs, take(snd xs, n-1))

take_cum(xs, n, res) =
  if n == 0
  then res
  else take_cum(snd xs, n - 1, (fst xs, res))

take_rev(xs, n) = take_cum(xs, n, 0)

nth(xs, n) =
  if n == 0
  then fst xs
  else nth(snd xs, n-1)

filter(f, xs) =
  if atom xs
  then 0
  else if f(fst xs)
       then (fst xs, filter(f, snd xs))
       else filter(f, snd xs)

filter_cum(f, xs, res) =
  if atom xs
  then res
  else filter_cum(f, snd xs, if f(fst xs) then (fst xs, res) else res)

filter_rev(f, xs) = filter_cum(f, xs, 0)

mod(a, b) = a - (a/b)*b

elem(a, xs) =
  if atom xs
  then 0
  else if eq(a, fst xs)
       then 1
       else elem(a, snd xs)

eq(xs, ys) =
  if atom xs
  then if atom ys
       then xs == ys
       else 0
  else if atom ys
       then 0
       else eq(fst xs, fst ys) && eq(snd xs, snd ys)

min_pos_cum(rs, idx, cumpos, cumval) =
  if atom rs
  then (cumpos, cumval)
  else if fst rs <= cumval
       then min_pos_cum(snd rs, idx+1, idx,    fst rs)
       else min_pos_cum(snd rs, idx+1, cumpos, cumval)

min_pos(rs) =     min_pos_cum(rs, 0, -1, max_int)
min_idx(rs) = snd min_pos_cum(rs, 0, -1, max_int)
min(rs)     = fst min_pos_cum(rs, 0, -1, max_int)