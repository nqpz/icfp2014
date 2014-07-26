revcum(xs, res) =
  if atom xs
  then res
  else revcum(snd xs, (fst xs, res))

rev(xs) = revcum(xs, 0)

lencum(xs, res) =
  if atom xs
  then res
  else lencum(snd xs, res+1)

len(xs) = lencum(xs, 0)

map(f, xs) =
  if atom xs
  then 0
  else (f(fst xs), map(f, snd xs))

mapcum(f, xs, res) =
  if atom xs
  then res
  else mapcum(f, snd xs, (f(fst xs), res))

maprev(f, xs) = mapcum(f, xs, 0)

drop(xs, n) =
  if n == 0
  then xs
  else drop(snd xs, n - 1)

take(xs, n) =
  if n == 0
  then 0
  else (fst xs, take(snd xs, n-1))

takecum(xs, n, res) =
  if n == 0
  then res
  else takecum(snd xs, n - 1, (fst xs, res))

takerev(xs, n) = takecum(xs, n, 0)

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

filtercum(f, xs, res) =
  if atom xs
  then res
  else filtercum(f, snd xs, if f(fst xs) then (fst xs, res) else res)

filterrev(f, xs) = filtercum(f, xs, 0)

mod(a, b) = a - (a/b)*b

elem(a, xs) =
  if atom xs
  then 0
  else if a == fst xs
       then 1
       else elem(a, snd xs)

listeq(xs, ys) =
  if atom xs
  then atom ys
  else fst xs == fst ys && listeq(snd xs, snd ys)
