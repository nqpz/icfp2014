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
  else mapcum(snd xs, (f(fst xs), res))

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

ghost_capabilities(idx) =
  map(\instr -> checkint(instr), nth(ghosts, idx))

checkint(instr) =
  let op = fst fst instr
      args = snd fst instr
  in if op == 13
     then fst args
     else -1

listeq(xs, ys) =
  if atom xs
  then atom ys
  else fst xs == fst ys && listeq(snd xs, snd ys)

get_tile(x, y) = nth(nth(fst world, y), x)
get_loc(void) = nth(nth(world, 1), 1)
get_direction(void) = nth(nth(world, 1), 2)

min_steps(rs, idx) = if atom rs then (rs, idx) else
                     let res = min_steps(snd rs, idx+1)
                     in if fst rs < fst res
                        then (fst rs, idx)
                        else res
nearest_pill(pos, steps, direction) =
    let x = fst pos
        y = snd pos
        tile = get_tile(x,y)
    in  if tile == 0 then (2147483647, -1) else
        if tile == 2
        then (steps, direction)
        else let right = nearest_pill((x+1,y), steps+1, 0)
                 left  = nearest_pill((x-1,y), steps+1, 1)
                 down  = nearest_pill((x,y+1), steps+1, 2)
                 up    = nearest_pill((x,y-1), steps+1, 3)
                 min   = min_steps(map(\x -> fst x, (up,right,down,left)), 0)
             in (steps, snd min)

##
print takerev((0,1,2,3,4,0), 2) ;

print len((0,1,2,3,4,0)) == 5 ;
print listeq(rev((0,1,2,3,4,0)) , (4, 3, 2, 1, 0, 0)) ;
print listeq(drop((0,1,2,3,4,0), 2) , (2, 3, 4, 0)) ;
print listeq(take((0,1,2,3,4,0), 2) , (0, 1, 0)) ;
print listeq(takerev((0,1,2,3,4,0), 2) , (1, 0, 0)) ;
print listeq(map((\x -> x + 5), (0,1,2,3,4,0)), (5, 6, 7, 8, 9, 0)) ;
print listeq(maprev((\x -> x + 5), (0,1,2,3,4,0)), (9, 8, 7, 6, 5, 0)) ;
print nth((1,2,3,4,0),3) == 4;
print listeq(filter(\x -> mod(x,2) == 0, (1,2,3,4,0)), (2, 4, 0)) ;
print listeq(filterrev(\x -> mod(x,2) == 0, (1,2,3,4,0)), (4, 2, 0)) ;
print listeq((1,2,3,4,0),(1,2,3,4,0)) == 1 ;
print listeq((1,2,3,5,0),(1,2,3,4,0)) == 0 ;
print elem(3, (1,2,3,5,0)) == 1 ;
print elem(4, (1,2,3,5,0)) == 0 ;
42
