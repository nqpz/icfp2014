len(xs) = if atom xs then 1 else 1 + len(snd xs)
map(f,xs) = if atom xs then f(xs) else (f(fst xs), map(f,snd xs))
nth(xs,n) = if n == 0 then if atom xs then xs else fst xs
            else nth(snd xs, n-1)
filter(f,xs) = if atom xs
               then if f(xs) then xs else 0
               else if f(fst xs) then (fst xs, filter(f, snd xs)) else filter(f, snd xs)
mod(a,b) = a - (a/b)*b
elem(a,xs) = if atom xs then a == xs else if a == fst xs then 1 else elem(a, snd xs)
ghost_capabilities(idx) = map(\instr -> checkint(instr), nth(ghosts, idx))
checkint(instr) = let op   = fst fst instr
                      args = snd fst instr
                  in if op == 13
                     then fst args
                     else -1
listeq(xs, ys) =
  if atom xs
  then if atom ys
       then xs == ys
       else 0
  else fst xs == fst ys && listeq(snd xs, snd ys)

get_tile(x,y) = nth(nth(fst world, y), x)
get_loc(void) = nth(nth(world, 1), 1)
get_direction(void) = nth(nth(world, 1), 2)
min_steps(rs, idx) = if atom rs then (rs, idx) else
                     let res = min_steps(snd rs, idx+1)
                     in if fst rs < fst res
                        then (rs, idx)
                        else res
nearest_pill(pos, steps, direction) =
    let x = fst pos
        y = snd pos
        tile = get_tile(x,y)
    in  if tile == 0 then (-1, 2147483647, -1) else
        if tile == 4
        then (steps, direction)
        else let right = nearest_pill((x+1,y), steps+1, 0)
                 left  = nearest_pill((x-1,y), steps+1, 1)
                 down  = nearest_pill((x,y+1), steps+1, 2)
                 up    = nearest_pill((x,y-1), steps+1, 3)
                 min   = min_steps(map(\x -> fst x, (up,right,down,left)), 0)
             in (steps, min)

##
print len((0,1,2,3,4)) ;
print map((\x -> x + 5), (0,1,2,3,4)) ;
print nth((1,2,3,4),3) ;
print filter(\x -> mod(x,2) == 0, (1,2,3,4)) ;
print listeq((1,2,3,4),(1,2,3,4)) ;
print listeq((1,2,3,5),(1,2,3,4)) ;
print get_tile(0,0) ;
print get_tile(11,12) ;
print get_loc(0) ;
print get_direction(0) ;
print min_steps((5, 3, 1, 6), 0) ;
print nearest_pill(get_loc(0), 0, -2) ;
42
