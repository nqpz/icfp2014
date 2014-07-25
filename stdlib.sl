len(xs) = if atom xs then 1 else 1 + len(snd xs)
map(f,xs) = if atom xs then f(xs) else (f(fst xs), map(f,snd xs))
nth(xs,n) = if n == 1 then fst xs else nth(snd xs, n-1)
filter(f,xs) = if atom xs
               then if f(xs) then xs else 0
               else if f(fst xs) then (fst xs, filter(f, snd xs)) else filter(f, snd xs)
mod(a,b) = a - (a/b)*b
elem(a,xs) = if atom xs then a == xs else if a == fst xs then 1 else elem(a, snd xs)
ghost_capabilities(idx) = map(\instr -> checkint(instr), nth(ghost, idx))
checkint(instr) = let op   = fst instr
                      args = snd instr
                  in if op == 13
                     then fst args
                     else -1
listeq(xs, ys) = if atom xs || atom ys then xs == ys
                 else if fst xs == fst ys then listeq(snd xs, snd ys) else 0
##
print len((0,1,2,3,4)) ;
print map((\x -> x + 5), (0,1,2,3,4)) ;
print nth((1,2,3,4),3) ;
print filter(\x -> mod(x,2) == 0, (1,2,3,4)) ;
print listeq((1,2,3,4),(1,2,3,4)) ;
print listeq((1,2,3,5),(1,2,3,4)) ;
42
