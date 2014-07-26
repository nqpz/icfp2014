nop(x)             = x
worldmap(x)        = x
lam_status(x)      = x
lam_loc(x)         = x
lam_dir(x)         = x
ghost_status(x)    = x
fruit_status(x)    = x

update_world(w) =
   worldmap      = fst w                  ;
   lam_status    = fst snd w              ;
   ghost_status  = fst snd snd w          ;
   fruit_status  = snd snd snd w          ;
   lam_loc       = fst snd lam_status     ;
   lam_dir       = fst snd snd lam_status ;
   0

get_tile(x, y) = nth(nth(worldmap, y), x)

ghost_capabilities(ghost) =
  map(\instr -> checkint(instr), ghost)

checkint(instr) =
  let op = fst fst instr
      args = snd fst instr
  in if op == 13
     then fst args
     else -1

min_steps(rs, idx) = if atom rs then (rs, idx) else
                     let res = min_steps(snd rs, idx+1)
                     in if fst rs < fst res
                        then (fst rs, idx)
                        else res
nearest_pill(pos, steps, direction) =
    let x = fst pos
        y = snd pos
    in let tile = get_tile(x,y)
    in  if tile == 0 || steps > 3 then (2147483647, -1) else
        if tile == 4
        then (steps, direction)
        else let right = nearest_pill((x+1,y), steps+1, 1)
                 left  = nearest_pill((x-1,y), steps+1, 3)
                 down  = nearest_pill((x,y+1), steps+1, 2)
                 up    = nearest_pill((x,y-1), steps+1, 0)
             in let min = min_steps((fst up,fst right,fst down,fst left), 0)
             in (steps, snd min)


##
nop = update_world(world) ;
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
print get_tile(0,0) ;
print get_tile(11,12) ;
print lam_loc ;
print lam_dir ;
print min_steps((999, 1, 999, 1), 0) ;
print nearest_pill((8,11),lam_loc, 0, -2) ;
42
>>>>>>> aaa4992b25544f0e322794e8fe8ce4cf49d9ae12
