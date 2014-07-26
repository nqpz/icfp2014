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

nearest_pill(pos, steps, direction) =
    let x = fst pos
        y = snd pos
    in let tile = get_tile(x,y)
    in  if tile == 0 || steps > 3 then (max_int, -1) else
        if tile == 4
        then (steps, direction)
        else let right = fst nearest_pill((x+1,y), steps+1, 1)
                 left  = fst nearest_pill((x-1,y), steps+1, 3)
                 down  = fst nearest_pill((x,y+1), steps+1, 2)
                 up    = fst nearest_pill((x,y-1), steps+1, 0)
             in (steps, min_idx((up, right, down, left, 0), 0))


##
nop = libstd_init(0) ;
nop = update_world(world) ;
print len((0,1,2,3,4,0)) == 5 ;
print eq(rev((0,1,2,3,4,0)) , (4, 3, 2, 1, 0, 0)) ;
print eq(drop((0,1,2,3,4,0), 2) , (2, 3, 4, 0)) ;
print eq(take((0,1,2,3,4,0), 2) , (0, 1, 0)) ;
print eq(take_rev((0,1,2,3,4,0), 2) , (1, 0, 0)) ;
print eq(map((\x -> x + 5), (0,1,2,3,4,0)), (5, 6, 7, 8, 9, 0)) ;
print eq(map_idx((\x y -> (x, y + 5)), 3, (0,1,2,3,4,0)), ((3, 5), (4, 6), (5, 7), (6, 8), (7, 9), 0)) ;
print eq(map_idx((\x y -> (x, y + 5)), 3, (0,1,2,3,4,0)), ((3, 5), (4, 6), (5, 7), (6, 8), (7, 9), 0)) ;
print eq(map_rev((\x -> x + 5), (0,1,2,3,4,0)), (9, 8, 7, 6, 5, 0)) ;
print eq(map_idx_rev((\x y -> (x, y + 5)), 3, (0,1,2,3,4,0)), ((7, 9), (6, 8), (5, 7), (4, 6), (3, 5), 0)) ;
print nth((1,2,3,4,0),3) == 4;
print eq(filter(\x -> mod(x,2) == 0, (1,2,3,4,0)), (2, 4, 0)) ;
print eq(filter_rev(\x -> mod(x,2) == 0, (1,2,3,4,0)), (4, 2, 0)) ;
print eq((1,2,3,4,0),(1,2,3,4,0)) == 1 ;
print eq((1,2,3,5,0),(1,2,3,4,0)) == 0 ;
print elem(3, (1,2,3,5,0)) == 1 ;
print elem(4, (1,2,3,5,0)) == 0 ;
print get_tile(0,0) ;
print get_tile(11,12) ;
print lam_loc ;
print lam_dir ;
print min_pos((999, 1, 999, 1, 0), 0) ;
print nearest_pill((8,11), lam_loc, 0, -2) ;
42
