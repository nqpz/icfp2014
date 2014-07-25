test_div(n)   = if n/2 == 2 then 1 else 0
test_cons1(n) = (n, n)
test_cons2(n) = (n, n+1)
test_cons3(n) = (n, 42)
test_fst(n)   = fst n
test_snd(n)   = snd n
test_atom(n)  = atom n

##
print test_div(4) ;
print test_div(2) ;
print test_cons1(0) ;
print test_cons2(0) ;
print test_cons3(0) ;
print test_fst((42, 43, 44)) ;
print test_snd((42, 43, 44)) ;
print test_atom((42, 43, 44)) ;
print test_atom(42, 43) ;
42