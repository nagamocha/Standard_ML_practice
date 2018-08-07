use "ex1.sml";

(*assume non-empty list of ints*)
val test1 = alternate([1,2,3,4]) = ~2
            andalso alternate([1,2,3]) = 2
            andalso alternate([1]) = 1

val test2 = min_max([1,1,1,1,1]) = (1,1)
            andalso min_max([2]) = (2,2)
            andalso min_max([2,1]) = (1,2)
            andalso min_max([9,4,5,1,5,4,2,5,9]) = (1,9)

val test3 = cum_sum([1,4,20]) = [1,5,25]
            andalso cum_sum([1]) = [1]
            andalso cum_sum([]) = []
            andalso cum_sum([0,0,0]) = [0,0,0]

val test4 = greeting(NONE) = "Hello there, you!"
            andalso greeting(SOME "Mary") = "Hello there, Mary!"

val test5 = repeat([1,2,3],[4,0,3]) = [1,1,1,1,3,3,3]
            andalso repeat([1],[6]) = [1,1,1,1,1,1]
            andalso repeat([5],[0]) = []
            andalso repeat([],[]) = []

val test6 = addOpt(SOME 1, SOME 2) = SOME 3
            andalso addOpt(NONE, SOME 2) = NONE
            andalso addOpt(NONE, NONE) = NONE
            andalso addOpt(SOME 1, NONE) = NONE

val test7 = addAllOpt([SOME 1, SOME 2, SOME 3]) = SOME 6
            andalso addAllOpt([NONE, SOME 10, SOME 20]) = SOME 30
            andalso addAllOpt([NONE, NONE]) = NONE
            andalso addAllOpt([]) = NONE

val test8 = any([]) = false
            andalso any([false, false, false]) = false
            andalso any([false, false, true]) = true
            andalso any([true]) = true

val test9 = all([]) = true
            andalso all([true, true, true]) = true
            andalso all([true, true, false]) = false
            andalso all([false]) = false
            andalso all([true]) = true

val test10 = zip([], []) = []
            andalso zip([1,2,3],[4,6]) = [(1,4),(2,6)]
            andalso zip([10,20,30,40], [11,12,13,14,15,16]) = [(10,11), (20,12), (30,13), (40,14)]
            andalso zip([4,4,4,4],[]) = []
            andalso zip([],[56,57,58]) = []
            andalso zip([1,1,1],[2,2,2]) = [(1,2),(1,2),(1,2)]

(*assumes both lists are nonempty*)
val test11 = zipRecycle([1,2,3],[1,2,3,4,5,6,7]) = [(1,1),(2,2),(3,3),(1,4),(2,5),(3,6),(1,7)]
            andalso zipRecycle([1],[9,8,7,6]) = [(1,9), (1,8), (1,7), (1,6)]
            andalso zipRecycle([1,1,1],[2,2,2]) = [(1,2),(1,2),(1,2)]
            andalso zipRecycle([10,20,30,40,50],[1,2]) = [(10,1),(20,2),(30,1),(40,2),(50,1)]


val test12 = zipOpt([1,2,3], [4,5,6]) = SOME [(1,4),(2,5),(3,6)]
            andalso zipOpt([1,2,3,5],[1,2,3]) = NONE
            andalso zipOpt([],[]) = SOME []


val test13 = lookup([("ab",1),("cd",2),("ef",3)], "fg") = NONE
            andalso lookup([], "fr") = NONE
            andalso lookup([("qwerty",1),("asdfg", 2), ("zxcvb",3)], "asdfg") = SOME 2


val test14 = splitup( [1,~2,4,~6,~10,6,0] ) =  ( [1,4,6,0], [~2,~6,~10])
            andalso splitup([1,2,3,4]) = ([1,2,3,4],[])
            andalso splitup([]) = ([],[])
            andalso splitup([~1]) = ([],[~1])


val test15 = splitAt([1,2,3,4,5,6,7],4) = ([1,2,3],[4,5,6,7])
            andalso splitAt([~1,4,7,3,10,~8,2,2],3) = ([~1,~8,2,2],[4,7,3,10])
            andalso splitAt([],1) = ([],[])
            andalso splitAt([10,11,12,13,14,15],10) = ([],[10,11,12,13,14,15])
            andalso splitAt([1,2,3,4,5],6) = ([1,2,3,4,5],[])


val test16 = isSorted([1,2,3,4,5]) = true
            andalso isSorted([]) = true (*trivially true *)
            andalso isSorted([1,2,3,4,~1]) = false
            andalso isSorted([4]) = true
            andalso isSorted([6,5,4,3,2,1]) = false
            andalso isSorted([1,1,1,1,1,1]) = true


val test17 = isAnySorted([1,2,3,4,5]) = true
            andalso isAnySorted([]) = true (*trivially true *)
            andalso isAnySorted([1,2,3,4,~1]) = false
            andalso isAnySorted([4]) = true
            andalso isAnySorted([6,5,4,3,2,1]) = true
            andalso isAnySorted([1,1,1,1,1,1]) = true


val test18 = sortedMerge([1,4,7],[5,8,9]) = [1,4,5,7,8,9]
            andalso sortedMerge([],[]) = []
            andalso sortedMerge([],[6,7,8,9]) = [6,7,8,9]
            andalso sortedMerge([4,5,6],[]) = [4,5,6]
            andalso sortedMerge([4,6,8,10],[1,2,3,5,7,9]) = [1,2,3,4,5,6,7,8,9,10]


val test19 = qsort([1,7,2,8,3,9,10]) = [1,2,3,7,8,9,10]
            andalso qsort([]) = []
            andalso qsort([5]) = [5]
            andalso qsort([10,1]) = [1,10]
            andalso qsort([10,9,8,7,6,5]) = [5,6,7,8,9,10]

val test20 = divide([1,2,3,4,5,6,7]) = ([1,3,5,7],[2,4,6])
            andalso divide([1]) = ([1],[])
            andalso divide([]) = ([],[])
            andalso divide([1,2,3,4,5,6,7,8]) = ([1,3,5,7],[2,4,6,8])
            andalso divide([1,2]) = ([1],[2])

val test21 = not_so_quick_sort([1,7,2,8,3,9,10]) = [1,2,3,7,8,9,10]
            andalso not_so_quick_sort([]) = []
            andalso not_so_quick_sort([5]) = [5]
            andalso not_so_quick_sort([10,1]) = [1,10]
            andalso not_so_quick_sort([10,9,8,7,6,5]) = [5,6,7,8,9,10]
(*
*)



























































(*END*)
