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

val test4 = false

val test5 = repeat([1,2,3],[4,0,3]) = [1,1,1,1,3,3,3]
            andalso repeat([1],[6]) = [1,1,1,1,1,1]
            andalso repeat([5],[0]) = []
            andalso repeat([],[]) = []

val test6 = false

val test7 = false

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





























































(*END*)
