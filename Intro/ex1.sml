
(*excercise questions courtesy of Charilaos SKidas
as part of the Programming Languages course - Daniel Grossman
majority of function comments sourced and edited from original prompts
 *)


(*function alternate : int list -> int that
takes a list of numbers and adds them with alternating sign.
For example [1,2,3,4] = 1 - 2 + 3 - 4 = -2 [1,2,3,4]=1-2+3-4=-2.*)
fun alternate(xs: int list) =
    let
        fun evaluate(xs: int list, index: int, acc: int) =
            if null xs then acc
            else evaluate(tl xs, index + 1, acc + (if index mod 2 = 0 then ~ (hd xs) else hd xs))
    in
        evaluate(xs, 1, 0)
    end


(*function min_max : int list -> int * int takes a non-empty
list of numbers, and returns a pair (min,max) of the minimum and
maximum of the numbers in the list.*)
fun min_max(xs: int list) =
    let
        fun get_max(xs: int list, curr: int) =
            if null xs then curr
            else get_max(tl xs, if hd xs > curr then hd xs else curr)

        fun get_min(xs: int list, curr: int) =
            if null xs then curr
            else get_min(tl xs, if hd xs < curr then hd xs else curr)
    in
        (get_min(tl xs, hd xs), get_max(tl xs, hd xs))
    end


(*function sum : int list -> int list
takes a list of numbers and returns a list of the partial sums of those numbers.
For example sum [1,4,20] = [1,5,25]*)
fun cum_sum(xs: int list) =
    let
        fun cum_sum_acc(xs: int list, acc: int) =
            if null xs then []
            else let val curr_sum = hd xs + acc in
            curr_sum :: cum_sum_acc(tl xs, curr_sum) end
    in
        cum_sum_acc(xs, 0)
    end


(*function greeting : string option -> string
given a string option SOME name returns the string "Hello there, ...!"
where the dots would be replaced by name. Note that the name is given as an option,
 so if it is NONE then is replaced the dots with "you".*)


(*Write a function repeat : int list * int list -> int list
that given a list of integers and another list of nonnegative integers, repeats the integers in the
first list according to the numbers indicated by the second list.
For example: repeat ([1,2,3], [4,0,3]) = [1,1,1,1,3,3,3]*)
fun repeat(xs: int list, ns: int list) =
    let
        fun gen_list(x: int, n: int) =
            if n = 0 then []
            else x::gen_list(x,n-1)
    in
        if null xs then []
        else gen_list(hd xs, hd ns) @ repeat(tl xs, tl ns)
    end


(*function addOpt : int option * int option -> int option
given two "optional" integers, adds them if they are both present (returning SOME of their sum),
or returns NONE if at least one of the two arguments is NONE.*)


(*function addAllOpt : int option list -> int option
given a list of "optional" integers, adds those integers that are there (i.e. adds all the SOMEi).
For example: addAllOpt ([SOME 1, NONE, SOME 3]) = SOME 4
If the list does not contain any SOME i in it, i.e. they are all NONE
or the list is empty, the function should return NONE.*)


(*function any : bool list -> bool that given
a list of booleans returns true if there is at least one of them
that is true, otherwise returns false. (If the list is
empty it should return false because there is no true.)*)
fun any(ps: bool list) =
    if null ps then false
    else hd ps orelse any(tl ps)

(*function all : bool list -> bool that given
a list of booleans returns true if all of them true,
otherwise returns false. (If the list is empty it should
return true because there is no false.)*)
fun all(ps: bool list) =
    if null ps then true
    else hd ps andalso all(tl ps)

(*function zip : int list * int list -> int * int
that given two lists of integers creates consecutive pairs, and stops when one of the lists is empty.
For example: zip ([1,2,3], [4, 6]) = [(1,4), (2,6)]*)

fun zip(xs: int list, ys: int list) =
 if null xs orelse null ys then []
 else (hd xs, hd ys)::zip(tl xs, tl ys)




































































(*END*)
