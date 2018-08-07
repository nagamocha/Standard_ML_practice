
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
fun greeting(nm: string option) =
    if isSome nm then "Hello there, " ^ valOf nm ^ "!"
    else "Hello there, you!"

(*function repeat : int list * int list -> int list
given a list of integers and another list of nonnegative integers, repeats the integers in the
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
fun addOpt(a: int option, b: int option) =
    if isSome a andalso isSome b then SOME (valOf a + valOf b)
    else NONE


(*function addAllOpt : int option list -> int option
given a list of "optional" integers, adds those integers that are there (i.e. adds all the SOMEi).
For example: addAllOpt ([SOME 1, NONE, SOME 3]) = SOME 4
If the list does not contain any SOME i in it, i.e. they are all NONE
or the list is empty, the function should return NONE.*)
fun addAllOpt(xs :int option list) =
    let
        fun get_all_ints(xs: int option list) =
            if null xs then []
            else if isSome (hd xs) then valOf (hd xs) :: get_all_ints(tl xs)
            else get_all_ints(tl xs)
        fun sum_all(xs: int list, sm: int) =
            if null xs then sm
            else sum_all(tl xs, hd xs + sm)
        val ints = get_all_ints(xs)
    in
        if null ints then NONE else SOME (sum_all(ints, 0))
    end


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


(*A version zipRecycle of zip,
where when one list is empty it starts recycling from its start until the other list completes.
For example: zipRecycle ([1,2,3], [1, 2, 3, 4, 5, 6, 7]) = [(1,1), (2,2), (3, 3), (1,4), (2,5), (3,6), (1,7)]

Note: this is really ugly, will rewrite it to reduce unecessary repetition

*)
fun zipRecycle(startA: int list, startB: int list) =
    let
        fun length(xs: int list, acc: int) = if null xs then acc else length(tl xs, acc + 1)
        val lengthA = length(startA, 0)
        val lengthB = length(startB, 0)
        val startShorter  =  if lengthA < lengthB then startA else startB
        fun construct(a: int list, b: int list) = (*assume a is the longer 1*)
            if null a then []
            else if null b then (hd a, hd startShorter)::construct(tl a, tl startShorter)
            else (hd a, hd b)::construct(tl a, tl b)
        fun swapPair(xs: (int * int) list) =
            if null xs then []
            else (#2 (hd xs), #1 (hd xs) ) :: swapPair(tl xs)
    in
        if lengthA  > lengthB then construct(startA, startB)
        else swapPair (construct(startB, startA))
    end

(*A version zipOpt of zip with return type (int * int) list option
This version should return SOME of a list when the original lists have the same length, and NONE if they do not.*)
fun zipOpt(xs: int list, ys: int list) =
    let
        fun length(xs: int list, acc: int) = if null xs then acc else length(tl xs, acc + 1)
        fun zip(xs: int list, ys: int list) =
            if null xs then []
            else (hd xs, hd ys):: zip(tl xs, tl ys)
    in
        if length(xs, 0) <> length(ys,0) then NONE
        else SOME (zip(xs, ys))
    end

(*function lookup : (string * int) list * string -> int option
that takes a list of pairs (s,i) and also a string s2 to look up.
It then goes through the list of pairs looking for the string s2 in the first component.
If it finds a match with corresponding number i, then it returns SOME i.
 If it does not, it returns NONE.*)
 fun lookup(xs: (string * int) list, s: string) =
    if null xs then NONE
    else if #1 (hd xs) = s then SOME (#2 (hd xs))
    else lookup(tl xs, s)


(*function splitup : int list -> int list * int list that given a list of integers
creates two lists of integers, one containing the non-negative entries,
the other containing the negative entries. Relative order must be preserved:
All non-negative entries must appear in the same order in which they were on the original list,
and similarly for the negative entries.*)
fun splitup(xs: int list) =
    let
        fun getNeg(xs: int list) =
            if null xs then []
            else if (hd xs) < 0 then hd xs :: getNeg(tl xs)
            else getNeg(tl xs)
        fun getNonNeg(xs: int list) =
            if null xs then []
            else if (hd xs) >= 0 then (hd xs):: getNonNeg(tl xs)
            else getNonNeg(tl xs)
    in
        (getNonNeg(xs), getNeg(xs))
    end


(*version splitAt : int list * int -> int list * int list of the previous function
takes an extra "threshold" parameter, and uses that instead of 0 as the s
eparating point for the two resulting lists.*)
fun splitAt(xs: int list, y) =
    let
        fun getLt(xs: int list) =
            if null xs then []
            else if (hd xs) < y then hd xs :: getLt(tl xs)
            else getLt(tl xs)
        fun getGt(xs: int list) =
            if null xs then []
            else if (hd xs) >= y then (hd xs):: getGt(tl xs)
            else getGt(tl xs)
    in
        (getLt(xs), getGt(xs))
    end


(*function isSorted : int list -> boolean that given a list of integers
determines whether the list is sorted in increasing order.*)
fun isSorted(xs: int list) =
    if null xs then true
    else if null (tl xs) then true
    else (hd xs) <= (hd (tl xs)) andalso isSorted(tl xs)


(*function isAnySorted : int list -> boolean, that given a list of integers
determines whether the list is sorted in either increasing or decreasing order.*)
fun isAnySorted(xs: int list) =
    let
        fun isSortedDesc(xs: int list) =
            if null xs then true
            else if null (tl xs) then true
            else (hd xs) >= (hd (tl xs)) andalso isSortedDesc(tl xs)
    in
        isSorted(xs) orelse isSortedDesc(xs)
    end



(*function sortedMerge : int list * int list -> int list that takes two
lists of integers that are each sorted from smallest to largest, and merges
them into one sorted list. For example: sortedMerge ([1,4,7], [5,8,9]) = [1,4,5,7,8,9].*)
fun sortedMerge(xs: int list, ys: int list) =
    if null xs then ys
    else if null ys then xs
    else if (hd xs < hd ys) then    (hd xs) :: sortedMerge(tl xs, ys) else
                                    (hd ys) :: sortedMerge(xs, tl ys)

(*function qsort : int list -> int list that works as follows:
Takes the first element out, and uses it as the "threshold" for splitAt.
It then recursively sorts the two lists produced by splitAt. Finally it brings
the two lists together.
Puts back in element that was taken out*)

fun qsort(xs: int list) =
    if null xs then []
    else
        let val Sep = splitAt(tl xs, hd xs)
        in
            qsort(#1 Sep) @ hd xs :: qsort(#2 Sep)
        end

(*function divide : int list -> int list * int list
takes a list of integers and produces two lists by alternating elements between the two lists.
For example: divide ([1,2,3,4,5,6,7]) = ([1,3,5,7], [2,4,6])*)
fun divide(xs: int list) =
    if null xs then ([],[]) else
    let
        fun alternate(xs: int list) =
            if null xs then []
            else if null (tl xs) then xs
            else hd xs :: alternate(tl (tl xs))
    in
        (alternate(xs), alternate(tl xs))
    end


(*another sorting function not_so_quick_sort:intlist->intlist
 works as follows: Given the initial list of integers, splits it in two lists using divide,
then recursively sorts those two lists, then merges them together with sortedMerge.*)
fun not_so_quick_sort(xs: int list) =
    if null xs then []
    else if null (tl xs) then xs
    else
        let val Dv = divide(xs) in
            sortedMerge(not_so_quick_sort(#1 Dv), not_so_quick_sort(#2 Dv))
        end



(*function fullDivide:int*int->int*int that given two numbers k and n
it attempts to evenly divide k into |n as many times as possible,
and returns a pair (d,n2) where d is the number of times
while n2 is the resulting n after all those divisions.
Examples: fullDivide(2,40)=(3,5) because 2*2*2*5=40
and fullDivide((3,10))=(0,10) because 3 does not divide 10.*)


(*Using fullDivide, write a function factorize:int->(int*int)list that given a number n
returns a list of pairs (d,k) where d is a prime number dividing n and k is the
number of times it fits. The pairs should be in increasing order of prime factor,
and the process should stop when the divisor considered surpasses the square root of n.
If you make sure to use the reduced number n2 given by fullDivide for each next step,
you should not need to test if the divisors are prime: If a number divides into n,
it must be prime (if it had prime factors, they would have been earlier prime factors
of n and thus reduced earlier).
Examples: factorize(20)=[(2,2),(5,1)]; factorize(36)=[(2,2),(3,2)]; factorize(1)=[].*)


(*function multiply:(int*int)list->int that given a factorization of a number n
as described in the previous problem computes back the number n.
So this should do the opposite of factorize.*)


(*Challenge (hard): Write a function all_products:(int*int)list->intlist
that given a factorization list result from factorize creates a
list all of possible products produced from using some or all of those prime factors
no more than the number of times they are available. This should end up being a
list of all the divisors of the number n that gave rise to the list.
Example: all_products([(2,2),(5,1)])=[1,2,4,5,10,20].
For extra challenge, your recursive process should return the numbers in this order,
as opposed to sorting them afterwards.*)

































































(*END*)
