fun merge([], ys) = ys
|	merge(xs, []) = xs
|	merge(x::xs, y::ys) =
	if x < y then
		x::merge(xs, y::ys)
	else
		y::merge(x::xs, ys);
		
fun split [] = ([],[])
|	split [a] = ([a],[])
|	split (a::b::cs) = 
		let val (M,N) =
			split cs in (a::M, b::N)
		end

fun mergesort [] = []
|	mergesort [a] = [a]
|   mergesort [a,b] =	if a <= b then
							[a,b]
						else [b,a]
|   mergesort L =
        let val (M,N) = split L
        in
          merge (mergesort M, mergesort N)
        end

		
		
		
fun insert x [] = [x]
|	insert x (y::ys) = 
	if x < y
		then x::y::ys
	else
		y :: (insert x ys)

fun insertionsort [] = []
|	insertionsort (x::xs) = insert x (insertionsort xs)






fun bsort [] = []
|	bsort [x] = [x]
|	bsort (x::y::xs) =   
					if(x>y)
						then y::x::xs
					else if (x=y)
						then x::y::xs
					else 
						x::bsort(y::xs);

fun	bubblesort x = 
				if (bsort x = x)
					then x
				else 
					bubblesort (bsort x);

					
				

fun quicksort nil = nil
|	quicksort (pivot :: rest) =
		let
			fun split(nil) = (nil,nil)
			|	split(x :: xs) =
			let
				val (below, above) = split(xs)
			in
				if x < pivot then
					(x :: below, above) 
                else
					(below, x :: above)
			end;
			val (below, above) = split(rest)
		in
			quicksort below @ [pivot] @ quicksort above
		end
		

		
		
		
		
fun selectionsort [] = []
|   selectionsort (first::last) =
    let
      fun select_r small ([], output) = small::(selectionsort output)
      |   select_r small (x::xs, output) =
            if (x< small) then
              select_r x (xs, small::output)
            else
              select_r small (xs, x::output)
    in
      select_r first (last, [])
    end
	