(* version 1: use a helper function *)
fun suffixes xs =
  let fun assoc xs =
	case xs of
	    [] => []
	  | x :: xs' => xs' :: assoc (xs')
  in
      xs :: assoc xs
  end
      
				      
fun ThirdSuffixes xs =
  case xs of
      [] => [] :: []
    | x :: xs' => xs :: ThirdSuffixes (xs')
	   
