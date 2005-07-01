open List;;
type lexique = V | N of char*lexique*lexique;;


let add_char s c = function
    | 1 -> s^Char.escaped c^"\n"
    | _ -> s^Char.escaped c^" ";;


let lexie s =
    let len = String.length s in
    let rec string_to_list accu = function
        | 0  -> accu
        | i -> string_to_list (s.[i-1]::accu) (i-1)

        in string_to_list [] len;;


let inter ls =
    let rec intersect accu = function
        | ([],_) | (_,[]) -> List.rev accu
        | (x1::l1 , x2::l2) -> if x1<x2 then intersect accu (l1 , x2::l2)
            else if x1>x2 then intersect accu (x1::l1 , l2)
            else (intersect (x1::accu) (l1,l2))
        in intersect [] ls;;


let rec add (a,lc) = match (a,lc) with
    | (a,[]) -> a
    | (V,y::lc) -> N(y,add(V,lc),V)
    | (N(x,fg,fd),y::lc) when x!=y -> N(x,fg,add(fd,y::lc))
    | (N(x,fg,fd),y::lc) -> N(x,add(fg,lc),fd);;


let rec trunc n =
    let help = function
        | V -> V
        | N(x,V,fd) when x!='~' -> fd
        | a -> a in
    function
        | V -> V
        | N(x,_,fd) when (n=0)&(x!='~') -> help (trunc n fd)
        | N(x,fg,fd) -> help (N(x,trunc (n-1) fg,trunc n fd));;


let rec term c = function
    | V -> V
    | N(x,fg,_) when x=c -> fg
    | N(_,_,fd) -> term c fd;;


let rec initiales = function
    | V -> []
    | N(x,_, fd) -> x::(initiales fd);;


let file_fold f u nom =
    let chan = open_in nom in
    let u = ref u in
    let _ = try
           while true do
              u := f (input_line chan) !u
           done
        with
           End_of_file -> close_in chan
    in !u;;


let load address = let f s a = add(a, lexie s) in file_fold f V address;;


let dico = load "./dico.txt";;


let suppr l = rev (tl (rev l));;


let run d =
    let rec fill (i,j,s,ag,fah) =
    let dicod = trunc d dico
    in
    match (i,j,inter (initiales ag, initiales (hd(rev fah)))) with
    | (0,_,_) -> print_string (s^"\n");flush stdout
    | (i,1,lex) -> iter (function c -> fill (i-1,d,add_char s c 1,dicod,(term c (hd (rev fah)))::(suppr fah))) lex
    | (i,j,lex) when i=d -> iter (function c -> fill (d,j-1,add_char s c j,term c ag,(term c (hd (rev fah)))::fah)) lex
    | (i,j,lex) -> iter (function '~' -> fill (i,j-1,add_char s '~' j,trunc (j-1) dico,(trunc (i-1) dico)::(suppr fah))
    | c -> fill (i,j-1,add_char s c j,term c ag,(term c (hd (rev fah)))::(suppr fah))) lex
in 
fill (d, d, "", trunc d dico, trunc d dico::[]);;
