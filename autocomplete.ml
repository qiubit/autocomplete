type tree =
	| Node of char * tree list ref

let empty () = Node ('A', ref [])

let value t =
	match t with
	| Node (x, _) -> x

let list_t t =
	match t with
	| Node (_, l) -> l

let rec add_word t word current_depth letters created_new =
	let lt = list_t t in
	if current_depth = String.length word then 
		if created_new then letters else letters-1 else
	let letter_list = List.filter (fun (Node (x, _)) -> x = (word.[current_depth])) !lt in
	if letter_list = [] then begin
		lt := Node (word.[current_depth], ref []) :: !lt;
		add_word (List.hd !lt) word (current_depth+1) letters true
	end else
		add_word (List.hd letter_list) word (current_depth+1) (letters+1) false
		
let aw w t = add_word t w 0 1 false;;

let instream = open_in "input.txt" in
let outstream = open_out "output.txt" in
let tests = int_of_string (input_line instream) in
begin
	for i=1 to tests do
		let t = empty () in
		let words = int_of_string (input_line instream) in
		let letters = ref 0 in
		for i=1 to words do
			let word = input_line instream in
			begin
				letters := !letters + aw word t
			end
		done;
		Printf.fprintf outstream "%s%d%s%d\n" "Case #" i ": " !letters
	done
end;;


