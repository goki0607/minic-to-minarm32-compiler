function int incrementInt(int x) {
	x = x + 1;
	return x;
}

// Adding an integer to a pointer multiplies the integer with the
// size of the type that is pointed to (pointer arithmetic)
function int incrementIntPointer(* int x) {
	x = x + 1;
	return x;
}

function int incrementCharPointer(* char x) {
	x = x + 1;
	return x;
}
