inputFile("output.txt").
outputFile("result.out").
swap:-
	inputFile(Input),
	outputFile(Output),
	open(Input, read, FileInput),
	open(Output, write, FileOutput),

	read_string(FileInput, "\n", "\t", _, SliceCountString),
	number_string(SliceCount, SliceCountString),
	writeln(FileOutput, SliceCount),
	swap_rec(FileInput, FileOutput, SliceCount),

	close(FileInput),
	close(FileOutput).

swap_rec(_, _, 0):-!.
swap_rec(FileInput, FileOutput, SliceIndex):-
	read_string(FileInput, "\n", "\t", _, SliceString),
	split_string(SliceString, " ", "", [R1String, R2String, C1String, C2String]),

	number_string(R1, R1String),
	number_string(R2, R2String),
	number_string(C1, C1String),
	number_string(C2, C2String),

	NewR1 is R1 - 1,
	NewR2 is R2 - 1,
	NewC1 is C1 - 1,
	NewC2 is C2 - 1,

	write(FileOutput, NewR1), write(FileOutput, " "),
	write(FileOutput, NewC1), write(FileOutput, " "),
	write(FileOutput, NewR2), write(FileOutput, " "),
	writeln(FileOutput, NewC2),

	NewSliceIndex is SliceIndex - 1,
	swap_rec(FileInput, FileOutput, NewSliceIndex).
