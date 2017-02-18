inputFile("medium.in").
outputFile("output.txt").
internal(0).

%% NOTE: reads the input file and stores the data into input(RowCount, ColumnCount, Minimum, Maximum, Pizza)
readInputFile:-
	inputFile(InputFile),
	open(InputFile, read, FileInput),

	% NOTE: read the first line and parse the parameters
	read_string(FileInput, "\n", "\t", _, Parameters),
	split_string(Parameters, " ", "", [RowCountString, ColumnCountString, MinimumString, MaximumString]),
	number_string(RowCount, RowCountString),
	number_string(ColumnCount, ColumnCountString),
	number_string(Minimum, MinimumString),
	number_string(Maximum, MaximumString),

	readRows(FileInput, RowCount, Pizza),
	assert(input(RowCount, ColumnCount, Minimum, Maximum, Pizza)),
	debugPrintReadFile,

	close(FileInput).

debugPrintReadFile:-
	internal(0), !.
debugPrintReadFile:-
	internal(1), !,
	input(RC, CC, Mi, Ma, Pi),
	write(RC), write(" "), 
	write(CC), write(" "),
	write(Mi), write(" "),
	write(Ma), write(" "),
	writeln(Pi).

% NOTE: read the next RowCount lines and return a list of rows (a list of row components)
readRows(FileInput, RowCount, Pizza):-
	readRowsRec(FileInput, 0, RowCount, [], ResultPizza), reverse(ResultPizza, Pizza).

readRowsRec(_, RowCount, RowCount, Pizza, Pizza):-!.
readRowsRec(FileInput, RowIndex, RowCount, PartialPizza, Pizza):-
	% NOTE: read a line from the input file and atomizes it into a list of t and m
	read_string(FileInput, "\n", "\t", _, PizzaRowString),
	string_lower(PizzaRowString, PizzaRowLowerString),
	atom_chars(PizzaRowLowerString, PizzaRow),

	NextRowIndex is RowIndex + 1,
	readRowsRec(FileInput, NextRowIndex, RowCount, [PizzaRow | PartialPizza], Pizza).

%% NOTE: writes the number of slices and the slices data in the output file
writeOutputFile(SliceCount, Solution):-
	outputFile(OutputFile),
	open(OutputFile, write, FileOutput),

	writeln(FileOutput, SliceCount),
	writeSolution(FileOutput, Solution),

	close(FileOutput).

writeSolution(_, []):-!.
writeSolution(FileOutput, [[] | Solution]):- 
	writeSolution(FileOutput, Solution), !.
writeSolution(FileOutput, [[R1, C1, R2, C2] | Solution]):-
	write(FileOutput, R1), write(FileOutput, " "),
	write(FileOutput, C1), write(FileOutput, " "),
	write(FileOutput, R2), write(FileOutput, " "),
	writeln(FileOutput, C2),

	writeSolution(FileOutput, Solution).

% test
r:-readInputFile.
w:-writeOutputFile(3, [[0, 0, 2, 1], [0, 2, 2, 2], [0, 3, 2, 4]]).
