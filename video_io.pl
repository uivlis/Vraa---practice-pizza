inputFile("kittens.in").
outputFile("output.txt").
internal(0).

%% READING
readInputFile:-
	inputFile(InputFile),
	open(InputFile, read, FileInput),

	% read PARAMETERS
	read_string(FileInput, "\n", "\t", _, ParametersLine),
	split_string(ParametersLine, " ", "", [VideoCountString, EndpointCountString, RequestCountString, CacheCountString, CacheCapacityString]),

	number_string(VideoCount, VideoCountString),
	number_string(EndpointCount, EndpointCountString),
	number_string(RequestCount, RequestCountString),
	number_string(CacheCount, CacheCountString),
	number_string(CacheCapacity, CacheCapacityString),

	% read VIDEO SIZES
	read_string(FileInput, "\n", "\t", _, VideoSizeLine),
	split_string(VideoSizeLine, " ", "", VideoSizeStringList),
	stringListToNumberList(VideoSizeStringList, VideoSizeList),

	% read ENDPOINT DATA
	readEndpoints(FileInput, EndpointCount, EndpointList),

	% read REQUEST DATA
	readRequests(FileInput, RequestCount, RequestList),

	assert(parameters(VideoCount, EndpointCount, RequestCount, CacheCount, CacheCapacity)),
	assert(video_sizes(VideoSizeList)),
	assert(endpoints(EndpointList)),
	assert(requests(RequestList)),
	debugPrintReadFile,

	close(FileInput).

stringListToNumberList([], []):-!.
stringListToNumberList([H | T], [RH | RT]):-
	number_string(RH, H),
	stringListToNumberList(T, RT).

readEndpoints(FileInput, EndpointCount, EndpointList):-
	readEndpointsRec(FileInput, 0, EndpointCount, EndpointList).
readEndpointsRec(_, EndpointCount, EndpointCount, []):-!.
readEndpointsRec(FileInput, EndpointIndex, EndpointCount, [[EndpointLatency, EndpointCacheCount, EnpointCacheList] | EndpointList]):-
	read_string(FileInput, "\n", "\t", _, EndpointLine),
	split_string(EndpointLine, " ", "", [EndpointLatencyString, EndpointCacheCountString]),
	number_string(EndpointLatency, EndpointLatencyString),
	number_string(EndpointCacheCount, EndpointCacheCountString),

	readEndpointCaches(FileInput, EndpointCacheCount, EnpointCacheList),

	NextEndpointIndex is EndpointIndex + 1,
	readEndpointsRec(FileInput, NextEndpointIndex, EndpointCount, EndpointList).

readEndpointCaches(FileInput, EndpointCacheCount, EndpointCacheList):-
	readEndpointCachesRec(FileInput, 0, EndpointCacheCount, EndpointCacheList).
readEndpointCachesRec(_, EndpointCacheCount, EndpointCacheCount, []):-!.
readEndpointCachesRec(FileInput, EndpointCacheIndex, EndpointCacheCount, [[EndpointCacheID, EndpointCacheLatency] | EnpointCacheList]):-
	read_string(FileInput, "\n", "\t", _, EndpointCacheLine),
	split_string(EndpointCacheLine, " ", "", [EndpointCacheIDString, EndpointCacheLatencyString]),
	number_string(EndpointCacheID, EndpointCacheIDString),
	number_string(EndpointCacheLatency, EndpointCacheLatencyString),

	NextEndpointCacheIndex is EndpointCacheIndex + 1,
	readEndpointCachesRec(FileInput, NextEndpointCacheIndex, EndpointCacheCount, EnpointCacheList).

readRequests(FileInput, RequestCount, RequestList):-
	readRequestsRec(FileInput, 0, RequestCount, RequestList).
readRequestsRec(_, RequestCount, RequestCount, []):-!.
readRequestsRec(FileInput, RequestIndex, RequestCount, [[VideoID, EndpointID, Requests] | RequestList]):-
	read_string(FileInput, "\n", "\t", _, RequestLine),
	split_string(RequestLine, " ", "", [VideoIDString, EndpointIDString, RequestsString]),
	number_string(VideoID, VideoIDString),
	number_string(EndpointID, EndpointIDString),
	number_string(Requests, RequestsString),

	NextRequestIndex is RequestIndex + 1,
	readRequestsRec(FileInput, NextRequestIndex, RequestCount, RequestList).

% WRITING
writeOutputFile:-
	outputFile(OutputFile),
	open(OutputFile, write, FileOutput),
	getVideoList(VideoList),
	length(VideoList, Caches),
	writeln(FileOutput, Caches),
	writeVideoList(FileOutput, VideoList), !,
	close(FileOutput).

getVideoList([[CacheID | Videos] | VideoList]):-
	solution(CacheID, _),
	getVideosForCacheID(CacheID, Videos),
	getVideoList(VideoList).
getVideoList([]):-!.

getVideosForCacheID(CacheID, [Video | Videos]):-
	retract(solution(CacheID, Video)),
	getVideosForCacheID(CacheID, Videos).
getVideosForCacheID(_, []).
	
writeVideoList(_, []):-!.
writeVideoList(FileOutput, [Video | VideoList]):-
	writeList(FileOutput, Video),
	writeVideoList(FileOutput, VideoList).

writeList(FileOutput, []):-
	writeln(FileOutput, ""), !.
writeList(FileOutput, [H | T]):-
	write(FileOutput, H), write(FileOutput, " "), 
	writeList(FileOutput, T).

% test
debugPrintReadFile:-
	internal(0), !.
debugPrintReadFile:-
	internal(1), !,
	parameters(VideoCount, EndpointCount, RequestCount, CacheCount, CacheCapacity),
	video_sizes(VideoSizeList),
	endpoints(EndpointList),
	requests(RequestList),
	writeln("Parameters"),
	write(VideoCount), write(" "), 
	write(EndpointCount), write(" "),
	write(RequestCount), write(" "),
	write(CacheCount), write(" "),
	writeln(CacheCapacity),
	writeln("Video Sizes"),
	writeln(VideoSizeList),
	writeln("Endpoints"),
	writeln(EndpointList),
	writeln("Requests"),
	writeln(RequestList).

r:-readInputFile.
w:-
	assert(solution(0, 2)),
	assert(solution(1, 3)),
	assert(solution(1, 1)),
	assert(solution(2, 0)),
	assert(solution(2, 1)),
	writeOutputFile.
