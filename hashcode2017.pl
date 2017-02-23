
betw(I,J,I) :- I =< J.
betw(I,J,K) :- I < J, I1 is I+1, betw(I1,J,K).

:-dynamic(parameters/5).
:-dynamic(video_sizes/1).
:-dynamic(endpoints/1).
:-dynamic(requests/1).
:-dynamic(solution/2).
:-dynamic(currentAvailableSpace/1).

youtube:-
	retract(parameters(VideoCount, EndpointCount, RequestDescrCount, CacheCount, CacheCapacityEach)),!,
	retract(video_sizes(VideoSizeList)),
	retract(endpoints(EndpointList)),
	retract(requests(RequestList)),
	EndpointCountM is EndpointCount - 1, !,
	betw(0, EndpointCountM, EndPointCurrent),
	member(Request, RequestList),
	Request = [IdVideo, EndPointCurrent, NrOfReq],
	getVideoSize(IdVideo, VideoSizeList, SizeVideo),
	getVideoSize(EndPointCurrent, EndpointList, DataCurrentEndp),
	getBestCache(SizeVideo, VideoSizeList, DataCurrentEndp, CacheCapacityEach, BestCache),
	assert(solution(BestCache, IdVideo)),
	fail.
youtube.

getVideoSize(IdVideo, [_|T], Size):-
	IdVideo > 0, !,
	IdNew is IDVideo - 1,
	getVideoSize(IdNew, T, Size).
getVideoSize(0, [H|_], H):-!.

getBestCache(SizeVideo, VideoSizeList, [DatacenterLatency, CacheCountConnections|T], CacheCapacityEach, CacheConnectedId):-
	member([CacheConnectedId, Latency], T),
	getAvailableSpace(CacheConnectedId, CacheCapacityEach, AvailableSpace, VideoSizeList),
	SizeVideo =< AvailableSpace.

getAvailableSpace(CacheConnectedId, CacheCapacityEach, AvailableSpace, VideoSizeList):-
	retract(solution(CacheConnectedId, IdVideo)),
	getVideoSize(IdVideo, VideoSizeList, SizeVideo),
	assert(currentAvailableSpace(SizeVideo)),
	fail.
getAvailableSpace(CacheConnectedId, CacheCapacityEach, AvailableSpace, VideoSizeList):- collectSpace(CacheConnectedId, VideoSizeList, 0, SpaceUsed), AvailableSpace is CacheCapacityEach - SpaceUsed.

collectSpace(CacheConnectedId, VideoSizeList, PartialSpace, SpaceUsed):-
	retract(solution(CacheConnectedId, IdVideo)),
	getVideoSize(IdVideo, VideoSizeList, Size),
	NewSize is PartialSpace + Size,!,
	collectSpace(CacheConnectedId, VideoSizeList, NewSize, SpaceUsed).
collectSpace(_, _, PartialSpace, PartialSpace).

