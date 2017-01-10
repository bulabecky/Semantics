/*Autumn 2016 - not working for third test */
% in order to transpose we need a min of 2 lists in our list of lists%
% first check if we have 2 empty lists%
%2 full lists -> read head of each, then new lists are the tails of previous lists. moving right reading the head of each tail iteration.%
% more than 2 lists -> 1st empty -> return false%
% more than 2 full lists -> same idea as 2 lists moving right%

tr([], []). % checks for 2 empty lists
tr([H1|T1], T3) :- tr(H1, [H1|T1], T3). % if two full lists, iterates through list 
										% taking the head 
										% of each then moving right
										% to the head of the tail
tr([], _, []). % 3 lists, 1st, return no
tr([_|T2], M, [T3|T4]) :- lists_firsts_rests(M, T3, Ms),tr(T2, Ms, T4).

lists_firsts_rests([], [], []).
lists_firsts_rests([[H1|Os]|Rest], [H1|T1], [Os|Oss]) :-lists_firsts_rests(Rest, T1, Oss).



/* January 2016 */
noah([],[],[]).
noah([H1|T1],[H2|T2],[H1,H2|T3]) :-
	noah(T1,T2,T3).
	
/* Autumn 2015 */
noah([],[],[]).
noah([H1|T1],[H2|T2],[H1,H2|T3]) :-
	noah(T1,T2,T3).
	
/*January 2015 */
doubleMember(X,Xs) :-
	member(X,Xs),
	del(X,Xs,N),
	member(X,N).
del(X,[X|Tail],Tail).
del(X,[Y|Tail],[Y|Tail1]) :-
	del(X,Tail,Tail1).