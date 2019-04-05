candidate_number(39473).

% solve_task(Task,Cost):-
%   my_agent(Agent),
%   query_world( agent_current_position, [Agent,P] ),
%   solve_task_bt(Task,[c(0,P),P],0,R,Cost,_NewPos),!,  % prune choice point for efficiency
%   reverse(R,[_Init|Path]),
%   query_world( agent_do_moves, [Agent,Path] ).

%%%%%%%%%% Useful predicates %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% backtracking depth-first search, needs to be changed to agenda-based A*
solve_task_bt(Task,Current,Depth,RPath,[cost(Cost),depth(Depth)],NewPos) :-
  achieved(Task,Current,RPath,Cost,NewPos).
solve_task_bt(Task,Current,D,RR,Cost,NewPos) :-
  Current = [c(F,P)|RPath],
  search(P,P1,R,C),
  \+ memberchk(R,RPath),  % check we have not been here already
  D1 is D+1,
  F1 is F+C,
  solve_task_bt(Task,[c(F1,P1),R|RPath],D1,RR,Cost,NewPos).  % backtrack search

achieved(go(Exit),Current,RPath,Cost,NewPos) :-
  Current = [c(Cost,NewPos)|RPath],
  ( Exit=none -> true
  ; otherwise -> RPath = [Exit|_]
  ).
achieved(find(O),Current,RPath,Cost,NewPos) :-
  Current = [c(Cost,NewPos)|RPath],
  ( O=none    -> true
  ; otherwise -> RPath = [Last|_],map_adjacent(Last,_,O)
  ).

search(F,N,N,1) :-
  map_adjacent(F,N,empty).

% -----------------------------------
% temp moving

solve_task(Task,Cost):-
  my_agent(Agent),
  query_world( agent_current_position, [Agent,P] ),
  bfs(Task, [[P]], [P|Path]),!,
  print(Path),
  query_world( agent_do_moves, [Agent,Path] ).

bfs(go(Target), [[Target|Path]|_],  Result) :- 
  print("reach"),
  reverse(Result, [Target|Path]).

bfs(Task, Queue, Result) :-
  print("depth"),
  Queue=[Path|Rest],
  Path = [Node|_],
  children(Node, Children),
  checkRepeated(Children, Queue, NonRepeated),
  forLoop(NonRepeated, Path, Rest, NewQueue),
  bfs(Task, NewQueue, Result).

checkRepeated(Children, [], NonRepeated):- Children = NonRepeated.
checkRepeated([], _,NonRepeated):- [] = NonRepeated.
checkRepeated(Children, Queue, NonRepeated):-
  Queue = [Path|Rest],
  exclude([P]>>memberchk(P, Path), Children, Result),
  checkRepeated(Result, Rest, NonRepeated).



forLoop([], _, Queue, Result):-
  Queue = Result.
forLoop(Kids, CurrentPath, Queue, Result) :-
    Kids=[Child|Children],
    append([Child], CurrentPath, NewPath),
    append(Queue, [NewPath], NewQueue),
    forLoop(Children, CurrentPath, NewQueue, Result).


children(Node, Children):-
  setof(A, search(Node, A, A, 1), Children).
children(_, Children):-
  [] = Children.

