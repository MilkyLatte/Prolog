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

search(F,N,O) :-
  map_adjacent(F,N,O).

% -----------------------------------
% temp moving

children(Node, Children):-
  setof((A, B) , search(Node, A, B), Children).
children(_, Children) :-
    []=Children.



checkRepeated(Children, [], NonRepeated) :-
    Children=NonRepeated.
checkRepeated([], _, NonRepeated) :-
    []=NonRepeated.
checkRepeated(Children, Agenda, NonRepeated) :-
    Agenda=[(Path, _, _)|Rest],
    exclude([P]>>memberchk(P, Path), Children, Result),
    checkRepeated(Result, Rest, NonRepeated).

test(Result) :-
    children(p(1, 1), Children),
    checkRepeated(Children, [([(p(1, 1), empty)],_,_)], Result).
 

navigate(From, Target, Best) :-
  % function that gets the current fuel of the agent
  % function that gets current position of the agent
    estrella(Target, [([(From, empty)],8,_)], Best).


estrella(Target, [([(Target, Type)|Path], Fuel, Score)|Rest], BestPath):-
([(Target, Type)|Path], Fuel, Score) = BestPath.

estrella(Target, Agenda, BestPath) :-
  Agenda = [Path|Paths],
  Path = ([(Current, _)|Rest], Fuel, Score),
  print(Fuel),
  print(Current),
  children(Current, Children),

  checkRepeated(Children, Agenda, Result),

  processPath(Result, Path, Target, NewPath),

  addChildren(Result, NewPath, Paths, NewAgenda),

  estrella(Target, NewAgenda, BestPath).


addChildren([], _, Agenda, Result):-
  Agenda = Result.
addChildren(Children, CurrentPath, Agenda, Result) :-
    Children=[Kid|Kids],
    Kid=(Node, Type),
    CurrentPath=(Path, Fuel, Score),
    (Fuel > 5 -> 
      (   Type=empty
      ->  append([Kid], Path, NewPath),
          NewFuel is Fuel -1,
          append(Agenda,[(NewPath, NewFuel, Score)],NewAgenda),
          addChildren(Kids, CurrentPath, NewAgenda, Result)
      ;   otherwise
      ->  addChildren(Kids, CurrentPath, Agenda, Result)
      )
      ; otherwise -> Agenda = [_|Many], Many = Result
      ).

heuristic(Path, Target, Result) :-
    Path=([First|Others], Fuel, _),
    First=(Node, _),
    map_distance(Node, Target, Distance),
    (   Fuel=0
    ->  H is 90+10*Distance
    ;   otherwise
    ->  H is 90*1/Fuel+10*Distance
    ),
    length([First|Others], L),
    G is L,
    Result is G+H.


processPath([], CurrentPath, Target, Result):-
  CurrentPath = (Path, Fuel, _),
  heuristic(CurrentPath, Target, NewScore),
  (Path, Fuel, NewScore) = Result.

processPath(Children, CurrentPath, Target, Result):-
  Children = [Kid|Kids],
  Kid = (_, Type),
  CurrentPath = (Path, _, Score),
  (Type = c(_) -> NewFuel is 100,
  (Path, NewFuel, Score, Type) = NewPath,
  processPath(Kids, NewPath, Target, Result)
  ; otherwise -> processPath(Kids, CurrentPath, Target, Result)).







%   bfs(go(Target), [[Target|Path]|_], Result) :-
%     print("reach"),
%     reverse(Result, [Target|Path]).

% bfs(Task, Queue, Result) :-
%     print("depth"),
%     Queue=[Path|Rest],
%     Path=[Node|_],
%     children(Node, Children),
%     checkRepeated(Children, Queue, NonRepeated),
%     forLoop(NonRepeated, Path, Rest, NewQueue),
%     bfs(Task, NewQueue, Result).

% checkRepeated(Children, [], NonRepeated) :-
%     Children=NonRepeated.
% checkRepeated([], _, NonRepeated) :-
%     []=NonRepeated.
% checkRepeated(Children, Queue, NonRepeated) :-
%     Queue=[Path|Rest],
%     exclude([P]>>memberchk(P, Path), Children, Result),
%     checkRepeated(Result, Rest, NonRepeated).



% forLoop([], _, Queue, Result) :-
%     Queue=Result.
% forLoop(Kids, CurrentPath, Queue, Result) :-
%     Kids=[Child|Children],
%     append([Child], CurrentPath, NewPath),
%     append(Queue, [NewPath], NewQueue),
%     forLoop(Children, CurrentPath, NewQueue, Result).


% children(Node, Children) :-
%     setof(A, search(Node, A, A, 1), Children).
% children(_, Children) :-
%     []=Children.
