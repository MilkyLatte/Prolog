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
solve_task(Task, Cost) :-
    Task=go(Target),
    query_world(check_pos, [Target, Type]),
    map_adjacent(Target, _, T),
    (   Type=empty
    ->  T=empty
    ->  my_agent(Agent),
        query_world(agent_current_position, [Agent, P]),
        query_world(agent_current_energy, [Agent, E]),
        ([(P, empty)], E, Cost)=Initial,
        heuristic(Initial, Target, Result),
        estrella(Target, [Initial], Result, Best, Flag),
        writeln("ESTRELLA WORKS"),
        writeln(Flag),
        (   Flag=1
        ->  writeln("HERE0"),
            writeln(Best),
            Best=([(Node, _)|Many], Energy, Score),
            writeln("HERE"),
            ([(Node, empty)], E, Score)=Temp,
            writeln("HERE1"),
            heuristic(Temp, Target, R),
            writeln("HERE2"),
            estrella(Target, [Temp], R, Continuation, _),
            writeln("HERE3"),
            Continuation=(Road, _, _),
            writeln("HERE4"),
            append(Road, Many, Final),
            writeln("HERe5"),
            reverse(Final, [_Init|Path]),
            writeln("HERE6"),
            moveNTopup(Path, Agent)
        ;   otherwise
        ->  print("HERE"),
            Best=(TupledPath, _, _),
            reverse(TupledPath, [_Init|Path]),
            moveNTopup(Path, Agent)
        )
    ).



heuristic(Path, Target, Result) :-
    Path=([First|Others], Fuel, _),
    First=(Node, _),
    % print("HEAD AT:"),
    % writeln(Node),
    map_distance(Node, Target, Distance),
    (   Fuel = 0
    ->  H is Distance
    % ;   Fuel < 30
    % ->  H is e ** 1/(Fuel * 0.01) + Distance
    ;   otherwise
    ->  H is 50 * (1/(Fuel)) + Distance
    ),
    length([First|Others], L),
    G is L,
    Result is G + H.

moveNTopup([], _):- print("here"), !.
moveNTopup(Path, Agent):-
  Path = [(Node, _)|Rest], 
  query_world( agent_do_moves, [Agent,[Node]]),
  ( map_adjacent(Node, _, c(C)) -> 
      query_world(agent_topup_energy, [Agent, c(C)]), 
      moveNTopup(Rest, Agent);
    otherwise -> 
      moveNTopup(Rest, Agent)).

children([], []).
children(Node, Children):-
  setof((A, B) , search(Node, A, B), Children).

checkRepeated(Children, Current, NonRepeated) :-
    Current = (Path, _, _),
    exclude([P]>>memberchk(P, Path), Children, NonRepeated).

 
getNElements(0, List, Temp, Result):-
  Temp = Result.
getNElements(Start, List, Temp,Result):-
  Next is Start-1,
  List = [One|Many],
  append(Temp, [One], New),
  getNElements(Next, Many, New, Result).

sampleNElements(0, _, Temp, Result):-
  Temp = Result, !.
sampleNElements(Counter, List, Temp, Result):-
  length(List, Length),
  random(0, Length, Index),
  nth0(Index, List, Elt),
  append([Elt], Temp, NewTemp),
  delete(List, Elt, NewList),
  NewCounter is Counter - 1,
  sampleNElements(NewCounter, NewList, NewTemp, Result).


estrella(Target, [([(Target, Type)|Path], Fuel, Score)|Rest], InitialScore, BestPath, Flag):-
  (Fuel > 20 ->  ([(Target, Type)|Path], Fuel, Score) = BestPath, 0 = Flag,!
  ; otherwise ->  estrella(Target, Rest, InitialScore, BestPath)).

 

estrella(Target, Agenda, InitialScore,BestPath, Flag) :-
  % writeln("============"),
  length(Agenda, Length),
  (Length > 1000 -> sampleNElements(500, Agenda, [], TheAgenda)
  ; otherwise -> Agenda = TheAgenda),
  TheAgenda = [Path|Paths],
  Path = ([(Current, _)|Rest], Fuel, Score),
  children(Current, Children),
  checkRepeated(Children, Path, Result),
  processPath(Result, Path, Target, NewPath, 0, F),
  (F = 1 -> writeln("INSIDE"), NewPath = BestPath,  1 = Flag, !
    ; otherwise -> 
      addChildren(Result, NewPath, Paths, InitialScore, NewAgenda),
      estrella(Target, NewAgenda, InitialScore, BestPath, Flag)).

addChildren([], _, Agenda, InitialScore, Result):-
  Agenda = Result.
addChildren(Children, CurrentPath, Agenda, InitialScore, Result) :-
    Children=[(Node, Type)|Kids],
    CurrentPath=(Path, Fuel, Score),
    New is Score - InitialScore,
    % print("SCORE:"),
    % writeln(New),
    (New < 20 -> 
      (   Type=empty
      ->  append([(Node, Type)], Path, NewPath),
          NewFuel is Fuel -1,
          append(Agenda,[(NewPath, NewFuel, Score)],NewAgenda),
          addChildren(Kids, CurrentPath, NewAgenda, InitialScore, Result)
      ;   otherwise
      ->  addChildren(Kids, CurrentPath, Agenda, InitialScore, Result)
      )
      ; otherwise ->  Agenda = Result
      ).



processPath([], CurrentPath, Target, Result, Temp, Flag):-
  CurrentPath = (Path, Fuel, _),
  heuristic(CurrentPath, Target, NewScore),
  (Path, Fuel, NewScore) = Result,
  Temp = Flag.

processPath(Children, CurrentPath, Target, Result, Temp, Flag):-
  Children = [(_, Type)|Kids],
  CurrentPath = (Path, Fuel, Score),
  (Type = c(_), Fuel < 50 -> NewFuel is 100,
  (Path, NewFuel, Score) = NewPath,
  processPath(Kids, NewPath, Target, Result, 1, Flag )
  ; otherwise -> processPath(Kids, CurrentPath, Target, Result, Temp,Flag)).


%   bfs(go(Target), [[Target|Path]|_], Result) :-
%     print("reach"),
%     reverse(Result, [Target|Path]).

convertPath([], [], []).
convertPath([], Path, Result):- 
  Path = Result,!.
convertPath(TupledPath, Path, Result):-
  TupledPath = [(Pos, _)|Rest],
  append([Pos], Path, NewPath),
  convertPath(Rest, NewPath, Result).


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