candidate_number(39572).

%%%%%%%%%% Useful predicates %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% backtracking depth-first search, needs to be changed to agenda-based A*
% solve_task_bt(Task,Current,Depth,RPath,[cost(Cost),depth(Depth)],NewPos) :-
%   achieved(Task,Current,RPath,Cost,NewPos).
% solve_task_bt(Task,Current,D,RR,Cost,NewPos) :-
%   Current = [c(F,P)|RPath],
%   search(P,P1,R,C),
%   \+ memberchk(R,RPath),  % check we have not been here already
%   D1 is D+1,
%   F1 is F+C,
%   solve_task_bt(Task,[c(F1,P1),R|RPath],D1,RR,Cost,NewPos).  % backtrack search

% achieved(go(Exit),Current,RPath,Cost,NewPos) :-
%   Current = [c(Cost,NewPos)|RPath],
%   ( Exit=none -> true
%   ; otherwise -> RPath = [Exit|_]
%   ).
% achieved(find(O),Current,RPath,Cost,NewPos) :-
%   Current = [c(Cost,NewPos)|RPath],
%   ( O=none    -> true
%   ; otherwise -> RPath = [Last|_],map_adjacent(Last,_,O)
%   ).

search(F,N,O) :-
  map_adjacent(F,N,O).

solve_task(Task, Cost) :-
    Task=go(Target),
    query_world(check_pos, [Target, Type]),
    my_agent(Agent),
    query_world(agent_current_position, [Agent, P]),
    query_world(agent_current_energy, [Agent, E]),
    map_adjacent(Target, _, T),
    (   Type=empty -> T=empty
    ->  
        ([(P, empty)], E, Cost) = Initial,
        heuristic(Initial, Target, Result), % find the initial heuristic from P to Target
        estrella(Target, [Initial], Result, Best, Flag),
        (   Flag=1
        ->  %with charging
            Best=([(Node, _)|Many], Energy, Score),
            ([(Node, empty)], 100, Score) = Temp, %energy recharged
            heuristic(Temp, Target, R), %find the heuristic from the charging station
            estrella(Target, [Temp], R, Continuation, _), %Continuation is the new Tupledpath
            Continuation = (Road, _, _),
            append(Road, Many, Final), % appending the new path to the previous path
            reverse(Final, [_Init|Path]),
            moveNTopup(Path, Agent, Target)
        ;   otherwise
        ->  %without charging, the TupledPath is the path
            Best=(TupledPath, _, _), 
            reverse(TupledPath, [_Init|Path]),
            moveNTopup(Path, Agent, Target)
        )
    ; map_adjacent(P, Target, _)
    -> ([(P, empty)], E, Cost) = Initial,
        heuristic(Initial, Target, Result), % find the initial heuristic from P to Target
        estrella(Target, [Initial], Result, Best, Flag)
    ).

heuristic(Path, Target, Result) :-
    Path=([First|Others], Fuel, _),
    First=(Node, _),
    map_distance(Node, Target, Distance),
    (   Fuel = 0 %to prevent divide by zero
    ->  H is Distance
    ;   otherwise
    ->  H is 50 * (1/(Fuel)) + Distance
    ),
    length([First|Others], L),
    G is L,
    Result is G + H.

moveNTopup([], _, _):- print("here"), !.
moveNTopup(Path, Agent, Target):-
  Path = [(Node, _)|Rest], 
  query_world(check_pos, [Node, Type]),
  %if the next position it is moving to is blocked ? find new path : move
  (Type = empty ->
    query_world( agent_do_moves, [Agent,[Node]]),
    writeln(Node),
    ( map_adjacent(Node, _, c(C)) -> 
        query_world(agent_topup_energy, [Agent, c(C)]), 
        moveNTopup(Rest, Agent, Target);
      otherwise -> 
        moveNTopup(Rest, Agent, Target)
    );
  otherwise -> writeln("Recompute"), 
  query_world(check_pos, [Target, Type]),
    (Type = empty -> 
      query_world( agent_current_position, [Agent, P]),
      query_world( agent_current_energy, [Agent, E]),
      ([(Node, empty)], E, _) = Temp, %energy recharged
      heuristic(Temp, Target, R), %find the heuristic from the charging station
      writeln(E),
      writeln(R),
      estrella(Target, [Temp], R, Continuation, _), %Continuation is the new Tupledpath
      Continuation = (Road, _, _),
      reverse(Road, [_Init|Path]),
      moveNTopup(Path, Agent, Target)
    ); 
    otherwise -> print("invalid"), fail
  ).

%children get both the node and its node type
children([], []).
children(Node, Children):-
  setof((A, B) , search(Node, A, B), Children).

checkRepeated(Children, Current, NonRepeated) :-
    Current = (Path, _, _),
    exclude([P]>>memberchk(P, Path), Children, NonRepeated).

%random taking a number of path from existing tree
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
  %if the path still gives agent a fuel above 20, then it is the final path,
  (Fuel > 20 ->  ([(Target, Type)|Path], Fuel, Score) = BestPath, 0 = Flag,!
  % else get rid of the path and keep searching using the Rest branch of the tree
  ; otherwise ->  estrella(Target, Rest, InitialScore, BestPath, Flag)).

estrella(Target, Agenda, InitialScore, BestPath, Flag) :-
  length(Agenda, Length),
  query_world(check_pos, [Target, Type]),
  (Type = empty ->
    (Length > 1000 -> sampleNElements(500, Agenda, [], TheAgenda)
    ; otherwise -> Agenda = TheAgenda),
    TheAgenda = [Path|Paths],
    Path = ([(Current, _)|Rest], Fuel, Score),
    children(Current, Children),
    checkRepeated(Children, Path, Result),
    processPath(Result, Path, Target, NewPath, 0, F),
    %return the path back to solve_task, so we can find a continued path towards the target
    (F = 1 -> writeln("INSIDE"), NewPath = BestPath, 1 = Flag, !;
    otherwise -> 
      addChildren(Result, NewPath, Paths, InitialScore, NewAgenda),
      estrella(Target, NewAgenda, InitialScore, BestPath, Flag)
    )
  ; otherwise -> fail
  ).



addChildren([], _, Agenda, InitialScore, Result):-
  Agenda = Result.

%only adding the empty child to the path when the child doesn't give a score that is too big from initialScore
addChildren(Children, CurrentPath, Agenda, InitialScore, Result) :-
    Children=[(Node, Type)|Kids],
    CurrentPath=(Path, Fuel, Score),
    New is Score - InitialScore,
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

%set the flag to 1 when we charge
processPath([], CurrentPath, Target, Result, Temp, Flag):-
  CurrentPath = (Path, Fuel, _),
  heuristic(CurrentPath, Target, NewScore),
  (Path, Fuel, NewScore) = Result,
  Temp = Flag.

%the function is dealing with charging only
%if the the child has type c and agent has less than fuel -> we charge; 
processPath(Children, CurrentPath, Target, Result, Temp, Flag):-
  Children = [(_, Type)|Kids],
  CurrentPath = (Path, Fuel, Score),
  (Type = c(_), Fuel < 50 -> NewFuel is 100,
  (Path, NewFuel, Score) = NewPath,
  processPath(Kids, NewPath, Target, Result, 1, Flag )
  ; otherwise -> processPath(Kids, CurrentPath, Target, Result, Temp, Flag)).

getNElements(0, List, Temp, Result):-
  Temp = Result.
getNElements(Start, List, Temp,Result):-
  Next is Start-1,
  List = [One|Many],
  append(Temp, [One], New),
  getNElements(Next, Many, New, Result).