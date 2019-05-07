% candidate_number(12345).

% Find hidden identity by repeatedly calling agent_ask_oracle(oscar,o(1),link,L)
% find_identity(-A)

% ===========================
% Diversion to part 2 and rest

find_identity(A):-
(part_module(2)   -> find_identity_2(A)
; otherwise -> find_identity_o(A)
).

% ===========================
% Part 2

find_identity_2(A):-
  findAllActor(Actors),
  find_actor(Actors, A).

find_actor([Actor|[]], A):-
  !,
  A = Actor.

find_actor(Actors, A):-
  agent_ask_oracle(oscar,o(1), link, Answer),
  valid_actor(Answer, Actors, Selected),
  find_actor(Selected, A).

% ===========================
% For Both Part 2 and rest

valid_actor(Answer, [], Next):-
  Next = [].

valid_actor(Answer, Actors, Selected):-
  Actors = [Actor|Rest],
  (  wp(Actor, WT), wt_link(WT, Answer)
  -> Selected = [Actor|Next]
  ;  otherwise -> Selected = Next),
  valid_actor(Answer, Rest, Next).
  

findAllActor(Actors):-
  findall(A, actor(A), Actors).

% ============================
% Rest

find_identity_o(A):-
  findAllActor(Actors),
  find_actor_o(Actors, A, []).

find_actor_o([Actor|[]], A, _) :-
  !,
  A = Actor.

or(A, B):- A;B.

last(X,[X]).
last(X,[_|Z]) :- last(X,Z).

find_actor_o(Actors, A, Visited) :-
  my_agent(Agent),
  query_world(agent_current_energy, [Agent, E]),
  ( E < 36
  -> writeln("Low fuel"),
     find_station(Agent, Station),
     [FP|Rest] = Station,
     [TG|RRest] = Rest,
  %    map_adjacent(FP, _, c(C)),
  %    reverse(Rev, Station),
  %    [First|Path] = Rev,
  %    query_world(agent_do_moves, [Agent, Path]),
  %    query_world(agent_topup_energy, [Agent, c(C)]);
    solve_task(go(TG), _);
    otherwise
  -> true), 
  !,
  find_oracle(Agent, Visited, P, ID, NewVisited), !,
  writeln(P),
  % writeln(ID),
  query_world(agent_current_position, [Agent, Current]),
  p(X,Y) = P,
  p(V,W) = Current,
  writeln(E),
  (or((X \= V), (Y \= W)) -> solve_task(go(P), _); otherwise -> true), !,
  writeln("Here"),
  query_world(agent_ask_oracle, [Agent, o(ID), link, Answer]),
  valid_actor(Answer, Actors, Selected),
  find_actor_o(Selected, A, NewVisited).


find_oracle(Agent, Visited, P, ID, NewVisited) :-
  query_world(agent_current_position, [Agent, Position]),
  length(Visited, Length),
  Number is 10 - Length,
  bfs(find(o(_)), [[Position]], P, ID, Visited, NewVisited, Number).

find_station(Agent, PD) :-
  query_world(agent_current_position, [Agent, Position]),
  bfs(find(c(_)), [[Position]], PD).


% =====================================
% BFS search

bfs(go(Target), [[Target|Path]|_],  Result, ID, Visited, NewVisited, Number) :- 
  % print("reach"),
  Target = Result,
  [ID|_] = Visited,
  NewVisited = Visited.

bfs(go(Target), [[Target|Path]|_], Result) :-
  [Target|Path] = Result.

bfs(find(o(_)), _, _, _, _, _, 0) :- !, writeln("No more oracle"), !, fail.

bfs(find(o(_)), Queue, Result, ID, Visited, NewVisited, Number) :-
  % print("depth"),
  Queue = [Path|Rest],
  Path = [Node|_],
  children_bfs(Node, Oracles, o(_)),
  children_bfs(Node, Children, empty),
  checkRepeated_bfs(Children, Queue, NonRepeated),
  forLoop_bfs(NonRepeated, Path, Rest, NewQueue), 
  NewNumber = Number - 1,
  ( Oracles = []
  -> bfs(find(o(_)), NewQueue, Result, ID, Visited, NewVisited, Number);
    otherwise
  -> (oraclePosition(Oracles, Visited, NewVisited, Position)
    -> bfs(go(Node), Queue, Result, ID, NewVisited, NextVisited, Number);
      otherwise
    -> bfs(find(o(_)), NewQueue, Result, ID, Visited, NewVisited, NewNumber))
      
  ).

bfs(find(c(_)), Queue, Result) :-
  Queue = [Path|Rest],
  Path = [Node|_],
  children_bfs(Node, Station, c(_)),
  children_bfs(Node, Children, empty),
  checkRepeated_bfs(Children, Queue, NonRepeated),
  forLoop_bfs(NonRepeated, Path, Rest, NewQueue), 

  ( Station = []
  -> bfs(find(c(_)), NewQueue, Result);
    otherwise
  -> bfs(go(Node), Queue, Result)).

oraclePosition([], _, _, _) :- fail.
oraclePosition(p(X,Y), Visited, NewVisited, Position) :- Position = p(X,Y), NewVisited = Visited.

oraclePosition(Oracles, Visited, NewVisited, Position) :-
  Oracles = [One|OfMany],
  query_world(check_pos, [One, Oid]),
  o(ID) = Oid,
  ( memberchk(ID, Visited)
  -> oraclePosition(OfMany, Visited, NewVisited, Position);
    otherwise
  -> p(X,Y) = One,
     append([ID], Visited, NewVisited),
     oraclePosition(p(X,Y), NewVisited, NextVisited, Position)
  ).

checkRepeated_bfs(Children, [], NonRepeated):- Children = NonRepeated.
checkRepeated_bfs([], _,NonRepeated):- [] = NonRepeated.

checkRepeated_bfs(Children, Queue, NonRepeated):-
  Queue = [Path|Rest],
  exclude([P]>>memberchk(P, Path), Children, Result),
  (Result = [] -> !, fail; otherwise -> true),
  checkRepeated_bfs(Result, Rest, NonRepeated).



forLoop_bfs([], _, Queue, Result):-
  Queue = Result.
forLoop_bfs(Kids, CurrentPath, Queue, Result) :-
    Kids=[Child|Children],
    append([Child], CurrentPath, NewPath),
    append(Queue, [NewPath], NewQueue),
    forLoop_bfs(Children, CurrentPath, NewQueue, Result).


children_bfs(Node, Children, C):-
  setof(A, map_adjacent(Node, A, C), Children).
children_bfs(_, Children, _):-
  [] = Children.
