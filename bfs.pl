% candidate_number(12345).

% Find hidden identity by repeatedly calling agent_ask_oracle(oscar,o(1),link,L)
% find_identity(-A)


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


find_identity_o(A):-
  findAllActor(Actors),
  find_actor_o(Actors, A, []).

find_actor_o(Actors, A, Visited) :-
  find_oracle(Visited, P, NewVisited), !,
  writeln(P),
  solve_task(go(P), C),
  A = P.

find_oracle(Visited, P, NewVisited) :-
  my_agent(Agent),
  query_world(agent_current_position, [Agent, Position]),
  bfs(find(o(_)), [[Position]], P, Visited, NewVisited).



% =====================================
% BFS search

bfs(go(Target), [[Target|Path]|_],  Result, Visited, NewVisited) :- 
  % print("reach"),
  Target = Result,
  NewVisited = Visited.

bfs(find(o(_)), Queue, Result, Visited, NewVisited) :-
  % print("depth"),
  Queue=[Path|Rest],
  Path = [Node|_],
  children(Node, Oracles, o(_)),
  children(Node, Children, empty),
  checkRepeated(Children, Queue, NonRepeated),
  forLoop(NonRepeated, Path, Rest, NewQueue),

  ( Oracles = []
  -> bfs(find(o(_)), NewQueue, Result, Visited, NewVisited);
    otherwise
  -> (oraclePosition(Oracles, Visited, NewVisited, Position)
    -> bfs(go(Node), Queue, Result, NewVisited, NextVisited);
      otherwise
    -> bfs(find(o(_)), NewQueue, Result, Visited, NewVisited))
      
  ).

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


children(Node, Children, C):-
  setof(A, map_adjacent(Node, A, C), Children).
children(_, Children, _):-
  [] = Children.
