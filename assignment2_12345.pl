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

% bfs(go(Target), [[Target|Path]|_],  Result) :- 
%   print("reach"),
%   reverse(Result, [Target|Path]).

% bfs(Task, Queue, Result) :-
%   print("depth"),
%   Queue=[Path|Rest],
%   Path = [Node|_],
%   children(Node, Children),
%   checkRepeated(Children, Queue, NonRepeated),
%   forLoop(NonRepeated, Path, Rest, NewQueue),
%   bfs(Task, NewQueue, Result).

checkRepeated(Children, [], NonRepeated):- Children = NonRepeated.
checkRepeated([], _,NonRepeated):- [] = NonRepeated.
checkRepeated(Children, Queue, NonRepeated):-
  Queue = [(Path, Score)|Rest],
  exclude([P]>>memberchk(P, Path), Children, Result),
  checkRepeated(Result, Rest, NonRepeated).





children(Node, Children):-
  setof(A, search(Node, A, A, 1), Children).
children(_, Children):-
  [] = Children.



heuristic(Path, Target, Result) :-
  Path=([First|String], Score),
  map_distance(First, Target, Distance),
  length([First|String], L),
  Result is L+Distance-1. 


forLoop([], _, Queue, Result):-
  Queue = Result.
forLoop(Kids, CurrentPath, Queue, Result) :-
    Kids=[Child|Children],
    CurrentPath = (C, S),
    append([Child], C, NewPath), 
    append(Queue, [(NewPath, S)], NewQueue),
    forLoop(Children, CurrentPath, NewQueue, Result).

% a_star(Target, [([Target|Path],Score)|_], Result) :-
%     reverse(R, [Target|Path]),
%     (R, Score) = Result.
% a_star(Target, Queue, Result) :-
%     print(Queue),
%     Queue=[Path|Rest],   
%     heuristic(Path, Target, Score),
%     Path=([Node|Nodes], S),
%     ([Node|Nodes], Score) = NewPath,    
%     children(Node, Children),
%     checkRepeated(Children, Rest, NonRepeated),
%     forLoop(NonRepeated, NewPath, Rest, NewQueue), %Path ([PATH], score)
%     a_star(Target, NewQueue, Result).

test(List, Add, Result):-
  (List, Add) = Result.


a_star(Target, [([Target|Path],Score)|Rest], BestPath, Result):-
  reverse(R, [Target|Path]),
  BestPath = (_, BScore),
  (Score < BScore -> ([Target|Path], Score) = BestPath),
  print("HERE").

    % BestPath = (BP, Update),
    % (Update < 10 -> BP = Result
    % ; otherwise -> a_star(Target, Rest, BestPath, Result)).

a_star(Target, Queue, BestPath, Result) :-
    Queue=[Path|Rest],   
    heuristic(Path, Target, Score),
    Path=([Node|Nodes], S),
    ([Node|Nodes], Score) = NewPath,    
    children(Node, Children),
    checkRepeated(Children, Rest, NonRepeated),
    forLoop(NonRepeated, NewPath, Rest, NewQueue), 
    a_star(Target, NewQueue, BestPath, Result).