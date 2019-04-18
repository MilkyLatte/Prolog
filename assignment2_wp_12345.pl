% candidate_number(12345).

% Find hidden identity by repeatedly calling agent_ask_oracle(oscar,o(1),link,L)
% find_identity(-A)


find_identity(A):-
(part_module(2)   -> find_identity_2(A)
; otherwise -> find_identity_o(A)
).

find_identity_2(A):-
  findAllActor(Actors),
  find_actor(Actors, A).

find_identity_o(A):-
  A='Not yet implemented'.

find_actor([Actor|[]], A):-
  !,
  A = Actor.

find_actor(Actors, A):-
  agent_ask_oracle(oscar,o(1), link, Answer),
  valid_actor(Answer, Actors, Selected),
  find_actor(Selected, A).

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