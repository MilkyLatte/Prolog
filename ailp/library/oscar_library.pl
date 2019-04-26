/*
 *  oscar_library.pl
 *  Only use predicates exported in module heading in your code!
 */

:- module(oscar_library,
  [ %%% map predicates %%% re-exported from game_predicates %%%
    map_adjacent/3,           % ?-map_adjacent(p(1,5),A,O).
    map_distance/3,           % ?-map_distance(p(1,5),p(2,3),D).
    %%% oscar predicates %%%
    say/2,
    %%% assignment part %%%
    api_/1,
    part_module/1,
    %%% moved from oscar.pl file %%%
    shell/0,                  % interactive shell for the grid world
    %%% re-exported from command_channel.pl %%%
    start/0,
    stop/0,
    %%% querying the world %%%
    query_world/2,
    possible_query/2,
    my_agent/1,
    leave_game/0,
    join_game/1,
    start_game/0,
    reset_game/0
  ]
).
randomMove():-
  random(0, 20, X),
  random(0, 20, Y),
  go(p(X, Y)),
  randomMove().

:- use_module(library(http/http_client), [http_post/4]).
:- use_module(game_predicates, [map_adjacent/3,map_distance/3]).
:- use_module('../command_channel.pl').
:- set_homepage('oscar.html').

:- dynamic
   api_/1,
   part_module/1,
   ailp_internal/1.

% Define part of the assignment
api_(4).
part_module(1).

/*
 *  API 4
 * Contains predicates which allow the communication between the agent/client and the server through http post requests.
 * All predicates allowed to be sent to the server are indicated by possible_query/2.
 *
 * Only use predicates exported in module heading in your code!
 */
referee_queries_path('http://127.0.0.1:8000/agent/queries').
%referee_queries_path('http://137.222.102.101:8000/agent/queries').

query_world(Pred, Args):-
  possible_query(Pred,Args),
  referee_queries_path(Path),
  term_to_atom(Args, NewArgs),
  http_post(Path,
    form_data([ pred = Pred,
          args = NewArgs
          ]),
    Reply, []),
  term_to_atom(TermReply,Reply),
  ( TermReply = fail -> fail
  ; otherwise-> Args = TermReply
  ).

possible_query(check_pos, [_Pos, _OID]).                     % ?-query_world( check_pos, [p(1,2), empty])
possible_query(agent_ask_oracle, [_Agent,_OID,_Q,_L]).       % ( agent_ask_oracle, [4, o(3), link, L)
possible_query(agent_current_energy, [_Agent,_E]).           % ( agent_current_energy, [6,E]).
possible_query(agent_current_position, [_Agent,_P]).         % ( agent_current_position, [oscar,P]).
possible_query(agent_topup_energy, [_Agent,_ChargStation]).  % ( agent_topup_energy, [2, c(1)]).
possible_query(agent_check_oracle, [_Agent, _Oracle]).       % ( agent_check_oracle, [9, o(1)]).
possible_query(agent_do_moves, [_Agent, _Path]).             % ( agent_do_moves, [ 1, Path]).
possible_query(internal_leave_game, [_Agent]).               % ( internal_leave_game, [ 2]).
possible_query(internal_join_game, [_Agent]).                % ( agent_join_game, [Agent]).
possible_query(game_status, [_Status]).                      % ( game_status, [stopped]).
possible_query(internal_start_game, []).
possible_query(ailp_reset, []).

join_game(Agent):-
  ( \+query_world(game_status,[running]) ->
    ( my_agent(Agent) -> format('Your agent has already joined the game')
    ; otherwise       -> query_world(internal_join_game, [Agent]),
                         assert(ailp_internal(agent(Agent)))
    )
  ; otherwise                            -> format('Cannot join! Game has already started')
  ).

leave_game:-
  my_agent(Agent),
  retract(ailp_internal(agent(Agent))),
  query_world(internal_leave_game, [Agent]).

start_game:-
  query_world(internal_start_game, []).

my_agent(Agent):-
  ailp_internal(agent(Agent)).

reset_game:-
  query_world(ailp_reset, []).
/*
 *  Part 4
 */

/*
 *  Moved from oscar.pl
 */
%%%%%%%%%% command shell %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

shell :-
  ( api_(4)   -> get_input(Input),( Input=stop -> true ; handle_input(Input),shell )
  ; otherwise -> show_response('Shell not available.')
  ).

handle_input(Input) :-
  ( Input = help           -> forall(shell2query(S,Q,_R),(numbervars((S,Q)),writes([S,' -- ',Q])))
  ; Input = demo           -> randomMove(1000, [], D),handle_input(D)
  ; Input = [H|T]          -> writeln(H), writes(['? ',H]),handle_input(H),handle_input(T)
  ; Input = []             -> true
  ; shell2query(Input,G,R) -> ( show_response(query(G)),call(G) -> show_response(R) ; writeln("======================"), show_response('This failed.'), writeln("=============================") )
  ; otherwise              -> show_response('Unknown command, please try again.')
  ).


randomMove(0, Temp, Result):-
  Temp = Result.
randomMove(N, Temp,Result):-
  random(1, 21, X),
  random(1, 21, Y),
  append(Temp, [energy, position, go(p(X, Y))], R),
  New is N-1,
  randomMove(New, R, Result).
shell_demo([reset,find(o(1)),ask(o(1),'What is the meaning of life, the universe and everything?'),go(p(7,7)),energy,position,go(p(19,9)),energy,position,call(map_adjacent(p(19,9),_P,_O)),topup(c(3)),energy,go(p(10,10)),energy]).

% get input from user
get_input(Input) :-
  prompt(_,'? '),
  read(Input).

% show answer to user
show_response(R) :-
  ( R=shell(Response)   -> writes(['! ',Response])
  ; R=query(Response)   -> \+ \+ (numbervars(Response),writes([': ',Response]))
  ; R=console(Response) -> my_agent(Agent),term_to_atom(Response,A),do_command([Agent,console,A])
  ; R=both(Response)    -> show_response(shell(Response)),show_response(console(Response))
  ; R=agent(Response)   -> my_agent(Agent),term_to_atom(Response,A),do_command([Agent,say,A])
  ; R=[H|T]             -> show_response(H),show_response(T)
  ; R=[]                -> true
  ; otherwise           -> writes(['! ',R])
  ).

writes(A) :-
  ( A=[]      -> nl
  ; A=nl      -> nl
  ; A=[H|T]   -> writes(H),writes(T)
  ; A=term(T) -> write(T)
  ; otherwise -> write(A)
  ).

% shell2query(+Command, +Goal, ?Response)
shell2query(setup,(join_game(_A),reset_game,start_game),ok).
shell2query(reset,(reset_game,start_game),ok).
shell2query(status,query_world(game_status,[S]),game_status(S)).
shell2query(whoami,my_agent(A),my_agent(A)).
shell2query(position,(my_agent(Agent),query_world( agent_current_position, [Agent,P] )),both(current_position(P))).
shell2query(energy,(my_agent(Agent),query_world( agent_current_energy, [Agent,E] )),both(current_energy(E))).
shell2query(topup(S),(my_agent(Agent),query_world( agent_topup_energy, [Agent,S] )),agent(topup)).
shell2query(Task,user:solve_task(Task,Cost),[console(Task),shell(term(Cost))]):-task(Task).
shell2query(identity,find_identity(A),both(identity(A))).
shell2query(ask(S,Q),(my_agent(Agent),query_world( agent_ask_oracle, [Agent,S,Q,A] )),A).
shell2query(call(G),findall(G,call(G),L),L).

task(go(_Pos)).
task(find(_O)).  % oracle o(N) or charging station c(N)
task(randomMove()).
 %% Extra predicates
say(Message, Agent) :-
  do_command([Agent, say, Message]).
