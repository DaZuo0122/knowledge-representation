/* Adventure Game — by Student Name
   SWI-Prolog file: adventure_game.pl
   Usage: consult('adventure_game.pl'). then start.
*/

:- dynamic i_am_at/1, at/2, holding/1, opened/1, hp/1, defeated/1, seed_rand/1.
:- retractall(i_am_at(_)), retractall(at(_, _)), retractall(holding(_)),
   retractall(opened(_)), retractall(hp(_)), retractall(defeated(_)),
   retractall(seed_rand(_)).

/* INITIAL WORLD STATE (dynamic facts) */
i_am_at(entrance).

% rooms: entrance, hall, armory, throne, pit
path(entrance, n, hall).
path(hall, s, entrance).
path(hall, e, armory).
path(armory, w, hall).
path(hall, n, throne).
path(throne, s, hall).
path(hall, d, pit).      % down = dangerous path
path(pit, u, hall).

% Objects placed in rooms
at(torch, entrance).
at(key_fire, armory).
at(sword, armory).
at(amulet, throne).
at(rope, entrance).

% Doors / chests (closed unless opened/used)
opened(chest1) :- fail.   % chest1 initially closed (non-dynamic rule form)
% We'll manage opened/1 dynamically when player opens them.

% Player stats
hp(10).                    % dynamic predicate holds player's HP
seed_rand(0).              % optional seed for randomness

/* -----------------------
   Basic actions: take/drop
   ----------------------- */

take(X) :-
    holding(X),
    write('**You are already holding it.**'), nl, !.

take(X) :-
    i_am_at(Place),
    at(X, Place),
    retract(at(X, Place)),
    assertz(holding(X)),
    write('**Taken.**'), nl, !.

take(_) :-
    write('**I don''t see that here.**'), nl.

drop(X) :-
    holding(X),
    i_am_at(Place),
    retract(holding(X)),
    assertz(at(X, Place)),
    write('**Dropped.**'), nl, !.

drop(_) :-
    write('**You aren''t holding that.**'), nl.

/* inventory */
inventory :-
    findall(O, holding(O), L),
    ( L = [] -> write('You are carrying nothing.'), nl
    ; write('You are carrying: '), write(L), nl ).

/* -----------------------
   Movement and look
   ----------------------- */

n :- go(n). s :- go(s). e :- go(e). w :- go(w). d :- go(d). u :- go(u).

go(Direction) :-
    i_am_at(Here),
    path(Here, Direction, There),
    retract(i_am_at(Here)),
    assertz(i_am_at(There)),
    !, look.

go(_) :-
    write('**You can''t go that way.**'), nl.

/* look describes current room and objects */
look :-
    i_am_at(Place),
    describe(Place),
    nl,
    notice_objects_at(Place),
    nl.

/* list objects at Place */
notice_objects_at(Place) :-
    at(X, Place),
    write('There is a '), write(X), write(' here.'), nl,
    fail.
notice_objects_at(_).

/* -----------------------
   Room descriptions
   ----------------------- */

describe(entrance) :-
    write('You are at the cave entrance. A torch socket flickers.'), nl,
    write('Paths: north to the hall, down to the pit (beware).').

describe(hall) :-
    write('A grand hall with murals. Exits: south, north, east, down.'), nl.

describe(armory) :-
    write('Old armory with rusted racks. A heavy chest sits in the corner.'), nl,
    ( opened(chest1) ->
        write('The chest is open.'), nl
    ; write('The chest is closed. You might be able to open it.'), nl ).

describe(throne) :-
    write('The throne room. A pedestal holds a faintly glowing amulet.'), nl.

describe(pit) :-
    write('A dark pit. The floor is treacherous. You should be careful here.'), nl.

/* -----------------------
   Interact: open chest, use rope, light torch
   ----------------------- */

open(chest1) :-
    i_am_at(armory),
    ( opened(chest1) ->
        write('**Chest is already open.**'), nl
    ; ( holding(sword) ->
            format('You pry the chest open using the sword. Inside: ~w.~n', [key_fire]),
            assertz(opened(chest1)),
            assertz(at(key_fire, armory))
      ; write('**The chest is too stiff. Maybe something can pry it open.**'), nl
      )
    ), !.

open(_) :-
    write('**There is nothing like that to open here.**'), nl.

/* use rope to climb pit safely */
use(rope) :-
    holding(rope),
    i_am_at(pit),
    write('You secure the rope and climb safely back up to the hall.'), nl,
    retract(i_am_at(pit)), assertz(i_am_at(hall)), look, !.

use(_) :-
    write('**You can''t use that now.**'), nl.

/* light torch to avoid hazards */
use(torch) :-
    holding(torch),
    write('You light the torch. It illuminates nearby danger and boosts morale.'), nl,
    hp_add(2), !.

hp_add(N) :-
    N > 0,
    retract(hp(H)),
    H2 is H + N,
    assertz(hp(H2)),
    write('**HP: '), write(H2), write('**'), nl.

/* -----------------------
   Random encounter in pit (introduces uncertainty)
   ----------------------- */

enter_pit :-
    i_am_at(hall),
    go(d),
    pit_event.

pit_event :-
    i_am_at(pit),
    rand_between(1, 6, R),
    ( R =< 3 ->
        write('A rock falls! You lose 3 HP.'), nl, hp_add(-3)
    ; R =:= 4 ->
        write('You find a small cache and spot a rope on the edge.'), nl,
        ( at(rope, pit) -> true ; assertz(at(rope, pit)) )
    ; write('You slip but catch the edge — you are shaken, not seriously harmed.'), nl
    ), check_alive.

/* hp_add for negatives handled here */
hp_add(N) :-
    N < 0,
    retract(hp(H)),
    H2 is H + N,
    assertz(hp(H2)),
    write('**HP: '), write(H2), write('**'), nl,
    ( H2 =< 0 -> lose ; true ).

/* Special wrapper for randomness: uses SWI-Prolog random and optional seed */
rand_between(A,B,R) :-
    seed_rand(S),
    ( S =:= 0 ->
        random_between(A,B,R)
    ; set_random(seed(S)), random_between(A,B,R)
    ).

/* -----------------------
   Combat with guardian (uses arithmetic + randomness)
   ----------------------- */

attack :-
    i_am_at(throne),
    ( defeated(guardian) ->
        write('The guardian is already defeated.'), nl
    ; ( holding(sword) ->
            write('You attack the guardian with your sword.'), nl,
            rand_between(1,6,R),
            Damage is R + 2,
            write('You deal '), write(Damage), write(' damage.'), nl,
            % simplified guardian HP track via defeated/1 threshold
            ( Damage >= 6 ->
                assertz(defeated(guardian)),
                write('The guardian falls. The amulet is safe to take.'), nl,
                check_win
            ; write('The guardian shakes off the blow and strikes back!'), nl,
              rand_between(1,4,D),
              write('You take '), write(D), write(' damage.'), nl,
              hp_add(-D)
            )
      ; write('You have no weapon! The guardian hits you hard.'), nl,
        rand_between(2,5,D2),
        hp_add(-D2)
      )
    ), !.

attack :-
    write('There is nothing to attack here.'), nl.

/* -----------------------
   Winning and losing rules
   ----------------------- */

check_win :-
    holding(amulet),
    holding(key_fire),
    write('*** You place the key and the amulet on the altar — a portal opens. You win! ***'), nl,
    finish.

win :-
    check_win.

lose :-
    write('*** You have perished in the adventure. Game over. ***'), nl,
    finish.

finish :-
    nl, write('The game is over. Please enter halt.'), nl.

/* check_alive helper */
check_alive :-
    hp(H),
    ( H =< 0 -> lose ; true ).

/* pick up amulet requires guardian defeated */
take(amulet) :-
    i_am_at(throne),
    ( defeated(guardian) ->
        retract(at(amulet, throne)), assertz(holding(amulet)),
        write('**You take the amulet.**'), nl, check_win
    ; write('**A spectral guardian prevents you from taking the amulet.**'), nl
    ), !.

/* -----------------------
   Utility and start/instructions
   ----------------------- */

instructions :-
    nl,
    write('Commands (Prolog style):'), nl,
    write('start.             -- start the game'), nl,
    write('n. s. e. w. d. u.  -- move directions (down/up included)'), nl,
    write('take(Object). drop(Object). inventory.'), nl,
    write('look. open(chest1). use(Object). attack.'), nl,
    write('start_rand(Seed).  -- optional: set RNG seed for reproducible runs'), nl,
    write('halt.               -- to quit'), nl, nl.

start :-
    retractall(hp(_)), assertz(hp(10)),
    retractall(defeated(_)),
    retractall(opened(_)),
    retractall(holding(_)),
    retractall(at(_, _)),
    % initial placements
    assertz(i_am_at(entrance)),
    assertz(at(torch, entrance)),
    assertz(at(rope, entrance)),
    assertz(at(sword, armory)),
    assertz(at(amulet, throne)),
    assertz(at(armory, hall)), % room markers (unused but present)
    assertz(at(key_fire, armory)), % key initially accessible when chest opened too
    assertz(seed_rand(0)),
    instructions,
    look.

/* set RNG seed for reproducible randomness */
start_rand(Seed) :-
    retractall(seed_rand(_)),
    assertz(seed_rand(Seed)),
    format('Random seed set to ~w. Start the game: start.~n', [Seed]).

/* quick status */
status :-
    i_am_at(P), write('Location: '), write(P), nl,
    hp(H), write('HP: '), write(H), nl,
    findall(X, holding(X), Inv), write('Holding: '), write(Inv), nl.
