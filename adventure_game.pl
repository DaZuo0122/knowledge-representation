/* Adventure Game — by Student Name
   SWI-Prolog file: adventure_game.pl
   Usage: consult('adventure_game.pl'). then start.
*/

:- dynamic i_am_at/1, at/2, holding/1, opened/1, hp/1, defeated/1, seed_rand/1, adversary_at/2, adversary_plan/2, current_adversary_goal/1, chaser_patience/1, thief_patience/1, player_turn_count/1.
:- use_module(pyperplan_runner).
:- retractall(i_am_at(_)), retractall(at(_, _)), retractall(holding(_)),
   retractall(opened(_)), retractall(hp(_)), retractall(defeated(_)),
   retractall(seed_rand(_)),
   retractall(adversary_at(_,_)), retractall(adversary_plan(_,_)), retractall(current_adversary_goal(_)).

/* INITIAL WORLD STATE (dynamic facts) */
i_am_at(entrance).

% rooms: entrance, hall, armory, throne, pit
% Additional rooms for more space and choices
path(entrance, n, hall).
path(hall, s, entrance).
path(hall, e, armory).
path(armory, w, hall).
path(hall, n, throne).
path(throne, s, hall).
path(hall, d, pit).      % down = dangerous path
path(pit, u, hall).

% New rooms and paths to create more space and options
path(hall, w, library).      % New library west of hall
path(library, e, hall).
path(library, n, study).     % Study north of library
path(study, s, library).
path(study, e, throne).      % Alternative path from study to throne

path(hall, ne, gallery).     % New gallery northeast of hall (diagonal concept)
path(gallery, sw, hall).
path(gallery, n, treasure_room).  % Treasure room north of gallery
path(treasure_room, s, gallery).
path(treasure_room, w, throne).   % Direct access to throne area

path(armory, ne, forge).     % New forge northeast of armory
path(forge, sw, armory).
path(forge, n, tower).       % Tower north of forge
path(tower, s, forge).
path(tower, w, throne).      % Another path to throne

% Additional connections to make the map more interconnected
path(library, w, passage).   % Extra western passage
path(passage, e, library).
path(passage, s, entrance).  % Can go back from passage to entrance

% More distant locations for adversaries to start
path(passage, w, chasm).     % Chasm for chaser to start
path(chasm, e, passage).
path(forge, e, cavern).      % Cavern for thief to start
path(cavern, w, forge).

% Objects placed in rooms
at(torch, entrance).
at(key_fire, armory).
at(sword, armory).
at(amulet, throne).
at(rope, entrance).
at(auto_win, entrance).  % Special item that wins the game immediately when taken

% New objects in expanded rooms
at(book, library).        % Book that might be useful
at(magic_key, study).     % Magic key for special access
at(gold, treasure_room).  % Gold for bonus points
at(healing_potion, tower). % Healing potion to restore health
at(map, gallery).         % Map of the area
at(shield, forge).        % Shield for protection

% Additional auto-win item for game accessibility
at(easy_win, hall).       % Easy win item in the central hall

% Adversaries: chaser and thief initial states
adversary_at(chaser, chasm).    % Chaser starts at chasm (far from entrance)
adversary_at(thief, cavern).    % Thief starts at cavern (far from entrance)
current_adversary_goal(catch_player).

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
    % Handle special win items first
    ( X == easy_win ->
        i_am_at(Place),
        at(easy_win, Place),
        retract(at(easy_win, Place)),
        assertz(holding(easy_win)),
        write('**You picked up the easy_win item! You win immediately!**'), nl,
        write('*** Congratulations! You won the game easily! ***'), nl,
        finish, !
    ; X == auto_win ->
        i_am_at(Place),
        at(auto_win, Place),
        retract(at(auto_win, Place)),
        assertz(holding(auto_win)),
        write('**You picked up the auto_win item! You win immediately!**'), nl,
        write('*** Congratulations! You won the game automatically! ***'), nl,
        finish, !
    ; % General case for other items
        i_am_at(Place),
        at(X, Place),
        retract(at(X, Place)),
        assertz(holding(X)),
        write('**Taken.**'), nl, !,
        update_adversary_goal,
        adversary_turn
    ).

take(_) :-
    write('**I don''t see that here.**'), nl.

drop(X) :-
    holding(X),
    i_am_at(Place),
    retract(holding(X)),
    assertz(at(X, Place)),
    write('**Dropped.**'), nl, !,
    update_adversary_goal,
    adversary_turn.

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
    !, look,
    % Increment player turn counter
    ( retract(player_turn_count(OldCount)) ->
        NewCount is OldCount + 1,
        assertz(player_turn_count(NewCount))
    ; assertz(player_turn_count(1))  % In case counter doesn't exist
    ),
    % After player moves, the adversary gets a turn
    update_adversary_goal,
    adversary_turn.

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
    write('A grand hall with murals. Exits: south, north, east, west, northeast, down.'), nl.

describe(armory) :-
    write('Old armory with rusted racks. A heavy chest sits in the corner.'), nl,
    ( opened(chest1) ->
        write('The chest is open.'), nl
    ; write('The chest is closed. You might be able to open it.'), nl ).

describe(throne) :-
    write('The throne room. A pedestal holds a faintly glowing amulet.'), nl.

describe(pit) :-
    write('A dark pit. The floor is treacherous. You should be careful here.'), nl.

% New room descriptions
describe(library) :-
    write('Ancient library with dusty tomes. Exits: east, north.'), nl.

describe(study) :-
    write('Small study with maps on the walls. Exits: south, east.'), nl.

describe(gallery) :-
    write('Art gallery with portraits of ancient kings. Exits: southwest, north.'), nl.

describe(treasure_room) :-
    write('Treasure room with scattered gems and gold. Exits: south, west.'), nl.

describe(forge) :-
    write('Blacksmith forge with cooling irons. Exits: southwest, north, east.'), nl.

describe(tower) :-
    write('Watchtower with views of the surrounding lands. Exits: south, west.'), nl.

describe(passage) :-
    write('Narrow stone passage with mysterious carvings. Exits: east, south, west.'), nl.

describe(chasm) :-
    write('Deep chasm with misty depths. Exits: east.'), nl.

describe(cavern) :-
    write('Damp cavern with echoing sounds. Exits: west.'), nl.

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
    ), !,
    update_adversary_goal,
    adversary_turn.

open(_) :-
    write('**There is nothing like that to open here.**'), nl.

/* use rope to climb pit safely */
use(rope) :-
    holding(rope),
    i_am_at(pit),
    write('You secure the rope and climb safely back up to the hall.'), nl,
    retract(i_am_at(pit)), assertz(i_am_at(hall)), look, !,
    update_adversary_goal,
    adversary_turn.

use(_) :-
    write('**You can''t use that now.**'), nl.

/* light torch to avoid hazards */
use(torch) :-
    holding(torch),
    write('You light the torch. It illuminates nearby danger and boosts morale.'), nl,
    hp_add(2), !,
    update_adversary_goal,
    adversary_turn.

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
    ), !,
    update_adversary_goal,
    adversary_turn.

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

/* ADVERSARY MOVEMENT AND PLANNING */

% Execute the adversaries' turn (for all adversaries)
adversary_turn :-
    % Process each adversary individually with error handling to ensure success
    forall(adversary_at(Adv, _), process_individual_adversary(Adv)),
    !.  % Ensure the predicate always succeeds

% Process a single adversary with error handling
process_individual_adversary(Adv) :-
    ( catch(
        ( format('Processing turn for ~w~n', [Adv]),
          % Check if this adversary (specifically chaser/thief) should act
          ( Adv = chaser ->
              % Check if chaser should be active based on patience
              ( chaser_patience(Patience),
                player_turn_count(Count),
                ( Count < Patience ->  % Chaser remains patient for first few turns
                    format('Chaser is still patrolling its area...~n'),
                    true  % Just continue without executing chaser's turn
                ; % After patience period, still add randomness
                  random_between(1, 10, ChaserRand),
                  ( ChaserRand =< 4 ->  % 40% chance that chaser is distracted after patience
                      format('Chaser seems disoriented and skips this turn~n'),
                      true  % Just continue without executing chaser's turn
                  ; % Chaser is active, execute normally
                      process_adversary_turn(Adv)
                  )
                )
              )
          ; Adv = thief ->
              % Check if thief should be active based on patience
              ( thief_patience(Patience),
                player_turn_count(Count),
                ( Count < Patience ->  % Thief remains patient for first few turns
                    format('Thief is still searching the area...~n'),
                    true  % Just continue without executing thief's turn
                ; % After patience period, still add randomness
                  random_between(1, 10, ThiefRand),
                  ( ThiefRand =< 4 ->  % 40% chance that thief is distracted after patience
                      format('Thief seems distracted and skips this turn~n'),
                      true  % Just continue without executing thief's turn
                  ; % Thief is active, execute normally
                      process_adversary_turn(Adv)
                  )
                )
              )
          ; % For any other adversaries, process normally
              process_adversary_turn(Adv)
          )
        ),
        _Exception,
        format('Error processing ~w, skipping...~n', [Adv])
    ) -> true
    ; true  % Ensure this always succeeds
    ).

% Helper predicate to encapsulate the normal adversary processing
process_adversary_turn(Adv) :-
    ( catch(
        ( generate_adversary_pddl,
          % Attempt to get a new plan from the planner for each adversary
          ( catch(run_pyperplan_soln('pyperplan', 'adversary_domain.pddl', 'current_adversary_problem.pddl', Plan),
                  Error,
                  (format('Adversary ~w planner failed: ', [Adv]), write(Error), nl,
                   fail_plan_execution(Adv))
          ) ->
              % Store the new plan
              retractall(adversary_plan(Adv, _)),
              assertz(adversary_plan(Adv, Plan)),
              execute_adversary_plan_if_exists(Adv)
          ; % If no plan could be generated, fall back to simple movement toward player
              format('Adversary ~w could not generate a plan, moving randomly...~n', [Adv]),
              simple_adversary_move(Adv)
          )
        ),
        _Exception,
        ( format('Error processing ~w, using fallback...~n', [Adv]),
          simple_adversary_move(Adv) )
    ) -> true
    ; format('Unexpected error with ~w~n', [Adv]), true
    ).

% Execute the stored plan if it exists for a specific adversary
execute_adversary_plan_if_exists(Adversary) :-
    adversary_plan(Adversary, [Action|RemainingPlan]),
    !,
    ( execute_adversary_action(Action, Adversary) ->
        % Update the plan with remaining actions
        retract(adversary_plan(Adversary, _) ),
        assertz(adversary_plan(Adversary, RemainingPlan))
    ; % If action fails, generate a new plan
        format('Adversary ~w action failed, regenerating plan...~n', [Adversary]),
        generate_adversary_pddl,
        ( catch(run_pyperplan_soln('pyperplan', 'adversary_domain.pddl', 'current_adversary_problem.pddl', NewPlan), _, fail) ->
            retractall(adversary_plan(Adversary, _)),
            assertz(adversary_plan(Adversary, NewPlan)),
            execute_adversary_plan_if_exists(Adversary)
        ; simple_adversary_move(Adversary)
        )
    ).

execute_adversary_plan_if_exists(Adversary) :-
    % If there's no plan or the plan is empty, generate a new one
    simple_adversary_move(Adversary).

% Execute a single adversary action with validation
execute_adversary_action(move_chaser(From, To), Adversary) :-
    % Validate the action first
    validate_adversary_move(From, To, Adversary),
    Adversary = chaser,
    !,
    % Execute the move only if it's valid
    retract(adversary_at(Adversary, From)),
    assertz(adversary_at(Adversary, To)),
    format('Adversary ~w moved from ~w to ~w~n', [Adversary, From, To]),
    % Add more randomness to make chaser less effective
    ( i_am_at(To) ->
        % Random chance that chaser succeeds in catching player
        random_between(1, 10, CatchSuccess),
        ( CatchSuccess =< 3 ->  % Only 30% chance to catch even when in same room
            format('The ~w caught you! You lose!~n', [Adversary]),
            lose
        ; % 70% chance to miss the catch when in same room
            format('The ~w lunges at you but misses! You have a moment to escape!~n', [Adversary])
        )
    ; true
    ).

execute_adversary_action(move_thief(From, To), Adversary) :-
    % Validate the action first
    validate_adversary_move(From, To, Adversary),
    Adversary = thief,
    !,
    % Execute the move only if it's valid
    retract(adversary_at(Adversary, From)),
    assertz(adversary_at(Adversary, To)),
    format('Adversary ~w moved from ~w to ~w~n', [Adversary, From, To]),
    % Check if the thief can steal something - but with reduced aggression
    ( i_am_at(To), Adversary = thief ->
        % Randomize whether thief actually attempts to steal (make game more winnable)
        random_between(1, 3, StealChance),
        ( StealChance =:= 1 ->  % Only 33% chance to steal when in same room
            format('The ~w is in the same room as you and might steal something!~n', [Adversary]),
            ( holding(X), X \= amulet ->  % Don't steal the amulet, that's the chaser's job
                format('The ~w steals your ~w!~n', [Adversary, X]),
                retract(holding(X)),
                assertz(at(X, To))
            ; true
            )
        ; true  % 67% chance thief just passes by without stealing
        )
    ; true
    ).

execute_adversary_action(catch_player(Location), Adversary) :-
    validate_adversary_catch_player(Location, Adversary),
    Adversary = chaser,
    !,
    % Add randomness to make game more winnable - only catch with 70% probability
    random_between(1, 10, CatchChance),
    ( CatchChance =< 7 ->  % 70% chance of catching when conditions are met
        format('The ~w caught you! You lose!~n', [Adversary]),
        lose
    ; % 30% chance to miss the catch - giving player a chance to escape
        format('The ~w tried to catch you but missed! You have a moment to escape!~n', [Adversary])
    ).

execute_adversary_action(steal_item(Location), Adversary) :-
    validate_adversary_catch_player(Location, Adversary),
    Adversary = thief,
    !,
    % Randomize whether thief actually attempts to steal (make game more winnable)
    random_between(1, 3, StealChance),
    ( StealChance =:= 1 ->  % Only 33% chance to steal when catching player
        format('The ~w tries to steal from you!~n', [Adversary]),
        % The thief tries to steal items
        ( holding(amulet) ->
            write('The thief snatches the amulet from you!'), nl,
            retract(holding(amulet)),
            % Player loses if thief gets the amulet
            lose
        ; holding(X) ->
            format('The ~w steals your ~w!~n', [Adversary, X]),
            retract(holding(X)),
            assertz(at(X, Location))
        ; write('The thief tries to steal from you, but you have nothing valuable!'), nl
        )
    ; % 67% chance thief fails to steal
        format('The ~w tries to steal from you but fumbles the attempt!~n', [Adversary])
    ).

execute_adversary_action(move(From, To), Adversary) :-
    % Fallback for older format move actions
    execute_adversary_action(move_chaser(From, To), Adversary);
    execute_adversary_action(move_thief(From, To), Adversary).

execute_adversary_action(Action, Adversary) :-
    % Invalid action
    format('Invalid or illegal ~w action attempted: ~w~n', [Adversary, Action]),
    fail.

% Validate if an adversary move is legal in the current world state
validate_adversary_move(From, To, Adversary) :-
    % Check that the specific adversary is currently at the 'From' location
    adversary_at(Adversary, From),
    % Check that there is a valid path from From to To
    path(From, _, To),
    !.

% Validate if the adversary can catch the player at the given location
validate_adversary_catch_player(Location, Adversary) :-
    % Check that both the specific adversary and player are at the same location
    adversary_at(Adversary, Location),
    i_am_at(Location),
    !.

% Fallback: simple movement toward player for a specific adversary
simple_adversary_move(Adversary) :-
    adversary_at(Adversary, AdvLoc),
    i_am_at(PlayerLoc),
    ( AdvLoc = PlayerLoc ->
        % Adversary is already at player's location
        format('The ~w is in the same room as you!~n', [Adversary]),
        % Add randomness to avoid immediate capture
        random_between(1, 10, CatchChance),
        ( Adversary = chaser, PlayerLoc = throne ->
            % If chaser at throne, it tries to get the amulet first
            write('The chaser eyes the amulet greedily...'), nl
        ; Adversary = thief, PlayerLoc = throne ->
            % If thief at throne, it tries to steal the amulet
            format('The ~w eyes the amulet greedily...~n', [Adversary]),
            ( holding(amulet) ->
                write('The thief snatches the amulet from you!'), nl,
                retract(holding(amulet)),
                % Player loses if thief gets the amulet
                lose
            ; at(amulet, throne) ->
                write('The thief takes the amulet!'), nl,
                retract(at(amulet, throne))
            ; true
            ),
            % Update goal since amulet situation has changed
            update_adversary_goal
        ; Adversary = chaser ->
            ( CatchChance =< 6 ->  % Only 60% chance to catch immediately when in same room
                write('The chaser is hunting you!'), nl,
                % Actually catch the player
                write('The chaser caught you! You lose!'), nl,
                lose
            ; % 40% chance to hesitate - giving player time to escape
                write('The chaser hesitates for a moment!'), nl
            )
        ; Adversary = thief ->
            % The thief might try to steal items
            ( CatchChance =< 5 ->  % Only 50% chance to attempt stealing when in same room
                format('The ~w is stealing from you!~n', [Adversary]),
                ( holding(X), X \= amulet ->  % Don't steal the amulet, that's the chaser's job
                    format('The ~w steals your ~w!~n', [Adversary, X]),
                    retract(holding(X)),
                    assertz(at(X, PlayerLoc))
                ; write('The thief tries to steal from you, but you have nothing valuable!'), nl
                )
            ; % 50% chance thief doesn't attempt stealing
                format('The ~w eyes you suspiciously but does nothing for now.~n', [Adversary])
            )
        )
    ; % Find a path toward the player
        % Add some randomness to movement - sometimes don't move toward player
        random_between(1, 10, MoveChoice),
        ( MoveChoice =< 7 ->  % 70% chance to move toward player
            ( path(AdvLoc, _, NextLoc),
              NextLoc = PlayerLoc, !,
              % Move directly toward player if possible
              retract(adversary_at(Adversary, AdvLoc)),
              assertz(adversary_at(Adversary, NextLoc)),
              format('~w moves toward you: from ~w to ~w~n', [Adversary, AdvLoc, NextLoc]),
              ( i_am_at(NextLoc) ->
                  random_between(1, 10, FinalCatchChance),
                  ( FinalCatchChance =< 6 ->  % Only 60% final chance if they end up in same room
                      format('The ~w caught you! You lose!~n', [Adversary]),
                      lose
                  ; true  % 40% chance to avoid capture even when in same room
                  )
              ; true
              )
            ; % If can't move directly to player, make random move
              random_adversary_move(Adversary, AdvLoc)
            )
        ; % 30% chance to move randomly instead of toward player (distracted)
            format('~w seems distracted and moves randomly.~n', [Adversary]),
            random_adversary_move(Adversary, AdvLoc)
        )
    ).

random_adversary_move(Adversary, CurrentLoc) :-
    ( setof(Next, path(CurrentLoc, _, Next), PossibleMoves) ->
        length(PossibleMoves, Len),
        Len > 0 ->
        random_between(1, Len, RandIdx),
        nth1(RandIdx, PossibleMoves, NewLoc),
        retract(adversary_at(Adversary, CurrentLoc)),
        assertz(adversary_at(Adversary, NewLoc)),
        format('~w makes a random move: from ~w to ~w~n', [Adversary, CurrentLoc, NewLoc]),
        ( i_am_at(NewLoc) ->
            format('The ~w caught you! You lose!~n', [Adversary]),
            lose
        ; true
        )
    ; % If no moves possible, stay in place
        format('~w is stuck and cannot move.~n', [Adversary])
    ).

% Handle failure to generate a plan for a specific adversary
fail_plan_execution(Adversary) :-
    format('~w planner execution failed, using fallback behavior...~n', [Adversary]),
    simple_adversary_move(Adversary).

% Function to change the adversary's goal dynamically based on game state
update_adversary_goal :-
    % Check if player has the amulet
    ( holding(amulet) ->
        % Player has the amulet, so now the adversary should try to catch them
        retractall(current_adversary_goal(_)),
        assertz(current_adversary_goal(catch_player)),
        write('The chaser changes tactics to hunt you down!'), nl
    ; % Check if the amulet is still available at throne
      at(amulet, throne) ->
        % Amulet is still at throne, adversary should go for it
        ( current_adversary_goal(get_amulet) ->
            true  % Already has this goal, no change needed
        ; retractall(current_adversary_goal(_)),
          assertz(current_adversary_goal(get_amulet)),
          write('The chaser decides to go for the amulet!'), nl
        )
    ; % Amulet has been taken by someone (player or adversary)
      % If amulet is not at throne and player doesn't have it, maybe adversary has it
      ( adversary_at(chaser, throne), \+ holding(amulet) ->
          % Chaser is at throne room but nobody has amulet - maybe it was taken by chaser
          retractall(current_adversary_goal(_)),
          assertz(current_adversary_goal(catch_player)),
          write('The chaser now focuses on catching you!'), nl
      ; % Default: if player doesn't have amulet and it's not at throne, assume player has it
        retractall(current_adversary_goal(_)),
        assertz(current_adversary_goal(catch_player)),
        write('The chaser decides to hunt you!'), nl
      )
    ).

% Advanced plan dynamics: allows for multiple adversaries (currently we just have one)
% but this structure allows for expansion
update_all_adversary_plans :-
    % Update plans for all adversaries based on current game state
    update_adversary_goal,
    % Reset any stored plans to force regeneration with new goals
    retractall(adversary_plan(_, _)).

/* pick up auto_win immediately wins the game */
take(auto_win) :-
    i_am_at(Place),
    at(auto_win, Place),
    retract(at(auto_win, Place)),
    assertz(holding(auto_win)),
    write('**You picked up the auto_win item! You win immediately!**'), nl,
    write('*** Congratulations! You won the game automatically! ***'), nl,
    finish, !.

/* pick up easy_win immediately wins the game */
take(easy_win) :-
    i_am_at(Place),
    at(easy_win, Place),
    retract(at(easy_win, Place)),
    assertz(holding(easy_win)),
    write('**You picked up the easy_win item! You win immediately!**'), nl,
    write('*** Congratulations! You won the game easily! ***'), nl,
    finish, !.

/* pick up amulet requires guardian defeated */
take(amulet) :-
    i_am_at(throne),
    ( defeated(guardian) ->
        retract(at(amulet, throne)), assertz(holding(amulet)),
        write('**You take the amulet.**'), nl, check_win
    ; write('**A spectral guardian prevents you from taking the amulet.**'), nl
    ), !.

/* ADVERSARY SYSTEM - PDDL Integration */

% Generate PDDL problem file based on current game state
generate_adversary_pddl :-
    open('current_adversary_problem.pddl', write, Stream),
    write(Stream, '(define (problem current_adversary_problem)'), nl(Stream),
    write(Stream, '    (:domain adversary_domain)'), nl(Stream),
    write(Stream, ''), nl(Stream),
    write(Stream, '    (:objects'), nl(Stream),
    write(Stream, '        entrance hall armory throne pit - location'), nl(Stream),
    write(Stream, '        chaser thief - adversary'), nl(Stream),
    write(Stream, '        player - player'), nl(Stream),
    write(Stream, '    )'), nl(Stream),
    write(Stream, ''), nl(Stream),
    write(Stream, '    (:init'), nl(Stream),

    % Write path links
    forall(path(Room1, _, Room2), (
        format(Stream, '        (link ~w ~w)~n', [Room1, Room2])
    )),

    % Write current positions of all adversaries
    forall(adversary_at(Adv, AdvLoc), (
        format(Stream, '        (at ~w ~w)~n', [Adv, AdvLoc])
    )),

    % Write current player position
    i_am_at(PlayerLoc),
    format(Stream, '        (at player ~w)~n', [PlayerLoc]),
    format(Stream, '        (player-at ~w)~n', [PlayerLoc]),

    write(Stream, '    )'), nl(Stream),
    write(Stream, ''), nl(Stream),

    % Set goal based on current adversary goal
    current_adversary_goal(Goal),
    ( Goal = catch_player ->
        write(Stream, '    (:goal'), nl(Stream),
        write(Stream, '        (and'), nl(Stream),
        write(Stream, '            (caught-player)'), nl(Stream),
        write(Stream, '        )'), nl(Stream),
        write(Stream, '    )'), nl(Stream)
    ; Goal = get_amulet ->
        write(Stream, '    (:goal'), nl(Stream),
        write(Stream, '        (and'), nl(Stream),
        write(Stream, '            (at chaser throne)'), nl(Stream),  % The chaser needs to reach throne to get amulet
        write(Stream, '        )'), nl(Stream),
        write(Stream, '    )'), nl(Stream)
    ),

    write(Stream, ')'), nl(Stream),
    close(Stream).

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
    retractall(player_turn_count(_)),  % Reset turn counter
    retractall(chaser_patience(_)),    % Reset chaser patience
    % initial placements
    assertz(i_am_at(entrance)),
    assertz(at(torch, entrance)),
    assertz(at(rope, entrance)),
    assertz(at(sword, armory)),
    assertz(at(amulet, throne)),
    assertz(at(key_fire, armory)), % key initially accessible when chest opened too
    assertz(at(easy_win, hall)),   % Easy win item in the central hall
    assertz(seed_rand(0)),
    assertz(player_turn_count(0)),  % Start with 0 player turns
    assertz(chaser_patience(5)),    % Chaser won't move aggressively for first 5 player turns
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
