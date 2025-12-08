---
marp: true
theme: gaia
paginate: true
backgroundColor: #fff
backgroundImage: url('https://marp.app/assets/hero-background.svg')
style: |
  section {
    font-family: 'Arial', sans-serif;
  }
  h1 {
    color: #2c3e50;
    text-align: center;
  }
  h2 {
    color: #3498db;
    border-bottom: 2px solid #3498db;
    padding-bottom: 5px;
  }
  code {
    background-color: #ecf0f1;
    padding: 2px 5px;
    border-radius: 3px;
    font-family: 'Courier New', monospace;
  }
  pre {
    background-color: #2c3e50;
    color: #ecf0f1;
    padding: 10px;
    border-radius: 5px;
    overflow: auto;
  }
  table {
    width: 100%;
    border-collapse: collapse;
    margin: 20px 0;
  }
  th {
    background-color: #3498db;
    color: white;
  }
  td, th {
    border: 1px solid #ddd;
    padding: 8px;
    text-align: left;
  }
  .columns {
    display: grid;
    grid-template-columns: 1fr 1fr;
    gap: 20px;
  }
---

# Guardian's Labyrinth
### Intelligent Dungeon Adventure Game
**JC4002 - Knowledge Representation Group Project**
Group 9 | Chen Zhenlan | Dai Jun | Weng Zixuan

---
## Presentation Team Roles
- **1**: Game concept, motivation, overall design
- **2**: Prolog knowledge base, rules system, uncertainty
- **3**: PDDL planning, adversary AI, technical integration

---

## Project Overview

### PDDL Integration for Adventure Game
This project implements an **intelligent adversary system** using:
- **PDDL** (Planning Domain Definition Language)
- **STRIPS planner** (Pyperplan)
- Integrated with **Prolog-based adventure game**

---
### Core Components
1. adventure_game.pl - Main game with integrated PDDL planning
2. pyperplan_runner.pl - Module to call Pyperplan from Prolog
3. adversary_domain.pddl - PDDL domain defining adversary actions
4. adversary_problem.pddl - PDDL problem with initial state/goals

---

## Intelligent Adversary System

### Two Types of Adversaries

| Adversary | Primary Goal | Strategy |
|-----------|--------------|----------|
| **Chaser** | Catch the player | Direct pursuit, especially when player has amulet |
| **Thief** | Steal items from player | Intercept items, indirect harassment |

---
### Key Features Implemented

**Plan Execution with Action Validation**
- Dynamic PDDL problem generation based on current game state
- Action validation before execution to ensure legal moves
- Fallback behavior when planner fails or no solution exists

---
**Plan Dynamics**
- Dynamic goal adjustment based on game state
- Multiple adversaries with different behaviors and goals
- Adversaries can work together (potential coordination via PDDL)

---
### Dynamic Goal Switching Example
```prolog
% When player obtains the amulet
update_adversary_goal :-
    ( holding(amulet) ->
        retractall(current_adversary_goal(_)),
        assertz(current_adversary_goal(catch_player)),
        write('Chaser changes tactics to hunt you down!')
    ; % Otherwise maintain current goal
      true
    ).
```

---

## System Architecture

### How It Works
```
1. Player performs action
   ↓
2. Game calls adversary_turn/0
   ↓
3. Generate updated PDDL problem file
   ↓
4. Call Pyperplan for plan generation
   ↓
5. Execute plan with validation
   ↓
6. Update game state, repeat
```

---
### Technical Implementation
```prolog
% PDDL Integration in Prolog
run_pyperplan_soln('pyperplan.exe', 
                   'adversary_domain.pddl', 
                   'current_adversary_problem.pddl', 
                   Plan).

% Fallback mechanism
adversary_turn :-
    ( catch(run_pyperplan_soln(...), Error, _) ->
        execute_plan(Plan)
    ; % If planner fails
      simple_adversary_move(Adversary)
    ).
```

---

## PDDL Domain Design

### STRIPS-Level PDDL
---
```lisp
(define (domain adversary_domain)
  (:requirements :strips :typing)
  (:types location adversary player)
  
  (:action move-chaser
    :parameters (?chaser ?from ?to)
    :precondition (and (at ?chaser ?from) 
                       (link ?from ?to))
    :effect (and (not (at ?chaser ?from))
                 (at ?chaser ?to)))
  
  (:action catch-player
    :parameters (?chaser ?loc)
    :precondition (and (at ?chaser ?loc)
                       (at player ?loc))
    :effect (caught-player)))
```

---
### Dynamic Problem Generation
- PDDL problem file (current_adversary_problem.pddl) is generated in real-time
- Reflects current positions of player and adversaries
- Adapts goals based on game state evolution

---

## Action Validation & Error Handling

### Pre-execution Validation
```prolog
% Validate adversary move before execution
validate_adversary_move(From, To, Adversary) :-
    adversary_at(Adversary, From),  % Adversary at start location
    path(From, _, To),              % Valid path exists
    \+ (i_am_at(To), adversary_at(_, To)).  % No collision
```
---
### No-Solution & Error Handling
**When planner finds no solution:**
- Fallback to simple heuristic-based movement
- Random movement toward player location
- Maintain current position if no valid moves

**When action execution fails:**
- Regenerate plan with current state
- Log error and continue gameplay
- Ensure game doesn't crash on planning failures

---

## Uncertainty & Randomness Implementation

### Probabilistic Event System
**Pit Event Example (50% chance of damage):**
```prolog
pit_event :-
    rand_between(1, 6, R),
    ( R =< 3 ->  % 50% probability
        write('Rock fall! -3 HP'), nl, hp_add(-3)
    ; R =:= 4 ->  % 16.7% probability
        write('Found rope!'), nl, assertz(at(rope, pit))
    ; write('Narrow escape!')  % 33.3% probability
    ).
```

---
### Combat Randomization
**Damage calculation with randomness:**
```prolog
attack :-
    rand_between(1,6,R),        % Random base damage
    Damage is R + 2,            % Sword bonus
    (Damage >= 6 -> defeat_guardian ; counter_attack).
```

---
**Adversary decision randomization:**
- Random movement when planning fails
- Probabilistic item theft attempts
- Variable chase aggressiveness

---

## Testing Methodology

### Method 1: Full Game Test
```prolog
% Start SWI-Prolog and load the game
?- [adventure_game].

% Start the game
?- start.

% Play with standard commands
n.        % go north
take(torch).  % take object
attack.   % combat
```
---
### Method 2: Direct PDDL Integration Test
```prolog
% Load modules
?- [adventure_game].
?- [pyperplan_runner].

% Test PDDL problem generation
?- generate_adversary_pddl.

% Test planner directly
?- run_pyperplan_soln('pyperplan.exe', 
                      'adversary_domain.pddl', 
                      'current_adversary_problem.pddl', 
                      Plan).
```

---

## Requirements & Setup

### System Requirements
- **SWI-Prolog** (version 8.4+ recommended)
- **Pyperplan** (STRIPS planner)
- **Python environment** with Pyperplan installed
- **PDDL 3.1** compatibility

---
### Usage
```prolog
% Ensure Pyperplan is accessible
% Load the game
?- [adventure_game].

% Start playing
?- start.
```

---

## Technical Challenges & Solutions

### Challenge 1: Prolog-PDDL Integration
**Problem**: External planner communication and state synchronization  
**Solution**: File-based interface with dynamic problem generation

### Challenge 2: Real-time Planning Performance
**Problem**: Planning latency affecting gameplay  
**Solution**: Asynchronous planning with fallback heuristics

---
### Challenge 3: Action Validation
**Problem**: Ensuring planner actions are legal in current world state  
**Solution**: Pre-execution validation using Prolog predicates

### Challenge 4: Fault Tolerance
**Problem**: Planner failures or "no solution" scenarios  
**Solution**: Graceful degradation to simple movement algorithms

---

## Live Demonstration Plan


- **1**: Introduction & game concept overview
- **2**: Technical walkthrough of Prolog implementation
- **3**: PDDL integration & AI adversary explanation

---
### Winning Run Demonstration
```
1. start.
2. take(torch). n. e. 
3. take(sword). open(chest1). take(key_fire).
4. w. n. attack. take(amulet).
*** Victory! (Automatic win detection) ***
```

---
### Losing Run Demonstration
```
Option 1: Direct capture
1. start. n. n. 
2. Chaser catches player at throne room
3. *** Game Over ***

Option 2: HP depletion
1. start. d. (enter pit)
2. Random damage events reduce HP to 0
3. *** You have perished ***
```

---
### Adversary Intelligence Showcase
```
Initial state: Chaser at throne, Thief at entrance
→ Player obtains amulet
→ Chaser switches from "get amulet" to "catch player"
→ Dynamic replanning demonstration
→ Thief attempts item theft
```

---

## Project Achievements

### Core Technical Achievements
1. **Complete PDDL Integration**: Successful STRIPS planner integration with Prolog
2. **Adaptive AI Opponents**: Adversaries that respond to player actions
3. **Robust Error Handling**: Fallback mechanisms for planning failures
4. **Multi-agent Coordination**: Two adversaries with complementary strategies

---
### Educational Value
- Practical implementation of knowledge representation concepts
- Demonstration of automated planning in game AI
- Integration of multiple AI techniques (logic programming + planning)

---

## Q&A Preparation

### Technical Questions
1. **"How do you handle the latency of external planner calls?"**
   - Asynchronous planning with game continuing during planning
   - Fallback to simple heuristics if planning takes too long

2. **"Why choose Pyperplan over other planners?"**
   - Lightweight, STRIPS-compliant, easy integration with Prolog
   - Suitable for educational purposes with clear output format

---
### Design Questions
3. **"What influenced your PDDL domain design choices?"**
   - Focus on STRIPS level for simplicity and reliability
   - Actions designed to reflect natural adversary behaviors

4. **"How did you test the PDDL integration?"**
   - Multiple testing methods (full game, direct planner calls)
   - Verification of action validity and goal achievement

---

## Conclusion & Future Work

### What We've Built
- A fully functional text adventure game with intelligent adversaries
- PDDL integration demonstrating automated planning concepts
- Dynamic game world with responsive AI opponents

---

### Final Thoughts
This project successfully demonstrates the integration of logic programming with automated planning, creating an engaging game experience while showcasing important knowledge representation concepts.

---

## Thank You!
**Group 9**: Chen Zhenlan | Dai Jun | Weng Zixuan  