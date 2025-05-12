/*  

  Tagliaerba Robot domain for INDIGOLOG

*/


:- dynamic controller/1.
:- discontiguous
    fun_fluent/1,
    rel_fluent/1,
    proc/2,
    causes_true/3,
    causes_false/3.


% There is nothing to do caching on (required becase cache/1 is static)
cache(_) :- fail.


/* PREDICATES */

/* define the grass cells */
cell(c11). cell(c12). cell(c13). cell(c14). cell(c15). cell(c16).
cell(c21). cell(c22). cell(c23). cell(c24). cell(c25). cell(c26).
cell(c31). cell(c32). cell(c33). cell(c34). cell(c35). cell(c36).
cell(c41). cell(c42). cell(c43). cell(c44). cell(c45). cell(c46).
cell(c51). cell(c52). cell(c53). cell(c54). cell(c55). cell(c56).
cell(c61). cell(c62). cell(c63). cell(c64). cell(c65). cell(c66).

/* define the horizontal bidirectional connections */
connected(c11,c12). connected(c12,c11).
connected(c12,c13). connected(c13,c12).
connected(c13,c14). connected(c14,c13).
connected(c14,c15). connected(c15,c14).
connected(c15,c16). connected(c16,c15).

connected(c21,c22). connected(c22,c21).
connected(c22,c23). connected(c23,c22).
connected(c23,c24). connected(c24,c23).
connected(c24,c25). connected(c25,c24).
connected(c25,c26). connected(c26,c25).

connected(c31,c32). connected(c32,c31).
connected(c32,c33). connected(c33,c32).
connected(c33,c34). connected(c34,c33).
connected(c34,c35). connected(c35,c34).
connected(c35,c36). connected(c36,c35).

connected(c41,c42). connected(c42,c41).
connected(c42,c43). connected(c43,c42).
connected(c43,c44). connected(c44,c43).
connected(c44,c45). connected(c45,c44).
connected(c45,c46). connected(c46,c45).

connected(c51,c52). connected(c52,c51).
connected(c52,c53). connected(c53,c52).
connected(c53,c54). connected(c54,c53).
connected(c54,c55). connected(c55,c54).
connected(c55,c56). connected(c56,c55).

connected(c61,c62). connected(c62,c61).
connected(c62,c63). connected(c63,c62).
connected(c63,c64). connected(c64,c63).
connected(c64,c65). connected(c65,c64).
connected(c65,c66). connected(c66,c65).

/* define the vertical bidirectional connections */

connected(c11,c21). connected(c21,c11).
connected(c21,c31). connected(c31,c21).
connected(c31,c41). connected(c41,c31).
connected(c41,c51). connected(c51,c41).
connected(c51,c61). connected(c61,c51).

connected(c12,c22). connected(c22,c12).
connected(c22,c32). connected(c32,c22).
connected(c32,c42). connected(c42,c32).
connected(c42,c52). connected(c52,c42).
connected(c52,c62). connected(c62,c52).

connected(c13,c23). connected(c23,c13).
connected(c23,c33). connected(c33,c23).
connected(c33,c43). connected(c43,c33).
connected(c43,c53). connected(c53,c43).
connected(c53,c63). connected(c63,c53).

connected(c14,c24). connected(c24,c14).
connected(c24,c34). connected(c34,c24).
connected(c34,c44). connected(c44,c34).
connected(c44,c54). connected(c54,c44).
connected(c54,c64). connected(c64,c54).

connected(c15,c25). connected(c25,c15).
connected(c25,c35). connected(c35,c25).
connected(c35,c45). connected(c45,c35).
connected(c45,c55). connected(c55,c45).
connected(c55,c65). connected(c65,c55).

connected(c16,c26). connected(c26,c16).
connected(c26,c36). connected(c36,c26).
connected(c36,c46). connected(c46,c36).
connected(c46,c56). connected(c56,c46).
connected(c56,c66). connected(c66,c56).


/* other predicates */
obstacle(c33).
chargingStation(c11).

/* FLUENTS */
rel_fluent(hasGrass(C)) :- cell(C).
rel_fluent(robotAt(C)) :- cell(C).
fun_fluent(batteryLevel).
rel_fluent(isRaining).
rel_fluent(change_weather).
rel_fluent(some_changes).

/* ACTIONS */

/* movement action */
prim_action(move(C1,C2)) :- cell(C1) , cell(C2).
poss(move(C1,C2),
  and(and(and(and(robotAt(C1),neg(obstacle(C2))),connected(C1,C2)),batteryLevel > 10),neg(isRaining))
).


/* cutting action */
prim_action(cutGrass(C1)) :- cell(C1).
poss(cutGrass(C1),and(and(and(robotAt(C1),hasGrass(C1)),batteryLevel > 10),neg(isRaining))).

/* return to charging station action */
prim_action(goToCharge) .
poss(goToCharge,or(batteryLevel =< 10,isRaining)).

prim_action(Act) :- exog_action(Act).
poss(Act, true) :- exog_action(Act).

/* EXOGENOUS ACTIONS */ 
exog_action(toggle_rain).
poss(toggle_rain,
  neg(isRaining)
).

/* SUCCESSOR STATE AXIOMS */ 
causes_false(cutGrass,hasGrass,true).
causes_val(cutGrass(_),batteryLevel,N,N is batteryLevel-2).

causes_false(move(C1,_),robotAt(C1),cell(C1)).
causes_true(move(_,C2),robotAt(C2),cell(C2)).
causes_val(move(_,_),batteryLevel,N, N is batteryLevel-1).

causes_true(goToCharge,robotAt(C),chargingStation(C)).
causes_false(goToCharge,robotAt(C2), neg(chargingStation(C2))).
causes_val(goToCharge,batteryLevel,50,true).

causes_true(toggle_rain,isRaining,neg(isRaining)).
causes_false(toggle_rain,isRaining,isRaining).
causes_true(toggle_rain, some_changes, true).

/* INITIAL STATE */
initially(batteryLevel,100).
initially(robotAt(C),true) :- cell(C),member(C,[c11]).
initially(hasGrass(c11), false).
initially(hasGrass(c12), true).
initially(hasGrass(c13), true).
initially(hasGrass(c14), true).
initially(hasGrass(c15), true).
initially(hasGrass(c16), true).
initially(hasGrass(c21), true).
initially(hasGrass(c22), true).
initially(hasGrass(c23), true).
initially(hasGrass(c24), true).
initially(hasGrass(c25), true).
initially(hasGrass(c26), true).
initially(hasGrass(c31), true).
initially(hasGrass(c32), true).
initially(hasGrass(c33), false).
initially(hasGrass(c34), true).
initially(hasGrass(c35), true).
initially(hasGrass(c36), true).
initially(hasGrass(c41), true).
initially(hasGrass(c42), true).
initially(hasGrass(c43), true).
initially(hasGrass(c44), true).
initially(hasGrass(c45), true).
initially(hasGrass(c46), true).
initially(hasGrass(c51), true).
initially(hasGrass(c52), true).
initially(hasGrass(c53), true).
initially(hasGrass(c54), true).
initially(hasGrass(c55), true).
initially(hasGrass(c56), true).
initially(hasGrass(c61), true).
initially(hasGrass(c62), true).
initially(hasGrass(c63), true).
initially(hasGrass(c64), true).
initially(hasGrass(c65), true).
initially(hasGrass(c66), true).
initially(isRaining,false).

/* REACTIVE CONTROLLER */ 


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%  INFORMATION FOR THE EXECUTOR
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Translations of domain actions to real actions (one-to-one)
actionNum(X, X).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% EOF
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

proc(cut_all_full,
  [  
    move(c11, c12),
    cutGrass(c12),
    move(c12, c13),
    cutGrass(c13),
    move(c13, c14),
    cutGrass(c14),
    move(c14, c15),
    cutGrass(c15),
    move(c15, c16),
    cutGrass(c16),
    move(c16, c26),
    cutGrass(c26),
    move(c26, c25),
    cutGrass(c25),
    move(c25, c24),
    cutGrass(c24),
    move(c24, c23),
    cutGrass(c23),
    move(c23, c22),
    cutGrass(c22),
    move(c22, c21),
    cutGrass(c21),
    move(c21, c31),
    cutGrass(c31),
    move(c31, c32),
    cutGrass(c32),
    move(c32, c33),
    cutGrass(c33),
    move(c33, c34),
    cutGrass(c34),
    move(c34, c35),
    cutGrass(c35),
    move(c35, c36),
    cutGrass(c36),
    move(c36, c46),
    cutGrass(c46),
    move(c46, c45),
    cutGrass(c45),
    move(c45, c44),
    cutGrass(c44),
    move(c44, c43),
    cutGrass(c43),
    move(c43, c42),
    cutGrass(c42),
    move(c42, c41),
    cutGrass(c41),
    move(c41, c51),
    cutGrass(c51),
    move(c51, c52),
    cutGrass(c52),
    move(c52, c53),
    cutGrass(c53),
    move(c53, c54),
    cutGrass(c54),
    move(c54, c55),
    cutGrass(c55),
    move(c55, c56),
    cutGrass(c56),
    move(c56, c66),
    cutGrass(c66),
    move(c66, c65),
    cutGrass(c65),
    move(c65, c64),
    cutGrass(c64),
    move(c64, c63),
    cutGrass(c63),
    move(c63, c62),
    cutGrass(c62),
    move(c62, c61),
    cutGrass(c61)
  ]
).

proc(pi_move,
  pi([from,to],if(and(robotAt(from),connected(from,to)),
      move(from,to),
      no_op)
  )
).

proc(pi_cutGrass,
  pi([c],if(hasGrass(c),cutGrass(c),no_op))
).

proc(cut_all,
  star([
    pi_move,
    pi_cutGrass
  ])
).

proc(cut_all_pi,
    pi([From,To],
      if(
        and(robotAt(From),and(connected(From,To),hasGrass(To))),
        [move(From,To),cutGrass(To)],
        no_op
      )
    )
).

proc(wait_until_not_rain,
  while(isRaining,[no_op])
).

proc(full_search, [
  if(isRaining,goToCharge,[]),
  wait_until_not_rain,
  cut_all
]).

proc(control(reactive_cut), [
  prioritized_interrupts([
    interrupt(true, [
      if(some_changes, unset(some_changes), []),
      gexec(neg(some_changes), full_search)
    ])
  ])
]).
