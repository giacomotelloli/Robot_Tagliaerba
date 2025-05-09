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
poss(goToCharge,batteryLevel =< 10).

prim_action(Act) :- exog_action(Act).
poss(Act, true) :- exog_action(Act).

/* EXOGENOUS ACTIONS */ 
exog_action(toggle_rain).

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

/* INITIAL STATE */
initially(batteryLevel,12).
initially(robotAt(C),true) :- cell(C),member(C,[c11]).
initially(hasGrass(C),true):- cell(C) , \+ obstacle(C) , \+ chargingStation(C).
initially(isRaining,false).

/* REACTIVE CONTROLLER */ 
proc(cut_all,
  star(
    pi([From, To],
      if(
        and(robotAt(From), and(hasGrass(To), connected(From, To))),
        [move(From, To), cutGrass(To)],
        no_op
      )
    )
  )
).


proc(control(reactive_cut),
  prioritized_interrupts([
    interrupt(isRaining, waitFor(neg(isRaining))),
    interrupt(batteryLevel =< 10, [goToCharge, waitFor(batteryLevel > 10)])
  ]),
    cut_all
  ]
).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%  INFORMATION FOR THE EXECUTOR
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Translations of domain actions to real actions (one-to-one)
actionNum(X, X).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% EOF
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
