;; instance2_classic.pddl (Media)
(define (problem instance2_classic)
  (:domain lawnmower_classic)
  (:objects
    robot1 - robot
    c11 c12 c13 c14 c15 c16
    c21 c22 c23 c24 c25 c26
    c31 c32 c33 c34 c35 c36
    c41 c42 c43 c44 c45 c46
    c51 c52 c53 c54 c55 c56
    c61 c62 c63 c64 c65 c66 - cell
  )
  (:init
    (at robot1 c11)
    (charging-station c11)
    (battery-full robot1)
    (obstacle c24) (obstacle c44)
    ;; tutte le celle senza ostacoli hanno erba
    (has-grass c11) (has-grass c12) (has-grass c13) (has-grass c14) (has-grass c15) (has-grass c16)
    (has-grass c21) (has-grass c22) (has-grass c23) (has-grass c25) (has-grass c26)
    (has-grass c31) (has-grass c32) (has-grass c33) (has-grass c34) (has-grass c35) (has-grass c36)
    (has-grass c41) (has-grass c42) (has-grass c43) (has-grass c45) (has-grass c46)
    (has-grass c51) (has-grass c52) (has-grass c53) (has-grass c54) (has-grass c55) (has-grass c56)
    (has-grass c61) (has-grass c62) (has-grass c63) (has-grass c64) (has-grass c65) (has-grass c66)
    ;; Adiacenze bidirezionali per griglia 6x6
    ;; Riga per riga (orizzontale)
    (adjacent c11 c12) (adjacent c12 c11) (adjacent c12 c13) (adjacent c13 c12) (adjacent c13 c14) (adjacent c14 c13) (adjacent c14 c15) (adjacent c15 c14) (adjacent c15 c16) (adjacent c16 c15)
    (adjacent c21 c22) (adjacent c22 c21) (adjacent c22 c23) (adjacent c23 c22) (adjacent c23 c24) (adjacent c24 c23) (adjacent c24 c25) (adjacent c25 c24) (adjacent c25 c26) (adjacent c26 c25)
    (adjacent c31 c32) (adjacent c32 c31) (adjacent c32 c33) (adjacent c33 c32) (adjacent c33 c34) (adjacent c34 c33) (adjacent c34 c35) (adjacent c35 c34) (adjacent c35 c36) (adjacent c36 c35)
    (adjacent c41 c42) (adjacent c42 c41) (adjacent c42 c43) (adjacent c43 c42) (adjacent c43 c44) (adjacent c44 c43) (adjacent c44 c45) (adjacent c45 c44) (adjacent c45 c46) (adjacent c46 c45)
    (adjacent c51 c52) (adjacent c52 c51) (adjacent c52 c53) (adjacent c53 c52) (adjacent c53 c54) (adjacent c54 c53) (adjacent c54 c55) (adjacent c55 c54) (adjacent c55 c56) (adjacent c56 c55)
    (adjacent c61 c62) (adjacent c62 c61) (adjacent c62 c63) (adjacent c63 c62) (adjacent c63 c64) (adjacent c64 c63) (adjacent c64 c65) (adjacent c65 c64) (adjacent c65 c66) (adjacent c66 c65)
    ;; Colonna per colonna (verticale)
    (adjacent c11 c21) (adjacent c21 c11) (adjacent c21 c31) (adjacent c31 c21) (adjacent c31 c41) (adjacent c41 c31) (adjacent c41 c51) (adjacent c51 c41) (adjacent c51 c61) (adjacent c61 c51)
    (adjacent c12 c22) (adjacent c22 c12) (adjacent c22 c32) (adjacent c32 c22) (adjacent c32 c42) (adjacent c42 c32) (adjacent c42 c52) (adjacent c52 c42) (adjacent c52 c62) (adjacent c62 c52)
    (adjacent c13 c23) (adjacent c23 c13) (adjacent c23 c33) (adjacent c33 c23) (adjacent c33 c43) (adjacent c43 c33) (adjacent c43 c53) (adjacent c53 c43) (adjacent c53 c63) (adjacent c63 c53)
    (adjacent c14 c24) (adjacent c24 c14) (adjacent c24 c34) (adjacent c34 c24) (adjacent c34 c44) (adjacent c44 c34) (adjacent c44 c54) (adjacent c54 c44) (adjacent c54 c64) (adjacent c64 c54)
    (adjacent c15 c25) (adjacent c25 c15) (adjacent c25 c35) (adjacent c35 c25) (adjacent c35 c45) (adjacent c45 c35) (adjacent c45 c55) (adjacent c55 c45) (adjacent c55 c65) (adjacent c65 c55)
    (adjacent c16 c26) (adjacent c26 c16) (adjacent c26 c36) (adjacent c36 c26) (adjacent c36 c46) (adjacent c46 c36) (adjacent c46 c56) (adjacent c56 c46) (adjacent c56 c66) (adjacent c66 c56)
  )
  (:goal
    (and
      (not (has-grass c11)) (not (has-grass c12)) (not (has-grass c13)) (not (has-grass c14)) (not (has-grass c15)) (not (has-grass c16))
      (not (has-grass c21)) (not (has-grass c22)) (not (has-grass c23)) (not (has-grass c25)) (not (has-grass c26))
      (not (has-grass c31)) (not (has-grass c32)) (not (has-grass c33)) (not (has-grass c34)) (not (has-grass c35)) (not (has-grass c36))
      (not (has-grass c41)) (not (has-grass c42)) (not (has-grass c43)) (not (has-grass c45)) (not (has-grass c46))
      (not (has-grass c51)) (not (has-grass c52)) (not (has-grass c53)) (not (has-grass c54)) (not (has-grass c55)) (not (has-grass c56))
      (not (has-grass c61)) (not (has-grass c62)) (not (has-grass c63)) (not (has-grass c64)) (not (has-grass c65)) (not (has-grass c66))
    )
  )
)
