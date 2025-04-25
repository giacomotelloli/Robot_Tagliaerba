(define (domain lawnmower_classic)
  (:requirements :strips :typing :numeric-fluents)

  (:types robot cell)

  (:predicates
    (at ?r - robot ?c - cell)
    (has-grass ?c - cell)
    (obstacle ?c - cell)
    (charging-station ?c - cell)
    (adjacent ?from - cell ?to - cell)
  )

  (:functions
    (battery-level ?r - robot) - number
  )

  (:action move
    :parameters (?r - robot ?from - cell ?to - cell)
    :precondition (and
      (at ?r ?from)
      (adjacent ?from ?to)
      (not (obstacle ?to))
      (>= (battery-level ?r) 1)
    )
    :effect (and
      (not (at ?r ?from))
      (at ?r ?to)
      (decrease (battery-level ?r) 1)
    )
  )

  (:action cut-grass
    :parameters (?r - robot ?c - cell)
    :precondition (and
      (at ?r ?c)
      (has-grass ?c)
      (>= (battery-level ?r) 2)
    )
    :effect (and
      (not (has-grass ?c))
      (decrease (battery-level ?r) 2)
    )
  )

  (:action charge
    :parameters (?r - robot ?c - cell)
    :precondition (and
      (at ?r ?c)
      (charging-station ?c)
    )
    :effect (assign (battery-level ?r) 10)
  )
)
