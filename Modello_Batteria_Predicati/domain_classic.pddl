;; domain_classic.pddl
(define (domain lawnmower_classic)
  (:requirements :strips :typing)

  (:types robot cell)

  (:predicates
    (at ?r - robot ?c - cell)
    (has-grass ?c - cell)
    (obstacle ?c - cell)
    (charging-station ?c - cell)
    (adjacent ?from - cell ?to - cell)
    (battery-full ?r - robot)
    (battery-mid ?r - robot)
    (battery-low ?r - robot)
  )

  (:action move
    :parameters (?r - robot ?from - cell ?to - cell)
    :precondition (and (at ?r ?from) (adjacent ?from ?to) (not (obstacle ?to)))
    :effect (and
      (not (at ?r ?from))
      (at ?r ?to)
    )
  )

  (:action cut-from-full
    :parameters (?r - robot ?c - cell)
    :precondition (and (at ?r ?c) (has-grass ?c) (battery-full ?r))
    :effect (and
      (not (has-grass ?c))
      (not (battery-full ?r))
      (battery-mid ?r)
    )
  )

  (:action cut-from-mid
    :parameters (?r - robot ?c - cell)
    :precondition (and (at ?r ?c) (has-grass ?c) (battery-mid ?r))
    :effect (and
      (not (has-grass ?c))
      (not (battery-mid ?r))
      (battery-low ?r)
    )
  )

  (:action charge-from-low
    :parameters (?r - robot ?c - cell)
    :precondition (and (at ?r ?c) (charging-station ?c) (battery-low ?r))
    :effect (and
      (not (battery-low ?r))
      (battery-mid ?r)
    )
  )

  (:action charge-from-mid
    :parameters (?r - robot ?c - cell)
    :precondition (and (at ?r ?c) (charging-station ?c) (battery-mid ?r))
    :effect (and
      (not (battery-mid ?r))
      (battery-full ?r)
    )
  )
)
