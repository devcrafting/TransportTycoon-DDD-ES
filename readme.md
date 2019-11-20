# Transport Tycoon DDD kata implementation in F\#

## First try

I tried [this kata](https://github.com/Softwarepark/exercises/blob/master/transport-tycoon.md) at [SocratesBE19](https://socratesbe.org).

We first had a 1h mob programming session to discover the subject, trying to follow rules given in exercise 1. We tried a classic TDD approach (with F#), leading to an implementation where we were doing maths tight to the given network, we did not have time to let emerge domain concepts (other than in naming variables).

[Mathias](https://twitter.com/mathiasverraes) and [Thomas](https://twitter.com/tcoopman) paired with a different approach (using Haskell), trying to build a type system first (Type Driven Development in mind I presume) to represent state of the world, splitting the problem with 1 hour slices. They did not have time neither to do more than a first type system, but it stimulated me to give it a try ;). We gave it another try in Haskell with Mathias with lots of interesting questions about how to model the right type.

By the way, I don't know which heuristic I could use to start with either approach. Any thoughts welcome [@clem_bouillier](https://twitter.com/clem_bouillier) ;).

## My Type Driven Development try

To handle the temporal model induced by this kata, I followed the proposal of Thomas and Mathias: from an initial state of the world, I needed a function to calculate the next state, and with recursivity, I would be able to know how long it takes to deliver all requested cargos.

So I tried first defining types that define the initial state of the world with main domain concepts: (moving) transports, (moving) cargos and stocks in some location (mainly initial request in factory, then some buffer in port, plus deliveries at destination warehouses A and B).

The main idea was that I did not intend to describe the world the best I can to map with reality. Writing examples of states at different point in time helped me to identify what was useful in the model of the world. For example, questions that arise discussing with Mathias were: Should we make Cargo know where it is ? Isn't it an entity rather than a value object ? What would be the boundaries between objects/aggregates if we had a larger network ?

In my implementation, I finally decided to do the simplest model I can, where Cargo are in fact just a destination Location, being stocked in Location, then loaded in waiting Tranport having a Position, Tranport moving around between Location, unloading Cargo when arriving and going back immediately. Each hour is then a try to sequentially load/move/unload each Transport (sharing stocks).

## Exercice 2 - logs that look like an event stream ;)

NB: I did Exercice 1 while Exercice 2 was revealed, but I promise I did not change my design after having a look at Exercice 2 ;). My first attempt was done trying to avoid Event Sourcing, but considering the functional way of designing types, we often have a function that take the previous state (encoded with state) and return the new one. This lead to a solution where adding events is not hard, instead of State -> Command -> State, you could have an intermediate `State -> Command -> Events` ("decide" function) + `Events -> State` ("project" function). I haven't been up to this solution for now: the two functions are still mixed (State -> Command -> State + accumulate Events).

So it was quite easy to add events, each branch of pattern matching from my load/move/unload functions. I just added an History to my world to get the events of the domain.

Note I had to add id for Transport, but did not really need to add id on Cargo for now (but having source hard coded).

I kept separated the log writing of my events from their domain definition.
