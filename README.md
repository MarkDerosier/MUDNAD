# MUDNAD
This is a work in progress. 

This is utilizing Cloud Haskell for decoupling of components, the Lobby (os) process is separate from the Game engine (os) process, they are on different Cloud Haskell nodes. (Erlang style actor model).

This allows for restarting the game engine without losing client connections. 


Redis is used for player accounts, although this is a bit of legacy cruft, a relation database would work much better for this. 

game components still need to be implemented, notably, time (the engine is event driven), and space (voxel). 
