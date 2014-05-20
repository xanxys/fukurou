A shogi AI with CUI that can beat [hamu-shogi](http://www.hozo.biz/shogi/).
Which means fukurou is at a very weak (starter) amature level.


Implementation
-------------------------------------------------------------------------------------------------------
* Iterative depth first search
* Negamax with alpha-beta pruning and pruning w/ transposition table (Zobrist hashing)
* Very simple evaluation function: hard-coded combination of # of pieces of each type and # of legal plays
* Shallow move-reordering (considering promoting moves first, etc.)
