				 %%%%%%%%%%%%%%%%%%%%%%%%%%%
				%                           %
				%                           %
				%	Test_strategy       %
				%                           %
				%                           %
				 %%%%%%%%%%%%%%%%%%%%%%%%%%%


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% Question 3 %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
test_strategy(N,FPS,SPS) :-
	statistics(walltime,[Start,_]),
	test_strategy_helper(N,FPS,SPS,WinL,MovesL),
	statistics(walltime,[End,_]),
	count(b,WinL,Nb),
	count(r,WinL,Nr),
	count(draw,WinL,Nd),
	max(MovesL,MaxL),
	min(MovesL,MinL),
	average(MovesL,AverageL),
	Time is End - Start,
	Average_time is Time/(N*1000),
	format('Number of blue = ~d~n', [Nb]),
	format('Number of red = ~d~n', [Nr]),
	format('Number of draws = ~d~n', [Nd]),
	format('Longest number of moves in a game = ~d~n', [MaxL]),
	format('Shortest number of moves in a game = ~d~n', [MinL]),
	format('Average game length = ~f~n', [AverageL]),
	format('Average game time ~f seconds.~n', [Average_time]).

%test_strategy(N,FPS,SPS,WinL,MovesL) returns a list MovesL of the number of moves to win and a list WinL of the winners of each of the N games
test_strategy_helper(N,FPS,SPS,WinL,MovesL) :-
	N = 0,
	WinL = [],
	MovesL = [].

test_strategy_helper(N,FPS,SPS,WinL,MovesL) :-
	N > 0,
	play(quiet,FPS,SPS,NumMoves,WinningPLayer),
	WinL = [WinningPLayer|T1],
	MovesL = [NumMoves|T2],
	L is N-1,
	test_strategy_helper(L,FPS,SPS,T1,T2).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% Helper functions for question 3 %%%%%%%%%%%%%%%%%%

%count number of elemets X in list L
count(X,L,N) :-
	findall(X,member(X,L),L1),
	length(L1,N).

%max(L,E) return the biggest element E of a list L
%base case
max(L,E) :-
	L = [H],
	E = H.

max(L,E) :-
	L = [X,Y|T],
	X > Y,
	L1 = [X|T],
	max(L1,E).

max(L,E) :-
	L = [X,Y|T],
	X =< Y,
	L1 = [Y|T],
	max(L1,E).

%min(L,E) return the biggest element E of a list L
%base case
min(L,E) :-
	L = [H],
	E = H.

min(L,E) :-
	L = [X,Y|T],
	X < Y,
	L1 = [X|T],
	min(L1,E).

min(L,E) :-
	L = [X,Y|T],
	X >= Y,
	L1 = [Y|T],
	min(L1,E).

%average(L,X) returns the average X of list L
average(L,X) :-
	sum(L,Y),
	length(L,N),
	X is Y/N.

%sum(L,Y) returns the sum Y of elements in list L
sum(L,Y) :-
	L = [],
	Y is 0.

sum(L,Y) :-
	L = [H|T],
	sum(T,Z),
	Y is Z+H.

				 %%%%%%%%%%%%%%%%%%%%%%%%%%%
				%                           %
				%                           %
				%	Strategies          %
				%                           %
				%                           %
				 %%%%%%%%%%%%%%%%%%%%%%%%%%%


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%	Auxiliary predicates
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%



%% strategy(Strategy, Player, Board, NewBoard, Move) %%
/* Auxiliary predicate, common to all strategies, generating all the moves
 before applying the best move (cf. strategy_best_move()) according to the given strategy */

strategy(Strategy, Player,Board,NewBoard,Move) :-
	determine_players(Player,Opponent,Board,Player_list),
	gen_all_moves(Player_list,Board,All_moves,[]),
	strategy_best_move(Strategy, All_moves,Player_list,Player,Opponent,Board,Move,_),
	alter_board(Move,Player_list,Update_player_list),
	rebuild_board(Update_player_list,Player,Board,NewBoard).


%% strategy_best_move(Strategy, All_moves, Player_list, Player, Opponent, Board, Move, Result) %%
/* Base case for strategy_best_move(), returning the only element of the list
 with its result calculated regarding a given strategy */

strategy_best_move(Strategy, All_moves,Player_list,Player,Opponent,Board,Move,Base_result):-
	All_moves = [Move],
	strategy_result(Strategy, Move,Player_list,Player,Opponent,Board,Base_result),
	Base_move = Move.


%% strategy_result(Strategy, Move,Player_list,Player,Opponent,Board,Next_result) %%
/* specific to each strategy  */






%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% Bloodlust %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%bloodlust(Player,Board,NewBoard,Move) returns the move from Player that produces the board state
%with the fewest number of opponent's pieces
bloodlust(Player,Board,NewBoard,Move) :-
	strategy(bloodlust, Player,Board,NewBoard,Move).

strategy_best_move(bloodlust, All_moves,Player_list,Player,Opponent,Board,Move,Base_result):-
	All_moves = [H|T],
	strategy_best_move(Strategy, T,Player_list,Player,Opponent,Board,Base_move,Base_result),
	strategy_result(Strategy, H,Player_list,Player,Opponent,Board,Next_result),
	((Base_result =< Next_result,
	  Result = Base_result,
	  Move = Base_move)
	   ;
	    (Base_result > Next_result,
	     Result = Next_result,
	     Move = H)).

strategy_result(bloodlust, One_move,Player_list,Player,Opponent,Board,Next_result):-
	alter_board(One_move,Player_list,Update_player_list),
	rebuild_board(Update_player_list,Player,Board,Update_board),
	next_generation(Update_board,Twist_board),
	count_result(Twist_board,Next_result,Opponent).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% Self Preservation %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%self_preservation(Player,Board,NewBoard,Move) returns the move from Player that produces the board state
%with the largest number of player's pieces
self_preservation(Player,Board,NewBoard,Move) :-
	strategy(self_preservation, Player,Board,NewBoard,Move).

strategy_best_move(self_preservation, All_moves,Player_list,Player,Opponent,Board,Move,Result) :-
	All_moves = [H|T],
	strategy_best_move(self_preservation, T,Player_list,Player,Opponent,Board,Base_move,Base_result),
	strategy_result(self_preservation, H,Player_list,Player,Opponent,Board,Next_result),
	((Base_result >= Next_result,
	  Result = Base_result,
	  Move = Base_move)
	   ;
	    (Base_result < Next_result,
	     Result = Next_result,
	     Move = H)).

strategy_result(self_preservation, One_move,Player_list,Player,Opponent,Board,Next_result) :-
	alter_board(One_move,Player_list,Update_player_list),
	rebuild_board(Update_player_list,Player,Board,Update_board),
	next_generation(Update_board,Twist_board),
	count_result(Twist_board,Next_result,Player).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% Land Grab %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%land_grab(Player,Board,NewBoard,Move) returns the move from Player that produces the board state
%with the largest difference between number of player's pieces and opponent's pieces.
land_grab(Player,Board,NewBoard,Move) :-
	strategy(land_grab, Player,Board,NewBoard,Move).

strategy_best_move(land_grab, All_moves,Player_list,Player,Opponent,Board,Move,Result) :-
	All_moves = [H|T],
	strategy_best_move(land_grab, T,Player_list,Player,Opponent,Board,Base_move,Base_result),
	strategy_result(land_grab, H,Player_list,Player,Opponent,Board,Next_result),
	((Base_result >= Next_result,
	  Result = Base_result,
	  Move = Base_move)
	   ;
	    (Base_result < Next_result,
	     Result = Next_result,
	     Move = H)).

strategy_result(land_grab, One_move,Player_list,Player,Opponent,Board,Next_result) :-
	alter_board(One_move,Player_list,Update_player_list),
	rebuild_board(Update_player_list,Player,Board,Update_board),
	next_generation(Update_board,Twist_board),
	count_result(Twist_board,Player_pieces,Player),
	count_result(Twist_board,Opponent_pieces,Opponent),
	Next_result = Player_pieces - Opponent_pieces.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% Minimax %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%minimax(Player,Board,NewBoard,Move) returns the move from Player that produces the board state
%with the fewest number of opponent's pieces
minimax(Player,Board,NewBoard,Move) :-
	strategy(minimax, Player,Board,NewBoard,Move).

strategy_best_move(minimax, All_moves,Player_list,Player,Opponent,Board,Move,Result) :-
	All_moves = [H|T],
	strategy_best_move(minimax, T,Player_list,Player,Opponent,Board,Base_move,Base_result),
	strategy_result(minimax, H,Player_list,Player,Opponent,Board,Next_result),
	((Base_result =< Next_result,
	  Result = Base_result,
	  Move = Base_move)
	   ;
	    (Base_result > Next_result,
	     Result = Next_result,
	     Move = H)).

strategy_result(minimax, One_move,Player_list,Player,Opponent,Board,Next_result) :-
	alter_board(One_move,Player_list,Update_player_list),
	rebuild_board(Update_player_list,Player,Board,Update_board),
	next_generation(Update_board,Twist_board),
	switch_players(Player,Twist_board,Opponent,Opponent_list),
	gen_all_moves(Opponent_list,Board,All_opponent_moves,[]),
	length(All_opponent_moves,L),
	((L > 0,
	  land_grab(Opponent,Twist_board,Next_board,_),
	  next_generation(Next_board,Next_twist_board),
	  count_result(Next_twist_board,Player_pieces,Opponent),
	  count_result(Next_twist_board,Opponent_pieces,Player),
	  Next_result is Player_pieces - Opponent_pieces)
	      ;
	  L = 0,
	  Next_result is -100).






%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% Helper functions for part 2 %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%determine_players(Player,Board,Player_list) returns the list of initial positions of Player
determine_players(Player,Opponent,Board,Player_list) :-
	Player = b,
	Opponent = r,
	Board = [Blist,_],
	Player_list = Blist.

determine_players(Player,Opponent,Board,Player_list) :-
	Player = r,
	Opponent = b,
	Board = [_,Rlist],
	Player_list = Rlist.

%switch_players(Player,Board,Opponent,Opponent_list) returns the list of initial positions of Opponent
switch_players(Player,Board,Opponent,Opponent_list) :-
	Player = b,
	Opponent = r,
	Board = [_,Rlist],
	Opponent_list = Rlist.

switch_players(Player,Board,Opponent,Opponent_list) :-
	Player = r,
	Opponent = b,
	Board = [Blist,_],
	Opponent_list = Blist.


%gen_all_moves(player,board,All_moves) generates a list All_moves that contain all possible moves of Player
gen_all_moves(Player_list,Board,All_moves,Empty_list) :-
	Player_list = [],
	All_moves = Empty_list.

gen_all_moves(Player_list,Board,All_moves,Empty_list) :-
	Player_list = [H|T],
	findall(Each_move, moves_per_piece(H,Board,Each_move), Sublist),
	append(Empty_list,Sublist,New_list),
	gen_all_moves(T,Board,All_moves,New_list).

						%move_per_piece(One_location,Board,One_move) returns the coordinates of a possible move starting at coordinates One_location
moves_per_piece(One_location,Board,One_move) :-
	Board = [Blist,Rlist],
	One_location = [R1,C1],
	((R2 is R1-1, C2 is C1+1)
        ;(R2 is R1, C2 is C1+1)
        ;(R2 is R1+1, C2 is C1+1)
        ;(R2 is R1+1, C2 is C1)
        ;(R2 is R1+1, C2 is C1-1)
        ;(R2 is R1, C2 is C1-1)
        ;(R2 is R1-1, C2 is C1-1)
        ;(R2 is R1-1, C2 is C1)),
	Sub_move = [R2,C2],
	R2 > 0,
	C2 > 0,
	R2 < 8,
	C2 < 8,
	\+member(Sub_move,Rlist),
	\+member(Sub_move,Blist),
	append(One_location,Sub_move,One_move).

%rebuild_board(Update_player_list,Player,Board,Update_board) 
rebuild_board(Update_player_list,Player,Board,Update_board) :-
	Player = b,
	Board = [_,Rlist],
	Update_board = [Update_player_list,Rlist].

rebuild_board(Update_player_list,Player,Board,Update_board) :-
	Player = r,
	Board = [Blist,_],
	Update_board = [Blist,Update_player_list].


%count_result for player b
count_result(Twist_board,Num_sub_move,Player) :-
	Player = b,
	Twist_board = [Blist,_],
	length(Blist,Num_sub_move).

count_result(Twist_board,Num_sub_move,Player) :-
	Player = r,
	Twist_board = [_,Rlist],
	length(Rlist,Num_sub_move).
