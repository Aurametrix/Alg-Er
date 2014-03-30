%% searching through different angles to find a clear path

-module(graph).
-compile(export_all).

-import(rover, [radians_to_degrees/1, distance/4]).

-include("rld.hrl").

in_path(X,Y,Dir,{m,Mx,My,_,_}) ->		% martians, they move
    in_path(X,Y,Dir,{a,Mx,My,0.4,0});
in_path(X,Y,Dir,{_Type,Ox,Oy,R,_}) ->		% craters and boulders
    A = math:atan2(Oy-Y, Ox-X),			% angle to in radians
    T = math:atan2(R, rover:distance(X,Y,Ox,Oy)),    
    B = (Dir =< A + T) and (Dir >= A - T),    
    %%io:format("~p, Dir = ~p, A = ~p, T = ~p~n", [B, Dir, A, T]),
    B.

d2r(D) when D > 180 -> d2r(360 - D);
d2r(D) -> (D * math:pi()) / 180.

is_clear(_W, _Dir, []) -> true;
is_clear(W, Dir, [H|T]) ->
    case in_path(W#rld.x, W#rld.y, d2r(Dir), H) of
	true -> false;
	false -> is_clear(W,Dir,T)
    end.


add_deg(A,B) ->
    if (A + B) < 0 -> 360 + (A + B);
       (A + B) > 360 -> (A + B) - 360;
       true -> A + B
    end.


search(_W, BestAngle, 180) ->			% no clear paths
    % io:format("nothing~n", []), 
    BestAngle;
search(W, BestAngle, Offset) ->
    case is_clear(W, add_deg(BestAngle, Offset), W#rld.objects) of
	true -> add_deg(BestAngle, Offset);
	false -> case is_clear(W, add_deg(BestAngle, - Offset), W#rld.objects) of
		     true -> add_deg(BestAngle, - Offset);
		     false -> search(W, BestAngle, Offset + 2)
		 end
    end.
			 

best_path(W, BestAngle) ->
    D = search(W, BestAngle, 0),
    % io:format("best = ~p, clean = ~p~n", [BestAngle, D]),
    D.
    
