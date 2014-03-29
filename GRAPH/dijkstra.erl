dijkstra(Graph,Start_node_name) ->

    io:format("dijkstra/2: start~n"),

    Paths = dict:new(),
    io:format("dijkstra/2: initialized empty Paths~n"),

    Unvisited = gb_sets:new(),
    io:format("dijkstra/2: initialized empty Unvisited nodes priority queue~n"),

    Unvisited_nodes = gb_sets:insert({0,Start_node_name,root},Unvisited),
    io:format("dijkstra/2: Added start node ~w with the weight 0 to the Unvisited nodes: ~w~n", [Start_node_name, Unvisited_nodes]),

    Paths_updated = loop_through_nodes(Graph,Paths,Unvisited_nodes),
    io:format("dijkstra/2: Finished searching for shortest paths: ~w~n", [Paths_updated]).




loop_through_nodes(Graph,Paths,Unvisited_nodes) ->
    %% We need this condition to stop looping through the Unvisited nodes if it is empty
    case gb_sets:is_empty(Unvisited_nodes) of
        false -> 
            {{Current_weight,Current_name,Previous_node}, Unvisited_nodes_updated} = gb_sets:take_smallest(Unvisited_nodes),
            case dict:is_key(Current_name,Paths) of
                false ->
                    io:format("loop_through_nodes: Found a new smallest unvisited node ~w~n",[Current_name]),

                    Paths_updated = dict:store(Current_name,{Previous_node,Current_weight},Paths),
                    io:format("loop_through_nodes: Updated Paths: ~w~n",[Paths_updated]),

                    Out_edges = digraph:out_edges(Graph,Current_name),
                    io:format("loop_through_nodes: Ready to iterate through the out edges of node ~w: ~w~n",[Current_name,Out_edges]),

                    Unvisited_nodes_updated_2 = loop_through_edges(Graph,Out_edges,Paths_updated,Unvisited_nodes_updated,Current_weight),
                    io:format("loop_through_nodes: Looped through out edges of the node ~w and updated Unvisited nodes: ~w~n",[Current_name,Unvisited_nodes_updated_2]),

                    loop_through_nodes(Graph,Paths_updated,Unvisited_nodes_updated_2);
                    true ->
                    loop_through_nodes(Graph,Paths,Unvisited_nodes_updated)
            end;
        true -> 
            Paths
    end.

loop_through_edges(Graph,[],Paths,Unvisited_nodes,Current_weight) ->
                    io:format("loop_through_edges: No more out edges ~n"),
    Unvisited_nodes;

loop_through_edges(Graph,Edges,Paths,Unvisited_nodes,Current_weight) ->
                    io:format("loop_through_edges: Start ~n"),
    [Current_edge|Rest_edges] = Edges,
    {Current_edge,Current_node,Neighbour_node,Edge_weight} = digraph:edge(Graph,Current_edge),
    case dict:is_key(Neighbour_node,Paths) of
        false ->
                    io:format("loop_through_edges: Inserting new neighbour node ~w into Unvisited nodes~n",[Current_node]),
            Unvisited_nodes_updated = gb_sets:insert({Current_weight+Edge_weight,Neighbour_node,Current_node},Unvisited_nodes),
                    io:format("loop_through_edges: The unvisited nodes are: ~w~n",[Unvisited_nodes_updated]),
            loop_through_edges(Graph,Rest_edges,Paths,Unvisited_nodes_updated,Current_weight);
        true ->
            loop_through_edges(Graph,Rest_edges,Paths,Unvisited_nodes,Current_weight)
    end.
