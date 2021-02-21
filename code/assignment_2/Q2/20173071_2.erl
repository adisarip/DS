% Course : Distributed Systems
% Assignment : 2, Q2
% Author: Aditya Saripalli
% Description: Implementing Bellman-Ford Algorithm with concurrency in Erlang

-module('20173071_2').
-export([start/1]).

start([InputFile, OutputFile]) ->
    {P, V, E, G, S} = parse_input(InputFile),
    X = initialize_distances(V, []),
    log_data(P, V, E, G, S, X),
    D = bellman_ford(P, V, E, G, S, X),
    write_output_data(D, OutputFile),
    io:format("[INFO] Output written to ~p~n", [OutputFile]).

parse_input(InputFile) ->
    io:format("[INFO] Parsing input file: ~p~n", [InputFile]),
    {ok, IFH} = file:open(InputFile, [read]),

    % read the number of processes
    {ok, Line1} = file:read_line(IFH),
    P = list_to_integer(element(1, list_to_tuple(string:tokens(Line1, "\n")))),

    % read the number of Vertices and Edges
    {ok, Line2} = file:read_line(IFH),
    {Vs,Es} = list_to_tuple(string:tokens(Line2, "\n| ")),
    {V,E} = {list_to_integer(Vs), list_to_integer(Es)},

    % Create the graph
    {S, G} = graph(IFH, E),
    {P, V, E, G, S}.


% Create the graph from the inout data
graph(IFH, E) ->
    io:format("[INFO] Creating graph from the input data~n"),
    create_graph(IFH, E, []).
create_graph(IFH, 0, G) ->
    {ok, Line} = file:read_line(IFH),
    S = list_to_integer(element(1, list_to_tuple(string:tokens(Line, "\n")))),
    {S, list_to_tuple(G)};
create_graph(IFH, E, G) ->
    {ok, Line} = file:read_line(IFH),
    {Us,Vs,Ws} = list_to_tuple(string:tokens(Line, "\n| ")),
    {U,V,W} = {list_to_integer(Us), list_to_integer(Vs), list_to_integer(Ws)},
    create_graph(IFH, E-1, G ++ [{U,V,W}] ++ [{V,U,W}]).

% Initialize the Distances from Source to each vertex
initialize_distances(0, D) -> list_to_tuple(D);
initialize_distances(V, D) -> initialize_distances(V-1, D ++ [{inf}]).

% Initiate Bellman-Ford Algorithm
bellman_ford(P, V, E, G, S, D) ->
    io:format("[INFO] Running Bellman-Ford on input graph~n"),
    D1 = setelement(S,D,0),
    D2 = relax(1, V, E, G, D1),
    D2.

% Relax all the edges |V|-1 times
relax(X, V, E, G, D) ->
    if
        X < V  ->
            D1 = relax_edges(1, E, G, D),
            relax(X+1, V, E, G, D1);
        X == V -> D
    end.

relax_edges(Y, E, G, D) ->
    if
        Y =< 2*E  ->
            D1 = update_distances(Y, G, D),
            relax_edges(Y+1, E, G, D1);
        Y > 2*E -> D
    end.

update_distances(Y, G, D) ->
    Dist_U = element(element(1,element(Y,G)),D),
    Dist_V = element(element(2,element(Y,G)),D),
    W = element(3,element(Y,G)),
    if
        (Dist_U =/= inf) and (Dist_U + W < Dist_V) ->
            setelement(element(2,element(Y,G)), D, Dist_U + W);
        true -> D
    end.

% Logging debug data
log_data(P, V, E, G, S, D) ->
    io:format("[INFO] No of Processes: ~p~n", [P]),
    io:format("[INFO] No of Vertices: ~p~n", [V]),
    io:format("[INFO] No of Edges: ~p~n", [E]),
    io:format("[INFO] Input Graph: ~p~n", [G]),
    io:format("[INFO] Source Vertex: ~p~n", [S]),
    io:format("[INFO] Distances initialized to: ~p~n", [D]).

% Write the output to output.txt file
write_output_data(D, OutputFile) ->
    {ok, OFH} = file:open(OutputFile, [write]),
    io:format(OFH, "Vertex    Distance From Source~n", []),
    print_distances(OFH, D, 1).

print_distances(OFH, D, X) ->
    Size = tuple_size(D),
    if
        X =< Size ->
            io:format(OFH, "  ~p                 ~p~n", [X, element(X,D)]),
            print_distances(OFH, D, X+1);
        X > Size -> ok
    end.
