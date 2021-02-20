-module(filetest).
-export([start/1, parse_file/1]).

start([Filename]) ->
    InputFile = atom_to_list(Filename),
    {ok, File} = file:open(InputFile, [read]),
    {ok, LineRaw} = file:read_line(File),
    [Token] = string:tokens(LineRaw, "\n"),
    Num = list_to_integer(Token),
    io:format("~p~n", [Num]).

parse_file([Filename]) ->
    {ok, Data} = file:read_file(Filename),
    io:format("~p~n", [binary_to_list(Data)]),
    parse(binary_to_list(Data)).

parse(Data) ->
    Lines = re:split(Data, "\n", []),
    io:format("~p~n", [Lines]),
    ParsedData = [[begin
                       case  re:split(Token, "\"", [] ) of
                           [_,T,_] -> list_to_integer(T);
                           [T] -> T; % if token is not surrounded by ""
                           [] -> <<"">>
                       end
                   end || Token <- re:split(Line, " ", [])] || Line <- Lines, Line =/= <<"">>],
    io:format("~p~n", [ParsedData]).
