% Course : Distributed Systems
% Assignment : 2, Q1
% Author: Aditya Saripalli
% Description: Implementing ring topology of processes in Erlang

-module('20173071_1').
-export([start/1, init_ring/5, process_ring/4]).

start([InputFile, OutputFile]) ->
    io:format("[INFO] Implementing ring topology of processes in Erlang~n"),
    io:format("[INFO] Reading input file: ~p~n", [InputFile]),
    {ok, IFH} = file:open(InputFile, [read]),
    {ok, OFH} = file:open(OutputFile, [write]),
    {ok, LineRaw} = file:read_line(IFH),
    [Line] = string:tokens(LineRaw, "\n"),
    [N1|T] = string:tokens(Line, " "),
    [PassToken|_] = T,
    {ProcCount, Token} = {list_to_integer(N1), PassToken},
    io:format(OFH, "Process Count:~p | Token:~s~n", [ProcCount,Token]),
    init_ring(ProcCount, Token, 0, self(), OFH),
    io:format("[INFO] Ring topology executed. Output written to file: ~p~n", [OutputFile]).


init_ring(ProcCount, Token, ProcIndex, InitProcPid, OFH) ->
    NewPid = spawn(?MODULE, process_ring, [ProcCount, ProcIndex+1, InitProcPid, OFH]),
    NewPid ! {Token, ProcIndex},
    receive
        {SToken, SIndex} ->
            io:format(OFH, "Process ~w received token ~s from Process ~w.~n", [ProcIndex, SToken, SIndex])
    end.

process_ring(ProcCount, ProcIndex, InitProcPid, OFH) ->
    receive
        {SToken, SIndex} ->
            io:format(OFH, "Process ~w received token ~s from Process ~w.~n", [ProcIndex, SToken, SIndex])
    end,
    if
        ProcIndex < ProcCount-1 ->
            NewPid = spawn(?MODULE, process_ring, [ProcCount, ProcIndex+1, InitProcPid, OFH]),
            NewPid ! {SToken, ProcIndex};
        ProcIndex == ProcCount-1 ->
            InitProcPid ! {SToken, ProcIndex}
    end.
