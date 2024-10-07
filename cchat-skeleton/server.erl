-module(server).
-export([start/1,stop/1]).

% Start a new server process with the given name
% Do not change the signature of this function.
start(ServerAtom) ->
    % TODO Implement function
    % - Spawn a new process which waits for a message, handles it, then loops infinitely
    % - Register this process to ServerAtom
    % - Return the process ID
    Pid = spawn(fun server_loop/0),
    register(ServerAtom, Pid),
    Pid.

% Stop the server process registered to the given name,
% together with any other associated processes
stop(ServerAtom) ->
    % TODO Implement function
    % Return ok
    ServerAtom ! stop.

server_loop() ->
    receive
        stop ->
            io:format("Stopping server ~n"),
            ok;
        _Msg ->
            io:format("Message received: ~s ~n", [_Msg]),
            server_loop()
    end.
