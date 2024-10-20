-module(server).
-export([start/1,stop/1,loop/0]).

%-behaviour(gen_server).
% Start a new server process with the given name
% Do not change the signature of this function.
start(ServerAtom) ->
    %spawn(server,loop,[]).
    Pid = spawn(server,loop,[]),
    register(ServerAtom, Pid),
    Pid.	
    % TODO Implement function
    % - Spawn a new process which waits for a message, handles it, then loops infinitely
    %X = [1,2,3,4], 
    %while(X).
    %gen_server:start_link({local, ServerAtom}, ?MODULE, [], []).
    % - Register this process to ServerAtom
    % - Return the process ID
   


loop() ->
    receive
        {Client, Str} ->
            Client ! {self(), "Recieved: " ++ Str}
    end,
    loop.

% Stop the server process registered to the given name,
% together with any other associated processes
stop(ServerAtom) ->
    % TODO Implement function
    % Return ok
    exit(ServerAtom,ok).
