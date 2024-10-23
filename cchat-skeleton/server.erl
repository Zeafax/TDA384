-module(server).
-export([start/1,stop/1,handler/2]).

-record(server_st, {
    channels, % list of channels on server
    nicks
}).



% Start a new server process with the given name
% Do not change the signature of this function.
start(ServerAtom) ->
    % TODO Implement function
    % - Spawn a new process which waits for a message, handles it, then loops infinitely
    % - Register this process to ServerAtom
    % - Return the process ID
    InitialState = #server_st{
            channels = [],
            nicks = []
        },
    genserver:start(ServerAtom,InitialState,fun handler/2).

handler(State, {join, Channel, Client, Nick}) ->
    %receive Msg -> io:format("Received: ~p~n", [Msg]) end.
    % Return if join is sucessful or not
    % create a channel process
    Result = lists:member(Channel, State#server_st.channels), 
    case Result of
        true ->
            % channel exists exists
            % check if user alredy there 
            case genserver:request(list_to_atom(Channel), {join, Client}) of
                joined ->
                    {reply, joined,  State#server_st{nicks = [Nick|State#server_st.nicks]}};
                failed ->
                    io:format("~ts~n", ["Failed" ++ Channel]),
                    {reply, failed, State}
            end;
        false ->
            % channel doesnt exist
            server_channel:start(list_to_atom(Channel),Client),
            {reply, joined, State#server_st{channels = [Channel | State#server_st.channels],nicks = [Nick|State#server_st.nicks]}}
    end;

handler(State, {nick, Nick, NewNick}) ->
    Result = lists:member(NewNick,State#server_st.nicks),
    case Result of
        true ->
            {reply, failed, State};
        false ->
            {reply, ok, State#server_st{nicks = lists:delete(Nick,[NewNick | State#server_st.nicks])}}
    end;

handler(State, terminate_channels) ->
    lists:foreach(fun(Channel) ->genserver:stop(list_to_atom(Channel)) end, State#server_st.channels),
    {reply, ok, State}.

% Stop the server process registered to the given name,
% together with any other associated processes
stop(ServerAtom) ->
    genserver:request(ServerAtom, terminate_channels),
    genserver:stop(ServerAtom).
