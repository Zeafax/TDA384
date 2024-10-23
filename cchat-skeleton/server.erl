-module(server).
-export([start/1,stop/1,handler/2]).

-record(server_st, {
    channels, % list of channels on server
    nicks % list of nicks on server
}).

% Start a new server process with the given name
% Do not change the signature of this function.
start(ServerAtom) ->
    % - Spawn a new process which waits for a message, handles it, then loops infinitely
    % - Register this process to ServerAtom
    % - Return the process ID
    InitialState = #server_st{
            channels = [], % list of channels on server
            nicks = [] % list of nicks on server
        },
    genserver:start(ServerAtom,InitialState,fun handler/2).

handler(State, {join, Channel, Client, Nick}) ->
    % Return if join is sucessful or not
    % create a channel process
    case lists:member(Channel, State#server_st.channels) of % check if channel exists
        true ->
            % channel exists exists
            % check if user alredy there 
            case genserver:request(list_to_atom(Channel), {join, Client}) of
                joined ->
                    {reply, joined,  State#server_st{nicks = [Nick|State#server_st.nicks]}}; % user joined
                failed ->
                    io:format("~ts~n", ["Failed" ++ Channel]), % user already in channel
                    {reply, failed, State}
            end;
        false ->
            % channel doesnt exist
            server_channel:start(list_to_atom(Channel),Client),
            {reply, joined, State#server_st{channels = [Channel | State#server_st.channels],nicks = [Nick|State#server_st.nicks]}}
    end;

handler(State, {nick, Nick, NewNick}) -> 
    case lists:member(NewNick,State#server_st.nicks) of  % check if new nick is already taken
        true ->
            {reply, failed, State}; % new nick is already taken
        false ->
            {reply, ok, State#server_st{nicks = lists:delete(Nick,[NewNick | State#server_st.nicks])}} % change nick
    end;

handler(State, terminate_channels) ->
    lists:foreach(fun(Channel) ->genserver:stop(list_to_atom(Channel)) end, State#server_st.channels), % stop all channels
    {reply, ok, State}.

% Stop the server process registered to the given name,
% together with any other associated processes
stop(ServerAtom) ->
    genserver:request(ServerAtom, terminate_channels),  % stop all channels
    genserver:stop(ServerAtom).  % stop the server
