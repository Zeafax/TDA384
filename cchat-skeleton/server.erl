-module(server).
-export([start/1,stop/1]).

-record(server_st, {
    channels, % Map of channel names to lists of nicks
    clients % Map of clients to channels
}).

% Start a new server process with the given name
% Do not change the signature of this function.
%% Start the server and register it under the given atom (ServerName)
start(ServerAtom) ->
    InitialState = #server_st{
        channels = #{},   % Map of channel names to lists of nicks
        clients = #{}     % Map of clients to nicks
    },
    genserver:start(ServerAtom, InitialState, fun handle/2). % Start the server with the initial state and the handle function

%% Stops the server
stop(ServerAtom) ->
    genserver:stop(ServerAtom).

% Join channel
handle(St, {join_channel, Nick, ClientID, Channel}) ->
    case maps:is_key(Channel, St#server_st.channels) of % Check if the channel exists
        true ->
        io:format("Channel exists~n"),
        case maps:get(Channel, St#server_st.channels) of % Get the list of nicks in the channel
            Nicks ->
                case lists:member(Nick, Nicks) of % Check if the user is already in the channel
                    true ->
                        io:format("User is already in the channel~n"),
                        {reply, failed, St};
                    false ->
                        NewNicks = lists:append(Nicks, [Nick]), % Add the user to the channel
                        NewChannels = maps:put(Channel, NewNicks, St#server_st.channels), % Update the channels map
                        NewClients = maps:put(Nick, ClientID, St#server_st.clients), % Store the client's Pid in the clients map
                        NewState = St#server_st{channels = NewChannels, clients = NewClients}, % Update the state
                        io:format("NewState: ~p~n", [NewState]),
                        {reply, ok, NewState}
                end
        end;
    false ->
        io:format("Channel does not exist~n"),
        NewChannels = maps:put(Channel, [Nick], St#server_st.channels), % Create the channel
        NewClients = maps:put(Nick, ClientID, St#server_st.clients), % Store the client's Pid in the clients map
        NewState = St#server_st{channels = NewChannels, clients = NewClients},
        io:format("NewState: ~p~n", [NewState]),
        {reply, ok, NewState}
    end;

% Leave channel
handle(St, {leave_channel, Nick, Channel}) ->
    case maps:is_key(Channel, St#server_st.channels) of % Check if the channel exists
        true ->
        case maps:get(Channel, St#server_st.channels) of % Get the list of nicks in the channel
            Nicks ->
                case lists:member(Nick, Nicks) of % Check if the user is in the channel
                    true ->
                        NewNicks = lists:delete(Nick, Nicks), % Remove the user from the channel
                        NewChannels = maps:put(Channel, NewNicks, St#server_st.channels), % Update the channels map
                        NewState = St#server_st{channels = NewChannels}, % Update the state
                        io:format("NewState: ~p~n", [NewState]),
                        {reply, ok, NewState};
                    false ->
                        {reply, {error, user_not_joined, "You are not in the channel"}, St}
                end
        end;
    false ->
        {reply, {error, user_not_joined, "You are not in the channel"}, St}
    end;

% Sending message (from client, to channel)
handle(St, {handle_message, Nick, Channel, Msg}) ->
    case maps:is_key(Channel, St#server_st.channels) of % Check if the channel exists
        true ->
            Nicks = maps:get(Channel, St#server_st.channels), % Get the list of nicks in the channel
            case lists:member(Nick, Nicks) of % Check if the user is in the channel
                true ->
                    lists:foreach(fun(NickInChannel) -> % Send the message to all users in the channel
                        ClientPid = maps:get(NickInChannel, St#server_st.clients), % Get the client's Pid
                        io:format("Message sent to ~p~n", [ClientPid]), 
                        spawn(fun() -> genserver:request(ClientPid, {message_receive, Channel, Nick, Msg}) end) % Send the message, spawn a new process for cuncurrency
                    end, lists:delete(Nick,Nicks)),
                    {reply, ok, St};
                false ->
                    {reply, {error, user_not_joined, "You are not in the channel"}, St}
            end;
        false ->
            {reply, {error, user_not_joined, "No such channel"}, St}
    end.