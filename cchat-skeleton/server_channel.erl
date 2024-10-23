-module(server_channel).
-export([start/2,handler/2]).

-record(ch_server_st, {
    users % list of user (client pids)
}).


% Start a new server channel process with the given name
start(ServerAtom,Client) ->
    genserver:start(ServerAtom,#ch_server_st{users = [Client]},fun handler/2). % Start the server channel process with the given name

% Function to leave the channel    
handler(State, {leave, Client}) ->
    case lists:member(Client, State#ch_server_st.users) of % Check if the user is in the channel 
        true ->
            {reply, ok, State#ch_server_st{users = lists:delete(Client, State#ch_server_st.users)}};  % Remove the user from the channel
        false -> 
            {reply, failed, State} % User is not in the channel
    end;

% Function to join the channel
handler(State, {join, Client})->
    case lists:member(Client, State#ch_server_st.users) of % Check if the user is already in the channel
        true ->
            {reply, failed,  State}; % User is already in the channel
        false ->
            {reply, joined,  State#ch_server_st{users = [Client | State#ch_server_st.users]}} % Add the user to the channel
    end;

% Function to messages in the channel
handler(State, {handle_message,Client, Nick, Channel, Msg}) ->
    case lists:member(Client, State#ch_server_st.users) of % Check if the user is in the channel
        true->
             lists:foreach(fun(ChClient) -> % Send the message to all users in the channel
                        spawn(fun() -> genserver:request(ChClient, {message_receive, Channel, Nick, Msg}) end) % Send the message, spawn a new process for cuncurrency
                    end, lists:delete(Client, State#ch_server_st.users)), % Send the message to all users except the sender
                {reply, ok, State};
        false ->
            % Channel doesn't exist
            {reply,failed,State}
    end.