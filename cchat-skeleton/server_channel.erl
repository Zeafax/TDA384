-module(server_channel).
-export([start/2,handler/2]).

-record(ch_server_st, {
    users % list of user (client pids)
}).


% Start a new server channel process with the given name
start(ServerAtom,Client) ->
    
    genserver:start(ServerAtom,#ch_server_st{users = [Client]},fun handler/2).

    
handler(State, {leave, Client}) ->
    Result = lists:member(Client, State#ch_server_st.users),
    case Result of
        true ->
            {reply, ok, State#ch_server_st{users = lists:delete(Client, State#ch_server_st.users)}};
        false ->
            {reply, failed, State}
    end;

handler(State, {join, Client})->
    Result = lists:member(Client, State#ch_server_st.users),
    case Result of
        true ->
            {reply, failed,  State};
        false ->
            {reply, joined,  State#ch_server_st{users = [Client | State#ch_server_st.users]}}
    end;


handler(State, {handle_message,Client, Nick, Channel, Msg}) ->
    case lists:member(Client, State#ch_server_st.users) of
        true->
             lists:foreach(fun(ChClient) -> % Send the message to all users in the channel
                        spawn(fun() -> genserver:request(ChClient, {message_receive, Channel, Nick, Msg}) end) % Send the message, spawn a new process for cuncurrency
                    end, lists:delete(Client, State#ch_server_st.users)),
                {reply, ok, State};
        false ->
            % Channel doesn't exist
            {reply,failed,State}
    end.
