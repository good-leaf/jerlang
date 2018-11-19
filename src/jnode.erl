-module(jnode).

-behaviour(gen_server).

%% API
-export([start_link/1]).

%% gen_server callbacks
-export([init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    terminate/2,
    code_change/3]).

-compile([
    export_all
]).
-define(SERVER, ?MODULE).
-define(JAVA_NODE, 'java_node@127.0.0.1').
-define(JAVA_PROCESS, {java_node, ?JAVA_NODE}).
-define(JAVA_MBOX, "java_node").
-record(state, {
    appname,
    callback,
    depjar,
    runconfig
}).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @end
%%--------------------------------------------------------------------
-spec(start_link(list()) ->
    {ok, Pid :: pid()} | ignore | {error, Reason :: term()}).
start_link(Args) ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [Args], []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server
%%
%% @spec init(Args) -> {ok, State} |
%%                     {ok, State, Timeout} |
%%                     ignore |
%%                     {stop, Reason}
%% @end
%%--------------------------------------------------------------------
-spec(init(Args :: term()) ->
    {ok, State :: #state{}} | {ok, State :: #state{}, timeout() | hibernate} |
    {stop, Reason :: term()} | ignore).
init([Args]) ->
    AppName = proplists:get_value(appname, Args),
    CallBack = proplists:get_value(callback, Args, {jnode, call_msg}),
    RunConfig = read_config(AppName),
    DepJar = proplists:get_value("run_jar", RunConfig),
    boot_java_node(RunConfig),
    {ok, #state{appname = AppName, callback = CallBack, depjar = DepJar, runconfig = RunConfig}}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%%
%% @end
%%--------------------------------------------------------------------
-spec(handle_call(Request :: term(), From :: {pid(), Tag :: term()},
    State :: #state{}) ->
    {reply, Reply :: term(), NewState :: #state{}} |
    {reply, Reply :: term(), NewState :: #state{}, timeout() | hibernate} |
    {noreply, NewState :: #state{}} |
    {noreply, NewState :: #state{}, timeout() | hibernate} |
    {stop, Reason :: term(), Reply :: term(), NewState :: #state{}} |
    {stop, Reason :: term(), NewState :: #state{}}).
handle_call(_Request, _From, State) ->
    {reply, ok, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%%
%% @end
%%--------------------------------------------------------------------
-spec(handle_cast(Request :: term(), State :: #state{}) ->
    {noreply, NewState :: #state{}} |
    {noreply, NewState :: #state{}, timeout() | hibernate} |
    {stop, Reason :: term(), NewState :: #state{}}).
handle_cast(_Request, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%%
%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
-spec(handle_info(Info :: timeout() | term(), State :: #state{}) ->
    {noreply, NewState :: #state{}} |
    {noreply, NewState :: #state{}, timeout() | hibernate} |
    {stop, Reason :: term(), NewState :: #state{}}).
handle_info({nodedown, ?JAVA_NODE}, #state{runconfig = RunConfig} = State) ->
    boot_java_node(RunConfig),
    {noreply, State};
handle_info(Info, #state{callback = {Mod, Fun}} = State) ->
    Mod:Fun(Info),
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%%
%% @spec terminate(Reason, State) -> void()
%% @end
%%--------------------------------------------------------------------
-spec(terminate(Reason :: (normal | shutdown | {shutdown, term()} | term()),
    State :: #state{}) -> term()).
terminate(_Reason, #state{runconfig = RunConfig}) ->
    kill_java_node(RunConfig), ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------
-spec(code_change(OldVsn :: term() | {down, term()}, State :: #state{},
    Extra :: term()) ->
    {ok, NewState :: #state{}} | {error, Reason :: term()}).
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

-spec send_msg(string(), string()) -> any().
send_msg(MsgType, MsgData) ->
    ?JAVA_PROCESS ! {whereis(?SERVER), MsgType, MsgData}.

call_msg(Message) ->
    io:format("recv java node msg:~p", [Message]).
%%%===================================================================
%%% Internal functions
%%%===================================================================
start_java_node(RunConfig) ->
    AppName = proplists:get_value("appname", RunConfig),
    Dir = code:priv_dir(list_to_atom(AppName)),
    Jar = proplists:get_value("run_jar", RunConfig),
    JarLog = proplists:get_value("java_log", RunConfig),
    Cmd = "nohup java -jar " ++ Dir ++ "/" ++ Jar ++ " " ++ Dir ++ "/erl.properties >> " ++ JarLog ++ "/java_node.log 2>&1 &",
    io:format("java cmd:~p", [Cmd]),
    os:cmd(Cmd).

ping_java_node() ->
    case net_adm:ping(?JAVA_NODE) of
        pong ->
            erlang:monitor_node(?JAVA_NODE, true);
        _ ->
            timer:sleep(1000),
            ping_java_node()
    end.

boot_java_node(RunConfig) ->
    kill_java_node(RunConfig),
    start_java_node(RunConfig),
    ping_java_node().

kill_java_node(RunConfig) ->
    Jar = proplists:get_value("run_jar", RunConfig),
    case string:tokens(os:cmd("ps -ef | grep '" ++ Jar ++ "' | grep -v grep"), "\n") of
        [Str | _] ->
            Pid = lists:nth(2, string:tokens(Str, " ")),
            os:cmd("kill -9 " ++ Pid),
            kill_java_node(RunConfig);
        [] ->
            ok
    end.

read_config(AppName) ->
    Dir = code:priv_dir(AppName),
    {ok, Context} = file:read_file(Dir ++ "/java_config"),
    ContextList = string:tokens(binary_to_list(Context), "\n"),
    RunConfig = lists:foldl(fun(Config, Acc) ->
                [K, V] = string:tokens(Config, "="),
                [{K, V} | Acc]
                end, [{"cookie", atom_to_list(erlang:get_cookie())}, {"appname", atom_to_list(AppName)}, {"java_node", atom_to_list(?JAVA_NODE)}], ContextList),
    write_config(Dir, RunConfig), RunConfig.


write_config(Dir, RunConfig) ->
    Data = lists:foldl(fun({K, V}, Acc) ->
        Value = K ++ "=" ++ V ++ "\n", string:concat(Acc, Value) end, "", RunConfig),
    file:write_file(Dir ++ "/erl.properties", Data).
