-module(inotify).
-export([start/1, stop/0]).
-export([test_start/0, test_end/2, test/0]).
-export([open/0, controlling_process/1, add/3, remove/2, list/0, close/1]).

-define(PORTPROG, "./inotify").

start(Controller) when is_pid(Controller) ->
    spawn(fun() ->
		  register(?MODULE, self()),
		  process_flag(trap_exit, true),
		  Port = open_port({spawn, ?PORTPROG},
				   [{packet, 2}, binary, exit_status]),
		  loop(Port, Controller)
		  %% The following is only used for development and testing
		  %% try loop(Port, Controller)
		  %% catch
		  %%    T:Err ->
		  %%	  error_logger:error_msg("catch: ~p:~p~n", [T, Err])
		  %% end
	  end).

stop() ->
    ?MODULE ! stop.


test_start() ->
    io:format("open~n"),
    {ok, F} = open(),
    io:format("list~n"),
    {ok, L} = list(),
    io:format("List returned ~p~n", [L]),
    {ok, W} = add(F, "file", all),
    {ok, F, W}.

test_end(F,W) ->
    io:format("remove~n"),
    ok = remove(F,W),
    io:format("close~n"),
    ok = close(F).

test() ->
    %% this is the test file
    Dir = "../test",
    File = "file",
    io:format("Simplistic test/example~n"),
    io:format("Start... "),
    start(self()),
    io:format("Open the port and receive a file descriptor... "),
    {ok, F} = open(),
    io:format("F = ~p~n", [F]),
    io:format("list.."),
    {ok, L} = list(),
    io:format("L = ~p~n", [L]),
    io:format("Watch for any changes in a Directory... "),
    {ok, W} = add(F, Dir, all),
    io:format("W = ~p~n", [W]),
    io:format("launch listener....~n"),
    Listener = spawn(fun() -> test_listener() end),
    controlling_process(Listener),
    io:format("start playing with the file...~n"),
    test_player(Dir ++ "/" ++ File),
    io:format("stop the listener...~n"),
    Listener ! stop,
    io:format("stop inotify controller...~n"),
    stop(),
    io:format("test is now concluded~n").

test_player(File) ->
    io:format("attempt to create file ~p~n", [File]),
    {ok, F} = file:open(File, write),
    timer:sleep(1000),
    io:format("write a message to file~n"),
    file:write(F, "a line of text~n"),
    timer:sleep(1000),
    io:format("close the file~n"),
    file:close(F),
    timer:sleep(1000),
    io:format("delete file ~p~n", [File]),
    file:delete(File),
    timer:sleep(1000),
    io:format("end playing with file~n").

test_listener() ->
    receive
	stop ->
	    ok;
	Msg ->
	    io:format("listener got: ~p~n", [Msg]),
	    test_listener()
    end.

%%
%% open() -> {ok, Fd} | {error, Reason}
%%
open() ->
    call_port({open}).
%%
%% add(Fd, Pathname, EventList) -> {ok, Wd} | {error, Reason}
%%
add(Fd, Pathname, EventList) when is_integer(Fd), is_list(Pathname),
				  (is_atom(EventList) orelse is_list(EventList)) ->
    call_port({add, Fd, Pathname, EventList}).

%%
%% controlling_process(Pid) -> ok
%%
controlling_process(Pid) ->
    ?MODULE ! {controlling_process, self(), Pid},
    receive
	{?MODULE, Result} ->
	    Result;
	Other ->
          Other
    end.

%%
%% remove(Fd, Wd) -> ok| {error, Reason}
%%
remove(Fd, Wd) when is_integer(Fd), is_integer(Wd) ->
    call_port({remove, Fd, Wd}).
%%
%% list() -> [Fds]
%%
list() ->
    call_port({list}).
%%
%% close(Fd) -> ok | {error, Reason}
%%
close(Fd) when is_integer(Fd) ->
    call_port({close, Fd}).


call_port(Msg) ->
    ?MODULE ! {call, self(), Msg},
    receive
	{?MODULE, Result} ->
	    Result;
	Other ->
	    Other
    end.

loop(Port, Controller) ->
    receive
	{call, Caller, Msg} ->
	    erlang:port_command(Port, term_to_binary(Msg)),
	    receive
		{Port, {data, Data}} ->
		    Caller ! binary_to_term(Data);
		{Port, {exit_status, Status}} when Status > 128 ->
		    exit({port_terminated, Status});
		{Port, {exit_status, Status}} ->
		    exit({port_terminated, Status});
		{'EXIT', Port, Reason} ->
		    exit(Reason)
                %%   following two lines used for development and testing only
		%% Other ->
		%%    io:format("received: ~p~n", [Other])
	    end,
	    loop(Port, Controller);
	{Port, {data, Msg}} ->
	    Controller ! binary_to_term(Msg),
	    loop(Port, Controller);
	{controlling_process, Caller, Pid} ->
	    Caller ! ok,
	    loop(Port, Pid);
	stop ->
	    erlang:port_close(Port),
	    exit(normal);
	{'EXIT', Port, Reason} ->
	    exit({port_terminated, Reason});
	_Other ->
	    loop(Port, Controller)
    end.
