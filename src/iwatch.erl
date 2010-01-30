%%
%% a simple watcher
%%
%% This is a simple test script which can be run from
%% the erl prompt. For example,
%%    
%% $ erl -pa .
%% Erlang (BEAM) emulator version 5.5.5 [source] [async-threads:0] [kernel-poll:false]
%%
%% Eshell V5.5.5  (abort with ^G)
%% 1> iwatch:run("../test").
%%  %% run touch ../test/file from another unix shell
%% inotify_erlang:note_read  len: 64 idx: 0 event 1 100 0 file
%% inotify_erlang:note_read  len: 64 idx: 32 event 1 20 0 file
%% got: {event,1,[create],0,"file"}
%% got: {event,1,[open],0,"file"}
%% inotify_erlang:note_read  len: 32 idx: 0 event 1 4 0 file
%% got: {event,1,[attrib],0,"file"}
%% inotify_erlang:note_read  len: 32 idx: 0 event 1 8 0 file
%% got: {event,1,[close_write],0,"file"}
%%
%% erl prompt will not return. I did say this was for testing!
%%
-module(iwatch).
-author('jeffm@ghostgun.com').


-export([
	 run/1
	]).

run(Dir) when is_list(Dir)  ->
    inotify:start(self()),
    %% hack to make sure inotify is running
    timer:sleep(200),
    {ok, F} = inotify:open(),
    inotify:list(),
    {ok, _W} = inotify:add(F, Dir, all),
    print_events().

print_events() ->
    receive
	Msg ->
	    io:format("got: ~p~n", [Msg]),
	    print_events()
    end.

	    

