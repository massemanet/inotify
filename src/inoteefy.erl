%% -*- mode: erlang; erlang-indent-level: 2 -*-
%%% Created : 28 Jan 2010 by Mats Cronqvist <masse@kreditor.se>

%% @doc
%% @end

-module(inoteefy).
-author('Mats Cronqvist').
-export([rec_info/1,init/1,handle_cast/2,handle_info/2]).

-export([start/0,stop/0,
         watch_file/2,unwatch_file/1]).

-record(ld,{port}).

-define(log(T),
        error_logger:info_report(
          [process_info(self(),current_function),{line,?LINE},T])).

% the api

start() ->
  gen_serv:start(?MODULE).

stop() -> 
  gen_serv:stop(?MODULE).

watch_file(File,Callback) -> 
  gen_server:cast(?MODULE,{watch,{File,Callback}}).

unwatch_file(File) -> 
  gen_server:cast(?MODULE,{unwatch,File}).

%% gen_serv callbacks
rec_info(ld) ->
  record_info(fields,ld).

init(_) ->
  gen_serv:unlink(),
  #ld{port=open_port()}.

handle_info({Port,{data,Msg}},LD = #ld{port=Port}) ->
  maybe_call_back(binary_to_term(Msg)),
  LD.

handle_cast({watch,Watch},LD) ->
  watch(Watch,LD);
handle_cast({unwatch,Unwatch},LD) ->
  unwatch(Unwatch,LD).

%% implementation details
open_port() ->
  Parts = [[filename:dirname(code:which(?MODULE)),"..",c_src],
           [code:priv_dir(inotify),bin]],
  try E = take_first(fun to_file/1, Parts),
      io:fwrite("using: ~p~n",[E]),
      open_port({spawn, E},[{packet, 2}, binary, exit_status])
  catch _:_ -> exit({inotify_binary_not_found,Parts})
  end.

to_file(Parts) ->
  true = filelib:is_regular(F=filename:join(Parts++[inotify])),
  F.

take_first(_,[]) -> exit({take_first,nothing_worked});
take_first(F,[H|T]) ->
  try F(H)
  catch _:_ -> take_first(F,T)
  end.

maybe_call_back({event,WD,Mask,Cookie,Name}) ->
  case get({wd,WD}) of
    undefined ->
      ?log([{got_event_without_callback,WD}]);
    {Filename,_FD,CB} ->
      try CB({Filename,Mask,Cookie,Name})
      catch C:R -> ?log([{callback_failed,Filename},{C,R}])
      end
  end.

watch({File,CB},LD) ->
  case filelib:is_regular(File) of
    false->
      ?log([{no_such_file,File}]),LD;
    true -> 
      try {ok,FD} = talk_to_port(LD#ld.port,{open}),
          {ok,WD} = talk_to_port(LD#ld.port,{add, FD, File, all}),
          put({file,File},{FD,WD,CB}),
          put({wd,WD},{File,FD,CB}),
          LD
      catch C:R -> 
          ?log([{error_watching_file,File},{C,R}]),LD
      end
  end.

unwatch(File,LD) ->
  case get({file,File}) of
    undefined   -> ?log([{not_watching,File}]),LD;
    {FD,WD,_CB} -> 
      try   talk_to_port(LD#ld.port,{remove,FD,WD}),
            talk_to_port(LD#ld.port,{close,FD})
      catch C:R -> ?log([{error_unwatching_file,File},{C,R}])
      end,
      erase({file,File}),
      erase({wd,WD}),
      LD
  end.

talk_to_port(Port,Msg) ->
  try
    erlang:port_command(Port, term_to_binary(Msg)),
    receive {Port, {data, Data}} -> binary_to_term(Data)
    after 1000                   -> throw(fd_open_timeout)
    end
  catch _:R -> throw({talking_to_port_failed,{R,Port,Msg}})
  end.
