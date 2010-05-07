%% -*- mode: erlang; erlang-indent-level: 2 -*-
%%% Created : 28 Jan 2010 by Mats Cronqvist <masse@kreditor.se>

%% @doc
%% @end

-module(inoteefy).
-author('Mats Cronqvist').
-export([init/1,terminate/2,handle_cast/2,handle_info/2]).

-export([start/0,stop/0,
         watch/2,unwatch/1]).

-record(ld,{port
            ,fd}).

-define(log(T),
        error_logger:info_report(
          [process_info(self(),current_function),{line,?LINE},T])).

% the api

start() ->
  gen_server:start({local, ?MODULE}, ?MODULE, [], []).

stop() -> 
  gen_server:cast(?MODULE,stop).

watch(File,Callback) -> 
  start(),
  gen_server:cast(?MODULE,{watch,{File,Callback}}).

unwatch(File) -> 
  gen_server:cast(?MODULE,{unwatch,File}).

%% gen_server callbacks

init(_) ->
  Port =open_port(),
  {ok,FD} = talk_to_port(Port,{open}),
  {ok,#ld{port=Port,fd=FD}}.

terminate(_,LD) ->
  talk_to_port(LD#ld.port,{close,LD#ld.fd}).

handle_info({Port,{data,Msg}},LD = #ld{port=Port}) ->
  maybe_call_back(binary_to_term(Msg)),
  {noreply,LD};
handle_info(Msg,LD) ->
  ?log({unknown_message,Msg}),
  {noreply,LD}.

handle_cast(stop,LD) ->
  {stop,normal,LD};
handle_cast({watch,Watch},LD) ->
  {noreply,do_watch(Watch,LD)};
handle_cast({unwatch,Unwatch},LD) ->
  {noreply,do_unwatch(Unwatch,LD)};
handle_cast(Msg,LD) ->
  ?log({unknown_message,Msg}),
  {noreply,LD}.

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
      case Mask of
        [ignored] -> ok;
        _ -> ?log([{got_event_without_callback,WD,Mask,Cookie,Name}])
      end;
    {Filename,CB} ->
      try CB({Filename,Mask,Cookie,Name})
      catch C:R -> ?log([{callback_failed,Filename},{C,R}])
      end
  end.

do_watch({File,CB},LD) ->
  try {ok,WD} = talk_to_port(LD#ld.port,{add, LD#ld.fd, File, all}),
      put({file,File},{WD,CB}),
      put({wd,WD},{File,CB}),
      LD
  catch C:R -> 
      ?log([{error_watching_file,File},{C,R}]),LD
  end.

do_unwatch(File,LD) ->
  case get({file,File}) of
    undefined   ->
      ?log([{not_watching,File}]),
      LD;
    {WD,_CB} -> 
      try talk_to_port(LD#ld.port,{remove,LD#ld.fd,WD})
      catch C:R -> ?log([{error_unwatching_file,File},{C,R}])
      end,
      erase({file,File}),
      erase({wd,WD}),
      LD
  end.

talk_to_port(Port,Msg) ->
  try
    erlang:port_command(Port, term_to_binary(Msg)),
    receive {Port, {data, D = <<131,104,2,_/binary>>}} -> binary_to_term(D)
    after 1000                   -> throw(talk_to_port_timeout)
    end
  catch _:R -> throw({talking_to_port_failed,{R,Port,Msg}})
  end.
