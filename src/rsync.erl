%%%-------------------------------------------------------------------
%%% @author Evgeny Khramtsov <ekhramtsov@process-one.net>
%%% @copyright (C) 2016, Evgeny Khramtsov
%%% @doc
%%% rsync is an Erlang wrapper for
%%% <a href="http://librsync.sourcefrog.net">librsync</a> library.
%%% It provides basic functions for direct files manipulations
%%% (sig/1, delta/2, etc) as well as low-level functions
%%% for fine-grained control (sig_init/0, delta_update/2, etc).
%%% The low-level interface is somewhat similar to the `hash' functions
%%% from `crypto' module.
%%% @end
%%% Created :  7 Jun 2016 by Evgeny Khramtsov <ekhramtsov@process-one.net>
%%%-------------------------------------------------------------------
-module(rsync).

-on_load(load_nif/0).

%% API
-export([sig_init/0, sig_update/2, sig_final/1,
	 loadsig_init/0, loadsig_update/2, loadsig_final/1,
	 delta_init/1, delta_update/2, delta_final/1,
	 patch_init/1, patch_update/2, patch_final/1,
	 load_nif/0, format_error/1, sig/1, delta/2, patch/2,
	 sig/2, delta/3, patch/3]).

-define(BLOCKSIZE, 1 bsl 16). %% 64kb

-type rsync_error_reason() :: done | blocked | running | test_skipped |
			      io_error | syntax_error | mem_error |
			      input_ended | bad_magic | unimplemented |
			      corrupt | internal_error | param_error |
			      unexplained_problem.
-type error_reason() :: rsync_error_reason() | file:posix().
-type error() :: {error, error_reason()}.
-type context() :: any().

-export_type([error_reason/0]).

%%%===================================================================
%%% API
%%%===================================================================
-spec sig(file:filename() | {raw, iodata()}) -> {ok, binary()} | error().
%% @doc Computes signature of `File'.
sig(File) ->
    sig(File, undefined).

-spec sig(file:filename() | {raw, iodata()}, file:filename()) -> ok | error();
	 (file:filename() | {raw, iodata()}, undefined) -> {ok, binary()} | error().
%% @doc Computes signature of a file `File' and writes it into a file `SigFile'.
sig(File, SigFile) ->
    try
	{ok, InFd} = case File of
			 {raw, Data} ->
			     {ok, iolist_to_binary(Data)};
			 _ ->
			     file:open(File, [read|file_opts()])
		     end,
	{ok, OutFd} = if SigFile == undefined ->
			      {ok, <<>>};
			 true ->
			      file:open(SigFile, [write|file_opts()])
		      end,
	Context = sig_init(),
	execute(Context, InFd, OutFd)
    catch error:{badmatch, {error, _} = Err} ->
	    Err
    end.

-spec delta(file:filename() | {raw, iodata()},
	    file:filename() | {raw, iodata()}) -> {ok, binary()} | error().
%% @doc Computes delta of a file `NewFile' using signature from file `SigFile'.
%% The signature file should be previously created using `sig/2'.
%% @see sig/2.
delta(SigFile, NewFile) ->
    delta(SigFile, NewFile, undefined).

-spec delta(file:filename() | {raw, iodata()},
	    file:filename() | {raw, iodata()},
	    file:filename()) -> ok | error();
	   (file:filename() | {raw, iodata()},
	    file:filename() | {raw, iodata()},
	    undefined) -> {ok, binary()} | error().
%% @doc Computes delta of a file `NewFile' using signature from file `SigFile'
%% and writes it into a file `DeltaFile'.
%% The signature file should be previously created using `sig/2'.
%% @see sig/2.
delta(SigFile, NewFile, DeltaFile) ->
    try
	{ok, SigFd} = case SigFile of
			  {raw, SigData} ->
			      {ok, iolist_to_binary(SigData)};
			  _ ->
			      file:open(SigFile, [read|file_opts()])
		      end,
	Context = loadsig_init(),
	ok = execute(Context, SigFd, undefined),
	{ok, InFd} = case NewFile of
			 {raw, NewFileData} ->
			     {ok, iolist_to_binary(NewFileData)};
			 _ ->
			     file:open(NewFile, [read|file_opts()])
		     end,
	{ok, OutFd} = if DeltaFile == undefined ->
			      {ok, <<>>};
			 true ->
			      file:open(DeltaFile, [write|file_opts()])
		      end,
	ok = delta_init(Context),
	execute(Context, InFd, OutFd)
    catch error:{badmatch, {error, _} = Err} ->
	    Err
    end.

-spec patch(file:filename() | {raw, iodata()},
	    file:filename() | {raw, iodata()}) -> {ok, binary()} | error().
%% @doc Applies delta from a file `DeltaFile' to a file `OrigFile'.
%% The delta file should be previously created using `delta/3'.
%% WARNING: whole `OrigFile' to be read into the memory
%% for this function to work. This is due to a limitation of librsync API
%% and Erlang's NIF API. Don't use this function for extremely large files.
%% @see delta/3.
patch(OrigFile, DeltaFile) ->
    patch(OrigFile, DeltaFile, undefined).

-spec patch(file:filename() | {raw, iodata()},
	    file:filename() | {raw, iodata()},
	    file:filename()) -> ok | error();
	   (file:filename() | {raw, iodata()},
	    file:filename() | {raw, iodata()},
	    undefined) -> {ok, binary()} | error().
%% @doc Applies delta from a file `DeltaFile' to a file `OrigFile'
%% and writes the result into a file `NewFile'.
%% The delta file should be previously created using `delta/3'.
%% WARNING: whole `OrigFile' to be read into the memory
%% for this function to work. This is due to a limitation of librsync API
%% and Erlang's NIF API. Don't use this function for extremely large files.
%% @see delta/3.
patch(OrigFile, DeltaFile, NewFile) ->
    try
	{ok, Data} = case OrigFile of
			 {raw, OrigData} ->
			     {ok, OrigData};
			 _ ->
			     file:read_file(OrigFile)
		     end,
	{ok, InFd} = case DeltaFile of
			 {raw, Delta} ->
			     {ok, Delta};
			 _ ->
			     file:open(DeltaFile, [read|file_opts()])
		     end,
	{ok, OutFd} = if NewFile == undefined ->
			      {ok, <<>>};
			 true ->
			      file:open(NewFile, [write|file_opts()])
		      end,
	Context = patch_init(Data),
	execute(Context, InFd, OutFd)
    catch error:{badmatch, {error, _} = Err} ->
	    Err
    end.

-spec sig_init() -> context().
%% @doc Initializes context for signature computation.
sig_init() ->
    erlang:nif_error(nif_not_loaded).

-spec sig_update(context(), iodata()) -> {ok, binary()} | error().
%% @doc Proceeds signature computation with addition from `Data'.
%% `Context' should be previously created using `sig_init/0'.
%% @see sig_init/0.
sig_update(Context, Data) ->
    job_iter(Context, Data).

-spec sig_final(context()) -> {ok, binary()} | error().
%% @doc Finishes signature computation.
%% `Context' should be previously created using `sig_init/0'.
%% @see sig_init/0.
sig_final(Context) ->
    job_done(Context).

-spec loadsig_init() -> context().
%% @doc Initializes context for signature loading.
%% The function (along with loadsig_update/2 and loadsig_final/1)
%% should be called prior to delta computation and the resulting
%% context should be passed to `delta_init/1'.
%% @see delta_init/1.
loadsig_init() ->
    erlang:nif_error(nif_not_loaded).

-spec loadsig_update(context(), iodata()) -> ok | error().
%% @doc Proceeds with signature loading with addition from `Data'.
%% `Context' should be previously created using `loadsig_init/0'.
%% @see loadsig_init/0.
loadsig_update(Context, Data) ->
    job_iter(Context, Data).

-spec loadsig_final(context()) -> ok | error().
%% @doc Finishes signature loading.
%% `Context' should be previously created using `loadsig_init/0'.
%% The context finished by this function should be passed
%% to `delta_init/1' function.
%% @see loadsig_init/0.
%% @see delta_init/1.
loadsig_final(Context) ->
    job_done(Context).

-spec delta_init(context()) -> ok | error().
%% @doc Re-initializes context for delta computation.
%% `Context' should be previously created using `loadsig_init/0' and
%% should be finished using `loadsig_final/1'.
%% @see loadsig_init/0
%% @see loadsig_final/1.
delta_init(_Context) ->
    erlang:nif_error(nif_not_loaded).

-spec delta_update(context(), iodata()) -> {ok, binary()} | error().
%% @doc Proceeds delta computations with addition from `Data'.
%% `Context' should be previously re-initialized using `delta_init/1'.
%% @see delta_init/1.
delta_update(Context, Data) ->
    job_iter(Context, Data).

-spec delta_final(context()) -> {ok, binary()} | error().
%% @doc Finishes delta computation.
%% `Context' should be previously re-initialized using `delta_init/1'.
%% @see delta_init/1.
delta_final(Context) ->
    job_done(Context).

-spec patch_init(iodata()) -> context().
%% @doc Initializes context for patch creation using data
%% from the original file.
%% WARNING: whole original file to be read into the memory
%% for this function to work. This is due to a limitation of librsync API
%% and Erlang's NIF API. Don't use this function for extremely large files.
%% @see patch_init/1.
patch_init(_Data) ->
    erlang:nif_error(nif_not_loaded).

-spec patch_update(context(), iodata()) -> {ok, binary()} | error().
%% @doc Proceeds patch creation with addition from `Data'.
%% `Context' should be previously created using `patch_init/1'. 
%% @see patch_init/1.
patch_update(Context, Data) ->
    job_iter(Context, Data).

-spec patch_final(context()) -> {ok, binary()} | error().
%% @doc Finished patch creation.
%% `Context' should be previously created using `patch_init/1'.
%% @see patch_init/1.
patch_final(Context) ->
    job_done(Context).

-spec format_error(error_reason()) -> string().
%% @doc Returns a descriptive string of the error `Reason'.
format_error(Reason) ->
    case file:format_error(Reason) of
	"unknown POSIX error" ->
	    format_error_nif(Reason);
	Txt ->
	    Txt
    end.

%% @private
load_nif() ->
    load_nif(get_so_path()).

%%%===================================================================
%%% Internal functions
%%%===================================================================
job_iter(_Context, _Data) ->
    erlang:nif_error(nif_not_loaded).

job_done(_Context) ->
    erlang:nif_error(nif_not_loaded).

format_error_nif(_Err) ->
    erlang:nif_error(nif_not_loaded).

execute(Context, In, Out) ->
    case read(In) of
	{ok, DataIn, NewIn} ->
	    case job_iter(Context, DataIn) of
		{ok, DataOut} ->
		    case write(Out, DataOut) of
			{ok, NewOut} ->
			    execute(Context, NewIn, NewOut);
			ok ->
			    execute(Context, NewIn, Out);
			{error, _} = Err ->
			    Err
		    end;
		ok ->
		    execute(Context, NewIn, Out);
		{error, _} = Err ->
		    Err
	    end;
	eof ->
	    case job_done(Context) of
		{ok, DataOut} ->
		    write(Out, DataOut);
		ok ->
		    ok;
		{error, _} = Err ->
		    Err
	    end;
	{error, _} = Err ->
	    Err
    end.

read(<<>>) ->
    eof;
read(In) when is_binary(In) ->
    {ok, In, <<>>};
read(Fd) ->
    case file:read(Fd, ?BLOCKSIZE) of
	{ok, Data} ->
	    {ok, Data, Fd};
	Other ->
	    Other
    end.

write(Out, Data) when is_binary(Out) ->
    {ok, <<Out/binary, Data/binary>>};
write(Fd, Data) ->
    file:write(Fd, Data).

file_opts() ->
    [raw, binary, {read_ahead, ?BLOCKSIZE}].

get_so_path() ->
    EbinDir = filename:dirname(code:which(?MODULE)),
    AppDir = filename:dirname(EbinDir),
    filename:join([AppDir, "priv", "lib"]).

load_nif(LibDir) ->
    SOPath = filename:join(LibDir, ?MODULE),
    case catch erlang:load_nif(SOPath, 0) of
        ok ->
            ok;
        Err ->
            error_logger:warning_msg("unable to load ~p NIF: ~p~n",
				     [?MODULE, Err]),
            Err
    end.
