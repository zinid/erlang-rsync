%%%-------------------------------------------------------------------
%%% @author Evgeny Khramtsov <ekhramtsov@process-one.net>
%%% @copyright (C) 2016, Evgeny Khramtsov
%%% @doc
%%%
%%% @end
%%% Created : 10 Jun 2016 by Evgeny Khramtsov <ekhramtsov@process-one.net>
%%%-------------------------------------------------------------------
-module(rsync_test).

-include_lib("eunit/include/eunit.hrl").

test_dir() ->
    {ok, Cwd} = file:get_cwd(),
    filename:join(filename:dirname(Cwd), "test").

tmp_dir() ->
    {ok, Cwd} = file:get_cwd(),
    Cwd.

local_file() ->
    filename:join(test_dir(), "local_content").

remote_file() ->
    filename:join(test_dir(), "remote_content").

sig_correct_file() ->
    filename:join(test_dir(), "signature").

delta_correct_file() ->
    filename:join(test_dir(), "delta").

sig_output_file() ->
    filename:join(tmp_dir(), "signature").

delta_output_file() ->
    filename:join(tmp_dir(), "delta").

patched_file() ->
    filename:join(tmp_dir(), "patched").

all_files_exist_test() ->
    ?assertEqual(true, filelib:is_regular(local_file())),
    ?assertEqual(true, filelib:is_regular(remote_file())),
    ?assertEqual(true, filelib:is_regular(sig_correct_file())),
    ?assertEqual(true, filelib:is_regular(delta_correct_file())).

sig_1_file_test() ->
    ?assertEqual(file:read_file(sig_correct_file()),
		 rsync:sig(local_file())).

sig_1_binary_test() ->
    {ok, Sig} = file:read_file(local_file()),
    ?assertEqual(file:read_file(sig_correct_file()),
		 rsync:sig({raw, Sig})).

sig_2_file_test() ->
    ?assertEqual(ok, rsync:sig(local_file(),
			       sig_output_file())),
    ?assertEqual(file:read_file(sig_correct_file()),
		 file:read_file(sig_output_file())).

sig_2_binary_test() ->
    {ok, Sig} = file:read_file(local_file()),
    ?assertEqual(ok, rsync:sig({raw, Sig}, sig_output_file())),
    ?assertEqual(file:read_file(sig_correct_file()),
		 file:read_file(sig_output_file())).

delta_2_file_file_test() ->
    ?assertEqual(file:read_file(delta_correct_file()),
		 rsync:delta(sig_output_file(), remote_file())).

delta_2_binary_binary_test() ->
    {ok, RemoteData} = file:read_file(remote_file()),
    {ok, SigData} = file:read_file(sig_output_file()),
    ?assertEqual(file:read_file(delta_correct_file()),
		 rsync:delta({raw, SigData}, {raw, RemoteData})).

delta_3_file_file_test() ->
    ?assertEqual(ok, rsync:delta(sig_output_file(),
				 remote_file(),
				 delta_output_file())),
    ?assertEqual(file:read_file(delta_correct_file()),
		 file:read_file(delta_output_file())).

delta_3_binary_binary_test() ->
    {ok, SigData} = file:read_file(sig_output_file()),
    {ok, RemoteData} = file:read_file(remote_file()),
    ?assertEqual(ok, rsync:delta({raw, SigData},
				 {raw, RemoteData},
				 delta_output_file())),
    ?assertEqual(file:read_file(delta_correct_file()),
		 file:read_file(delta_output_file())).

patch_2_file_file_test() ->
    ?assertEqual(file:read_file(remote_file()),
		 rsync:patch(local_file(), delta_output_file())).

patch_2_binary_binary_test() ->
    {ok, LocalData} = file:read_file(local_file()),
    {ok, DeltaData} = file:read_file(delta_output_file()),
    ?assertEqual(file:read_file(remote_file()),
		 rsync:patch({raw, LocalData}, {raw, DeltaData})).

patch_3_file_file_test() ->
    ?assertEqual(ok, rsync:patch(local_file(),
				 delta_output_file(),
				 patched_file())),
    ?assertEqual(file:read_file(remote_file()),
		 file:read_file(patched_file())).

patch_3_binary_binary_test() ->
    {ok, LocalData} = file:read_file(local_file()),
    {ok, DeltaData} = file:read_file(delta_output_file()),
    ?assertEqual(ok, rsync:patch({raw, LocalData},
				 {raw, DeltaData},
				 patched_file())),
    ?assertEqual(file:read_file(remote_file()),
		 file:read_file(patched_file())).

sig_test() ->
    {ok, Data} = file:read_file(local_file()),
    Context = rsync:sig_init(),
    {ok, Res0} = rsync:sig_update(Context, Data),
    {ok, Res1} = rsync:sig_final(Context),
    ?assertEqual(ok, file:write_file(sig_output_file(),
				     <<Res0/binary, Res1/binary>>)),
    ?assertEqual(file:read_file(sig_correct_file()),
		 file:read_file(sig_output_file())).

delta_test() ->
    {ok, SigData} = file:read_file(sig_output_file()),
    Context = rsync:loadsig_init(),
    ok = rsync:loadsig_update(Context, SigData),
    ok = rsync:loadsig_final(Context),
    {ok, Data} = file:read_file(remote_file()),
    ok = rsync:delta_init(Context),
    {ok, Res0} = rsync:delta_update(Context, Data),
    {ok, Res1} = rsync:delta_final(Context),
    ?assertEqual(ok, file:write_file(delta_output_file(),
				     <<Res0/binary, Res1/binary>>)),
    ?assertEqual(file:read_file(delta_correct_file()),
		 file:read_file(delta_output_file())).

patch_test() ->
    {ok, LocalData} = file:read_file(local_file()),
    {ok, Delta} = file:read_file(delta_output_file()),
    Context = rsync:patch_init(LocalData),
    {ok, Res0} = rsync:patch_update(Context, Delta),
    {ok, Res1} = rsync:patch_final(Context),
    ?assertEqual(ok, file:write_file(patched_file(),
				     <<Res0/binary, Res1/binary>>)),
    ?assertEqual(file:read_file(remote_file()),
		 file:read_file(patched_file())).
