%% Copyright (c) 2011 Opscode, Inc.  All Rights Reserved.
%%
%% This file is provided to you under the Apache License,
%% Version 2.0 (the "License"); you may not use this file
%% except in compliance with the License.  You may obtain
%% a copy of the License at
%%
%%   http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing,
%% software distributed under the License is distributed on an
%% "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
%% KIND, either express or implied.  See the License for the
%% specific language governing permissions and limitations
%% under the License.
-module(automeck_record).

-define(ARG_PROTOS,  [{0, ""},
                      {1, "A"},
                      {2, "A,B"},
                      {3, "A,B,C"},
                      {4, "A,B,C,D"},
                      {5, "A,B,C,D,E"},
                      {6, "A,B,C,D,E,F"},
                      {7, "A,B,C,D,E,F,G"},
                      {8, "A,B,C,D,E,F,G,H"},
                      {9, "A,B,C,D,E,F,G,H,I"},
                      {10, "A,B,C,D,E,F,G,H,I,J"}]).

-export([from_file/1,
         from_list/1,
         log_result/5,
         finish/1,
         abort/1]).

from_file(Path) ->
    {ok, Descs} = file:consult(Path),
    from_list(Descs).

from_list([{record, OutputPath, Descs0}]) ->
    Descs = [{Module, Exports, first} || {Module, Exports} <- Descs0],
    FileName = output_file(OutputPath),
    ok = filelib:ensure_dir(FileName),
    file:delete(FileName),
    ok = insert_interceptors(FileName, Descs),
    {ok, FileName}.

abort(FileName) ->
    meck:unload(),
    file:delete(FileName),
    ok.

finish(FileName) ->
    meck:unload(),
    {ok, Calls} = file:consult(FileName),
    MockConfig = generate_mock_config(Calls),
    save_mock_config(filename:dirname(FileName), MockConfig).

insert_interceptors(_OutputFile, []) ->
    ok;
insert_interceptors(OutputFile, [{Module, all, first}|T]) ->
    Exports = Module:module_info(exports),
    insert_interceptors(OutputFile, [{Module, Exports, first}|T]);
insert_interceptors(OutputFile, [{Module, Exports, first}|T]) ->
    {module, Module} = code:ensure_loaded(Module),
    meck:new(Module),
    insert_interceptors(OutputFile, [{Module, Exports}|T]);
insert_interceptors(OutputFile, [{Module, Exports}|T]) ->
    OrigModule = list_to_atom(atom_to_list(Module) ++ "_meck_original"),
    AllExports = sets:from_list(OrigModule:module_info(exports)),
    PassthruExports = sets:to_list(sets:subtract(AllExports, sets:from_list(Exports))),
    [build_interceptor(OutputFile, Module, OrigModule, Name, Arity) || {Name, Arity} <- Exports,
                                                                       Name /= module_info],
    [build_passthru(Module, Name, Arity) || {Name, Arity} <- PassthruExports,
                                            Name /= module_info],
    insert_interceptors(OutputFile, T).

build_interceptor(OutputFile, Module, OrigModule, Name, Arity) ->
    Args = proplists:get_value(Arity, ?ARG_PROTOS),
    Fmt = "fun(~s) -> R = ~p:~p(~s), automeck_record:log_result(~p, ~p, ~p, ~s, R), R end.",
    Code = lists:flatten(io_lib:format(Fmt, [Args, OrigModule, Name, Args, OutputFile,
                                             Module, Name, "[" ++ Args ++ "]"])),
    meck:expect(Module, Name, automeck_compile:compile(Code)).

build_passthru(Module, Name, Arity) ->
    Args = proplists:get_value(Arity, ?ARG_PROTOS),
    Fmt = "fun(~s) -> meck:passthrough([~s]) end.",
    Code = lists:flatten(io_lib:format(Fmt, [Args, Args])),
    meck:expect(Module, Name, automeck_compile:compile(Code)).

log_result(OutputFile, Module, Fun, Args, Result) ->
    file:write_file(OutputFile, io_lib:format("{~p, ~p, [{~p, ~p}]}.~n",
                                            [Module, Fun, Args, Result]),
                    [append]).

output_file(OutputPath) ->
    filename:join([OutputPath, "automeck_record.session"]).

generate_mock_config(Calls) ->
    Config0 = generate_mock_config(Calls, orddict:new()),
    {mock, [{Mod, Name, lists:usort(Impls)} || {{Mod, Name}, Impls} <- orddict:to_list(Config0)]}.

generate_mock_config([], Config) ->
    Config;
generate_mock_config([{Module, Name, Impl}|T], Config) ->
    Key = {Module, Name},
    Config1 = case orddict:is_key(Key, Config) of
                  false ->
                      orddict:store(Key, Impl, Config);
                  true ->
                      orddict:append_list(Key, Impl, Config)
              end,
    generate_mock_config(T, Config1).

save_mock_config(OutputDir, Config) ->
    OutputFile = filename:join([OutputDir, "mocks.config"]),
    ok = file:write_file(OutputFile, io_lib:format("~p.~n", [Config])),
    {ok, OutputFile}.
