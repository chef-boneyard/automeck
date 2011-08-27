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
-module(automeck_compile).

-export([compile/1,
         compile/2]).

compile(Code) ->
    compile(Code, []).

compile(Code, Bindings) ->
    {ok, Toks, _} = erl_scan:string(Code),
    {ok, Exprs} = erl_parse:parse_exprs(Toks),
    {value, Value, _} = erl_eval:exprs(Exprs, Bindings),
    Value.
