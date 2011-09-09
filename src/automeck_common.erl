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
-module(automeck_common).

-export([parse_opts/2,
	 generate_filename/3,
	 output_file/2,
	 conf_file/2,
	 get_session_id/1,
	 increment_session_id/1,
	 reset_session_ids/0,
	 escape_data/1,
	 unescape_data/1
	]).


-include_lib("automeck_common.hrl").
-include_lib("eunit/include/eunit.hrl").

reset_session_ids() ->
    ets:delete(automeck_sessions).

init_ets_table() ->
    case ets:info(automeck_sessions) of
	undefined -> ets:new(automeck_sessions, [set, public, named_table]);
	_ -> ok
    end,
    automeck_sessions.

get_session_id(Session) ->
    Table = init_ets_table(),
%    ?debugVal(ets:info(Table)),
    case ets:lookup(Table, Session) of
	[] -> 0;
	[{Session, Id}] -> Id
    end.

increment_session_id(Session) ->
    Table = init_ets_table(),
%    ?debugVal(ets:info(Table)),
    SessionId = 
	case ets:lookup(Table, Session) of
	    [] -> 0;
	    [{Session, Id}] -> Id
	end,
    ets:insert(Table, {Session, SessionId+1}),
    SessionId.

parse_opts(OutputPath, Opts) ->
    SessionName = proplists:get_value(session_name, Opts, none),
    MockName = proplists:get_value(mock_name, Opts, "mocks"),
    SessionId = get_session_id(SessionName),
    OutputPath2 = 
	case proplists:get_value(output_path, Opts) of
	    undefined -> OutputPath;
	    Path -> Path
	end,
    #automeck_state{session_name = SessionName,
		    session_id = SessionId,
		    mock_name = MockName,
		    output_path = OutputPath2,
		    opts = Opts}.

generate_filename(BaseName, Ext,
		  #automeck_state{session_name = SessionName, 
				  session_id =  SessionId}) ->
    FilenameParts = 
	case SessionName of
	    none -> [BaseName, Ext];
	    _ -> [BaseName, "_" , 
		  atom_to_list(SessionName), "_", integer_to_list(SessionId),
		  Ext]
	end,
    lists:flatten(FilenameParts).


output_file(OutputPath, #automeck_state{} = State) ->
    filename:join([OutputPath, 
		   generate_filename("automeck_record", ".session", State)]).

conf_file(OutputPath, #automeck_state{} = State) ->
    filename:join([OutputPath, 
		   generate_filename(State#automeck_state.mock_name, 
				     ".config", State)]).


%%%
%%% Some entities are not properly parseable by file:consult. These include
%%% pids and refs. Convert them into a form that doesn't choke consult.
%%%
escape_data(Term) when is_pid(Term) ->
    {'_automeck_pid', erlang:pid_to_list(Term)};
escape_data(Term) when is_reference(Term) ->
    {'_automeck_ref', erlang:ref_to_list(Term)};
escape_data(Term) when is_list(Term) ->
    escape_data(Term, []);
escape_data(Term) when is_tuple(Term) ->
%    ?debugVal(Term),
    R = list_to_tuple(escape_data(tuple_to_list(Term), [])),
%    ?debugVal(R)
    R;
escape_data(Term) -> 
    Term.

%%% Reverses lists; don't forget to reverse result
escape_data([], Acc) ->
    lists:reverse(Acc);
escape_data([H|T], Acc) ->
    escape_data(T, [escape_data(H) | Acc]);
escape_data(X, Acc) -> % improper list
%    ?debugVal({X, Acc}),
    R = prepend_list_reverse(escape_data(X), Acc),
%    ?debugVal(R),
    R.

%%% prepends B onto A, reversing B. B must be a proper list.
prepend_list_reverse(A, []) -> 
    A;
prepend_list_reverse(A, [H|T]) -> 
    prepend_list_reverse([H | A], T).
    


%%% This may not be the best idea; perhaps should generate unique pid/refs for each 
%%% incoming escaped version
unescape_data({'_automeck_pid', PidString})  ->
    {'_automeck_pid', PidString};
unescape_data({'_automeck_ref', RefString}) ->
    {'_automeck_ref', RefString};
unescape_data(Term) when is_list(Term) ->
    unescape_data(Term, []);
unescape_data(Term) when is_tuple(Term) ->
      list_to_tuple(unescape_data(tuple_to_list(Term), []));
unescape_data(Term) -> 
    Term.

unescape_data([], Acc) ->
    lists:reverse(Acc);
unescape_data([H|T], Acc) ->
    unescape_data(T, [unescape_data(H) | Acc]);
unescape_data(X, Acc) -> % improper list
%    ?debugVal({X, Acc}),
    R = prepend_list_reverse(unescape_data(X), Acc),
%    ?debugVal(R),
    R.
