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
	 get_unique_session_id/1,
	 reset_session_ids/0
	]).


-include_lib("automeck_common.hrl").
-include_lib("eunit/include/eunit.hrl").

reset_session_ids() ->
    ets:delete(automeck_sessions).

get_unique_session_id(Session) ->
    case ets:info(automeck_sessions) of
	undefined ->
	    ets:new(automeck_sessions, [set, public, named_table]);
	_ -> ok
    end, 
    SessionId = 
	case ets:lookup(automeck_sessions, Session) of
	    [] -> 0;
	    [{Session, Id}] -> Id
	end,
    ets:insert(automeck_sessions, {Session, SessionId+1}),
    SessionId.

parse_opts(OutputPath, Opts) ->
    SessionName = proplists:get_value(session_name, Opts, none),
    MockName = proplists:get_value(mock_name, Opts, "mocks"),
    SessionId = get_unique_session_id(SessionName),
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
