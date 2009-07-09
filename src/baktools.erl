%% ``The contents of this file are subject to the Erlang Public License,
%% Version 1.1, (the "License"); you may not use this file except in
%% compliance with the License. You should have received a copy of the
%% Erlang Public License along with this software. If not, it can be
%% retrieved via the world wide web at http://www.erlang.org/.
%% 
%% Software distributed under the License is distributed on an "AS IS"
%% basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See
%% the License for the specific language governing rights and limitations
%% under the License.
%% 
%% The Initial Developer of the Original Code is Jacob Perkins.
%% Portions created by Jacob Perkins are Copyright 2009, Mr Buzz, Inc.
%% All Rights Reserved.''
%%
%% @author Jacob Perkins
%% @copyright 2009 Mr Buzz, Inc.

%% @doc `mnesia' backup utility functions.

-module(baktools).

-export([backup/0, backup/1, backup/3]).

backup() -> backup(mnesia:system_info(tables)).

backup(Tables) -> backup(Tables, "backup", "/tmp/mnesia.bak").

%% @doc Activate, backup, then deactivate a checkpoint named `Name' with
%% minimal representation for `Tables'.
backup(Tables, Name, Bakfile) ->
	{ok, Name, _Nodes} = mnesia:activate_checkpoint([{name, Name}, {min, Tables}]),
	ok = mnesia:backup_checkpoint(Name, Bakfile),
	ok = mnesia:deactivate_checkpoint(Name).