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

%% @doc Release packaging and handling functions.

-module(reltools).

-export([make_release/1, make_release/2, make_upgrade/1,
		 make_upgrade/2, make_upgrade/3]).
-export([create_RELEASES/1, create_RELEASES/3, current_release/0,
		 permanent_release/0, install_release/1, set_release_vsn/1]).

%%%%%%%%%%%%%%%%%%%%%%%%
%% systools utilities %%
%%%%%%%%%%%%%%%%%%%%%%%%

base_opts(Root) ->
	Path = filename:join([Root, "releases"]),
	[{path, [Path]}, {outdir, filename:join(Root, "releases")}].

%% @doc Make release with default root directory.
make_release(Name) -> make_release(Name, code:root_dir()).

% TODO: adapt make_release to do what actually works for creating a first-target
% system. Could actually go in and edit bin/erl and bin/start, as well as make
% RELEASES file and start_erl.data file, and copy in start.boot. Then it would
% be complete except for CLI args, but those could be included as a string option.
% Can assume code:root_dir() is for erts location and boot stuff, but pass in 
% opts with additional dir info.

%% @doc Uses systools to make boot scripts and release package. Opts should only
%% contain options common to systools:make_script/2 and systools:make_tar/2.
make_release(Name, Root) ->
	Opts = [no_module_tests | base_opts(Root)],
	ok = systools:make_script(Name, Opts),
	ok = systools:make_tar(Name, [{dirs, [include, src]}, {erts, Root} | Opts]),
	% cleanup script and boot files
	BaseName = filename:basename(Name),
	io:format("~s~n", [filename:join([Root, "releases", BaseName ++ ".tar.gz"])]),
	ok = file:delete(filename:join([Root, "releases", BaseName ++ ".script"])),
	ok = file:delete(filename:join([Root, "releases", BaseName ++ ".boot"])).

% NOTE: for this to work, need to use erl from the target system, which is probably
% not the one in your PATH
make_upgrade(Name) ->
	% make sure sasl and release_handler is started
	application:start(sasl),
	{Old, Vsn} = current_release(),
	% old release file location is $ROOT/releases/Vsn/Old.rel
	From = filename:join([code:root_dir(), "releases", Vsn, Old]),
	make_upgrade(Name, From).

%% @doc Make upgrade release with default root directory.
make_upgrade(Name, From) -> make_upgrade(Name, From, code:root_dir()).

%% @doc Make upgrade release.
make_upgrade(Name, From, Root) ->
	ok = systools:make_relup(Name, [From], [From], base_opts(Root)),
	ok = make_release(Name, Root),
	% cleanup relup
	ok = file:delete(filename:join([Root, "releases", "relup"])).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% release_handler utilities %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% @doc Create initial RELEASES file for named release.
create_RELEASES(Name) -> create_RELEASES(code:root_dir(), Name, []).

%% @doc Create initial RELEASES file for named release in Root dir with Apps.
create_RELEASES(Root, Name, Apps) ->
	RelDir = filename:join(Root, "releases"),
	RelFile = filename:join(RelDir, Name ++ ".rel"),
	release_handler:create_RELEASES(Root, RelDir, RelFile, Apps).

current_release() ->
	case lists:keysearch(current, 4, release_handler:which_releases()) of
		{value, {Name, Vsn, _, current}} -> {Name, Vsn};
		false -> permanent_release()
	end.

permanent_release() ->
	case lists:keysearch(permanent, 4, release_handler:which_releases()) of
		{value, {Name, Vsn, _, permanent}} -> {Name, Vsn};
		false -> undefined
	end.

%% @doc Permanently install named release. Assumes Name.tar.gz is located in
%% release directory.
install_release(Name) ->
	{ok, Vsn} = release_handler:unpack_release(Name),
	set_release_vsn(Vsn).

%% @doc Set permanent release version.
set_release_vsn(Vsn) ->
	{ok, Old, Desc} = release_handler:check_install_release(Vsn),
	{ok, Old, Desc} = release_handler:install_release(Vsn),
	ok = release_handler:make_permanent(Vsn),
	{ok, Vsn, Old, Desc}.
