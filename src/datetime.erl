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

%% @doc Datetime conversion functions to convert to and from datetime tuples
%% for standard formats.
%%
%% @type date() = {Year::integer(), Month::integer(), Day::integer()}
%% @type time() = {Hour::integer(), Min::integer(), Sec::integer()}
%% @type datetime() = {date(), time()}
%% @type daterange() = {date(), date()}

-module(datetime).

-export([fromstring/1, tostring/1, iso_datetime/1, iso_string/1]).
-export([time_diff/2, seconds_diff/2]).
-export([range_split_years/1, range_dates/1, in_range/2]).

%%%%%%%%%%%%%%%%%%%%%%%%
%% string conversions %%
%%%%%%%%%%%%%%%%%%%%%%%%

%% @doc Convert rfc1123 format string to a datetime tuple.
%% @spec fromstring(string()) -> datetime()
%% @equiv httpd_util:convert_request_date
fromstring(S) -> httpd_util:convert_request_date(S).

%% @doc Convert a UTC datetime tuple to a rfc1123 format string.
%% @spec tostring(datetime()) -> string()
%% @see httpd_util:rfc1123_date/1
%% @see calendar:universal_time_to_local_time/1
tostring(DateTime) ->
	% rfc1123_date assumes that DateTime is a local time, and converts it to
	% universal/gmt before converting to a string, so we need to compensate
	httpd_util:rfc1123_date(calendar:universal_time_to_local_time(DateTime)).

%% @doc Convert ISO format string to a datetime tuple.
%% @spec iso_datetime(string()) -> datetime()
iso_datetime(S) ->
	S2 = string:strip(string:strip(S, both, $"), right, $Z),
	
	case estring:splitc(S2, $T) of
		{S2, []} ->
			{Date, Time} = estring:splitc(S2, $ ),
			iso_datetime(Date, Time);
		{Date, Time} ->
			iso_datetime(Date, Time)
	end.

iso_datetime(Date, Time) ->
	{ok, [Year, Mon, Day], []} = io_lib:fread("~d-~d-~d", Date),
	{ok, [Hour, Min, Sec], []} = io_lib:fread("~d:~d:~d", Time),
	{{Year, Mon, Day}, {Hour, Min, Sec}}.

%% @doc Convert date or datetime tuple to ISO format string.
%% @spec iso_string(Date::date()|datetime()) -> string()
iso_string({Year, Mon, Day}) ->
	iso_string({{Year, Mon, Day}, {0, 0, 0}});
iso_string({{Year, Mon, Day}, {Hour, Min, Sec}}) ->
	Args = [Year, Mon, Day, Hour, Min, Sec],
	S = io_lib:format("~B-~2.10.0B-~2.10.0BT~2.10.0B:~2.10.0B:~2.10.0BZ", Args),
	lists:flatten(S).

%%%%%%%%%%%%%%%
%% time math %%
%%%%%%%%%%%%%%%

time_diff(DateTime, Seconds) ->
	Secs = calendar:datetime_to_gregorian_seconds(DateTime) + Seconds,
	calendar:gregorian_seconds_to_datetime(Secs).

seconds_diff(DT1, DT2) ->
	Secs1 = calendar:datetime_to_gregorian_seconds(DT1),
	Secs2 = calendar:datetime_to_gregorian_seconds(DT2),
	abs(Secs1 - Secs2).

%%%%%%%%%%%%%%%%%
%% date ranges %%
%%%%%%%%%%%%%%%%%

%% @doc Given a date range, returns a list of date ranges, each range inclusive
%% to the same year. The first range begins with the first date, and the last
%% range ends with the second date.
%% @spec range_split_years(daterange()) -> [daterange()]
range_split_years({When, Until}) when Until < When ->
	[];
range_split_years({When, When}) ->
	[{When, When}];
range_split_years({{Year, _, _}=When, {Year, _, _}=Until}) ->
	[{When, Until}];
range_split_years({{Year, _, _}=When, Until}) ->
	range_split_years([{When, {Year, 12, 31}}], Year + 1, Until).

range_split_years(Ranges, Year, {Year, _, _}=Until) ->
	lists:reverse([{{Year, 1, 1}, Until} | Ranges]);
range_split_years(Ranges, Year, Until) ->
	range_split_years([{{Year, 1, 1}, {Year, 12, 31}} | Ranges], Year + 1, Until).

%% @doc Expand the date range into a list of all dates included in the range.
%% @spec range_dates(daterane()) -> [date()]
range_dates({When, Until}) when Until < When ->
	[];
range_dates({When, When}) ->
	[When];
range_dates({{Year, Month, Day}=When, Until}) ->
	Days = calendar:date_to_gregorian_days(Year, Month, Day),
	range_dates([When], Days + 1, Until).

range_dates([Until | _]=Dates, _, Until) ->
	lists:reverse(Dates);
range_dates(Dates, Days, Until) ->
	Date = calendar:gregorian_days_to_date(Days),
	range_dates([Date | Dates], Days + 1, Until).

in_range({_, _, _}=Date, {When, Until}) ->
	in_range({Date, {0, 0, 0}}, {When, Until});
in_range(Datetime, {{_, _}=When, {_, _}=Until}) ->
	Datetime >= When andalso Datetime =< Until;
in_range(Datetime, {{_, _, _}=When, {_, _, _}=Until}) ->
	in_range(Datetime, {{When, {0, 0, 0}}, {Until, {23, 59, 59}}});
in_range(_, _) ->
	throw(badarg).
