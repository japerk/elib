-module(datetime_SUITE).

-compile(export_all).

-include("ct.hrl").

%%%%%%%%%%%%%%%%%
%% common_test %%
%%%%%%%%%%%%%%%%%

all() -> [range_split_years, range_dates, in_range].

init_per_suite(Config) -> Config.

end_per_suite(Config) -> Config.

%%%%%%%%%%%%%%%%
%% test cases %%
%%%%%%%%%%%%%%%%

range_split_years(_Config) ->
	Jan1_08 = {2008, 1, 1},
	Dec1_08 = {2008, 12, 1},
	[{Jan1_08, Dec1_08}] = datetime:range_split_years({Jan1_08, Dec1_08}),
	
	Dec31_08 = {2008, 12, 31},
	Jan1_09 = {2009, 1, 1},
	May2_09 = {2009, 5, 2},
	TwoYear = [{Jan1_08, Dec31_08}, {Jan1_09, May2_09}],
	TwoYear = datetime:range_split_years({Jan1_08, May2_09}),
	
	Dec31_09 = {2009, 12, 31},
	Jan1_10 = {2010, 1, 1},
	Feb5_10 = {2010, 2, 5},
	MultiYear = [{Jan1_08, Dec31_08}, {Jan1_09, Dec31_09}, {Jan1_10, Feb5_10}],
	MultiYear = datetime:range_split_years({Jan1_08, Feb5_10}).

range_dates(_Config) ->
	Jan1to3 = [{2008, 1, 1}, {2008, 1, 2}, {2008, 1, 3}],
	Jan1to3 = datetime:range_dates({{2008, 1, 1}, {2008, 1, 3}}),
	
	Jan30toFeb2 = [{2008, 1, 30}, {2008, 1, 31}, {2008, 2, 1}, {2008, 2, 2}],
	Jan30toFeb2 = datetime:range_dates({{2008, 1, 30}, {2008, 2, 2}}),
	
	Dec30toJan2 = [{2008, 12, 30}, {2008, 12, 31}, {2009, 1, 1}, {2009, 1, 2}],
	Dec30toJan2 = datetime:range_dates({{2008, 12, 30}, {2009, 1, 2}}).

in_range(_Config) ->
	% TODO: test datetime:in_range/2
	Datetime = {{2009, 3, 13}, {11, 9, 30}},
	Date = {2009, 3, 13},
	MarchDateRange = {{2009, 3, 1}, {2009, 3, 31}},
	FebDateRange = {{2009, 2, 1}, {2009, 2, 28}},
	
	true = datetime:in_range(Datetime, MarchDateRange),
	true = datetime:in_range(Date, MarchDateRange),
	false = datetime:in_range(Datetime, FebDateRange),
	false = datetime:in_range(Datetime, FebDateRange).
	% TODO: more test cases
