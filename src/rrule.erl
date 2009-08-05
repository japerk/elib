%% @doc rrule provides a set of function to related to time recurrence.
%% 
%%
%% @type constraint() = {atom(), term()}
%% @type rule() = tuple(list(constraint()), DurationInHour::integer())
%% @type rules() = list(rule())
%%
%% @author Bruno Mahe
-module(rrule).


-export([in_range/2]).
-export([satisfy/3]).
-export([merge_datetimes/3, merge_datetimes/1]).


%% @doc Convert day of the week into an integer usable by the calendar module
%% @see calendar:day_of_the_week/1
%% @see calendar:day_of_the_week/3
%% @spec day_of_the_week(atom()) -> integer()
day_of_the_week(monday)    -> 1;
day_of_the_week(tuesday)   -> 2;
day_of_the_week(wednesday) -> 3;
day_of_the_week(thursday)  -> 4;
day_of_the_week(friday)    -> 5;
day_of_the_week(saturday)  -> 6;
day_of_the_week(sunday)    -> 7.


%% @doc Convert month name into an integer 
%% @spec month(atom()) -> integer()
month(january)   ->  1;
month(february)  ->  2;
month(march)     ->  3;
month(april)     ->  4;
month(may)       ->  5;
month(june)      ->  6;
month(july)      ->  7;
month(august)    ->  8;
month(september) ->  9;
month(october)   -> 10;
month(november)  -> 11;
month(december)  -> 11.





find_month_in_range(Month, BeginningRangeDateTime, EndRangeDateTime) ->
	find_month_in_range(Month, BeginningRangeDateTime, EndRangeDateTime, []).


find_month_in_range(Month, BeginningRangeDateTime, EndRangeDateTime, ListMonths) ->
		
		{ {BeginRangeYear, BeginRangeMonth, BeginRangeDay}, BeginningRangeTime} = BeginningRangeDateTime,

		{ {EndRangeYear, EndRangeMonth, EndRangeDay}, EndRangeTime} = EndRangeDateTime,

		{Begin, End} = if 
								BeginRangeYear =:= EndRangeYear,
									Month >= BeginRangeMonth, 
									Month =< EndRangeMonth ->
													{EndDay, EndTime} = if
																					EndRangeMonth =:= Month ->  {EndRangeDay, EndRangeTime};
																					true -> {calendar:last_day_of_the_month(EndRangeYear, Month), {23, 59, 59}}
																				end,
													{BeginDay, BeginTime} = if
																						BeginRangeMonth =:= Month ->  {BeginRangeDay, BeginningRangeTime};
																						true -> {1, {0, 0, 0}}
																					end,
													{
														{ {EndRangeYear, Month, BeginDay}, BeginTime },
														{ {EndRangeYear, Month,   EndDay}, EndTime   }	
													};

								BeginRangeYear =/= EndRangeYear,
									BeginRangeYear < EndRangeYear,
									Month < EndRangeMonth -> 
																		{
																			{ {EndRangeYear, Month, 1}, {0, 0, 0} },
																			{ {EndRangeYear, Month, calendar:last_day_of_the_month(EndRangeYear, Month)}, {23, 59, 59} }
																		};

								BeginRangeYear =/= EndRangeYear,
									BeginRangeYear < EndRangeYear,
									Month =:= EndRangeMonth -> 
																		{
																			{ {EndRangeYear, Month, 1}, {0, 0, 0} },
																			{ {EndRangeYear, Month, EndRangeDay}, EndRangeTime }
																		};

								BeginRangeYear =:= EndRangeYear - 1,
									BeginRangeYear < EndRangeYear,
									BeginRangeMonth =:= Month -> 
																		{
																			BeginningRangeDateTime,
																			{ {EndRangeYear-1, Month, calendar:last_day_of_the_month(EndRangeYear, Month)}, {23, 59, 59} }
																		};
								
								BeginRangeYear =:= EndRangeYear - 1,
									BeginRangeYear < EndRangeYear,
									BeginRangeMonth < Month -> 
																		{
																			{ {EndRangeYear-1, Month, 1}, {0, 0, 0} },
																			{ {EndRangeYear-1, Month, calendar:last_day_of_the_month(EndRangeYear, Month)}, {23, 59, 59} }
																		};
													
								BeginRangeYear =/= EndRangeYear,
									BeginRangeYear < EndRangeYear -> 
																		{
																			{ {EndRangeYear-1, Month, 1}, {0, 0, 0} },
																			{ {EndRangeYear-1, Month, calendar:last_day_of_the_month(EndRangeYear, Month)}, {23, 59, 59} }
																		};
								true -> {none, none}
		end,


		case {Begin, End} of

			{none, none} -> ListMonths;
			_ -> 
					TBegin = calendar:datetime_to_gregorian_seconds(Begin),
					TEnd = calendar:datetime_to_gregorian_seconds(End),
					TBeginningRangeDataTime = calendar:datetime_to_gregorian_seconds(BeginningRangeDateTime),
					if
						TBegin >= TBeginningRangeDataTime,
						TBegin =< TEnd -> 
										find_month_in_range(Month, BeginningRangeDateTime, datetime:time_diff(Begin, -3600*24), [{Begin, End} | ListMonths]);
						true -> ListMonths
					end

		end.
		

find_day_in_range(Day, BeginningRangeDateTime, EndRangeDateTime) ->
	find_day_in_range(Day, BeginningRangeDateTime, EndRangeDateTime, []).

find_day_in_range(Day, BeginningRangeDateTime, EndRangeDateTime, ListDays) ->
	
		{BeginningRangeDate, BeginningRangeTime} = BeginningRangeDateTime,

		{EndRangeDate, _} = EndRangeDateTime,
		{_, _, EndRangeDay} = EndRangeDate,

		FoundListDays = case EndRangeDay of
										Day -> 	BeginSubRange = case EndRangeDate of
																					BeginningRangeDate -> {EndRangeDate, BeginningRangeTime};
																					_ -> {EndRangeDate, {0, 0, 0}}
																		end,
													[{BeginSubRange, EndRangeDateTime} | ListDays];
										_ ->  ListDays
							end,

		{NewEndRangeDate, _} = datetime:time_diff(EndRangeDateTime, -3600*24),

		
		TBegin = calendar:datetime_to_gregorian_seconds(BeginningRangeDateTime),
		TEnd = calendar:datetime_to_gregorian_seconds({NewEndRangeDate, {23,59,59}}),

		if 
			TBegin =< TEnd ->	find_day_in_range(Day, BeginningRangeDateTime, {NewEndRangeDate, {23,59,59}}, FoundListDays);
			true -> FoundListDays
		end.


%% @doc Returns a list or ranges which satisfy a list of constraints 
%% @spec satisfy(rule(), datetime(), datetime()) -> list({datetime(), datetime())
satisfy({[], _}, When, Until) -> {When, Until};

satisfy({[{month, Month}| Constraints], Duration}, BeginRange, EndRange) when is_atom(Month) ->
		ConstraintMonth = month(Month),

		satisfy({[{month, ConstraintMonth}|Constraints], Duration}, BeginRange, EndRange);

satisfy({[{month, Month}|Constraints], Duration}, BeginRange, EndRange) when is_integer(Month), Month > 0, Month < 13 ->

		CheckRanges = fun({BeginSubRange, EndSubRange}) ->
									satisfy({Constraints, Duration}, BeginSubRange, EndSubRange)
							end,

		lists:map(
						CheckRanges,
						find_month_in_range(Month, BeginRange, EndRange)
					);

satisfy({[{day, Day}|Constraints], Duration}, BeginRange, EndRange) when is_integer(Day), Day > 0, Day < 32 ->

		CheckRanges = fun({BeginSubRange, EndSubRange}) ->
									satisfy({Constraints, Duration}, BeginSubRange, EndSubRange)
							end,

		lists:map(
						CheckRanges,
						find_day_in_range(Day, BeginRange, EndRange)
					);

satisfy({[{weekday, WeekDay}|Constraints], Duration}, BeginRange, EndRange) when is_atom(WeekDay) ->
	
		{BeginRangeDate, BeginRangeTime} = BeginRange,
	
		{EndRangeDate, EndRangeTime} = EndRange,


		ListDates = [ Date || Date <- datetime:range_dates({BeginRangeDate, EndRangeDate}), calendar:day_of_the_week(Date) =:= day_of_the_week(WeekDay) ],
		
		CompleteDates = lists:map(
												fun(Date) when Date =:= BeginRangeDate, Date =:= EndRangeDate ->
																				{ {Date, BeginRangeTime}, {Date, EndRangeTime}};
													(Date) when Date =:= BeginRangeDate ->
																				{ {Date, BeginRangeTime}, {Date, {23, 59, 59}}};
													(Date) when Date =:= EndRangeDate ->
																				{ {Date, {0, 0, 0}}, {Date, EndRangeTime}};
													(Date) -> 
																				{ {Date, {0, 0, 0}}, {Date, {23, 59, 59}}}
												end,
												ListDates
											),

		CheckRanges = fun({BeginSubRange, EndSubRange}) ->
									satisfy({Constraints, Duration}, BeginSubRange, EndSubRange)
							end,

		lists:map(
						CheckRanges,
						CompleteDates
					);



satisfy({[{time, Hour}|Constraints], Duration}, BeginRange, EndRange) when is_integer(Hour), Hour > 0, Hour < 25 ->
						satisfy({[{time, {Hour, 0}}|Constraints], Duration}, BeginRange, EndRange);
satisfy({[{time, {Hour, Minutes}}|Constraints], Duration}, BeginRange, EndRange) when is_integer(Hour), Hour > 0, Hour < 25 ->

		{BeginRangeDate, BeginRangeTime} = BeginRange,
		{BeginRangeHour, BeginRangeMinute, BeginRangeSeconds} = BeginRangeTime,
	
		{EndRangeDate, EndRangeTime} = EndRange,
		{EndRangeHour, EndRangeMinute, EndRangeSeconds} = EndRangeTime,


		ListDates = datetime:range_dates({BeginRangeDate, EndRangeDate}),
		
		CompleteDates = lists:map(
												fun(Date) ->

														Begin = if
																	Date =:= BeginRangeDate,
																		Hour*3600+Minutes*60 < BeginRangeHour*3600+60*BeginRangeMinute+BeginRangeSeconds -> {Date, {BeginRangeHour, BeginRangeMinute, BeginRangeSeconds}};

																	 Date =:= EndRangeDate,
																	 	Hour*60+Minutes > EndRangeHour*60 -> none;

																	true -> {Date, {Hour, Minutes, 0}}
																end,

														case Begin of

																none -> {none, none};
																_ -> 
																
																
																		DatePlusDuration = datetime:time_diff({Date, {Hour, Minutes, 0}}, Duration),

																		DatePlusDurationInSec = calendar:datetime_to_gregorian_seconds(DatePlusDuration),
																		EndRangeInSec = calendar:datetime_to_gregorian_seconds(EndRange),

																		End = if
																					Date =:= EndRangeDate,
																					DatePlusDurationInSec >= EndRangeInSec  -> {EndRangeDate, {EndRangeHour, EndRangeMinute, EndRangeSeconds}};
			
																				true -> DatePlusDuration
																			end,
			
																		{Begin, End}
															end

												end,
												ListDates
											),

		CheckRanges = fun({BeginSubRange, EndSubRange}) ->
									case {BeginSubRange, EndSubRange} of
												{none, none} -> [];
												_ -> satisfy({Constraints, Duration}, BeginSubRange, EndSubRange)
									end
							end,

		lists:map(
						CheckRanges,
						CompleteDates
					).

%% @doc Merge a list of datetimes.
%% @spec merge_datetimes(list(datetime())) -> list(datetime())
merge_datetimes(DateTimes) ->
				merge_datetimes(DateTimes, DateTimes, []).

merge_datetimes(DTs, [DateTime|ListDateTimes], Result) ->
			{DTBegin, DTEnd} = DateTime,

			Merge = lists:map(
										fun({DB, DE}) when  DTBegin < DB andalso DB < DTEnd ->

														if
															DTEnd < DE -> {DTBegin, DE};
															true 			->	[]
														end;
												({ {{_,_,_},{_,_,_}}, {{_,_,_}, {_,_,_}} }) ->
														[]
										end,
										DTs
									),

			merge_datetimes(DTs, ListDateTimes, [DateTime,Merge|Result]);

merge_datetimes(_, [], Result) ->
				lists:flatten(Result).


%% @doc Check if a range match the constraints defined by some rules
%% @spec in_range(rules(), {datetime(), datetime()}) -> bool()
in_range(Rules, {When, Until}) when is_list(Rules) -> lists:any(fun(Rule) -> in_range(Rule, {When, Until})end, Rules);

in_range(Rule, {When, Until}) when is_tuple(Rule) -> 


		Candidates = lists:flatten(
							satisfy(Rule, When, Until)
						),
						
						
		lists:any(
						fun({Begin, End}) ->
							datetime:range_in_range({When, Until}, {Begin, End})
						end,
						
						merge_datetimes(Candidates, Candidates, [])

					).



