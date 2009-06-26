%% @doc Datetime conversion functions to convert to and from datetime tuples
%% for standard formats.
%%
%% @type datetime() = {date(), time()}
%% @type date() = {Year::integer(), Month::integer(), Day::integer()}
%% @type time() = {Hour::integer(), Min::integer(), Sec::integer()}
%% @type daterange() = {date(), date()}
%%
%% @author Bruno Mahe
-module(rrule).


-compile(export_all).


day_of_the_week(monday)    -> 1;
day_of_the_week(tuesday)   -> 2;
day_of_the_week(wednesday) -> 3;
day_of_the_week(thursday)  -> 4;
day_of_the_week(friday)    -> 5;
day_of_the_week(saturday)  -> 6;
day_of_the_week(sunday)    -> 7.


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



new() -> [].

%				Constraints                                Duration
%Rule => {[{month, june}, {weekday, monday}, {hour, 8}], 8}) 
%Rules => [Rule]


add_rule(Rules, Rule) -> [Rule|Rules].


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
		{BeginRangeYear, BeginRangeMonth, BeginRangeDay} = BeginningRangeDate,

		{EndRangeDate, EndRangeTime} = EndRangeDateTime,
		{EndRangeYear, EndRangeMonth, EndRangeDay} = EndRangeDate,

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

find_hour_in_range(Hour, Duration, BeginningRangeDateTime, EndRangeDateTime) ->
	find_hour_in_range(Hour, Duration, BeginningRangeDateTime, EndRangeDateTime, []).

find_hour_in_range(Hour, Duration, BeginningRangeDateTime, EndRangeDateTime, ListHours) ->
	
		{BeginningRangeDate, BeginningRangeTime} = BeginningRangeDateTime,
		{BeginRangeYear, BeginRangeMonth, BeginRangeDay} = BeginningRangeDate,
		{BeginningRangeHour, BeginningRangeMinute, BeginningRangeSeconds} = BeginningRangeTime,

		TBeginRange = calendar:datetime_to_gregorian_seconds(BeginningRangeDateTime),

		{EndRangeDate, EndRangeTime} = EndRangeDateTime,
		{EndRangeYear, EndRangeMonth, EndRangeDay} = EndRangeDate,
		{EndRangeHour, EndRangeMinute, EndRangeSeconds} = EndRangeTime,


		TEndRange = calendar:datetime_to_gregorian_seconds(EndRangeDateTime),
		

		{Begin, End} = if 

								TEndRange - TBeginRange < Duration * 3600 -> {none, none};

								Hour =< EndRangeHour ->
																NewBeginningDT = { EndRangeDate, {Hour, 0, 0} },
														
																Diff = datetime:seconds_diff(NewBeginningDT, EndRangeDateTime),
																if
																		Diff < Duration*3600 ->
																										{
																											NewBeginningDT,
																											EndRangeDateTime
																										};
																		true -> 
																										{
																											NewBeginningDT,
																											datetime:time_diff(NewBeginningDT, Duration*3600)
																										}
																end;

								Hour > EndRangeHour,
									TEndRange - TBeginRange > 48 ->
																	{PreviousDay, PreviousTime} = datetime:time_diff(EndRangeDateTime, -3600*24),
																
																	{ 
																		{ PreviousDay, {Hour, 0, 0} },
																		datetime:time_diff({PreviousDay, {Hour, 0, 0}}, Duration*3600)
																	};
																	

								true -> {none, none}


							end,
	


		case {Begin, End} of

					{none, none} -> ListHours;

					_ ->	TBegin = calendar:datetime_to_gregorian_seconds(Begin),
							TEnd = calendar:datetime_to_gregorian_seconds(End),

							if 
								TBegin =< TEnd ->	find_hour_in_range(Hour, Duration, BeginningRangeDateTime, datetime:time_diff(Begin, -1), [{Begin, End}|ListHours]);
								true -> ListHours
							end
		end.



satisfy({[], _}, _, _, ListRanges) -> ListRanges;

satisfy({[{month, Month}| Constraints], Duration}, BeginRange, EndRange, ListRanges) when is_atom(Month) ->
		ConstraintMonth = month(Month),

		satisfy({[{month, ConstraintMonth}|Constraints], Duration}, BeginRange, EndRange, ListRanges);

satisfy({[{month, Month}|Constraints], Duration}, BeginRange, EndRange, ListRanges) when is_integer(Month), Month > 0, Month < 13 ->

		CheckRanges = fun({BeginSubRange, EndSubRange}) ->
									satisfy({Constraints, Duration}, BeginSubRange, EndSubRange, {BeginSubRange, EndSubRange})
							end,

		lists:map(
						CheckRanges,
						find_month_in_range(Month, BeginRange, EndRange)
					);

satisfy({[{day, Day}|Constraints], Duration}, BeginRange, EndRange, ListRanges) when is_integer(Day), Day > 0, Day < 32 ->

		CheckRanges = fun({BeginSubRange, EndSubRange}) ->
									satisfy({Constraints, Duration}, BeginSubRange, EndSubRange, {BeginSubRange, EndSubRange})
							end,

		lists:map(
						CheckRanges,
						find_day_in_range(Day, BeginRange, EndRange)
					);

satisfy({[{weekday, WeekDay}|Constraints], Duration}, BeginRange, EndRange, ListRanges) when is_atom(WeekDay) ->
	
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
									satisfy({Constraints, Duration}, BeginSubRange, EndSubRange, {BeginSubRange, EndSubRange})
							end,

		lists:map(
						CheckRanges,
						CompleteDates
					);



satisfy({[{hour, Hour}|Constraints], Duration}, BeginRange, EndRange, ListRanges) when is_integer(Hour), Hour > 0, Hour < 25 ->

%		CheckRanges = fun({BeginSubRange, EndSubRange}) ->
%									satisfy({Constraints, Duration}, BeginSubRange, EndSubRange, {BeginSubRange, EndSubRange})
%							end,

%		lists:map(
%						CheckRanges,
%						find_hour_in_range(Hour, Duration, BeginRange, EndRange)
%					).

	
		{BeginRangeDate, BeginRangeTime} = BeginRange,
		{BeginRangeHour, BeginRangeMinute, BeginRangeSeconds} = BeginRangeTime,
	
		{EndRangeDate, EndRangeTime} = EndRange,
		{EndRangeHour, EndRangeMinute, EndRangeSeconds} = EndRangeTime,


		ListDates = datetime:range_dates({BeginRangeDate, EndRangeDate}),
		
		CompleteDates = lists:map(
												fun(Date) ->

														Begin = if
																	Date =:= BeginRangeDate,
																		Hour*3600 < BeginRangeHour*3600+60*BeginRangeMinute+BeginRangeSeconds -> {Date, {BeginRangeHour, BeginRangeMinute, BeginRangeSeconds}};

																	 Date =:= EndRangeDate,
																	 	Hour > EndRangeHour -> none;

																	true -> {Date, {Hour, 0, 0}}
																end,

														case Begin of

																none -> {none, none};
																_ -> 
																
																
																		DatePlusDuration = datetime:time_diff({Date, {Hour, 0, 0}}, Duration*3600),
																		{DDDate, DDTime} = DatePlusDuration,
																		{DDHour, DDMinute, DDSeconds} = DDTime,
%%% TODOOOOOOOOO comparing in gregorian seconds
																		End = if
																					Date =:= EndRangeDate,
																					DDHour*3600 >= EndRangeHour*3600+EndRangeMinute*60+EndRangeSeconds -> {EndRangeDate, {EndRangeHour, EndRangeMinute, EndRangeSeconds}};
			
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
												_ -> satisfy({Constraints, Duration}, BeginSubRange, EndSubRange, {BeginSubRange, EndSubRange})
									end
							end,

		lists:map(
						CheckRanges,
						CompleteDates
					).


	



%in_range({Constraints, Duration} = Rule, {Date, Time})  ->
			
%;

in_range(Rules, {Date, Time} = DateTime) when is_list(Rules) -> lists:any(fun(Rule) -> in_range(Rule, DateTime)end, Rules).






%in_range(Rules, {When, Until}) -> true | false.
