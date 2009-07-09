%% @author Philip Robinson
%% @doc Erlang Macro Processor.
%% See [http://chlorophil.blogspot.com/2007/04/erlang-macro-processor-v2-part-i.html]

-module(emp2).
-author("Philip Robinson").
-vsn('1.0').
-export([parse_transform/2]).

parse_transform(AST, _Options) ->
    Mods = lists:flatten([Mods || {attribute,_Line,macro_modules,Mods} <- AST]),
    lists:flatten([node_parse(Node, Mods) || Node <- AST]).
node_parse({attribute,Line,macro,{Mod,Fun,Args}}, _Mods) ->
    ast_from_results(parse_form, lists:flatten([apply(Mod,Fun,Args)|" "]), Line, []);
node_parse(Node={call,Line,{remote,_,{atom,_,Mod},{atom,_,Fun}},Args}, Mods) ->
    case lists:member(Mod, Mods) of
        true ->
            ArgsLiteral = [Value || {_Type,_Line,Value} <- Args],
            Results = lists:flatten([apply(Mod,Fun,ArgsLiteral)|". "]),
            case length(Results) of
                1 -> hd(Results);
                _ -> {block,Line,ast_from_results(parse_exprs,Results,Line,[])}
                end;
        false -> setelement(4,Node,node_parse(Args, Mods))
        end;
node_parse(Node, Mods) when is_list(Node) ->
    [node_parse(Element, Mods) || Element <- Node];
node_parse(Node, Mods) when is_tuple(Node) ->
    list_to_tuple([node_parse(Element, Mods) || Element <- tuple_to_list(Node)]);
node_parse(Node, _Mods) -> Node.

%args_from_ast(AST) -> [Value || {_Type,_Line,Value} <- AST].

ast_from_results(FunParse, ResultsString, LineStart, ASTResults) ->
    case remove_leading_whitespace(ResultsString) of
        "" -> lists:flatten(lists:reverse(ASTResults));
        String ->
            {done,{ok,Tokens,LineEnd},StringRest} =
                erl_scan:tokens([], String, LineStart),
            {ok, AST} = erl_parse:FunParse(Tokens),
            ast_from_results(FunParse, StringRest, LineEnd, [AST|ASTResults])
        end.

remove_leading_whitespace([9 |String]) -> remove_leading_whitespace(String);
remove_leading_whitespace([10|String]) -> remove_leading_whitespace(String);
remove_leading_whitespace([32|String]) -> remove_leading_whitespace(String);
remove_leading_whitespace(    String ) -> String.
