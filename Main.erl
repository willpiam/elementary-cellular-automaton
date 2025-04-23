%%%------------------------------------------------------------
%%%  ca.erl  –  Elementary Cellular Automaton → PBM
%%%------------------------------------------------------------
-module('Main').
-export([main/0, main/1]).

-define(INPUT_FILE, "input.txt").

%%---------------------------------------- public entry points
main()            -> main([]).           %% erl -noshell -run ca main
main(Args) ->
    File          = case Args of
                       [Filename | _] -> Filename;
                       _              -> ?INPUT_FILE
                    end,
    {ok, Bin}     = file:read_file(File),
    [RuleS, InitS, GenS | _] =
        [string:trim(L) || L <- string:split(binary_to_list(Bin), "\n", all),
                           L =/= ""],
    {Rule,_}      = string:to_integer(RuleS),
    {Gens,_}      = string:to_integer(GenS),
    run(Rule, InitS, Gens).

%%---------------------------------------- core logic
run(Rule, InitStr, Gens) ->
    InitLen       = length(InitStr),
    Width         = InitLen + 2*Gens,
    RuleBits      = rule_bits(Rule),

    InitCells     = [if C==$1->1; true->0 end || C<-InitStr],
    Offset        = (Width - InitLen) div 2,
    Row0          = lists:duplicate(Offset,0) ++ InitCells ++
                    lists:duplicate(Width-Offset-InitLen,0),

    Rows          = [Row0 | evolve(Row0, RuleBits, Gens-1)],

    ensure_dir("results"),
    FileName      = filename(Rule, Gens, InitStr),
    write_pbm(FileName, Width, Gens, Rows),
    ok.

rule_bits(N) ->
    [ (N bsr K) band 1 || K <- lists:seq(7,0,-1) ].

evolve(_Row,_Rule,0) -> [];
evolve(Row,Rule,N)   ->
    Next = next_row(Row,Rule),
    [Next | evolve(Next,Rule,N-1)].

next_row(Row,Rule) ->
    P = [0|Row] ++ [0],   % pad L and R with zeros
    [ apply_rule(L,C,R,Rule)
      || {L,C,R} <- zip3(P,lists:nthtail(1,P),lists:nthtail(2,P)) ].

apply_rule(L,C,R,Rule) ->
    Idx = 7 - (L*4 + C*2 + R),
    lists:nth(Idx+1, Rule).

zip3([],_,_)               -> [];
zip3(_,[],_)               -> [];
zip3(_,_,[])               -> [];
zip3([A|As],[B|Bs],[C|Cs]) -> [{A,B,C}|zip3(As,Bs,Cs)].

ensure_dir(D) ->
    case filelib:is_dir(D) of true->ok; false->file:make_dir(D) end.

filename(R,G,I) ->
    lists:flatten(io_lib:format("results/r~B_g~B_i~s_erlang.pbm",[R,G,I])).

write_pbm(File,W,H,Rows) ->
    Header = io_lib:format("P1\n~B ~B\n",[W,H]),
    Body   = lists:append([row_to_str(R) || R<-Rows]),
    file:write_file(File, list_to_binary(Header ++ Body)).

row_to_str(R) -> [ if X==1->$1; true->$0 end || X<-R ] ++ "\n".
