%%% Elementary cellular automaton. Finite frame, light cone when 000 stays 0.
-module('Main').
-export([main/0, main/1]).

main() -> main([]).
main(_Args) ->
    case file:read_file("input.txt") of
        {ok, Bin} -> run(Bin);
        {error, _} -> die("cannot read input.txt")
    end.

die(Msg) ->
    io:put_chars(standard_error, [Msg, $\n]),
    halt(1).

trim(Bin) -> trim_right(trim_left(Bin)).

trim_left(<<C, Rest/binary>>) when C =:= $\s; C =:= $\t; C =:= $\r -> trim_left(Rest);
trim_left(Bin) -> Bin.

trim_right(Bin) ->
    trim_right(Bin, byte_size(Bin)).
trim_right(Bin, 0) -> Bin;
trim_right(Bin, N) ->
    case binary:at(Bin, N - 1) of
        C when C =:= $\s; C =:= $\t; C =:= $\r -> trim_right(Bin, N - 1);
        _ -> binary:part(Bin, 0, N)
    end.

strip_cr(Bin) ->
    case byte_size(Bin) of
        0 -> Bin;
        N ->
            case binary:at(Bin, N - 1) of
                $\r -> binary:part(Bin, 0, N - 1);
                _ -> Bin
            end
    end.

parse_int(Bin) ->
    try
        case trim(Bin) of
            <<"-", Rest/binary>> -> -binary_to_integer(Rest);
            T -> binary_to_integer(T)
        end
    catch
        _:_ -> error
    end.

valid_seed(<<>>) -> false;
valid_seed(Bin) -> valid_seed_loop(Bin).

valid_seed_loop(<<>>) -> true;
valid_seed_loop(<<C, Rest/binary>>) when C =:= $0; C =:= $1 -> valid_seed_loop(Rest);
valid_seed_loop(_) -> false.

run(Bin) ->
    Normalized = binary:replace(Bin, <<"\r\n">>, <<"\n">>, [global]),
    Lines = binary:split(Normalized, <<"\n">>, [global]),
    case Lines of
        [RuleS, Seed0, GenS | _] ->
            Rule = parse_int(RuleS),
            Seed = strip_cr(Seed0),
            Gens = parse_int(GenS),
            case Rule of
                error -> die("invalid rule");
                R when R < 0; R > 255 -> die("invalid rule");
                R ->
                    case valid_seed(Seed) of
                        false -> die("invalid initial conditions");
                        true ->
                            case Gens of
                                error -> die("invalid generations");
                                N when N < 1; N > 16#7fffffff -> die("invalid generations");
                                N -> evolve(R, Seed, N)
                            end
                    end
            end;
        _ ->
            die("invalid input")
    end.

evolve(Rule, Seed, Gens) ->
    N = byte_size(Seed),
    Width = N + 2 * Gens,
    Even = (Rule band 1) =:= 0,
    RuleTuple = list_to_tuple([(Rule bsr K) band 1 || K <- lists:seq(0, 7)]),
    Prev = place_seed(array:new(Width, [{default, 0}, {fixed, true}]), Gens, Seed, 0),
    ensure_dir("results"),
    Name = lists:flatten(io_lib:format("results/r~B_g~B_i~s_erlang.pbm", [Rule, Gens, Seed])),
    {ok, Fd} = file:open(Name, [write, binary, raw]),
    ok = file:write(Fd, io_lib:format("P1\n~B ~B\n", [Width, Gens])),
    loop(Fd, Prev, RuleTuple, Even, N, Gens, 0, Width),
    file:close(Fd).

place_seed(Row, _G, <<>>, _I) -> Row;
place_seed(Row, G, <<$1, Rest/binary>>, I) ->
    place_seed(array:set(G + I, 1, Row), G, Rest, I + 1);
place_seed(Row, G, <<_, Rest/binary>>, I) ->
    place_seed(Row, G, Rest, I + 1).

loop(Fd, Prev, Rule, Even, N, G, Gen, Width) ->
    write_row(Fd, Prev, Width),
    case Gen + 1 of
        G -> ok;
        _ ->
            Next = step(Prev, Rule, Even, N, G, Gen, Width),
            loop(Fd, Next, Rule, Even, N, G, Gen + 1, Width)
    end.

write_row(Fd, Row, Width) ->
    Bytes = [case array:get(J, Row) of 1 -> $1; _ -> $0 end || J <- lists:seq(0, Width - 1)],
    ok = file:write(Fd, [Bytes, $\n]).

step(Prev, Rule, Even, N, G, Gen, Width) ->
    Next = array:new(Width, [{default, 0}, {fixed, true}]),
    {Lo, Hi} = case Even of
        true -> {G - (Gen + 1), G + N + (Gen + 1)};
        false -> {0, Width}
    end,
    fill(Prev, Next, Rule, Lo, Hi, Width).

fill(_Prev, Next, _Rule, J, Hi, _Width) when J >= Hi -> Next;
fill(Prev, Next, Rule, J, Hi, Width) ->
    L = case J > 0 of true -> array:get(J - 1, Prev); false -> 0 end,
    C = array:get(J, Prev),
    R = case J + 1 < Width of true -> array:get(J + 1, Prev); false -> 0 end,
    Idx = (L bsl 2) bor (C bsl 1) bor R,
    fill(Prev, array:set(J, element(Idx + 1, Rule), Next), Rule, J + 1, Hi, Width).

ensure_dir(D) ->
    case file:make_dir(D) of
        ok -> ok;
        {error, eexist} -> ok;
        {error, _} -> die("cannot create results")
    end.
