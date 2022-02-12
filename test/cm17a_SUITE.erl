%%% % @noformat
-module(cm17a_SUITE).

-include_lib("common_test/include/ct.hrl").

-export([
        all/0
    ]).

-export([
        encode/1,
        insn/1
    ]).

all() ->
    [encode, insn].

encode(_Config) ->
    <<213, 170, 80, 40, 173>> = cm17a:encode($D, 3, off).

insn(_Config) ->
    Insn = case os:type() of
        {unix, linux} ->
            [{21526, 6, 350}, {21527, 2, 1}, {21526, 6, 0}, {21527, 2, 1},
             {21526, 6, 0}, {21527, 4, 1}, {21526, 6, 0}, {21527, 2, 1},
             {21526, 6, 0}, {21527, 4, 1}, {21526, 6, 0}, {21527, 2, 1},
             {21526, 6, 0}, {21527, 4, 1}, {21526, 6, 0}, {21527, 2, 1},
             {21526, 6, 0}, {21527, 2, 1}, {21526, 6, 0}, {21527, 4, 1},
             {21526, 6, 0}, {21527, 2, 1}, {21526, 6, 0}, {21527, 4, 1},
             {21526, 6, 0}, {21527, 2, 1}, {21526, 6, 0}, {21527, 4, 1},
             {21526, 6, 0}, {21527, 2, 1}, {21526, 6, 0}, {21527, 4, 1},
             {21526, 6, 0}, {21527, 4, 1}, {21526, 6, 0}, {21527, 2, 1},
             {21526, 6, 0}, {21527, 4, 1}, {21526, 6, 0}, {21527, 2, 1},
             {21526, 6, 0}, {21527, 4, 1}, {21526, 6, 0}, {21527, 4, 1},
             {21526, 6, 0}, {21527, 4, 1}, {21526, 6, 0}, {21527, 4, 1},
             {21526, 6, 0}, {21527, 4, 1}, {21526, 6, 0}, {21527, 4, 1},
             {21526, 6, 0}, {21527, 2, 1}, {21526, 6, 0}, {21527, 4, 1},
             {21526, 6, 0}, {21527, 2, 1}, {21526, 6, 0}, {21527, 4, 1},
             {21526, 6, 0}, {21527, 4, 1}, {21526, 6, 0}, {21527, 4, 1},
             {21526, 6, 0}, {21527, 2, 1}, {21526, 6, 0}, {21527, 4, 1},
             {21526, 6, 0}, {21527, 2, 1}, {21526, 6, 0}, {21527, 4, 1},
             {21526, 6, 0}, {21527, 2, 1}, {21526, 6, 0}, {21527, 2, 1},
             {21526, 6, 0}, {21527, 4, 1}, {21526, 6, 0}, {21527, 2, 1},
             {21526, 6, 0}, {21526, 6, 350}];
        {unix, BSD} when BSD =:= freebsd; BSD =:= netbsd; BSD =:= openbsd ->
            [{2147775596, 6, 350}, {2147775595, 2, 1}, {2147775596, 6, 0},
             {2147775595, 2, 1}, {2147775596, 6, 0}, {2147775595, 4, 1},
             {2147775596, 6, 0}, {2147775595, 2, 1}, {2147775596, 6, 0},
             {2147775595, 4, 1}, {2147775596, 6, 0}, {2147775595, 2, 1},
             {2147775596, 6, 0}, {2147775595, 4, 1}, {2147775596, 6, 0},
             {2147775595, 2, 1}, {2147775596, 6, 0}, {2147775595, 2, 1},
             {2147775596, 6, 0}, {2147775595, 4, 1}, {2147775596, 6, 0},
             {2147775595, 2, 1}, {2147775596, 6, 0}, {2147775595, 4, 1},
             {2147775596, 6, 0}, {2147775595, 2, 1}, {2147775596, 6, 0},
             {2147775595, 4, 1}, {2147775596, 6, 0}, {2147775595, 2, 1},
             {2147775596, 6, 0}, {2147775595, 4, 1}, {2147775596, 6, 0},
             {2147775595, 4, 1}, {2147775596, 6, 0}, {2147775595, 2, 1},
             {2147775596, 6, 0}, {2147775595, 4, 1}, {2147775596, 6, 0},
             {2147775595, 2, 1}, {2147775596, 6, 0}, {2147775595, 4, 1},
             {2147775596, 6, 0}, {2147775595, 4, 1}, {2147775596, 6, 0},
             {2147775595, 4, 1}, {2147775596, 6, 0}, {2147775595, 4, 1},
             {2147775596, 6, 0}, {2147775595, 4, 1}, {2147775596, 6, 0},
             {2147775595, 4, 1}, {2147775596, 6, 0}, {2147775595, 2, 1},
             {2147775596, 6, 0}, {2147775595, 4, 1}, {2147775596, 6, 0},
             {2147775595, 2, 1}, {2147775596, 6, 0}, {2147775595, 4, 1},
             {2147775596, 6, 0}, {2147775595, 4, 1}, {2147775596, 6, 0},
             {2147775595, 4, 1}, {2147775596, 6, 0}, {2147775595, 2, 1},
             {2147775596, 6, 0}, {2147775595, 4, 1}, {2147775596, 6, 0},
             {2147775595, 2, 1}, {2147775596, 6, 0}, {2147775595, 4, 1},
             {2147775596, 6, 0}, {2147775595, 2, 1}, {2147775596, 6, 0},
             {2147775595, 2, 1}, {2147775596, 6, 0}, {2147775595, 4, 1},
             {2147775596, 6, 0}, {2147775595, 2, 1}, {2147775596, 6, 0},
             {2147775596, 6, 350}];
        {unix, sunos} ->
            [{29723, 6, 350}, {29724, 2, 1}, {29723, 6, 0}, {29724, 2, 1},
             {29723, 6, 0}, {29724, 4, 1}, {29723, 6, 0}, {29724, 2, 1},
             {29723, 6, 0}, {29724, 4, 1}, {29723, 6, 0}, {29724, 2, 1},
             {29723, 6, 0}, {29724, 4, 1}, {29723, 6, 0}, {29724, 2, 1},
             {29723, 6, 0}, {29724, 2, 1}, {29723, 6, 0}, {29724, 4, 1},
             {29723, 6, 0}, {29724, 2, 1}, {29723, 6, 0}, {29724, 4, 1},
             {29723, 6, 0}, {29724, 2, 1}, {29723, 6, 0}, {29724, 4, 1},
             {29723, 6, 0}, {29724, 2, 1}, {29723, 6, 0}, {29724, 4, 1},
             {29723, 6, 0}, {29724, 4, 1}, {29723, 6, 0}, {29724, 2, 1},
             {29723, 6, 0}, {29724, 4, 1}, {29723, 6, 0}, {29724, 2, 1},
             {29723, 6, 0}, {29724, 4, 1}, {29723, 6, 0}, {29724, 4, 1},
             {29723, 6, 0}, {29724, 4, 1}, {29723, 6, 0}, {29724, 4, 1},
             {29723, 6, 0}, {29724, 4, 1}, {29723, 6, 0}, {29724, 4, 1},
             {29723, 6, 0}, {29724, 2, 1}, {29723, 6, 0}, {29724, 4, 1},
             {29723, 6, 0}, {29724, 2, 1}, {29723, 6, 0}, {29724, 4, 1},
             {29723, 6, 0}, {29724, 4, 1}, {29723, 6, 0}, {29724, 4, 1},
             {29723, 6, 0}, {29724, 2, 1}, {29723, 6, 0}, {29724, 4, 1},
             {29723, 6, 0}, {29724, 2, 1}, {29723, 6, 0}, {29724, 4, 1},
             {29723, 6, 0}, {29724, 2, 1}, {29723, 6, 0}, {29724, 2, 1},
             {29723, 6, 0}, {29724, 4, 1}, {29723, 6, 0}, {29724, 2, 1},
             {29723, 6, 0}, {29723, 6, 350}]
    end,
    Insn = cm17a:insn(<<213, 170, 80, 40, 173>>).
