-module(fail_max_function_length).

-export([f5/1, f10/1, f15/1]).

f5(_) -> %% 1
    %% 2
    %% 3

    ok. %% 5

f10(_) -> %% 1
    %% 2
    %% 3
    %% 4

    %% 6
    %% 7

    %% 9
    ok. %% 10

f15(_) -> %% 1
    %% 2
    %% 3
    %% 4

    %% 6
    %% 7
    %% 8
    %% 9

    %% 11
    %% 12

    %% 14
    ok. %% 15
