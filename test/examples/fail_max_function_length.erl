-module(fail_max_function_length).

-export([f5/1, f10/1, f15/1]).

f5(_) -> %% 1
    %% 2
    %% 3
    %% 4
    ok. %% 5

f10(_) -> %% 1
    %% 2
    %% 3
    %% 4
    %% 5
    %% 6
    %% 7
    %% 8
    %% 9
    ok.

f15(_) -> %% 1
    %% 2
    %% 3
    %% 4
    %% 5
    %% 6
    %% 7
    %% 8
    %% 9
    %% 10
    %% 11
    %% 12
    %% 13
    %% 14
    ok.