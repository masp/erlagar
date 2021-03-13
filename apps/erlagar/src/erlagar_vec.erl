-module(erlagar_vec).

-export([add/2, mul/2, sub/2, length/1]).

add(V1, V2) ->
    {X1, Y1} = V1,
    {X2, Y2} = V2,
    {X1 + X2, Y1 + Y2}.

mul(V1, S1) ->
    {X1, Y1} = V1,
    {X1 * S1, Y1 * S1}.

sub(V1, V2) ->
    {X1, Y1} = V1,
    {X2, Y2} = V2,
    {X1 - X2, Y1 - Y2}.

length(V1) ->
    {X1, Y1} = V1,
    math:sqrt((X1 * X1) + (Y1 * Y1)).
