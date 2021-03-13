-type position() :: #{x := number(), y := number()}.

% From okeuday/uuid
-type uuid() :: <<_:128>>.

-type player() :: #{
    id := pid(),
    uuid := uuid(),
    name := string(),
    mass := number(),
    pos := position(),
    hue := 0..355
}.

-define(SERVER_TICK_PERIOD, round(1000 / 20)).
