-module(erlagar_ws).

-export([init/2]).

-export([websocket_init/1]).
-export([websocket_handle/2]).
-export([websocket_info/2]).
-export([terminate/3]).

-include("erlagar.hrl").

-record(state, {
    id :: pid(),
    uuid :: uuid(),
    last_heartbeat = 0 :: number(),
    target = {0, 0} :: position(),
    screen_dims = {0, 0} :: position(),
    player = #{name => <<"unknown">>} :: player()
}).

-define(JOIN, <<"join">>).
-define(HEARTBEAT, <<"0">>).
-define(FIRE_FOOD, <<"1">>).
-define(SPLIT_CELL, <<"2">>).
-define(PING, <<"pingcheck">>).
-define(PONG, <<"pongcheck">>).

init(Req, _) ->
    #{peer := {IP, Port}} = Req,
    io:format("player connected: ~p~n", [{IP, Port}]),
    {cowboy_websocket, Req, #state{}, #{
        idle_timeout => 30000,
        max_frame_size => 65536
    }}.

reply(Event) ->
    reply(Event, #{}).

reply(Event, Payload) ->
    P1 = Payload#{e => Event},
    {text, jsone:encode(P1)}.

websocket_init(State) ->
    {ok, State#state{
        id = self(),
        last_heartbeat = erlang:monotonic_time(second)
    }}.

% mass_to_radius(Mass) -> 4 + math:sqrt(Mass) * 6.

uuid_to_json(Uuid) ->
    list_to_binary(uuid:uuid_to_string(Uuid)).

-spec player_to_json(player()) -> map().
player_to_json(Player) ->
    #{pos := {X, Y}, mass := Mass, hue := Hue, uuid := Uuid} = Player,
    #{
        id => uuid_to_json(Uuid),
        x => X,
        y => Y,
        mass => Mass,
        hue => Hue
    }.

welcome_reply(Player) ->
    reply(<<"welcome">>, player_to_json(Player)).

websocket_handle({text, Msg}, State) ->
    Json = jsone:decode(Msg),
    #{<<"e">> := Event} = Json,
    case Event of
        ?JOIN ->
            {ok, _} = timer:send_interval(?SERVER_TICK_PERIOD, timer),
            #{<<"name">> := Name} = Json,
            {ok, Player} = erlagar_server:register(Name),
            {[welcome_reply(Player)], State#state{player = Player}};
        ?PING ->
            {[reply(?PONG)], State};
        ?HEARTBEAT ->
            #{<<"x">> := X, <<"y">> := Y} = Json,
            {[], State#state{
                last_heartbeat = erlang:monotonic_time(second),
                target = {X, Y}
            }};
        ?FIRE_FOOD ->
            io:format("fire food~n"),
            {[], State};
        ?SPLIT_CELL ->
            io:format("split cell~n")
    end;
websocket_handle(_Data, State) ->
    {[], State}.

-define(MAX_LENGTH, 128).

calc_new_pos(Player, Target) ->
    #{pos := Pos} = Player,
    Dir = erlagar_vec:sub(Target, Pos),
    Len = erlagar_vec:length(Dir),
    LimitedLen = min(Len, ?MAX_LENGTH) * (?SERVER_TICK_PERIOD / 1000),
    Offset = erlagar_vec:mul(erlagar_vec:mul(Dir, 1 / Len), LimitedLen),
    erlagar_vec:add(Pos, Offset).

websocket_info(timer, State) ->
    NewPos = calc_new_pos(State#state.player, State#state.target),
    erlagar_server:move(NewPos),
    {[], State};
websocket_info({state, PlayersMap}, State) ->
    #{id := MyId} = State#state.player,
    #{MyId := MyPlayer} = PlayersMap,
    PlayersJson = maps:from_list([
        {uuid_to_json(maps:get(uuid, V)), player_to_json(V)}
        || V <- maps:values(PlayersMap)
    ]),
    {[reply(<<"state">>, #{players => PlayersJson})], State#state{
        player = MyPlayer
    }};
websocket_info(Info, State) ->
    io:format("bad message: ~p~n", [Info]),
    {[], State}.

terminate(Reason, _, State) ->
    #{name := Name} = State#state.player,
    io:format("player '~s' disconnected: ~p~n", [Name, Reason]),
    ok.
