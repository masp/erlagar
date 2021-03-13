-module(erlagar_server).

-behavior(gen_server).

-include("erlagar.hrl").

-export([start_link/0]).
-export([register/1, move/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2]).

-record(state, {
    players :: #{pid() => player()}
}).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init(_Args) ->
    % For uuid:new
    quickrand:seed(),
    {ok, _} = timer:send_interval(?SERVER_TICK_PERIOD, timer),
    {ok, #state{
        players = #{}
    }}.

-spec create_player(pid(), string()) -> player().
create_player(OwnerPid, Name) ->
    {ok, Mass} = application:get_env(erlagar, default_mass),
    #{
        id => OwnerPid,
        uuid => uuid:get_v4(),
        name => Name,
        mass => Mass,
        pos => {100, 100},
        hue => rand:uniform(360)
    }.

-spec register_player(pid(), string(), #state{}) -> #state{}.
register_player(OwnerPid, Player, State) ->
    Players = State#state.players,
    State#state{
        players = Players#{
            OwnerPid => Player
        }
    }.

-spec register(string()) -> player().
register(Name) ->
    gen_server:call(?MODULE, {register, Name}).

handle_call({register, Name}, From, State) ->
    {Pid, _} = From,
    % if the player disconnects, we want to remove him from the list of players
    _ = erlang:monitor(process, Pid),
    NewPlayer = create_player(Pid, Name),
    {reply, {ok, NewPlayer}, register_player(Pid, NewPlayer, State)}.

% Cast calls
-spec move(position()) -> none().
move(Target) ->
    gen_server:cast(?MODULE, {move, self(), Target}).

handle_cast({move, From, Target}, State) ->
    Players = State#state.players,
    #{From := Player} = Players,

    {noreply, State#state{
        players = Players#{
            From := Player#{pos := Target}
        }
    }}.

send_state(Player, State) ->
    #{id := To} = Player,
    To ! {state, State#state.players}.

handle_info(timer, State) ->
    [send_state(P, State) || P <- maps:values(State#state.players)],
    {noreply, State};
handle_info({'DOWN', _, process, Pid, _}, State) ->
    {noreply, State#state{
        players = maps:remove(Pid, State#state.players)
    }}.
