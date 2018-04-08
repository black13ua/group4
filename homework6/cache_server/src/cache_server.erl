%%%-------------------------------------------------------------------
%%% @author $author
%%% @copyright (C) $year, $company
%%% @doc
%%%
%%% @end
%%% Created : $fulldate
%%%-------------------------------------------------------------------
-module(cache_server).

-behaviour(gen_server).

%% API
-export([start_link/1]).

%% gen_server callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

%% Exported Functions
-export([delete_obsolete/0, insert/2, insert/3, lookup/1, lookup_by_date/2]).

-include_lib("stdlib/include/ms_transform.hrl").

-define(SERVER, ?MODULE).
-define(DEFAULT_TTL, 300).

-record(state, {}).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link(Options) ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, Options, []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server
%%
%% @spec init(Args) -> {ok, State} |
%%                     {ok, State, Timeout} |
%%                     ignore |
%%                     {stop, Reason}
%% @end
%%--------------------------------------------------------------------
init(Options) ->
    DropInterval = proplists:get_value(drop_interval, Options),
    DropIntervalMilisec = DropInterval * 1000,
    self() ! create_ets,
    timer:send_interval(DropIntervalMilisec, clean_ets),
    {ok, #state{}}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%%
%% @spec handle_call(Request, From, State) ->
%%                                   {reply, Reply, State} |
%%                                   {reply, Reply, State, Timeout} |
%%                                   {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, Reply, State} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%%
%% @spec handle_cast(Msg, State) -> {noreply, State} |
%%                                  {noreply, State, Timeout} |
%%                                  {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_cast(_Msg, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%%
%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_info(create_ets, State) ->
	ets:new(?MODULE, [public, ordered_set, named_table]),
    {noreply, State};
handle_info(clean_ets, State) ->
    delete_obsolete(),
    {noreply, State};
handle_info(_Info, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%%
%% @spec terminate(Reason, State) -> void()
%% @end
%%--------------------------------------------------------------------
terminate(_Reason, _State) ->
    ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
        {ok, State}.

%%%===================================================================
%%% Exported functions
%%%===================================================================
insert(K, V) ->
    insert(K, V, ?DEFAULT_TTL).

insert(K, V, TTL) ->
    Now = erlang:system_time(seconds),
    case ets:insert(?MODULE, {K, {V, TTL, Now + TTL}}) of
        true ->
            ok;
        _ ->
            {error, key_not_inserted}
    end. 

lookup(K) ->
    Now = erlang:system_time(seconds),
    case ets:lookup(?MODULE, K) of
        [{K, {V,_TTL,Expire}}] when Now < Expire ->
            {ok, V};
        _ ->
            {ok, []}
    end.

lookup_by_date(DateFrom, DateTo) ->
    case check_dates(DateFrom, DateTo) of
        {ok, {DFU, DTU}} when DFU < DTU ->
            lookup_by_dates_ms(DFU, DTU);
        {ok, _} ->
            {error, date_to_less_than_date_from};
        {error, Error} ->
            {error, Error}
    end.

%%%===================================================================
%%% Internal functions
%%%===================================================================
lookup_by_dates_ms(DFU, DTU) ->
    Now = erlang:system_time(seconds),
    MS = ets:fun2ms(fun({K, {V, _TTL, Expire}})
                      when
                         DFU =< Expire andalso
                         DTU >= Expire andalso
                         Expire > Now
                      -> [{key, K}, {value, V}]
                    end),
    Result = ets:select(?MODULE, MS),
    {ok, Result}.

check_dates(DateFrom = {{YDF,MDF,DDF},{THF,TMF,TSF}},
              DateTo = {{YDT,MDT,DDT},{THT,TMT,TST}})
                 when
                   THF >= 0, THF =< 23, THT >= 0, THT =< 23,
                   TMF >= 0, TMF =< 59, TMT >= 0, TMT =< 59,
                   TSF >= 0, TSF =< 59, TST >= 0, TST =< 59 ->
    DatesCheck = [calendar:valid_date(D) || D <- [{YDF,MDF,DDF}, {YDT,MDT,DDT}]],
    case DatesCheck of
        [true,true] ->
            DateFromUnixtime = date_to_unixtime(DateFrom),
            DateToUnixtime = date_to_unixtime(DateTo),
            {ok, {DateFromUnixtime, DateToUnixtime}};
        _ ->
            {error, dates_check_failed}
    end;
check_dates(_DateFrom, _DateTo) ->
    {error, dates_possible_wrong}.

delete_obsolete() ->
    Now = erlang:system_time(seconds),
    First = ets:first(?MODULE),
    delete_obsolete(First, Now).

delete_obsolete(K, Now) ->
    case ets:lookup(?MODULE, K) of
        [{K, {_V, _TTL, Expire}}] when Now > Expire ->
            Next = ets:next(?MODULE, K),
            ets:delete(?MODULE, K),
            delete_obsolete(Next, Now);
        [{K, {_V, _TTL, _Expire}}] ->
            Next = ets:next(?MODULE, K),
            delete_obsolete(Next, Now);
        [] ->
            ok;
        '$end_of_table' ->
            ok
    end.

date_to_unixtime(Date) ->
    calendar:datetime_to_gregorian_seconds(Date) - 62167219200.

