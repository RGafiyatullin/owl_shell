-module (owl_shell_session).
-behaviour (gen_server).

-export ([
		init/1, enter_loop/2,
		handle_call/3,
		handle_cast/2,
		handle_info/2,
		terminate/2,
		code_change/3
	]).

-export ([
		start_link/2,
		attach/2,
		post_exprs/2
	]).

-export ([
		start_link_eval_process/3,
		eval_process_enter_loop/3
	]).

-define( attach( Connection ), {?MODULE, attach, Connection} ).
-define( post_exprs( Exprs ), {?MODULE, post_exprs, Exprs} ).
-define( process_exprs( Exprs ), {?MODULE, process_exprs, Exprs} ).
-define( exprs_processed( Value, Bindings ), {?MODULE, exprs_processed, Value, Bindings} ).

start_link( SessionSup, SessionID ) ->
	proc_lib:start_link( ?MODULE, enter_loop, [ SessionSup, SessionID ] ).


attach( Session, Connection ) ->
	gen_server:call( Session, ?attach( Connection ) ).

post_exprs( Session, Exprs ) ->
	gen_server:cast( Session, ?post_exprs( Exprs ) ).


start_link_eval_process( SessionID, Session, InitialBindings ) ->
	proc_lib:start_link( ?MODULE, eval_process_enter_loop, [ SessionID, Session, InitialBindings ] ).

-record( ep, {
		'#module' = ?MODULE :: ?MODULE,
		bindings :: term(),
		session :: pid(),
		session_id :: term()
	} ).

eval_process_enter_loop( SessionID, Session, InitialBindings ) ->
	ok = proc_lib:init_ack( {ok, self()} ),
	eval_process_loop( #ep{ session_id = SessionID, session = Session, bindings = InitialBindings } ).

eval_process_loop( S = #ep{ bindings = Bindings0, session = Session } ) ->
	receive
		?process_exprs( Exprs ) ->
			{value, Value, Bindings1} = erl_eval:exprs( Exprs, Bindings0 ),
			ok = gen_server:cast( Session, ?exprs_processed( Value, Bindings1 ) ),
			eval_process_loop( S #ep{ bindings = Bindings1 } )
	end.




-record(s, {
		'#module' = ?MODULE :: ?MODULE,
		session_sup :: pid(),
		evals_sup :: pid(),
		session_id :: term(),

		eval_process :: pid(),
		bindings :: term(),
		current_connection :: undefined | pid(),
		eval_process_ready = false :: boolean(),
		exprs_q = queue:new() :: queue:queue( term() ),
		result_id = 1 :: pos_integer()
	}).

enter_loop( SessionSup, SessionID ) ->
	ok = proc_lib:init_ack({ok, self()}),
	Bindings = erl_eval:new_bindings(),
	[EvalsSup] = [ P || { evals_sup, P, _, _ } <- supervisor:which_children( SessionSup ) ],

	{ok, EvalProcess} = supervisor:start_child( EvalsSup, [ self(), Bindings ] ),
	_MonRef = erlang:monitor( process, EvalProcess ),

	S0 = #s{
			session_sup = SessionSup,
			evals_sup = EvalsSup,
			session_id = SessionID,
			eval_process = EvalProcess,
			bindings = Bindings,
			eval_process_ready = true
		},
	gen_server:enter_loop( ?MODULE, [], S0 ).

init( _ ) -> {stop, enter_loop_used}.

handle_call( ?attach( Connection ), GenReplyTo, S ) ->
	handle_call_attach( Connection, GenReplyTo, S );

handle_call( _Unexpected, _GenReplyTo, S ) ->
	{reply, badarg, S}.

handle_cast( ?post_exprs( Exprs ), S ) ->
	handle_cast_post_exprs( Exprs, S );
handle_cast( ?exprs_processed( Value, NewBindings ), S ) ->
	handle_cast_exprs_processed( Value, NewBindings, S );

handle_cast( _Unexpected, S ) ->
	{noreply, S}.

handle_info( {'DOWN', _MonRef, process, EvalProcessDead, Reason}, S ) ->
	handle_info_down_eval_process( EvalProcessDead, Reason, S );

handle_info( Unexpected, S ) ->
	io:format("Unexpected info: ~p", [ Unexpected ]),
	{noreply, S}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.




handle_call_attach( NewConnection, _GenReplyTo, S = #s{ eval_process = EP, current_connection = OldConnection }) ->
	case OldConnection of
		undefined -> ok;
		_ ->
			_ = erlang:send( OldConnection, {?MODULE, detached, self()} ),
			ok
	end,

	true = erlang:group_leader( NewConnection, EP ),

	{reply, ok, S #s{ current_connection = NewConnection }}.

handle_cast_post_exprs( Exprs, S = #s{ eval_process_ready = false, exprs_q = EQ0 } ) ->
	EQ1 = queue:in( Exprs, EQ0 ),
	{noreply, S #s{ exprs_q = EQ1 }};
handle_cast_post_exprs( Exprs, S = #s{ eval_process_ready = true, eval_process = EP } ) ->
	_ = erlang:send( EP, ?process_exprs( Exprs ) ),
	{noreply, S #s{ eval_process_ready = false }}.

handle_cast_exprs_processed( Value, NewBindings, S0 = #s{} ) ->
	{ok, S1} = maybe_report_value_to_the_connection( Value, S0 #s{ bindings = NewBindings } ),
	{ok, S2} = maybe_cast_process_next_exprs( S1 ),
	{noreply, S2}.

handle_info_down_eval_process( EvalProcessDead, Reason, S0 = #s{} ) ->
	{ok, S1} = maybe_report_error_to_the_connection( EvalProcessDead, Reason, S0 ),
	{ok, S2} = start_fresh_eval_process( S1 ),
	{ok, S3} = maybe_cast_process_next_exprs( S2 ),
	{noreply, S3}.

start_fresh_eval_process( S0 = #s{ evals_sup = EvalsSup, bindings = Bindings } ) ->
	{ok, EP} = supervisor:start_child( EvalsSup, [ self(), Bindings ] ),
	_MonRef = erlang:monitor( process, EP ),
	{ok, S0 #s{ eval_process = EP }}.

maybe_report_error_to_the_connection( EvalProcessDead, Reason, S0 = #s{ session_id = SessionID, result_id = RID0, current_connection = Conn } ) ->
	RID1 = RID0 + 1,
	S1 = S0 #s{ result_id = RID1 },

	case Conn of
		undefined -> ok;
		_ ->
			ok = owl_shell_conn:post_error( Conn, SessionID, RID0, EvalProcessDead, Reason )
	end,
	{ok, S1}.


maybe_report_value_to_the_connection( Value, S0 = #s{ session_id = SessionID, result_id = RID0, current_connection = Conn } ) ->
	RID1 = RID0 + 1,
	S1 = S0 #s{ result_id = RID1 },

	case Conn of
		undefined -> ok;
		_ ->
			ok = owl_shell_conn:post_result( Conn, SessionID, RID0, Value )
	end,
	{ok, S1}.

maybe_cast_process_next_exprs( S = #s{ eval_process = EP, exprs_q = EQ } ) ->
	case queue:peek( EQ ) of
		{value, Exprs} ->
			_ = erlang:send( EP, ?process_exprs( Exprs ) ),
			{ok, S #s{ eval_process_ready = false, exprs_q = queue:drop( EQ ) }};
		empty ->
			{ok, S #s{ eval_process_ready = true }}
	end.


