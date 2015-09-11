-module (owl_shell_conn).
-behaviour (gen_server).

-export ([start_link/3, post_result/4, post_error/5]).
-export ([init_it/3]).

-export ([
		init/1,
		handle_call/3,
		handle_cast/2,
		handle_info/2,
		terminate/2,
		code_change/3
	]).

-define(io, owl_shell_conn_io).
-define(post_result( SID, RID, Value ), { post_result, SID, RID, Value }).
-define(post_error( SID, RID, EvalProcessDead, Reason ), { post_error, SID, RID, EvalProcessDead, Reason }).

-spec start_link( pid(), pid(), pid() ) -> {ok, pid()}.
start_link( TopSup, ConnSupSup, ConnSup ) -> proc_lib:start_link( ?MODULE, init_it, [ TopSup, ConnSupSup, ConnSup ] ).

post_result( Connection, SID, RID, Result ) ->
	ok = gen_server:cast( Connection, ?post_result( SID, RID, Result ) ).

post_error( Connection, SID, RID, EvalProcessDead, Reason ) ->
	ok = gen_server:cast( Connection, ?post_error( SID, RID, EvalProcessDead, Reason ) ).

-record( s, {
		'#module' = ?MODULE :: ?MODULE,
		io :: pid(),
		top_sup :: pid(),
		conn_sup_sup :: pid(),
		conn_sup :: pid(),

		token_q = queue:new() :: queue:queue( erl_scan:token() ),
		active_session :: pid() | undefined,
		active_session_id :: undefined | integer()
	} ).

init( _ ) -> {stop, enter_loop_used}.

init_it( TopSup, ConnSupSup, ConnSup ) ->
	ok = proc_lib:init_ack( {ok, self()} ),
	[ IO ] = [ P || {io, P, _, _} <- supervisor:which_children( ConnSup ) ],
	ok = ?io:acquire( IO ),
	{ok, SID, Session} = session_create_and_attach( TopSup ),
	S0 = #s{
			top_sup = TopSup,
			conn_sup_sup = ConnSupSup,
			conn_sup = ConnSup,

			io = IO,

			active_session_id = SID,
			active_session = Session
		},

	{ok, S1} = io_write_prompt( S0 ),
	gen_server:enter_loop(?MODULE, [], S1).



handle_call( _Unexpected, _GenReplyTo, S ) -> {reply, badarg, S}.

handle_cast( ?post_result( SID, RID, Result ), S ) ->
	handle_cast_post_result( SID, RID, Result, S );
handle_cast( ?post_error( SID, RID, EvalProcessDead, Reason ), S ) ->
	handle_cast_post_error( SID, RID, EvalProcessDead, Reason, S );
handle_cast( _Unexpected, S ) -> {noreply, S}.

handle_info( {?io, data, IO, Data}, S ) ->
	handle_info_io_data( IO, Data, S );
handle_info( {?io, closed, IO}, S ) ->
	handle_info_io_closed( IO, S );

handle_info( {io_request, ReplyTo, ReplyAs, Request}, S ) ->
	handle_info_io_request( ReplyTo, ReplyAs, Request, S );

handle_info( _Unexpected, S ) -> {noreply, S}.

terminate( _Reason, _S ) -> ignore.

code_change( _OldVsn, S, _Extra ) -> {ok, S}.






handle_info_io_closed( IO, _S = #s{ io = IO, conn_sup_sup = SupSup, conn_sup = Sup } ) ->
	_ = supervisor:terminate_child( SupSup, Sup ).

handle_info_io_data( IO, Data, S0 = #s{ io = IO, token_q = TokQ0 } ) ->
	case erl_scan:string( binary_to_list( Data ) ) of
		{ok, Tokens, _EndPos} ->
			TokQ1 = lists:foldl( fun queue:in/2, TokQ0, Tokens ),
			handle_info_io_data_process_token_queue( S0 #s{ token_q = TokQ1 } );

		{error, _ErrorInfo, _ErrorLocation} ->
			{noreply, S0}
	end.

handle_info_io_data_process_token_queue( S0 = #s{ token_q = TokQ0, active_session = Session, io = IO } ) ->
	case token_queue_get_expr( TokQ0 ) of
		error -> {noreply, S0};
		{ok, ExprTokens, TokQ1} ->
			S1 = S0 #s{ token_q = TokQ1 },
			case erl_parse:parse_exprs( ExprTokens ) of
				{ok, Exprs} ->
					ok = owl_shell_session:post_exprs( Session, Exprs ),
					handle_info_io_data_process_token_queue( S1 );
				{error, {Line, erl_parse, ErrorIOL}} ->
					?io:write( IO, io_lib:format("* ~p: ~s~n", [ Line, ErrorIOL ]) ),
					handle_info_io_data_process_token_queue( S1 )
			end
	end.

session_create_and_attach( TopSup ) ->
	[ SessionSupSup ] = [ P || { session_sup_sup, P, _, _ } <- supervisor:which_children( TopSup ) ],
	SID =
		case [ ID || { {session_sup, ID}, _, _, _ } <- supervisor:which_children( SessionSupSup ) ] of
			[] -> 1;
			ExistingIDs = [ _ | _ ] -> lists:max( ExistingIDs ) + 1
		end,
	{ok, SessionSup} = owl_shell_session_sup_sup:start_child( SessionSupSup, SID ),
	[ Session ] = [ P || { session, P, _, _ } <- supervisor:which_children( SessionSup ) ],
	ok = owl_shell_session:attach( Session, self() ),
	{ok, SID, Session}.

token_queue_get_expr( Q ) ->
	token_queue_get_expr( Q, [] ).

token_queue_get_expr( Q, Acc ) ->
	case queue:peek( Q ) of
		{value, Dot = {dot, _}} ->
			{ok, lists:reverse( [ Dot | Acc ] ), queue:drop( Q )};
		{value, Token} ->
			token_queue_get_expr( queue:drop( Q ), [ Token | Acc ] );
		empty ->
			error
	end.



io_write_prompt( S0 = #s{ io = IO, active_session_id = SID } ) ->
	?io:write( IO, io_lib:format( "[~p session: ~p] # ", [ node(), SID ] ) ),
	{ok, S0}.

handle_cast_post_result( SID, RID, Result, S0 = #s{ io = IO }) ->
	?io:write( IO, io_lib:format( "v(~p:~p) = ~p~n", [ SID, RID, Result ]) ),
	{ok, S1} = io_write_prompt( S0 ),
	{noreply, S1}.

handle_cast_post_error( SID, RID, EvalProcessDead, Reason, S0 = #s{ io = IO } ) ->
	?io:write( IO, io_lib:format( "v(~p:~p) : ~p terminated with exception~n", [ SID, RID, EvalProcessDead ] ) ),
	?io:write( IO, io_lib:format( "\t~p~n", [ Reason ] ) ),
	{ok, S1} = io_write_prompt( S0 ),
	{noreply, S1}.

handle_info_io_request( ReplyTo, ReplyAs, Request, S = #s{ io = IO } ) ->
	% io:format("~p: Inbound IO-request: ~p~n", [ self(), Request ]),
	ReplyWith =
		case Request of
			{ put_chars, Chars } ->
				_Reply = ?io:write( IO, Chars );

			{ put_chars, _Enc, Chars } ->
				_Reply = ?io:write( IO, Chars );

			{ put_chars, _Enc, M, F, A } ->
				_Reply = ?io:write( IO, erlang:apply( M, F, A ) );

			{ put_chars, M, F, A } ->
				_Reply = ?io:write( IO, erlang:apply( M, F, A ) );

			getopts ->
				_Reply = [ {binary, false}, {encoding, unicode} ];

			_Unexpected ->
				% io:format("Unexpected IO-request: ~p~n", [Unexpected]),
				{error, enotsup}
		end,
	_ = erlang:send( ReplyTo, {io_reply, ReplyAs, ReplyWith} ),
	{noreply, S}.

