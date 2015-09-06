-module (owl_shell_conn).
-export ([start_link/3, post_result/4, post_error/5]).
-export ([init_it/3]).

-define(io, owl_shell_conn_io).
-define(post_result( SID, RID, Value ), { post_result, SID, RID, Value }).
-define(post_error( SID, RID, EvalProcessDead, Reason ), { post_error, SID, RID, EvalProcessDead, Reason }).

-spec start_link( pid(), pid(), pid() ) -> {ok, pid()}.
start_link( TopSup, ConnSupSup, ConnSup ) -> proc_lib:start_link( ?MODULE, init_it, [ TopSup, ConnSupSup, ConnSup ] ).

post_result( Connection, SID, RID, Result ) ->
	_ = erlang:send( Connection, ?post_result( SID, RID, Result ) ),
	ok.

post_error( Connection, SID, RID, EvalProcessDead, Reason ) ->
	_ = erlang:send( Connection, ?post_error( SID, RID, EvalProcessDead, Reason ) ),
	ok.

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

	eval_loop( S1 ).

eval_loop( S0 = #s{ io = IO } ) ->
	receive
		{?io, data, IO, Data} ->
			handle_io_data( Data, S0 );

		{?io, closed, IO} ->
			handle_io_closed( S0 );

		?post_result( SID, RID, Result ) ->
			?io:write( IO, io_lib:format( "v(~p:~p) = ~p~n", [ SID, RID, Result ]) ),
			{ok, S1} = io_write_prompt( S0 ),
			eval_loop( S1 );

		?post_error( SID, RID, EvalProcessDead, Reason ) ->
			?io:write( IO, io_lib:format( "v(~p:~p) : ~p terminated with exception~n", [ SID, RID, EvalProcessDead ] ) ),
			?io:write( IO, io_lib:format( "\t~p~n", [ Reason ] ) ),
			{ok, S1} = io_write_prompt( S0 ),
			eval_loop( S1 );

		{io_request, ReplyTo, ReplyAs, Request} ->
			case Request of
				{ put_chars, _Enc, Chars } ->
					?io:write( IO, Chars );
				{ put_chars, Chars } ->
					?io:write( IO, Chars );

				{ put_chars, _Enc, M, F, A } ->
					?io:write( IO, erlang:apply( M, F, A ) );
				{ put_chars, M, F, A } ->
					?io:write( IO, erlang:apply( M, F, A ) );


				_ ->
					?io:write( IO, io_lib:format( "UNKNOWN-IO-REQUEST: ~p~n", [ Request ] ) )
			end,
			_ = erlang:send( ReplyTo, {io_reply, ReplyAs, ok} ),
			eval_loop( S0 );

		Rubbish ->
			io:format("Recvd rubbish: ~p~n", [ Rubbish ]),
			eval_loop( S0 )
	end.


handle_io_closed( #s{ conn_sup_sup = SupSup, conn_sup = Sup } ) ->
	_ = supervisor:terminate_child( SupSup, Sup ).


handle_io_data( Data, S0 = #s{ io = _IO, token_q = TokQ0 } ) ->
	% io:format("DataIn: ~p~n", [ Data ]),
	% ok = ?io:write( IO, <<"okay, keep talking...\n">> ),

	case erl_scan:string( binary_to_list( Data ) ) of
		{ok, Tokens, _EndPos} ->
			% io:format("\tTokens: ~p~n", [ Tokens ]),
			TokQ1 = lists:foldl( fun queue:in/2, TokQ0, Tokens ),
			handle_io_data_process_token_queue( S0 #s{ token_q = TokQ1 } );
		{error, _ErrorInfo, _ErrorLocation} ->
			% io:format("\tError: ~p (~p)~n", [ ErrorInfo, ErrorLocation ]),
			eval_loop( S0 )
	end.


handle_io_data_process_token_queue( S0 = #s{ token_q = TokQ0, active_session = Session } ) ->
	case token_queue_get_expr( TokQ0 ) of
		error -> eval_loop( S0 );

		{ok, ExprTokens, TokQ1} ->
			S1 = S0 #s{ token_q = TokQ1 },
			case erl_parse:parse_exprs( ExprTokens ) of
				{ok, Exprs} ->
					% io:format("\t\tExprs: ~p~n", [ Exprs ]),
					ok = owl_shell_session:post_exprs( Session, Exprs ),

					handle_io_data_process_token_queue( S1 );
				{error, {Line, erl_parse, ErrorIOL}} ->
					io:format("\tError: (~p) ~s~n", [ Line, ErrorIOL ]),
					handle_io_data_process_token_queue( S1 )
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

io_write_prompt( S0 = #s{ io = IO, active_session_id = SID } ) ->
	?io:write( IO, io_lib:format( "[session: ~p] # ", [ SID ] ) ),
	{ok, S0}.

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

