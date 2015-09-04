-module (owl_shell_conn).
-export ([start_link/2]).
-export ([init_it/2]).

-define(io, owl_shell_conn_io).

-spec start_link( pid(), pid() ) -> {ok, pid()}.
start_link( ShellSup, SessionSup ) -> proc_lib:start_link( ?MODULE, init_it, [ ShellSup, SessionSup ] ).

-record( s, {
		'#module' = ?MODULE :: ?MODULE,
		io :: pid(),
		session_sup_sup :: pid(),
		session_sup :: pid(),

		token_q = queue:new() :: queue:queue( erl_scan:token() )
	} ).

init_it( ShellSup, SessionSup ) ->
	ok = proc_lib:init_ack( {ok, self()} ),
	[ IO ] = [ P || {io, P, _, _} <- supervisor:which_children( SessionSup ) ],
	ok = ?io:acquire( IO ),
	S0 = #s{ io = IO, session_sup_sup = ShellSup, session_sup = SessionSup },

	eval_loop( S0 ).

eval_loop( S0 = #s{ io = IO } ) ->
	receive
		{?io, data, IO, Data} ->
			handle_io_data( Data, S0 );
		{?io, closed, IO} ->
			handle_io_closed( S0 )
	end.

handle_io_closed( #s{ session_sup_sup = SupSup, session_sup = Sup } ) ->
	_ = supervisor:terminate_child( SupSup, Sup ).

handle_io_data( Data, S0 = #s{ io = IO, token_q = TokQ0 } ) ->
	io:format("DataIn: ~p~n", [ Data ]),
	ok = ?io:write( IO, <<"okay, keep talking...\n">> ),

	case erl_scan:string( binary_to_list( Data ) ) of
		{ok, Tokens, _EndPos} ->
			io:format("\tTokens: ~p~n", [ Tokens ]),
			TokQ1 = lists:foldl( fun queue:in/2, TokQ0, Tokens ),
			handle_io_data_process_token_queue( S0 #s{ token_q = TokQ1 } );
		{error, ErrorInfo, ErrorLocation} ->
			io:format("\tError: ~p (~p)~n", [ ErrorInfo, ErrorLocation ]),
			eval_loop( S0 )
	end.

handle_io_data_process_token_queue( S0 = #s{ token_q = TokQ0 } ) ->
	case token_queue_get_expr( TokQ0 ) of
		error -> eval_loop( S0 );

		{ok, ExprTokens, TokQ1} ->
			S1 = S0 #s{ token_q = TokQ1 },
			case erl_parse:parse_exprs( ExprTokens ) of
				{ok, Exprs} ->
					io:format("\t\tExprs: ~p~n", [ Exprs ]),



					handle_io_data_process_token_queue( S1 );
				{error, {Line, erl_parse, ErrorIOL}} ->
					io:format("\tError: (~p) ~s~n", [ Line, ErrorIOL ]),
					handle_io_data_process_token_queue( S1 )
			end
	end.

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

