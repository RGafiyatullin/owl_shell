-module (owl_shell_conn_io).

-export ([
		start_link/1,
		acquire/1,
		write/2
	]).
-export ([init_it/1]).

-define( init_timeout, 120000 ).

-define( buff_size, 1024 ).
-define( loose_loop_timeout, 100 ).
-define( tight_loop_timeout, 0 ).
-define( acquire( CP ), {?MODULE, acquire, CP} ).
-define( write( DataIOL ), {?MODULE, write, DataIOL} ).
-define( socket_closed( P ), { ?MODULE, closed, P } ).
-define( input_data( P, D ), { ?MODULE, data, P, D } ).


start_link( FD ) -> proc_lib:start_link( ?MODULE, init_it, [ FD ] ).
acquire( IO ) ->
	_ = erlang:send( IO, ?acquire( self() ) ),
	ok.
write( IO, DataIOL ) ->
	_ = erlang:send( IO, ?write( DataIOL ) ),
	ok.

-record( s, {
		'#module' = ?MODULE :: ?MODULE,
		fd :: integer(),
		cp :: pid()
	} ).

init_it( FD ) ->
	ok = proc_lib:init_ack( {ok, self()} ),
	{ok, ControllingProcess} = wait_for_controlling_process(),

	% io:format("cp: ~p; fd: ~p~n", [ ControllingProcess, FD ]),
	S0 = #s{ fd = FD, cp = ControllingProcess },
	loop_tight( S0 ).


wait_for_controlling_process() ->
	receive
		?acquire( CP ) ->
			{ok, CP}
	after
		?init_timeout ->
			{error, timeout}
	end.

loop( Timeout, S ) ->
	% io:format("loop( ~p, _ )~n", [ Timeout ]),
	receive
		?write( DataIOL ) ->
			data_write( DataIOL, S )
	after
		Timeout ->
			try_reading_then_loop( fun loop_loose/1, S )
	end.


loop_tight( S = #s{} ) -> loop( ?tight_loop_timeout, S ).
loop_loose( S = #s{} ) -> loop( ?loose_loop_timeout, S ).

try_reading_then_loop( LoopF, S = #s{ fd = FD, cp = CP } ) ->
	case procket:read( FD, ?buff_size ) of
		{ok, <<>>} -> socket_closed( S );
		{ok, SomeData} when is_binary( SomeData ) ->
			_ = erlang:send( CP, ?input_data( self(), SomeData ) ),
			loop_tight( S );
		{error, eagain} ->
			LoopF( S )
	end.

data_write( DataIOL, S = #s{ fd = FD } ) ->
	% ok = io:format("~p:data_write( ~p, _ )~n", [ ?MODULE, DataIOL ]),
	Data = iolist_to_binary( [DataIOL] ),
	case procket:write( FD, Data ) of
		{error, epipe} -> socket_closed( S );
		ok -> loop_tight( S )
	end.

socket_closed( #s{ cp = CP } ) ->
	_ = erlang:send( CP, ?socket_closed( self() ) ),
	receive
		_ -> exit(normal)
	end.
