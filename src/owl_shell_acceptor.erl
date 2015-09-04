-module (owl_shell_acceptor).

-export ([start_link/2]).
-export ([init_it/2]).

start_link( Sup, PathToSock ) ->
	proc_lib:start_link( ?MODULE, init_it, [ Sup, PathToSock ] ).

init_it( Sup, PathToSock ) ->
	ok = proc_lib:init_ack( {ok, self()} ),
	[ GenUnix ] = [ P || {gen_unix, P, _, _} <- supervisor:which_children( Sup ) ],
	[ ConnSupSup ] = [ P || {conn_sup_sup, P, _, _} <- supervisor:which_children( Sup ) ],
	{ok, LSockFD} = gen_unix:listen( GenUnix, PathToSock ),

	accept_loop( ConnSupSup, GenUnix, LSockFD ).

accept_loop( ConnSupSup, GenUnix, LSockFD ) ->
	{ok, SockFD} = gen_unix:accept( GenUnix, LSockFD ),
	{ok, _ShellSrv} = supervisor:start_child( ConnSupSup, [ SockFD ] ),
	accept_loop( ConnSupSup, GenUnix, LSockFD ).

