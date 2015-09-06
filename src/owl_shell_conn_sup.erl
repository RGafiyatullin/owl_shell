-module (owl_shell_conn_sup).
-behaviour (supervisor).
-export ([start_link/3]).
-export ([init/1]).

-spec start_link( TopSup :: pid(), ConnSupSup :: pid(), integer() ) -> {ok, pid()}.
start_link( TopSup, ConnSupSup, FD ) ->
	supervisor:start_link( ?MODULE, { conn_sup, TopSup, ConnSupSup, FD } ).

init( {conn_sup, TopSup, ConnSupSup, FD} ) ->
	SupType = {one_for_all, 0, 1},
	ChildSpecs = [
			{ io, {owl_shell_conn_io, start_link, [ FD ]}, permanent, 1000, worker, [ owl_shell_conn_io ] },
			{ connection, {owl_shell_conn, start_link, [ TopSup, ConnSupSup, self() ] }, permanent, 1000, worker, [ owl_shell_conn ] }
		],
	{ok, {SupType, ChildSpecs}}.


