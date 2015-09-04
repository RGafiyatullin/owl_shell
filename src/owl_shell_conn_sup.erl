-module (owl_shell_conn_sup).
-behaviour (supervisor).
-export ([start_link/2]).
-export ([init/1]).

-spec start_link( ConnSupSup :: pid(), integer() ) -> {ok, pid()}.
start_link( ConnSupSup, FD ) ->
	supervisor:start_link( ?MODULE, { conn_sup, ConnSupSup, FD } ).

init( {conn_sup, ConnSupSup, FD} ) ->
	SupType = {one_for_all, 0, 1},
	ChildSpecs = [
			{ io, {owl_shell_conn_io, start_link, [ FD ]}, permanent, 1000, worker, [ owl_shell_conn_io ] },
			{ connection, {owl_shell_conn, start_link, [ ConnSupSup, self() ] }, permanent, 1000, worker, [ owl_shell_conn ] }
		],
	io:format(" - owl_shell_conn_sup (~p) => { ~p, ~p } ~n", [ FD, SupType, ChildSpecs ]),
	{ok, {SupType, ChildSpecs}}.


