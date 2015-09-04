-module (owl_shell_sup).
-behaviour (supervisor).

-export ([start_link/1]).
-export ([init/1]).

-spec start_link( UnixSocketFile :: string() ) -> {ok, pid()} | {error, Reason :: term()}.
start_link( UnixSocketFile ) -> supervisor:start_link( ?MODULE, { top_sup, UnixSocketFile } ).

init( { top_sup, UnixSocketFile } ) ->
	SupType = {one_for_all, 0, 1},
	ChildSpecs = [
			{gen_unix, {gen_unix, start_link, [ [] ]}, permanent, 1000, worker, [ gen_unix ]},
			{conn_sup_sup, {supervisor, start_link, [ ?MODULE, conn_sup_sup ]}, permanent, infinity, supervisor, [ ?MODULE ]},
			{acceptor, {owl_shell_acceptor, start_link, [self(), UnixSocketFile]}, permanent, 1000, worker, [ owl_shell_acceptor ]}
		],
	{ok, {SupType, ChildSpecs}};

init( conn_sup_sup ) ->
	SupType = {simple_one_for_one, 0, 1},
	ChildSpecs = [ {shell, {owl_shell_conn_sup, start_link, [ self() ]}, temporary, infinity, supervisor, [ owl_shell_conn_sup ]} ],
	{ok, {SupType, ChildSpecs}}.

