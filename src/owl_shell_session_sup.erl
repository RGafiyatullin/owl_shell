-module (owl_shell_session_sup).
-behaviour (supervisor).

-export ([start_link/1]).
-export ([init/1]).

start_link( SessionID ) ->
	supervisor:start_link( ?MODULE, {session, SessionID} ).

init( { session, SessionID } ) ->
	SupType = { one_for_all, 0, 1 },
	ChildSpecs = [
			{ evals_sup, {supervisor, start_link, [ ?MODULE, {evals_sup, SessionID} ]}, permanent, infinity, supervisor, [ ?MODULE ] },
			{ session, {owl_shell_session, start_link, [ self(), SessionID ]}, permanent, 1000, worker, [ owl_shell_session ] }
		],
	{ok, {SupType, ChildSpecs}};
init( {evals_sup, SessionID} ) ->
	SupType = { simple_one_for_one, 0, 1 },
	ChildSpecs = [
			{eval, {owl_shell_session, start_link_eval_process, [ SessionID ]}, temporary, 1000, worker, [ owl_shell_session ]}
		],
	{ok, {SupType, ChildSpecs}}.

