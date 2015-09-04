-module (owl_shell_session_sup).
-behaviour (supervisor).

init( { session, SessionID } ) ->
	SupType = { one_for_all, 0, 1 },
	ChildSpecs = [
			{ evals_sup, {supervisor, start_link, [ ?MODULE, { evals_sup } ]}, permanent, infinity, supervisor, [ ?MODULE ] },
			{ session, {owl_shell_session, start_link, [ self() ]}, permanent, 1000, worker, [ owl_shell_session ] }
		],
	{ok, {SupType, ChildSpecs}}.
