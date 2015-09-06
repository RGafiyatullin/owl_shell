-module (owl_shell_session_sup_sup).

-export ([start_link/1, start_child/2]).
-export ([init/1]).
-export ([session_sup_child_id/1]).

session_sup_child_id( SessionID ) ->
	{session_sup, SessionID}.


start_link( TopSup ) ->
	supervisor:start_link( ?MODULE, {session_sup_sup, TopSup} ).
start_child( SessionSupSup, SessionID ) ->
	supervisor:start_child( SessionSupSup,
		{ session_sup_child_id( SessionID ),
			{ owl_shell_session_sup, start_link, [ SessionSupSup, SessionID ] },
			temporary, infinity, supervisor, [ owl_shell_session_sup ] } ).


init( {session_sup_sup, _TopSup} ) ->
	SupType = {one_for_one, 0, 1},
	ChildSpecs = [],
	{ok, {SupType, ChildSpecs}}.
