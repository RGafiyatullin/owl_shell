-module (owl_shell_app).
-behaviour (application).

-export ([start/2, stop/1]).

start(_StartType, _StartArgs) ->
	case application:get_env( owl_shell, uds_file ) of
		{ok, File} ->
    		owl_shell_sup:start_link( File );
    	undefined ->
    		owl_shell_sup:start_link( undefined )
    end.

stop(_State) ->
    ok.
