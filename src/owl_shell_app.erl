-module (owl_shell_app).
-behaviour (application).

-export ([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    owl_shell_sup:start_link("test.sock").

stop(_State) ->
    ok.
