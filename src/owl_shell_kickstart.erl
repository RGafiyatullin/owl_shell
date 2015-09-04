-module (owl_shell_kickstart).

-export ([start/0, draw/0, draw_application/1]).

start() ->
	application:start(sasl),
	application:start(gen_unix),
	application:start(owl_shell).

draw() ->
	lists:foreach( fun draw_application/1, application:which_applications() ).

draw_application( {AppName, _, _} ) ->
	io:format("APP: ~p~n", [ AppName ]),
	case application_controller:get_master( AppName ) of
		undefined -> io:format("\t no sup-tree~n");
		AppMaster ->
			io:format("\tmaster: ~p~n", [ AppMaster ]),
			draw_app_master( AppMaster )
	end.

draw_app_master( AppMaster ) ->
	{Child, Name} = application_master:get_child( AppMaster ),

	io:format("\t\t~p / ~p~n", [ Child, Name ]).
