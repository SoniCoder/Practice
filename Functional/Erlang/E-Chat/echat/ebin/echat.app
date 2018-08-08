{application, 'echat', [
	{description, "New project"},
	{vsn, "0.1.0"},
	{modules, ['baseinfo','client_api','db_functions','echat','message','tabmgr']},
	{registered, []},
	{applications, [kernel,stdlib]},
	{env, []}
]}.