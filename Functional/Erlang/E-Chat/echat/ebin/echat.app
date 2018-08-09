{application, 'echat', [
	{description, "New project"},
	{vsn, "0.1.0"},
	{modules, ['baseinfo','client_api','clientinfo','db_functions','echat','message','scriber','tabmgr']},
	{registered, []},
	{applications, [kernel,stdlib]},
	{env, []}
]}.