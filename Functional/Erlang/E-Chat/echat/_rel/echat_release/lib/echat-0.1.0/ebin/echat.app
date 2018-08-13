{application, 'echat', [
	{description, "New project"},
	{vsn, "0.1.0"},
	{modules, ['baseinfo','client_api','clientinfo','db_functions','echat','echat_app','echat_sup','message','scriber','tabmgr']},
	{registered, [echat_sup]},
	{applications, [kernel,stdlib]},
	{mod, {echat_app, []}},
	{env, []}
]}.