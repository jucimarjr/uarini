-define(VSN, 0.1).
-define(TEAM, [
		'uea_ludus@googlegroups.com',
		'Jucimar Jr',
		'Emiliano Firmino',
		'Daniel Henrique',
		'Rodrigo Bernardino']).
-define(YEAR, 2012).
-define(CONSTR_NAME, new_).

-record(class, {
			name    = "",	 % ClassName
			parent  = null, % ParentName
			attrs	= [],	 % AttrList
			methods	= [],	 % MethodList
			constr 	= [],	 % ConstrList
			export	= [],	 % ExportList
			static	= []}). % StaticList
