RUN_install.txt is for Docker use...

This is a skeleton for a shiny app.
When creating a new app, you should copy this directory and delete only the contents of the 'shiny/modules' directory.

ALL media [for any module] should be placed in the 'www' directory.

CodeFiles => multiple depth levels
Functions, Classes, Variables => only one depth level created [you can make directories, but all files are "sourced" at the same depth level]

Favicons work only locally.

css conventions:
	global:
		__name_...
	
	in a module:
		moduleName__name...

js conventions:
	var module_name = {};
	
	module_name.f = ...

shinyjs conventions:
	global:
		shinyjs.__my_function
	
	in a module:
		shinyjs.module__my_function




