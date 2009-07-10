% testutils is a library not an application.
% This file is for compatibility issues only.
{application, testutils, [
	{description, "Test utilities for Proximetry usage"},
	{vsn, "1.0"},
	{modules, [
		smerl
	]},
	{registered, []},
	{applications, [kernel, stdlib]}
]}.
