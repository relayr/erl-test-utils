% testutils is a library not an application.
% This file is for compatibility issues only.
{application, testutils, [
	{description, "Test utilities for Proximetry usage"},
	{vsn, "1.0"},
	{modules, [
		smerl,
		test_utils
	]},
	{registered, []},
	{applications, [kernel, stdlib, hamcrest]}
]}.
