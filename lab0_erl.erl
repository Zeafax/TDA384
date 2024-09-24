-module(lab0_erl).
	-export([hello_world/0]).

	hello_world() -> io:fwrite("hello, world\n").