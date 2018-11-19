PROJECT = jerlang
PROJECT_DESCRIPTION = New project
PROJECT_VERSION = 0.1.0

include erlang.mk

purge:
	rm -rf _rel
	rm -rf ./ebin/*.beam
	rm -rf ./ebin/*.app
	rm -rf deps/*/ebin/*.app
	rm -rf deps/*/ebin/*.beam
	rm -rf deps/*/.idea
	rm -rf deps/*/.erlang.mk
	rm -rf deps/*/test
	rm -rf deps/*/examples
	rm -rf deps/*/doc
	rm -rf deps/*/.git
	rm -rf deps/*/*.d