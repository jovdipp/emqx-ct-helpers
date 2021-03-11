
compile: deps
	@rebar3 compile

clean:
	@rm -rf _build cover deps logs log data
	@rm -f rebar.lock compile_commands.json cuttlefish

deps:
	@rebar3 get-deps


ct:
	@rebar3 ct -v

eunit:
	@rebar3 eunit -v

xref:
	@rebar3 xref

