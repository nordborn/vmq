vmq
=====

An OTP application
Simple OTP-only MQ without external deps

Build
-----

    $ rebar3 compile

Use
---

Default backend is Mnesia disc_copies (i.e. RAM + DISC tables)

	Topic = <<"test">>,
	vmq:topic_new(Topic),
	Val = 1,
	vmq:put(Topic, Val),
	ValConsumed = vmq:consume(Topic).
	
To use GenServer as the backend (fast in-memory with disc read/write actions on start/stop):

	vmq:backend_module(vmq_gs),
	vmq:topic_new(...),
	...
	
