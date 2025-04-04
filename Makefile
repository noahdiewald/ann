build:
	node build.js
	dune build

server:
	dune exec -- ann ipsum.json &

stop:
	curl -X POST http://localhost:8080/exit || true

reload: stop build server

watchcycle:
	while true; do \
		inotifywait -qr -e modify client src; \
		make reload; \
	done

watch: build server watchcycle
