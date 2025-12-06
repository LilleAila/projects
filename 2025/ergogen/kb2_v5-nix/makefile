all: build

build: config ergogen cases

config:
	nix-build -o config.yaml --show-trace

ergogen:
	npx ergogen .

cases:
	for i in output/cases/*.jscad; do npx @jscad/cli@1 "$$i" -of stla; done

clean:
	rm config.yaml
	rm -rf output
