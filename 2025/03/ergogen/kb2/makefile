all: build

build: ergogen

ergogen:
	npx ergogen .

cases:
	for i in output/cases/*.jscad; do npx @jscad/cli@1 "$$i" -of stla; done

clean:
	rm -rf output
