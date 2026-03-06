default: compile upload monitor

compile:
	arduino-cli compile main

upload:
	arduino-cli upload main

monitor:
	@trap "exit" INT; \
		while true; do arduino-cli monitor main; done

dependencies:
	arduino-cli core install "esp32:esp32"
	arduino-cli lib install "NimBLE-Arduino"
