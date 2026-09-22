#!/usr/bin/env bash

# WORDS_SOURCE="https://github.com/AllenZLink/Kilordle/raw/refs/heads/main/src/util/words.ts"
WORDS_SOURCE="https://github.com/AllenZLink/Kilordle/raw/c3697b8241f5749e9284d7edae11f218c97894f4/src/util/words.ts"

curl -sL "$WORDS_SOURCE" | grep -oE "'[^']+'" | tr -d "'" > words.csv
