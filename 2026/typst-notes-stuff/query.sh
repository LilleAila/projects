#!/usr/bin/env bash

# Simple script to query metadata from a typst note
typst eval '
(
  meta: query(<note-meta>).first(default: (value: (:))).value,
  links: query(link).map(it => it.dest).filter(d => type(d) == str and d.starts-with("typst-note://")),
  headings: query(heading).map(h => (level: h.level, body: h.body))
)
' --in test.typ --root .
