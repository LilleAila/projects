#import "preamble.typ": *
#show: note-document.with((
  id: "test-note",
  title: "Test note",
  author: "Olai Solsvik",
  date: "2026-11-25",
  lang: "nb",
  aliases: ("foo", "bar"),
))

= Hello, World!

#link("typst-note://test2")[Test 2!]

#lorem(100)

abcdef

== test123

#lorem(10000)
