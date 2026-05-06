#import "@preview/colorful-boxes:1.4.2": colorbox
#import "@preview/numbly:0.1.0": numbly
#import "@preview/touying:0.5.3": *
#import themes.metropolis: *
#import "@preview/callisto:0.2.5"

#show: metropolis-theme.with(
  aspect-ratio: "16-9",
  align: horizon,
  config-info(
    title: [Historisk utvikling av en teknologisk produkt],
    subtitle: [Historien av transistorer og datamaskiner],
    author: [Olai],
    date: [2026-05-04],
    institution: [Amalie Skram VGS],
  ),
)

#set text(lang: "nb", font: "DejaVu Sans")

#set heading(numbering: numbly("{1}.", default: "1.1"))

#show raw: set text(font: "JetBrainsMono NF")
#show raw.where(block: true): x => {
  set text(size: 0.8em)
  block(
    fill: luma(245),
    stroke: 1pt + luma(200),
    inset: 10pt,
    radius: 4pt,
    width: 100%,
    x,
  )
}
#show raw.where(block: false): x => {
  h(1pt)
  box(
    fill: luma(240),
    stroke: 1pt + luma(200),
    inset: (x: 3pt),
    outset: (y: 3pt),
    radius: 2pt,
    x,
  )
  h(1pt)
}

#title-slide()

= Innhold <touying:hidden>

#outline(title: none, indent: 1em, depth: 2)

Et eller annet som siteres @code[s.~1]

#bibliography("bibliography.yaml", style: "apa")
