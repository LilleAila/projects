#import "@preview/colorful-boxes:1.4.2": colorbox
#import "@preview/meander:0.4.4"

#let note(body) = block(breakable: false, above: 8pt, below: 8pt)[#colorbox(
  title: "Note",
  color: "blue",
  body,
)]

#let task(body) = block(breakable: false)[#colorbox(
  title: "Task",
  color: "green",
  body,
)]

#let Unit(u) = [$upright(#u)$]
#let unit(u) = [$thin Unit(#u)$]
#let pm = sym.plus.minus
#let ans(x) = math.underline(math.underline(x))

#let note-document(meta, body) = [
  #let title = meta.at("title", default: "Untitled")
  #let author = meta.at("author", default: "")
  #let date = meta.at("date", default: "")
  #let lang = meta.at("lang", default: "en")

  #let preview = sys.inputs.at("preview", default: "") == "1"

  #set text(
    font: "DejaVu Sans",
    size: 9pt,
    lang: "nb",
  )

  #set par(spacing: 2.0em, leading: 0.8em)
  #show heading: set block(below: 1.2em)
  #show bibliography: set par(spacing: 1.4em)

  #set page(
    paper: "a4",
    margin: (top: 4cm, bottom: 4cm, left: 1.5cm, right: 1.5cm),
    columns: 1,
    header: {
      grid(
        columns: (1fr, auto),
        [
          #title
          #if preview [(preview)]
        ],
        align(right)[#author],
      )
      line(length: 100%)
    },
    footer: context {
      line(length: 100%)
      grid(
        columns: (1fr, auto),
        [#date], align(right)[#counter(page).display("1/1", both: true)],
      )
    },
  )

  #show link: x => underline(text(fill: blue, x))
  #show cite: set text(fill: rgb("#666666"), size: 8pt)

  #show raw: set text(font: "JetBrainsMono NF")
  #show raw.where(block: true): x => block(
    fill: luma(245),
    stroke: 1pt + luma(200),
    inset: 10pt,
    radius: 4pt,
    width: 100%,
    above: 16pt,
    below: 16pt,
    {
      set par(
        hanging-indent: 1.5em,
        leading: 0.6em,
      )
      x
    },
  )
  #show raw.where(block: false): x => (
    text(weight: "bold", fill: rgb("#b31d28"), x)
  )


  #let content = [
    #align(center)[
      #stack(
        spacing: 1.5em,
        text(size: 14pt, weight: "bold")[#title],
        text(size: 10pt)[#author],
        text(size: 8pt)[#date],
      )
    ]

    #body
  ]

  #metadata(meta) <note-meta>

  #content
]
