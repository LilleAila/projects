// 42keebs Frood RD2840
// Pro Micro-compatible
// Params
//  orientation: default is down
//    if down, power led will face the pcb
//    if up, power led will face away from pcb

module.exports = {
  params: {
    designator: "MCU",
    orientation: "down",

    Dm: { type: "net", value: "Dm" },
    RAW: { type: "net", value: "RAW" },
    GND: { type: "net", value: "GND" },
    RST: { type: "net", value: "RST" },
    VCC: { type: "net", value: "VCC" },
    D29: { type: "net", value: "D29" },
    D28: { type: "net", value: "D28" },
    D27: { type: "net", value: "D27" },
    D26: { type: "net", value: "D26" },
    D22: { type: "net", value: "D22" },
    D20: { type: "net", value: "D20" },
    D23: { type: "net", value: "D23" },
    D21: { type: "net", value: "D21" },

    D12: { type: "net", value: "D12" },
    D13: { type: "net", value: "D13" },
    D14: { type: "net", value: "D14" },
    D15: { type: "net", value: "D15" },
    D16: { type: "net", value: "D16" },

    Dp: { type: "net", value: "Dp" },
    D0: { type: "net", value: "D0" },
    D1: { type: "net", value: "D1" },
    D2: { type: "net", value: "D2" },
    D3: { type: "net", value: "D3" },
    D4: { type: "net", value: "D4" },
    D5: { type: "net", value: "D5" },
    D6: { type: "net", value: "D6" },
    D7: { type: "net", value: "D7" },
    D8: { type: "net", value: "D8" },
    D9: { type: "net", value: "D9" },
  },
  body: (p) => {
    const standard = `
      (module ProMicro (layer F.Cu) (tedit 5B307E4C)
      ${p.at /* parametric position */}

      ${"" /* footprint reference */}
      (fp_text reference "${p.ref}" (at 0 0) (layer F.SilkS) ${p.ref_hide} (effects (font (size 1.27 1.27) (thickness 0.15))))
      (fp_text value "" (at 0 0) (layer F.SilkS) hide (effects (font (size 1.27 1.27) (thickness 0.15))))

      ${"" /* illustration of the (possible) USB port overhang */}
      (fp_line (start -19.304 -3.81) (end -14.224 -3.81) (layer Dwgs.User) (width 0.15))
      (fp_line (start -19.304 3.81) (end -19.304 -3.81) (layer Dwgs.User) (width 0.15))
      (fp_line (start -14.224 3.81) (end -19.304 3.81) (layer Dwgs.User) (width 0.15))
      (fp_line (start -14.224 -3.81) (end -14.224 3.81) (layer Dwgs.User) (width 0.15))

      ${"" /* component outline */}
      (fp_line (start -17.78 8.89) (end 15.24 8.89) (layer F.SilkS) (width 0.15))
      (fp_line (start 15.24 8.89) (end 15.24 -8.89) (layer F.SilkS) (width 0.15))
      (fp_line (start 15.24 -8.89) (end -17.78 -8.89) (layer F.SilkS) (width 0.15))
      (fp_line (start -17.78 -8.89) (end -17.78 8.89) (layer F.SilkS) (width 0.15))
      `;
    function pins(def_neg, def_pos) {
      return `
        ${"" /* extra border around "RAW", in case the rectangular shape is not distinctive enough */}
        (fp_line (start -15.24 ${def_pos}6.35) (end -12.7 ${def_pos}6.35) (layer F.SilkS) (width 0.15))
        (fp_line (start -15.24 ${def_pos}6.35) (end -15.24 ${def_pos}8.89) (layer F.SilkS) (width 0.15))
        (fp_line (start -12.7 ${def_pos}6.35) (end -12.7 ${def_pos}8.89) (layer F.SilkS) (width 0.15))

        ${"" /* pin names */}
        (fp_text user D- (at -16.51 ${def_pos}10.4 ${p.r - 45}) (layer F.SilkS) (effects (font (size 0.8 0.8) (thickness 0.15))))
        (fp_text user RAW (at -13.97 ${def_pos}10.4 ${p.r - 45}) (layer F.SilkS) (effects (font (size 0.8 0.8) (thickness 0.15))))
        (fp_text user GND (at -11.43 ${def_pos}10.4 ${p.r - 45}) (layer F.SilkS) (effects (font (size 0.8 0.8) (thickness 0.15))))
        (fp_text user RST (at -8.89 ${def_pos}10.4 ${p.r - 45}) (layer F.SilkS) (effects (font (size 0.8 0.8) (thickness 0.15))))
        (fp_text user VCC (at -6.35 ${def_pos}10.4 ${p.r - 45}) (layer F.SilkS) (effects (font (size 0.8 0.8) (thickness 0.15))))
        (fp_text user D29 (at -3.81 ${def_pos}10.4 ${p.r - 45}) (layer F.SilkS) (effects (font (size 0.8 0.8) (thickness 0.15))))
        (fp_text user D28 (at -1.27 ${def_pos}10.4 ${p.r - 45}) (layer F.SilkS) (effects (font (size 0.8 0.8) (thickness 0.15))))
        (fp_text user D27 (at 1.27 ${def_pos}10.4 ${p.r - 45}) (layer F.SilkS) (effects (font (size 0.8 0.8) (thickness 0.15))))
        (fp_text user D26 (at 3.81 ${def_pos}10.4 ${p.r - 45}) (layer F.SilkS) (effects (font (size 0.8 0.8) (thickness 0.15))))
        (fp_text user D22 (at 6.35 ${def_pos}10.4 ${p.r - 45}) (layer F.SilkS) (effects (font (size 0.8 0.8) (thickness 0.15))))
        (fp_text user D20 (at 8.89 ${def_pos}10.4 ${p.r - 45}) (layer F.SilkS) (effects (font (size 0.8 0.8) (thickness 0.15))))
        (fp_text user D23 (at 11.43 ${def_pos}10.4 ${p.r - 45}) (layer F.SilkS) (effects (font (size 0.8 0.8) (thickness 0.15))))
        (fp_text user D21 (at 13.97 ${def_pos}10.4 ${p.r - 45}) (layer F.SilkS) (effects (font (size 0.8 0.8) (thickness 0.15))))

        (fp_text user D12 (at 16.9 ${def_neg}5.08 ${p.r - 45}) (layer F.SilkS) (effects (font (size 0.8 0.8) (thickness 0.15))))
        (fp_text user D13 (at 16.9 ${def_neg}2.54 ${p.r - 45}) (layer F.SilkS) (effects (font (size 0.8 0.8) (thickness 0.15))))
        (fp_text user D14 (at 16.9 ${def_neg}0 ${p.r - 45}) (layer F.SilkS) (effects (font (size 0.8 0.8) (thickness 0.15))))
        (fp_text user D15 (at 16.9 ${def_pos}2.54 ${p.r - 45}) (layer F.SilkS) (effects (font (size 0.8 0.8) (thickness 0.15))))
        (fp_text user D15 (at 16.9 ${def_pos}5.08 ${p.r - 45}) (layer F.SilkS) (effects (font (size 0.8 0.8) (thickness 0.15))))

        (fp_text user D+ (at -16.51 ${def_neg}10.4 ${p.r - 45}) (layer F.SilkS) (effects (font (size 0.8 0.8) (thickness 0.15))))
        (fp_text user D0 (at -13.97 ${def_neg}10.4 ${p.r - 45}) (layer F.SilkS) (effects (font (size 0.8 0.8) (thickness 0.15))))
        (fp_text user D1 (at -11.43 ${def_neg}10.4 ${p.r - 45}) (layer F.SilkS) (effects (font (size 0.8 0.8) (thickness 0.15))))
        (fp_text user GND (at -8.89 ${def_neg}10.4 ${p.r - 45}) (layer F.SilkS) (effects (font (size 0.8 0.8) (thickness 0.15))))
        (fp_text user GND (at -6.35 ${def_neg}10.4 ${p.r - 45}) (layer F.SilkS) (effects (font (size 0.8 0.8) (thickness 0.15))))
        (fp_text user D2 (at -3.81 ${def_neg}10.4 ${p.r - 45}) (layer F.SilkS) (effects (font (size 0.8 0.8) (thickness 0.15))))
        (fp_text user D3 (at -1.27 ${def_neg}10.4 ${p.r - 45}) (layer F.SilkS) (effects (font (size 0.8 0.8) (thickness 0.15))))
        (fp_text user D4 (at 1.27 ${def_neg}10.4 ${p.r - 45}) (layer F.SilkS) (effects (font (size 0.8 0.8) (thickness 0.15))))
        (fp_text user D5 (at 3.81 ${def_neg}10.4 ${p.r - 45}) (layer F.SilkS) (effects (font (size 0.8 0.8) (thickness 0.15))))
        (fp_text user D6 (at 6.35 ${def_neg}10.4 ${p.r - 45}) (layer F.SilkS) (effects (font (size 0.8 0.8) (thickness 0.15))))
        (fp_text user D7 (at 8.89 ${def_neg}10.4 ${p.r - 45}) (layer F.SilkS) (effects (font (size 0.8 0.8) (thickness 0.15))))
        (fp_text user D8 (at 11.43 ${def_neg}10.4 ${p.r - 45}) (layer F.SilkS) (effects (font (size 0.8 0.8) (thickness 0.15))))
        (fp_text user D9 (at 13.97 ${def_neg}10.4 ${p.r - 45}) (layer F.SilkS) (effects (font (size 0.8 0.8) (thickness 0.15))))

        ${"" /* and now the actual pins */}
        (pad 2 thru_hole circle (at -16.51 ${def_pos}7.62 0) (size 1.7526 1.7526) (drill 1.0922) (layers *.Cu *.SilkS *.Mask) ${p.Dm})
        (pad 1 thru_hole rect (at -13.97 ${def_pos}7.62 ${p.r}) (size 1.7526 1.7526) (drill 1.0922) (layers *.Cu *.SilkS *.Mask) ${p.RAW})
        (pad 2 thru_hole circle (at -11.43 ${def_pos}7.62 0) (size 1.7526 1.7526) (drill 1.0922) (layers *.Cu *.SilkS *.Mask) ${p.GND})
        (pad 3 thru_hole circle (at -8.89 ${def_pos}7.62 0) (size 1.7526 1.7526) (drill 1.0922) (layers *.Cu *.SilkS *.Mask) ${p.RST})
        (pad 4 thru_hole circle (at -6.35 ${def_pos}7.62 0) (size 1.7526 1.7526) (drill 1.0922) (layers *.Cu *.SilkS *.Mask) ${p.VCC})
        (pad 5 thru_hole circle (at -3.81 ${def_pos}7.62 0) (size 1.7526 1.7526) (drill 1.0922) (layers *.Cu *.SilkS *.Mask) ${p.D29})
        (pad 6 thru_hole circle (at -1.27 ${def_pos}7.62 0) (size 1.7526 1.7526) (drill 1.0922) (layers *.Cu *.SilkS *.Mask) ${p.D28})
        (pad 7 thru_hole circle (at 1.27 ${def_pos}7.62 0) (size 1.7526 1.7526) (drill 1.0922) (layers *.Cu *.SilkS *.Mask) ${p.D27})
        (pad 8 thru_hole circle (at 3.81 ${def_pos}7.62 0) (size 1.7526 1.7526) (drill 1.0922) (layers *.Cu *.SilkS *.Mask) ${p.D26})
        (pad 9 thru_hole circle (at 6.35 ${def_pos}7.62 0) (size 1.7526 1.7526) (drill 1.0922) (layers *.Cu *.SilkS *.Mask) ${p.D22})
        (pad 10 thru_hole circle (at 8.89 ${def_pos}7.62 0) (size 1.7526 1.7526) (drill 1.0922) (layers *.Cu *.SilkS *.Mask) ${p.D20})
        (pad 11 thru_hole circle (at 11.43 ${def_pos}7.62 0) (size 1.7526 1.7526) (drill 1.0922) (layers *.Cu *.SilkS *.Mask) ${p.D23})
        (pad 12 thru_hole circle (at 13.97 ${def_pos}7.62 0) (size 1.7526 1.7526) (drill 1.0922) (layers *.Cu *.SilkS *.Mask) ${p.D21})

        (pad 13 thru_hole circle (at 13.97 ${def_neg}5.08 0) (size 1.7526 1.7526) (drill 1.0922) (layers *.Cu *.SilkS *.Mask) ${p.D12})
        (pad 14 thru_hole circle (at 13.97 ${def_neg}2.54 0) (size 1.7526 1.7526) (drill 1.0922) (layers *.Cu *.SilkS *.Mask) ${p.D13})
        (pad 15 thru_hole circle (at 13.97 ${def_neg}0 0) (size 1.7526 1.7526) (drill 1.0922) (layers *.Cu *.SilkS *.Mask) ${p.D14})
        (pad 16 thru_hole circle (at 13.97 ${def_pos}2.54 0) (size 1.7526 1.7526) (drill 1.0922) (layers *.Cu *.SilkS *.Mask) ${p.D15})
        (pad 17 thru_hole circle (at 13.97 ${def_pos}5.08 0) (size 1.7526 1.7526) (drill 1.0922) (layers *.Cu *.SilkS *.Mask) ${p.D16})

        (pad 18 thru_hole circle (at -16.51 ${def_neg}7.62 0) (size 1.7526 1.7526) (drill 1.0922) (layers *.Cu *.SilkS *.Mask) ${p.Dp})
        (pad 19 thru_hole circle (at -13.97 ${def_neg}7.62 0) (size 1.7526 1.7526) (drill 1.0922) (layers *.Cu *.SilkS *.Mask) ${p.D0})
        (pad 20 thru_hole circle (at -11.43 ${def_neg}7.62 0) (size 1.7526 1.7526) (drill 1.0922) (layers *.Cu *.SilkS *.Mask) ${p.D1})
        (pad 21 thru_hole circle (at -8.89 ${def_neg}7.62 0) (size 1.7526 1.7526) (drill 1.0922) (layers *.Cu *.SilkS *.Mask) ${p.GND})
        (pad 22 thru_hole circle (at -6.35 ${def_neg}7.62 0) (size 1.7526 1.7526) (drill 1.0922) (layers *.Cu *.SilkS *.Mask) ${p.GND})
        (pad 23 thru_hole circle (at -3.81 ${def_neg}7.62 0) (size 1.7526 1.7526) (drill 1.0922) (layers *.Cu *.SilkS *.Mask) ${p.D2})
        (pad 24 thru_hole circle (at -1.27 ${def_neg}7.62 0) (size 1.7526 1.7526) (drill 1.0922) (layers *.Cu *.SilkS *.Mask) ${p.D3})
        (pad 25 thru_hole circle (at 1.27 ${def_neg}7.62 0) (size 1.7526 1.7526) (drill 1.0922) (layers *.Cu *.SilkS *.Mask) ${p.D4})
        (pad 26 thru_hole circle (at 3.81 ${def_neg}7.62 0) (size 1.7526 1.7526) (drill 1.0922) (layers *.Cu *.SilkS *.Mask) ${p.D5})
        (pad 27 thru_hole circle (at 6.35 ${def_neg}7.62 0) (size 1.7526 1.7526) (drill 1.0922) (layers *.Cu *.SilkS *.Mask) ${p.D6})
        (pad 28 thru_hole circle (at 8.89 ${def_neg}7.62 0) (size 1.7526 1.7526) (drill 1.0922) (layers *.Cu *.SilkS *.Mask) ${p.D7})
        (pad 29 thru_hole circle (at 11.43 ${def_neg}7.62 0) (size 1.7526 1.7526) (drill 1.0922) (layers *.Cu *.SilkS *.Mask) ${p.D8})
        (pad 30 thru_hole circle (at 13.97 ${def_neg}7.62 0) (size 1.7526 1.7526) (drill 1.0922) (layers *.Cu *.SilkS *.Mask) ${p.D9})
      `;
    }
    if (p.orientation == "down") {
      return `
        ${standard}
        ${pins("-", "")})
        `;
    } else {
      return `
        ${standard}
        ${pins("", "-")})
        `;
    }
  },
};
