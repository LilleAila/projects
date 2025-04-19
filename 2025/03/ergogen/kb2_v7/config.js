// Key size (choc)
const kx = 18,
  ky = 17;

// Switch size
const sx = 14.3,
  sy = 14.3,
  sz = 2;

// Padding
const p = 2,
  px = kx + p * 2,
  py = ky + p * 2;

const tolerance = 0.5; // Tolerance between PCB and case edge

// Case dimensions (compared to center of outer key)
const wall_width = 2; // also spacing between pcb and wall

// Screw size (M2x4)
const screwHeadSize = 4 / 2,
  standoffInner = 3.3 / 2,
  standoffOuter = 5.8 / 2;

// Heights
const bottomHeight = 1,
  standoffHeight = 3,
  pcbHeight = 1.6,
  topInset = 0.4,
  topHeight = 2.2,
  screwHeadHeight = 1.6;

const fillet = 3;

const mkWhere = (ref, a, b) => ({ ref, shift: [a, b] });
const mkAgg = (a, b) => ({ ref: { aggregate: { parts: [a, b] } } });

const mkBoard = (padding) => [
  mkWhere("left_outer_ltop", -kx / 2 - padding, ky / 2 + padding),
  mkWhere("right_outer_rtop", kx / 2 + padding, ky / 2 + padding),
  mkWhere("right_outer_rbottom", kx / 2 + padding, -ky / 2 - padding),
  mkWhere("right_middle_rbottom", -kx / 2 + padding, -ky / 2 - padding),
  mkWhere("rthumbs_outer_rttop", kx / 2 + padding, -ky / 2 - padding),
  mkWhere("lthumbs_outer_lttop", -kx / 2 - padding, -ky / 2 - padding),
  mkWhere("left_middle_lbottom", kx / 2 - padding, -ky / 2 - padding),
  mkWhere("left_outer_lbottom", -kx / 2 - padding, -ky / 2 - padding),
];

const mountingLocations = [
  // Left
  mkAgg("left_outer_ltop", "left_pinky_lbottom"),
  {
    ...mkAgg("left_inner_ltop", "left_inner_lbottom"),
    shift: [kx / 2, 0],
  },
  {
    ...mkAgg("left_inner_lbottom", "lthumbs_inner_lttop"),
    shift: [kx / 2, 0],
  },

  // Thumbs
  {
    ...mkAgg("lthumbs_outer_lttop", "lthumbs_inner_lttop"),
    shift: [0, ky / 2],
  },
  {
    ...mkAgg("rthumbs_inner_rttop", "rthumbs_outer_rttop"),
    shift: [0, ky / 2],
  },

  // Right
  mkAgg("right_outer_rtop", "right_pinky_rbottom"),
  {
    ...mkAgg("right_inner_rtop", "right_inner_rbottom"),
    shift: [-kx / 2, 0],
  },
  {
    ...mkAgg("right_inner_rbottom", "rthumbs_inner_rttop"),
    shift: [-kx / 2, 0],
  },
];

return {
  points: {
    zones: {
      // KiCAD placement
      left: {
        anchor: { shift: [100, -100] },
        key: { padding: ky, spread: kx },
        columns: {
          outer: { key: { column_net: "D23" } },
          pinky: { key: { column_net: "D20" } },
          ring: { key: { column_net: "D22" } },
          middle: { key: { column_net: "D26" } },
          index: { key: { column_net: "D27" } },
          inner: { key: { column_net: "D28" } },
        },
        rows: {
          lbottom: { row_net: "D21" },
          ltop: { row_net: "D29" },
        },
      },

      right: {
        anchor: { ref: "left_inner_lbottom", shift: [3 * kx, 0] },
        key: { padding: ky, spread: kx },
        columns: {
          inner: { key: { column_net: "D3" } },
          index: { key: { column_net: "D4" } },
          middle: { key: { column_net: "D5" } },
          ring: { key: { column_net: "D6" } },
          pinky: { key: { column_net: "D7" } },
          outer: { key: { column_net: "D8" } },
        },
        rows: {
          rbottom: { row_net: "D9" },
          rtop: { row_net: "D2" },
        },
      },

      lthumbs: {
        anchor: { ref: "left_middle_lbottom", shift: [kx, -2 * ky] },
        key: { padding: ky, spread: kx },
        columns: {
          outer: { key: { column_net: "D27" } },
          inner: { key: { column_net: "D28" } },
        },
        rows: {
          lttop: { row_net: "D1" },
        },
      },

      rthumbs: {
        anchor: { ref: "right_middle_rbottom", shift: [-2 * kx, -2 * ky] },
        key: { padding: ky, spread: kx },
        columns: {
          inner: { key: { column_net: "D3" } },
          outer: { key: { column_net: "D4" } },
        },
        rows: {
          rttop: { row_net: "D1" },
        },
      },
    },
  },

  outlines: {
    switches: [{ what: "rectangle", where: true, size: [sx, sy] }],

    microcontroller: [
      {
        what: "polygon",
        operation: "stack",
        points: [
          mkWhere(
            "left_inner_ltop",
            kx / 2 + (kx - sx) / 2,
            ky / 2 + p + wall_width * 2,
          ),
          mkWhere(
            "right_inner_rtop",
            -kx / 2 - (kx - sx) / 2,
            ky / 2 + p + wall_width * 2,
          ),
          mkWhere("right_inner_rbottom", -kx / 2 - (kx - sx) / 2, -ky / 2),
          mkWhere("left_inner_lbottom", kx / 2 + (kx - sx) / 2, -ky / 2),
        ],
      },
    ],

    // The pcb
    board: [
      {
        what: "polygon",
        operation: "stack",
        points: mkBoard(p),
        fillet,
      },
    ],

    // Lip (on which the pcb rests)
    _innerLip: [
      {
        what: "polygon",
        operation: "stack",
        points: mkBoard(0),
        fillet,
      },
    ],

    _outerLip: [
      {
        what: "polygon",
        operation: "stack",
        points: mkBoard(p + tolerance),
        fillet,
      },
    ],

    lip: [{ name: "_outerLip" }, { name: "_innerLip", operation: "subtract" }],

    // Case walls
    innerCase: [
      {
        what: "polygon",
        operation: "stack",
        points: mkBoard(p + tolerance),
        fillet,
      },
    ],

    outerCase: [
      {
        what: "polygon",
        operation: "stack",
        points: mkBoard(p + tolerance + wall_width),
        fillet,
      },
    ],

    caseWalls: [
      { name: "outerCase" },
      { name: "innerCase", operation: "subtract" },
    ],

    topPlate: [
      { name: "outerCase" },
      { operation: "subtract", name: "microcontroller", fillet },
      { operation: "subtract", name: "switches" },
    ],

    // Standoffs
    _standoffsInner: mountingLocations.map((where) => ({
      what: "circle",
      radius: standoffInner,
      where,
    })),

    _standoffsOuter: mountingLocations.map((where) => ({
      what: "circle",
      radius: standoffOuter,
      where,
    })),

    standoffs: [
      { name: "_standoffsOuter" },
      { name: "_standoffsInner", operation: "subtract" },
    ],

    // Holes in top plate for screw head
    screwHoles: mountingLocations.map((where) => ({
      what: "circle",
      radius: screwHeadSize,
      where,
    })),
  },

  pcbs: {
    pcb: {
      outlines: { main: { outline: "board" } },

      footprints: {
        choc_hotswap: {
          what: "choc",
          where: true,
          params: {
            keycaps: true,
            reverse: false,
            hotswap: true,
            from: "{{column_net}}",
            to: "{{colrow}}",
          },
        },

        diode: {
          what: "diode",
          where: true,
          params: {
            from: "{{colrow}}",
            to: "{{row_net}}",
          },
          adjust: { shift: [0, -5] },
        },

        promicro: {
          what: "frood",
          params: {
            orientation: "down",
          },
          where: {
            ...mkAgg("left_inner_ltop", "right_inner_rtop"),
            shift: [0, -7.5],
            rotate: -90,
          },
        },

        reset: {
          what: "button",
          params: {
            from: "GND",
            to: "RST",
            side: "B",
          },
          where: mkAgg("left_inner_ltop", "right_inner_rbottom"),
        },

        ...Object.fromEntries(
          mountingLocations.map((where, i) => [
            `hole${i}`,
            {
              what: "mounthole",
              where,
            },
          ]),
        ),
      },
    },
  },

  cases: {
    // Bottom
    bottom: [
      { name: "outerCase", extrude: bottomHeight },
      {
        name: "caseWalls",
        extrude: standoffHeight + pcbHeight + topInset,
        operation: "add",
        shift: [0, 0, bottomHeight],
      },
      {
        name: "lip",
        extrude: standoffHeight,
        operation: "add",
        shift: [0, 0, bottomHeight],
      },
      {
        name: "standoffs",
        extrude: standoffHeight,
        operation: "add",
        shift: [0, 0, bottomHeight],
      },
    ],

    // Top
    top: [
      { name: "topPlate", extrude: topHeight },
      { name: "caseWalls", extrude: topInset, operation: "subtract" },
      {
        name: "screwHoles",
        extrude: screwHeadHeight,
        operation: "subtract",
      },
    ],

    // Mock PCB
    pcb: [
      { name: "board", extrude: pcbHeight },
      { name: "_standoffsInner", extrude: pcbHeight, operation: "subtract" },
    ],
  },
};
