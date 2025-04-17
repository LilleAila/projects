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

// Case dimensions (compared to center of outer key)
const cix = 0.5 * ky + p * 2,
  ciy = 0.5 * ky + p * 2,
  cox = 0.5 * ky + p * 3,
  coy = 0.5 * ky + p * 3;

// Screw size (M2x4)
const screwHeadSize = 4 / 2,
  standoffInner = 3.3 / 2,
  standoffOuter = 5.8 / 2;

// Heights
const bottomHeight = 1,
  standoffHeight = 3,
  pcbHeight = 1.6,
  topHeight = 2.2,
  screwHeadHeight = 1.6;

const mkWhere = (ref, a, b) => ({ ref, shift: [a, b] });
const mkAgg = (a, b) => ({ ref: { aggregate: { parts: [a, b] } } });

const mountingLocations = [
  mkAgg("left_outer_ltop", "left_pinky_lbottom"),
  {
    ...mkAgg("left_inner_ltop", "left_inner_lbottom"),
    shift: [0.5 * kx, 0],
  },
  {
    ...mkAgg("left_index_ltop", "left_inner_ltop"),
    shift: [-0.5, -0.1 * ky],
  },
  {
    ...mkAgg("left_index_lbottom", "left_inner_lbottom"),
    shift: [-1, -0.4 * ky],
  },
  {
    ...mkAgg("left_inner_lbottom", "lthumbs_inner_lttop"),
    shift: [0.5 * kx, 0],
  },

  mkAgg("right_outer_rtop", "right_pinky_rbottom"),
  {
    ...mkAgg("right_inner_rtop", "right_inner_rbottom"),
    shift: [-0.5 * kx, 0],
  },
  {
    ...mkAgg("right_index_rtop", "right_inner_rtop"),
    shift: [-0.5, -0.1 * ky],
  },
  {
    ...mkAgg("right_index_rbottom", "right_inner_rbottom"),
    shift: [1, -0.4 * ky],
  },
  {
    ...mkAgg("right_inner_rbottom", "rthumbs_inner_rttop"),
    shift: [-0.5 * kx, 0],
  },

  {
    ...mkAgg("lthumbs_inner_lttop", "lthumbs_outer_lttop"),
    shift: [0, -0.1 * ky],
  },
  {
    ...mkAgg("rthumbs_inner_rttop", "rthumbs_outer_rttop"),
    shift: [0, -0.1 * ky],
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
          mkWhere("left_inner_ltop", 0.5 * kx, cox),
          mkWhere("right_inner_rtop", -0.5 * kx, cox),
          mkWhere("right_inner_rbottom", -0.5 * kx, -0.5 * ky),
          mkWhere("left_inner_lbottom", 0.5 * kx, -0.5 * ky),
        ],
      },
    ],

    board: [
      {
        what: "polygon",
        operation: "stack",
        points: [
          mkWhere("left_outer_ltop", -0.5 * px, 0.5 * py),
          mkWhere("right_outer_rtop", 0.5 * px, 0.5 * py),
          mkWhere("right_outer_rbottom", 0.5 * px, -0.5 * py),
          mkWhere("right_middle_rbottom", -0.5 * kx + p, -0.5 * py),
          mkWhere("rthumbs_outer_rttop", 0.5 * kx + p, -0.5 * py),
          mkWhere("lthumbs_outer_lttop", -0.5 * kx - p, -0.5 * py),
          mkWhere("left_middle_lbottom", 0.5 * kx - p, -0.5 * py),
          mkWhere("left_outer_lbottom", -0.5 * px, -0.5 * py),
        ],
      },
    ],

    outerCase: [
      {
        what: "polygon",
        operation: "stack",
        points: [
          mkWhere("left_outer_ltop", -cox, coy),
          mkWhere("right_outer_rtop", cox, coy),
          mkWhere("right_outer_rbottom", cox, -coy),
          mkWhere("right_middle_rbottom", -0.5 * kx + 3 * p, -coy),
          mkWhere("rthumbs_outer_rttop", 0.5 * kx + 3 * p, -coy),
          mkWhere("lthumbs_outer_lttop", -0.5 * kx - 3 * p, -coy),
          mkWhere("left_middle_lbottom", 0.5 * kx - 3 * p, -coy),
          mkWhere("left_outer_lbottom", -cox, -coy),
        ],
      },
    ],

    innerCase: [
      {
        what: "polygon",
        operation: "stack",
        points: [
          mkWhere("left_outer_ltop", -cix, ciy),
          mkWhere("right_outer_rtop", cix, ciy),
          mkWhere("right_outer_rbottom", cix, -ciy),
          mkWhere("right_middle_rbottom", -0.5 * kx + 2 * p, -ciy),
          mkWhere("rthumbs_outer_rttop", 0.5 * kx + 2 * p, -ciy),
          mkWhere("lthumbs_outer_lttop", -0.5 * kx - 2 * p, -ciy),
          mkWhere("left_middle_lbottom", 0.5 * kx - 2 * p, -ciy),
          mkWhere("left_outer_lbottom", -cix, -ciy),
        ],
      },
    ],

    topPlate: [
      { name: "outerCase" },
      { operation: "subtract", name: "switches" },
    ],

    mountingHoles: mountingLocations.map((where) => ({
      what: "circle",
      radius: standoffInner,
      where,
    })),

    standoffs: mountingLocations.map((where) => ({
      what: "circle",
      radius: standoffOuter,
      where,
    })),

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
    _bottomWall: [
      { name: "outerCase", extrude: standoffHeight },
      { name: "innerCase", extrude: standoffHeight, operation: "subtract" },
    ],

    _standoffs: [
      { name: "standoffs", extrude: standoffHeight },
      { name: "mountingHoles", extrude: standoffHeight, operation: "subtract" },
    ],

    bottom: [
      { name: "outerCase", extrude: bottomHeight },
      {
        what: "case",
        name: "_standoffs",
        operation: "add",
        shift: [0, 0, bottomHeight],
      },
      {
        what: "case",
        name: "_bottomWall",
        operation: "add",
        shift: [0, 0, bottomHeight],
      },
    ],

    // Top
    _topWall: [
      { name: "outerCase", extrude: pcbHeight },
      { name: "innerCase", extrude: pcbHeight, operation: "subtract" },
    ],

    top: [
      { name: "topPlate", extrude: topHeight, shift: [0, 0, pcbHeight] },
      {
        name: "screwHoles",
        extrude: screwHeadHeight,
        operation: "subtract",
        shift: [0, 0, pcbHeight],
      },
      { what: "case", name: "_topWall", operation: "add" },
      {
        name: "microcontroller",
        extrude: topHeight + pcbHeight,
        operation: "subtract",
      },
    ],

    // Mock PCB
    pcb: [
      { name: "board", extrude: pcbHeight },
      { name: "mountingHoles", extrude: pcbHeight, operation: "subtract" },
    ],
  },
};
