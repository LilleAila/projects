{
  pkgs ? import <nixpkgs> { },
}:
let
  inherit (pkgs) lib;
in
pkgs.writeText "config.yaml" (
  lib.generators.toYAML { } (
    let
      # Choc keys
      kx = 18;
      ky = 17;
      # Switch size (below the lip)
      # 13.8 + some padding
      sx = 14.3;
      sy = 14.3;
      sz = 2;
      # Padding
      p = 2;
      px = kx + p * 2;
      py = ky + p * 2;
      # Case outer and inner (distance from center of key)
      cix = 0.5 * ky + p * 2;
      ciy = 0.5 * ky + p * 2;
      cox = 0.5 * ky + p * 3;
      coy = 0.5 * ky + p * 3;
      # Screws (radius)
      screwHeadSize = 4 / 2;
      standoffInner = 3.3 / 2;
      standoffOuter = 5.8 / 2;
      # Other values
      bottomHeight = 1;
      standoffHeight = 3;
      pcbHeight = 1.6;
      topHeight = 2.2;
      screwHeadHeight = 1.6;

      mkWhere = ref: a: b: {
        inherit ref;
        shift = [
          a
          b
        ];
      };

      mkAgg = a: b: {
        ref.aggregate.parts = [
          a
          b
        ];
      };

      mountingLocations = [
        (mkWhere "left_outer_ltop" (0.5 * kx) (-0.5 * kx))
        (mkWhere "left_inner_ltop" (-0.5 * kx) (-0.5 * kx))
        (mkWhere "right_inner_rtop" (0.5 * kx) (-0.5 * kx))
        (mkWhere "right_outer_rtop" (-0.5 * kx) (-0.5 * kx))
        (mkAgg "left_index_lbottom" "lthumbs_outer_lttop")
        (mkAgg "right_index_rbottom" "rthumbs_outer_rttop")
        {
          ref.aggregate.parts = [
            "lthumbs_inner_lttop"
            "rthumbs_inner_rttop"
          ];
          shift = [
            (-10)
            0
          ];
        }
        {
          ref.aggregate.parts = [
            "lthumbs_inner_lttop"
            "rthumbs_inner_rttop"
          ];
          shift = [
            10
            0
          ];
        }
      ];

      circle = radius: where: {
        what = "circle";
        inherit radius where;
      };
    in
    {
      points = {
        zones = {
          left = {
            anchor = {
              shift = [
                100
                (-100)
              ];
            };
            key = {
              padding = 1 * ky;
              spread = 1 * kx;
            };
            columns = {
              outer = {
                key.column_net = "D23";
              };
              pinky = {
                key.column_net = "D20";
              };
              ring = {
                key.column_net = "D22";
              };
              middle = {
                key.column_net = "D26";
              };
              index = {
                key.column_net = "D27";
              };
              inner = {
                key.column_net = "D28";
              };
            };
            rows = {
              lbottom = {
                row_net = "D21";
              };
              ltop = {
                row_net = "D29";
              };
            };
          };
          right = {
            key = {
              padding = 1 * ky;
              spread = 1 * kx;
            };
            anchor = {
              ref = "left_inner_lbottom";
              shift = [
                (3 * ky)
                0
              ];
            };
            columns = {
              inner = {
                key.column_net = "D3";
              };
              index = {
                key.column_net = "D4";
              };
              middle = {
                key.column_net = "D5";
              };
              ring = {
                key.column_net = "D6";
              };
              pinky = {
                key.column_net = "D7";
              };
              outer = {
                key.column_net = "D8";
              };
            };
            rows = {
              rbottom = {
                row_net = "D9";
              };
              rtop = {
                row_net = "D2";
              };
            };
          };
          lthumbs = {
            key = {
              padding = 1 * ky;
              spread = 1 * kx;
            };
            anchor = {
              ref = "left_middle_lbottom";
              shift = [
                kx
                (-2 * ky)
              ];
            };
            columns = {
              outer = {
                key.column_net = "D27";
              };
              inner = {
                key.column_net = "D28";
              };
            };
            rows = {
              lttop = {
                row_net = "D1";
              };
            };
          };
          rthumbs = {
            key = {
              padding = 1 * ky;
              spread = 1 * kx;
            };
            anchor = {
              ref = "right_middle_rbottom";
              shift = [
                (-2 * kx)
                (-2 * ky)
              ];
            };
            columns = {
              inner = {
                key.column_net = "D3";
              };
              outer = {
                key.column_net = "D4";
              };
            };
            rows = {
              rttop = {
                row_net = "D1";
              };
            };
          };
        };
      };

      outlines = {
        switches = [
          {
            what = "rectangle";
            where = true;
            size = [
              sx
              sy
            ];
          }
        ];
        microcontroller = [
          {
            what = "polygon";
            operation = "stack";
            points = [
              (mkWhere "left_inner_ltop" (0.5 * kx) cox)
              (mkWhere "right_inner_rtop" (-0.5 * kx) cox)
              (mkWhere "right_inner_rbottom" (-0.5 * kx) (-0.5 * ky))
              (mkWhere "left_inner_lbottom" (0.5 * kx) (-0.5 * ky))
            ];
          }
        ];
        board = [
          {
            what = "polygon";
            operation = "stack";
            points = [
              (mkWhere "left_outer_ltop" (-0.5 * px) (0.5 * py))
              (mkWhere "right_outer_rtop" (0.5 * px) (0.5 * py))
              (mkWhere "right_outer_rbottom" (0.5 * px) (-0.5 * py))
              (mkWhere "right_middle_rbottom" (-0.5 * kx + p) (-0.5 * py))
              (mkWhere "rthumbs_outer_rttop" (0.5 * kx + p) (-0.5 * py))
              (mkWhere "lthumbs_outer_lttop" (-0.5 * kx - p) (-0.5 * py))
              (mkWhere "left_middle_lbottom" (0.5 * kx - p) (-0.5 * py))
              (mkWhere "left_outer_lbottom" (-0.5 * px) (-0.5 * py))
            ];
          }
        ];
        outerCase = [
          {
            what = "polygon";
            operation = "stack";
            points = [
              (mkWhere "left_outer_ltop" (-cox) coy)
              (mkWhere "right_outer_rtop" cox coy)
              (mkWhere "right_outer_rbottom" cox (-coy))
              (mkWhere "right_middle_rbottom" (-0.5 * kx + 3 * p) (-coy))
              (mkWhere "rthumbs_outer_rttop" (0.5 * kx + 3 * p) (-coy))
              (mkWhere "lthumbs_outer_lttop" (-0.5 * kx - 3 * p) (-coy))
              (mkWhere "left_middle_lbottom" (0.5 * kx - 3 * p) (-coy))
              (mkWhere "left_outer_lbottom" (-cox) (-coy))
            ];
          }
        ];
        innerCase = [
          {
            what = "polygon";
            operation = "stack";
            points = [
              (mkWhere "left_outer_ltop" (-cix) ciy)
              (mkWhere "right_outer_rtop" cix ciy)
              (mkWhere "right_outer_rbottom" cix (-ciy))
              (mkWhere "right_middle_rbottom" (-0.5 * kx + 2 * p) (-ciy))
              (mkWhere "rthumbs_outer_rttop" (0.5 * kx + 2 * p) (-ciy))
              (mkWhere "lthumbs_outer_lttop" (-0.5 * kx - 2 * p) (-ciy))
              (mkWhere "left_middle_lbottom" (0.5 * kx - 2 * p) (-ciy))
              (mkWhere "left_outer_lbottom" (-cix) (-ciy))
            ];
          }
        ];
        topPlate = [
          { name = "outerCase"; }
          {
            operation = "subtract";
            name = "switches";
          }
        ];

        mountingHoles = map (circle standoffInner) mountingLocations;
        standoffs = map (circle standoffOuter) mountingLocations;
        screwHoles = map (circle screwHeadSize) mountingLocations;
      };

      pcbs = {
        pcb = {
          outlines = {
            main = {
              outline = "board";
            };
          };
          footprints =
            {
              choc_hotswap = {
                what = "choc";
                where = true;
                params = {
                  keycaps = true;
                  reverse = false;
                  hotswap = true;
                  from = "{{column_net}}";
                  to = "{{colrow}}";
                };
              };
              diode = {
                what = "diode";
                where = true;
                params = {
                  from = "{{colrow}}";
                  to = "{{row_net}}";
                };
                adjust = {
                  shift = [
                    0
                    (-5)
                  ];
                };
              };
              promicro = {
                what = "frood";
                params = {
                  orientation = "down";
                };
                where = {
                  ref.aggregate.parts = [
                    "left_inner_ltop"
                    "right_inner_rtop"
                  ];
                  shift = [
                    0
                    (-7.5)
                  ];
                  rotate = -90;
                };
              };
              reset = {
                what = "button";
                params = {
                  from = "GND";
                  to = "RST";
                  side = "B";
                };
                where = {
                  ref.aggregate.parts = [
                    "left_inner_ltop"
                    "right_inner_rbottom"
                  ];
                  shift = [
                    0
                    0
                  ];
                };
              };
            }
            // builtins.listToAttrs (
              lib.imap0 (index: where: {
                name = "hole${toString index}";
                value = {
                  what = "mounthole";
                  inherit where;
                };
              }) mountingLocations
            );
        };
      };

      cases = {
        _bottomWall = [
          {
            name = "outerCase";
            extrude = standoffHeight;
          }
          {
            name = "innerCase";
            extrude = standoffHeight;
            operation = "subtract";
          }
        ];
        _standoffs = [
          {
            name = "standoffs";
            extrude = standoffHeight;
            operation = "add";
          }
          {
            name = "mountingHoles";
            extrude = standoffHeight;
            operation = "subtract";
          }
        ];
        bottom = [
          {
            name = "outerCase";
            extrude = bottomHeight;
          }
          {
            what = "case";
            name = "_standoffs";
            operation = "add";
            shift = [
              0
              0
              bottomHeight
            ];
          }
          {
            what = "case";
            name = "_bottomWall";
            operation = "add";
            shift = [
              0
              0
              bottomHeight
            ];
          }
        ];
        _topWall = [
          {
            name = "outerCase";
            extrude = pcbHeight;
          }
          {
            name = "innerCase";
            extrude = pcbHeight;
            operation = "subtract";
          }
        ];
        top = [
          {
            name = "topPlate";
            extrude = topHeight;
            shift = [
              0
              0
              pcbHeight
            ];
          }
          {
            name = "screwHoles";
            extrude = screwHeadHeight;
            operation = "subtract";
            shift = [
              0
              0
              pcbHeight
            ];
          }
          {
            what = "case";
            name = "_topWall";
            operation = "add";
          }
          {
            name = "microcontroller";
            extrude = topHeight + pcbHeight;
            operation = "subtract";
          }
        ];
        pcb = [
          {
            name = "board";
            extrude = pcbHeight;
          }
          {
            name = "mountingHoles";
            extrude = pcbHeight;
            operation = "subtract";
          }
        ];
      };
    }
  )
)
