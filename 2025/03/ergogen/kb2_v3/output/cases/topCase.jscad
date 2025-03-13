function standoff_extrude_4_outline_fn(){
    return CAG.circle({"center":[197.5,-139],"radius":2.5})
.union(
    CAG.circle({"center":[241,-117],"radius":2.5})
).union(
    CAG.circle({"center":[154,-117],"radius":2.5})
).union(
    CAG.circle({"center":[304,-92],"radius":2.5})
).union(
    CAG.circle({"center":[232,-92],"radius":2.5})
).union(
    CAG.circle({"center":[163,-92],"radius":2.5})
).union(
    CAG.circle({"center":[109,-92],"radius":2.5})
).extrude({ offset: [0, 0, 4] });
}


function mounting_extrude_4_outline_fn(){
    return CAG.circle({"center":[197.5,-139],"radius":1.5})
.union(
    CAG.circle({"center":[241,-117],"radius":1.5})
).union(
    CAG.circle({"center":[154,-117],"radius":1.5})
).union(
    CAG.circle({"center":[304,-92],"radius":1.5})
).union(
    CAG.circle({"center":[232,-92],"radius":1.5})
).union(
    CAG.circle({"center":[163,-92],"radius":1.5})
).union(
    CAG.circle({"center":[109,-92],"radius":1.5})
).extrude({ offset: [0, 0, 4] });
}


function topPlate_extrude_2_outline_fn(){
    return new CSG.Path2D([[87.5,-112.5],[87.5,-70.5]]).appendPoint([325.5,-70.5]).appendPoint([325.5,-112.5]).appendPoint([254,-112.5]).appendPoint([254,-146.5]).appendPoint([141,-146.5]).appendPoint([141,-112.5]).appendPoint([87.5,-112.5]).close().innerToCAG()
.subtract(
    new CSG.Path2D([[251.85,-90.15],[266.15,-90.15]]).appendPoint([266.15,-75.85]).appendPoint([251.85,-75.85]).appendPoint([251.85,-90.15]).close().innerToCAG()
.union(
    new CSG.Path2D([[251.85,-107.15],[266.15,-107.15]]).appendPoint([266.15,-92.85]).appendPoint([251.85,-92.85]).appendPoint([251.85,-107.15]).close().innerToCAG()
).union(
    new CSG.Path2D([[110.85,-90.15],[125.15,-90.15]]).appendPoint([125.15,-75.85]).appendPoint([110.85,-75.85]).appendPoint([110.85,-90.15]).close().innerToCAG()
).union(
    new CSG.Path2D([[110.85,-107.15],[125.15,-107.15]]).appendPoint([125.15,-92.85]).appendPoint([110.85,-92.85]).appendPoint([110.85,-107.15]).close().innerToCAG()
).union(
    new CSG.Path2D([[92.85,-90.15],[107.15,-90.15]]).appendPoint([107.15,-75.85]).appendPoint([92.85,-75.85]).appendPoint([92.85,-90.15]).close().innerToCAG()
).union(
    new CSG.Path2D([[92.85,-107.15],[107.15,-107.15]]).appendPoint([107.15,-92.85]).appendPoint([92.85,-92.85]).appendPoint([92.85,-107.15]).close().innerToCAG()
).union(
    new CSG.Path2D([[233.85,-141.15],[248.15,-141.15]]).appendPoint([248.15,-126.85]).appendPoint([233.85,-126.85]).appendPoint([233.85,-141.15]).close().innerToCAG()
).union(
    new CSG.Path2D([[215.85,-141.15],[230.15,-141.15]]).appendPoint([230.15,-126.85]).appendPoint([215.85,-126.85]).appendPoint([215.85,-141.15]).close().innerToCAG()
).union(
    new CSG.Path2D([[164.85,-141.15],[179.15,-141.15]]).appendPoint([179.15,-126.85]).appendPoint([164.85,-126.85]).appendPoint([164.85,-141.15]).close().innerToCAG()
).union(
    new CSG.Path2D([[146.85,-141.15],[161.15,-141.15]]).appendPoint([161.15,-126.85]).appendPoint([146.85,-126.85]).appendPoint([146.85,-141.15]).close().innerToCAG()
).union(
    new CSG.Path2D([[305.85,-90.15],[320.15,-90.15]]).appendPoint([320.15,-75.85]).appendPoint([305.85,-75.85]).appendPoint([305.85,-90.15]).close().innerToCAG()
).union(
    new CSG.Path2D([[305.85,-107.15],[320.15,-107.15]]).appendPoint([320.15,-92.85]).appendPoint([305.85,-92.85]).appendPoint([305.85,-107.15]).close().innerToCAG()
).union(
    new CSG.Path2D([[287.85,-90.15],[302.15,-90.15]]).appendPoint([302.15,-75.85]).appendPoint([287.85,-75.85]).appendPoint([287.85,-90.15]).close().innerToCAG()
).union(
    new CSG.Path2D([[287.85,-107.15],[302.15,-107.15]]).appendPoint([302.15,-92.85]).appendPoint([287.85,-92.85]).appendPoint([287.85,-107.15]).close().innerToCAG()
).union(
    new CSG.Path2D([[269.85,-90.15],[284.15,-90.15]]).appendPoint([284.15,-75.85]).appendPoint([269.85,-75.85]).appendPoint([269.85,-90.15]).close().innerToCAG()
).union(
    new CSG.Path2D([[269.85,-107.15],[284.15,-107.15]]).appendPoint([284.15,-92.85]).appendPoint([269.85,-92.85]).appendPoint([269.85,-107.15]).close().innerToCAG()
).union(
    new CSG.Path2D([[233.85,-90.15],[248.15,-90.15]]).appendPoint([248.15,-75.85]).appendPoint([233.85,-75.85]).appendPoint([233.85,-90.15]).close().innerToCAG()
).union(
    new CSG.Path2D([[233.85,-107.15],[248.15,-107.15]]).appendPoint([248.15,-92.85]).appendPoint([233.85,-92.85]).appendPoint([233.85,-107.15]).close().innerToCAG()
).union(
    new CSG.Path2D([[215.85,-90.15],[230.15,-90.15]]).appendPoint([230.15,-75.85]).appendPoint([215.85,-75.85]).appendPoint([215.85,-90.15]).close().innerToCAG()
).union(
    new CSG.Path2D([[215.85,-107.15],[230.15,-107.15]]).appendPoint([230.15,-92.85]).appendPoint([215.85,-92.85]).appendPoint([215.85,-107.15]).close().innerToCAG()
).union(
    new CSG.Path2D([[164.85,-90.15],[179.15,-90.15]]).appendPoint([179.15,-75.85]).appendPoint([164.85,-75.85]).appendPoint([164.85,-90.15]).close().innerToCAG()
).union(
    new CSG.Path2D([[164.85,-107.15],[179.15,-107.15]]).appendPoint([179.15,-92.85]).appendPoint([164.85,-92.85]).appendPoint([164.85,-107.15]).close().innerToCAG()
).union(
    new CSG.Path2D([[146.85,-90.15],[161.15,-90.15]]).appendPoint([161.15,-75.85]).appendPoint([146.85,-75.85]).appendPoint([146.85,-90.15]).close().innerToCAG()
).union(
    new CSG.Path2D([[146.85,-107.15],[161.15,-107.15]]).appendPoint([161.15,-92.85]).appendPoint([146.85,-92.85]).appendPoint([146.85,-107.15]).close().innerToCAG()
).union(
    new CSG.Path2D([[128.85,-90.15],[143.15,-90.15]]).appendPoint([143.15,-75.85]).appendPoint([128.85,-75.85]).appendPoint([128.85,-90.15]).close().innerToCAG()
).union(
    new CSG.Path2D([[128.85,-107.15],[143.15,-107.15]]).appendPoint([143.15,-92.85]).appendPoint([128.85,-92.85]).appendPoint([128.85,-107.15]).close().innerToCAG()
)).extrude({ offset: [0, 0, 2] });
}




                function _standoffs_case_fn() {
                    

                // creating part 0 of case _standoffs
                let _standoffs__part_0 = standoff_extrude_4_outline_fn();

                // make sure that rotations are relative
                let _standoffs__part_0_bounds = _standoffs__part_0.getBounds();
                let _standoffs__part_0_x = _standoffs__part_0_bounds[0].x + (_standoffs__part_0_bounds[1].x - _standoffs__part_0_bounds[0].x) / 2
                let _standoffs__part_0_y = _standoffs__part_0_bounds[0].y + (_standoffs__part_0_bounds[1].y - _standoffs__part_0_bounds[0].y) / 2
                _standoffs__part_0 = translate([-_standoffs__part_0_x, -_standoffs__part_0_y, 0], _standoffs__part_0);
                _standoffs__part_0 = rotate([0,0,0], _standoffs__part_0);
                _standoffs__part_0 = translate([_standoffs__part_0_x, _standoffs__part_0_y, 0], _standoffs__part_0);

                _standoffs__part_0 = translate([0,0,0], _standoffs__part_0);
                let result = _standoffs__part_0;
                
            
                    return result;
                }
            
            

                function _holes_case_fn() {
                    

                // creating part 0 of case _holes
                let _holes__part_0 = mounting_extrude_4_outline_fn();

                // make sure that rotations are relative
                let _holes__part_0_bounds = _holes__part_0.getBounds();
                let _holes__part_0_x = _holes__part_0_bounds[0].x + (_holes__part_0_bounds[1].x - _holes__part_0_bounds[0].x) / 2
                let _holes__part_0_y = _holes__part_0_bounds[0].y + (_holes__part_0_bounds[1].y - _holes__part_0_bounds[0].y) / 2
                _holes__part_0 = translate([-_holes__part_0_x, -_holes__part_0_y, 0], _holes__part_0);
                _holes__part_0 = rotate([0,0,0], _holes__part_0);
                _holes__part_0 = translate([_holes__part_0_x, _holes__part_0_y, 0], _holes__part_0);

                _holes__part_0 = translate([0,0,0], _holes__part_0);
                let result = _holes__part_0;
                
            
                    return result;
                }
            
            

                function _switches_case_fn() {
                    

                // creating part 0 of case _switches
                let _switches__part_0 = topPlate_extrude_2_outline_fn();

                // make sure that rotations are relative
                let _switches__part_0_bounds = _switches__part_0.getBounds();
                let _switches__part_0_x = _switches__part_0_bounds[0].x + (_switches__part_0_bounds[1].x - _switches__part_0_bounds[0].x) / 2
                let _switches__part_0_y = _switches__part_0_bounds[0].y + (_switches__part_0_bounds[1].y - _switches__part_0_bounds[0].y) / 2
                _switches__part_0 = translate([-_switches__part_0_x, -_switches__part_0_y, 0], _switches__part_0);
                _switches__part_0 = rotate([0,0,0], _switches__part_0);
                _switches__part_0 = translate([_switches__part_0_x, _switches__part_0_y, 0], _switches__part_0);

                _switches__part_0 = translate([0,0,0], _switches__part_0);
                let result = _switches__part_0;
                
            
                    return result;
                }
            
            

                function topCase_case_fn() {
                    

                // creating part 0 of case topCase
                let topCase__part_0 = _standoffs_case_fn();

                // make sure that rotations are relative
                let topCase__part_0_bounds = topCase__part_0.getBounds();
                let topCase__part_0_x = topCase__part_0_bounds[0].x + (topCase__part_0_bounds[1].x - topCase__part_0_bounds[0].x) / 2
                let topCase__part_0_y = topCase__part_0_bounds[0].y + (topCase__part_0_bounds[1].y - topCase__part_0_bounds[0].y) / 2
                topCase__part_0 = translate([-topCase__part_0_x, -topCase__part_0_y, 0], topCase__part_0);
                topCase__part_0 = rotate([0,0,0], topCase__part_0);
                topCase__part_0 = translate([topCase__part_0_x, topCase__part_0_y, 0], topCase__part_0);

                topCase__part_0 = translate([0,0,0], topCase__part_0);
                let result = topCase__part_0;
                
            

                // creating part 1 of case topCase
                let topCase__part_1 = _holes_case_fn();

                // make sure that rotations are relative
                let topCase__part_1_bounds = topCase__part_1.getBounds();
                let topCase__part_1_x = topCase__part_1_bounds[0].x + (topCase__part_1_bounds[1].x - topCase__part_1_bounds[0].x) / 2
                let topCase__part_1_y = topCase__part_1_bounds[0].y + (topCase__part_1_bounds[1].y - topCase__part_1_bounds[0].y) / 2
                topCase__part_1 = translate([-topCase__part_1_x, -topCase__part_1_y, 0], topCase__part_1);
                topCase__part_1 = rotate([0,0,0], topCase__part_1);
                topCase__part_1 = translate([topCase__part_1_x, topCase__part_1_y, 0], topCase__part_1);

                topCase__part_1 = translate([0,0,0], topCase__part_1);
                result = result.subtract(topCase__part_1);
                
            

                // creating part 2 of case topCase
                let topCase__part_2 = _switches_case_fn();

                // make sure that rotations are relative
                let topCase__part_2_bounds = topCase__part_2.getBounds();
                let topCase__part_2_x = topCase__part_2_bounds[0].x + (topCase__part_2_bounds[1].x - topCase__part_2_bounds[0].x) / 2
                let topCase__part_2_y = topCase__part_2_bounds[0].y + (topCase__part_2_bounds[1].y - topCase__part_2_bounds[0].y) / 2
                topCase__part_2 = translate([-topCase__part_2_x, -topCase__part_2_y, 0], topCase__part_2);
                topCase__part_2 = rotate([0,0,0], topCase__part_2);
                topCase__part_2 = translate([topCase__part_2_x, topCase__part_2_y, 0], topCase__part_2);

                topCase__part_2 = translate([0,0,0], topCase__part_2);
                result = result.union(topCase__part_2);
                
            
                    return result;
                }
            
            
        
            function main() {
                return topCase_case_fn();
            }

        