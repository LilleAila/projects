function topPlate_extrude_2_2_outline_fn(){
    return new CSG.Path2D([[85.5,-116.5],[85.5,-66.5]]).appendPoint([348.5,-66.5]).appendPoint([348.5,-116.5]).appendPoint([277,-116.5]).appendPoint([277,-150.5]).appendPoint([157,-150.5]).appendPoint([157,-116.5]).appendPoint([85.5,-116.5]).close().innerToCAG()
.subtract(
    new CSG.Path2D([[254.85,-141.15],[269.15,-141.15]]).appendPoint([269.15,-126.85]).appendPoint([254.85,-126.85]).appendPoint([254.85,-141.15]).close().innerToCAG()
.union(
    new CSG.Path2D([[254.85,-90.15],[269.15,-90.15]]).appendPoint([269.15,-75.85]).appendPoint([254.85,-75.85]).appendPoint([254.85,-90.15]).close().innerToCAG()
).union(
    new CSG.Path2D([[254.85,-107.15],[269.15,-107.15]]).appendPoint([269.15,-92.85]).appendPoint([254.85,-92.85]).appendPoint([254.85,-107.15]).close().innerToCAG()
).union(
    new CSG.Path2D([[110.85,-90.15],[125.15,-90.15]]).appendPoint([125.15,-75.85]).appendPoint([110.85,-75.85]).appendPoint([110.85,-90.15]).close().innerToCAG()
).union(
    new CSG.Path2D([[110.85,-107.15],[125.15,-107.15]]).appendPoint([125.15,-92.85]).appendPoint([110.85,-92.85]).appendPoint([110.85,-107.15]).close().innerToCAG()
).union(
    new CSG.Path2D([[92.85,-90.15],[107.15,-90.15]]).appendPoint([107.15,-75.85]).appendPoint([92.85,-75.85]).appendPoint([92.85,-90.15]).close().innerToCAG()
).union(
    new CSG.Path2D([[92.85,-107.15],[107.15,-107.15]]).appendPoint([107.15,-92.85]).appendPoint([92.85,-92.85]).appendPoint([92.85,-107.15]).close().innerToCAG()
).union(
    new CSG.Path2D([[236.85,-141.15],[251.15,-141.15]]).appendPoint([251.15,-126.85]).appendPoint([236.85,-126.85]).appendPoint([236.85,-141.15]).close().innerToCAG()
).union(
    new CSG.Path2D([[182.85,-141.15],[197.15,-141.15]]).appendPoint([197.15,-126.85]).appendPoint([182.85,-126.85]).appendPoint([182.85,-141.15]).close().innerToCAG()
).union(
    new CSG.Path2D([[164.85,-141.15],[179.15,-141.15]]).appendPoint([179.15,-126.85]).appendPoint([164.85,-126.85]).appendPoint([164.85,-141.15]).close().innerToCAG()
).union(
    new CSG.Path2D([[326.85,-90.15],[341.15,-90.15]]).appendPoint([341.15,-75.85]).appendPoint([326.85,-75.85]).appendPoint([326.85,-90.15]).close().innerToCAG()
).union(
    new CSG.Path2D([[326.85,-107.15],[341.15,-107.15]]).appendPoint([341.15,-92.85]).appendPoint([326.85,-92.85]).appendPoint([326.85,-107.15]).close().innerToCAG()
).union(
    new CSG.Path2D([[308.85,-90.15],[323.15,-90.15]]).appendPoint([323.15,-75.85]).appendPoint([308.85,-75.85]).appendPoint([308.85,-90.15]).close().innerToCAG()
).union(
    new CSG.Path2D([[308.85,-107.15],[323.15,-107.15]]).appendPoint([323.15,-92.85]).appendPoint([308.85,-92.85]).appendPoint([308.85,-107.15]).close().innerToCAG()
).union(
    new CSG.Path2D([[290.85,-90.15],[305.15,-90.15]]).appendPoint([305.15,-75.85]).appendPoint([290.85,-75.85]).appendPoint([290.85,-90.15]).close().innerToCAG()
).union(
    new CSG.Path2D([[290.85,-107.15],[305.15,-107.15]]).appendPoint([305.15,-92.85]).appendPoint([290.85,-92.85]).appendPoint([290.85,-107.15]).close().innerToCAG()
).union(
    new CSG.Path2D([[272.85,-90.15],[287.15,-90.15]]).appendPoint([287.15,-75.85]).appendPoint([272.85,-75.85]).appendPoint([272.85,-90.15]).close().innerToCAG()
).union(
    new CSG.Path2D([[272.85,-107.15],[287.15,-107.15]]).appendPoint([287.15,-92.85]).appendPoint([272.85,-92.85]).appendPoint([272.85,-107.15]).close().innerToCAG()
).union(
    new CSG.Path2D([[236.85,-90.15],[251.15,-90.15]]).appendPoint([251.15,-75.85]).appendPoint([236.85,-75.85]).appendPoint([236.85,-90.15]).close().innerToCAG()
).union(
    new CSG.Path2D([[236.85,-107.15],[251.15,-107.15]]).appendPoint([251.15,-92.85]).appendPoint([236.85,-92.85]).appendPoint([236.85,-107.15]).close().innerToCAG()
).union(
    new CSG.Path2D([[182.85,-90.15],[197.15,-90.15]]).appendPoint([197.15,-75.85]).appendPoint([182.85,-75.85]).appendPoint([182.85,-90.15]).close().innerToCAG()
).union(
    new CSG.Path2D([[182.85,-107.15],[197.15,-107.15]]).appendPoint([197.15,-92.85]).appendPoint([182.85,-92.85]).appendPoint([182.85,-107.15]).close().innerToCAG()
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
)).extrude({ offset: [0, 0, 2.2] });
}


function screwHoles_extrude_1_6_outline_fn(){
    return CAG.circle({"center":[217,-134],"radius":2})
.union(
    CAG.circle({"center":[253,-125.5],"radius":2})
).union(
    CAG.circle({"center":[235,-117],"radius":2})
).union(
    CAG.circle({"center":[235,-91.5],"radius":2})
).union(
    CAG.circle({"center":[253,-108.5],"radius":2})
).union(
    CAG.circle({"center":[253,-91.5],"radius":2})
).union(
    CAG.circle({"center":[325,-91.5],"radius":2})
).union(
    CAG.circle({"center":[181,-125.5],"radius":2})
).union(
    CAG.circle({"center":[199,-117],"radius":2})
).union(
    CAG.circle({"center":[199,-91.5],"radius":2})
).union(
    CAG.circle({"center":[181,-108.5],"radius":2})
).union(
    CAG.circle({"center":[181,-91.5],"radius":2})
).union(
    CAG.circle({"center":[109,-91.5],"radius":2})
).extrude({ offset: [0, 0, 1.6] });
}


function microcontroller_extrude_3_8000000000000003_outline_fn(){
    return new CSG.Path2D([[199,-108.5],[199,-68.5]]).appendPoint([235,-68.5]).appendPoint([235,-108.5]).appendPoint([199,-108.5]).close().innerToCAG()
.extrude({ offset: [0, 0, 3.8000000000000003] });
}


function outerCase_extrude_1_6_outline_fn(){
    return new CSG.Path2D([[85.5,-116.5],[85.5,-66.5]]).appendPoint([348.5,-66.5]).appendPoint([348.5,-116.5]).appendPoint([277,-116.5]).appendPoint([277,-150.5]).appendPoint([157,-150.5]).appendPoint([157,-116.5]).appendPoint([85.5,-116.5]).close().innerToCAG()
.extrude({ offset: [0, 0, 1.6] });
}


function innerCase_extrude_1_6_outline_fn(){
    return new CSG.Path2D([[87.5,-112.5],[87.5,-70.5]]).appendPoint([346.5,-70.5]).appendPoint([346.5,-112.5]).appendPoint([275,-112.5]).appendPoint([275,-146.5]).appendPoint([159,-146.5]).appendPoint([159,-112.5]).appendPoint([87.5,-112.5]).close().innerToCAG()
.extrude({ offset: [0, 0, 1.6] });
}




                function _topWall_case_fn() {
                    

                // creating part 0 of case _topWall
                let _topWall__part_0 = outerCase_extrude_1_6_outline_fn();

                // make sure that rotations are relative
                let _topWall__part_0_bounds = _topWall__part_0.getBounds();
                let _topWall__part_0_x = _topWall__part_0_bounds[0].x + (_topWall__part_0_bounds[1].x - _topWall__part_0_bounds[0].x) / 2
                let _topWall__part_0_y = _topWall__part_0_bounds[0].y + (_topWall__part_0_bounds[1].y - _topWall__part_0_bounds[0].y) / 2
                _topWall__part_0 = translate([-_topWall__part_0_x, -_topWall__part_0_y, 0], _topWall__part_0);
                _topWall__part_0 = rotate([0,0,0], _topWall__part_0);
                _topWall__part_0 = translate([_topWall__part_0_x, _topWall__part_0_y, 0], _topWall__part_0);

                _topWall__part_0 = translate([0,0,0], _topWall__part_0);
                let result = _topWall__part_0;
                
            

                // creating part 1 of case _topWall
                let _topWall__part_1 = innerCase_extrude_1_6_outline_fn();

                // make sure that rotations are relative
                let _topWall__part_1_bounds = _topWall__part_1.getBounds();
                let _topWall__part_1_x = _topWall__part_1_bounds[0].x + (_topWall__part_1_bounds[1].x - _topWall__part_1_bounds[0].x) / 2
                let _topWall__part_1_y = _topWall__part_1_bounds[0].y + (_topWall__part_1_bounds[1].y - _topWall__part_1_bounds[0].y) / 2
                _topWall__part_1 = translate([-_topWall__part_1_x, -_topWall__part_1_y, 0], _topWall__part_1);
                _topWall__part_1 = rotate([0,0,0], _topWall__part_1);
                _topWall__part_1 = translate([_topWall__part_1_x, _topWall__part_1_y, 0], _topWall__part_1);

                _topWall__part_1 = translate([0,0,0], _topWall__part_1);
                result = result.subtract(_topWall__part_1);
                
            
                    return result;
                }
            
            

                function top_case_fn() {
                    

                // creating part 0 of case top
                let top__part_0 = topPlate_extrude_2_2_outline_fn();

                // make sure that rotations are relative
                let top__part_0_bounds = top__part_0.getBounds();
                let top__part_0_x = top__part_0_bounds[0].x + (top__part_0_bounds[1].x - top__part_0_bounds[0].x) / 2
                let top__part_0_y = top__part_0_bounds[0].y + (top__part_0_bounds[1].y - top__part_0_bounds[0].y) / 2
                top__part_0 = translate([-top__part_0_x, -top__part_0_y, 0], top__part_0);
                top__part_0 = rotate([0,0,0], top__part_0);
                top__part_0 = translate([top__part_0_x, top__part_0_y, 0], top__part_0);

                top__part_0 = translate([0,0,1.6], top__part_0);
                let result = top__part_0;
                
            

                // creating part 1 of case top
                let top__part_1 = screwHoles_extrude_1_6_outline_fn();

                // make sure that rotations are relative
                let top__part_1_bounds = top__part_1.getBounds();
                let top__part_1_x = top__part_1_bounds[0].x + (top__part_1_bounds[1].x - top__part_1_bounds[0].x) / 2
                let top__part_1_y = top__part_1_bounds[0].y + (top__part_1_bounds[1].y - top__part_1_bounds[0].y) / 2
                top__part_1 = translate([-top__part_1_x, -top__part_1_y, 0], top__part_1);
                top__part_1 = rotate([0,0,0], top__part_1);
                top__part_1 = translate([top__part_1_x, top__part_1_y, 0], top__part_1);

                top__part_1 = translate([0,0,1.6], top__part_1);
                result = result.subtract(top__part_1);
                
            

                // creating part 2 of case top
                let top__part_2 = _topWall_case_fn();

                // make sure that rotations are relative
                let top__part_2_bounds = top__part_2.getBounds();
                let top__part_2_x = top__part_2_bounds[0].x + (top__part_2_bounds[1].x - top__part_2_bounds[0].x) / 2
                let top__part_2_y = top__part_2_bounds[0].y + (top__part_2_bounds[1].y - top__part_2_bounds[0].y) / 2
                top__part_2 = translate([-top__part_2_x, -top__part_2_y, 0], top__part_2);
                top__part_2 = rotate([0,0,0], top__part_2);
                top__part_2 = translate([top__part_2_x, top__part_2_y, 0], top__part_2);

                top__part_2 = translate([0,0,0], top__part_2);
                result = result.union(top__part_2);
                
            

                // creating part 3 of case top
                let top__part_3 = microcontroller_extrude_3_8000000000000003_outline_fn();

                // make sure that rotations are relative
                let top__part_3_bounds = top__part_3.getBounds();
                let top__part_3_x = top__part_3_bounds[0].x + (top__part_3_bounds[1].x - top__part_3_bounds[0].x) / 2
                let top__part_3_y = top__part_3_bounds[0].y + (top__part_3_bounds[1].y - top__part_3_bounds[0].y) / 2
                top__part_3 = translate([-top__part_3_x, -top__part_3_y, 0], top__part_3);
                top__part_3 = rotate([0,0,0], top__part_3);
                top__part_3 = translate([top__part_3_x, top__part_3_y, 0], top__part_3);

                top__part_3 = translate([0,0,0], top__part_3);
                result = result.subtract(top__part_3);
                
            
                    return result;
                }
            
            
        
            function main() {
                return top_case_fn();
            }

        