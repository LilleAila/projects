function topPlate_extrude_2_2_outline_fn(){
    return new CSG.Path2D([[85.5,-114.5],[85.5,-68.5]]).appendPoint([345.5,-68.5]).appendPoint([345.5,-114.5]).appendPoint([274,-114.5]).appendPoint([274,-148.5]).appendPoint([157,-148.5]).appendPoint([157,-114.5]).appendPoint([85.5,-114.5]).close().innerToCAG()
.subtract(
    new CSG.Path2D([[251.85,-141.15],[266.15,-141.15]]).appendPoint([266.15,-126.85]).appendPoint([251.85,-126.85]).appendPoint([251.85,-141.15]).close().innerToCAG()
.union(
    new CSG.Path2D([[251.85,-90.15],[266.15,-90.15]]).appendPoint([266.15,-75.85]).appendPoint([251.85,-75.85]).appendPoint([251.85,-90.15]).close().innerToCAG()
).union(
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
    new CSG.Path2D([[182.85,-141.15],[197.15,-141.15]]).appendPoint([197.15,-126.85]).appendPoint([182.85,-126.85]).appendPoint([182.85,-141.15]).close().innerToCAG()
).union(
    new CSG.Path2D([[164.85,-141.15],[179.15,-141.15]]).appendPoint([179.15,-126.85]).appendPoint([164.85,-126.85]).appendPoint([164.85,-141.15]).close().innerToCAG()
).union(
    new CSG.Path2D([[323.85,-90.15],[338.15,-90.15]]).appendPoint([338.15,-75.85]).appendPoint([323.85,-75.85]).appendPoint([323.85,-90.15]).close().innerToCAG()
).union(
    new CSG.Path2D([[323.85,-107.15],[338.15,-107.15]]).appendPoint([338.15,-92.85]).appendPoint([323.85,-92.85]).appendPoint([323.85,-107.15]).close().innerToCAG()
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
    return CAG.circle({"center":[225.5,-134],"radius":2})
.union(
    CAG.circle({"center":[205.5,-134],"radius":2})
).union(
    CAG.circle({"center":[259,-117],"radius":2})
).union(
    CAG.circle({"center":[172,-117],"radius":2})
).union(
    CAG.circle({"center":[322,-92],"radius":2})
).union(
    CAG.circle({"center":[250,-92],"radius":2})
).union(
    CAG.circle({"center":[181,-92],"radius":2})
).union(
    CAG.circle({"center":[109,-92],"radius":2})
).extrude({ offset: [0, 0, 1.6] });
}


function microcontroller_extrude_3_8000000000000003_outline_fn(){
    return new CSG.Path2D([[199,-108.5],[199,-68.5]]).appendPoint([232,-68.5]).appendPoint([232,-108.5]).appendPoint([199,-108.5]).close().innerToCAG()
.extrude({ offset: [0, 0, 3.8000000000000003] });
}


function outerCase_extrude_1_6_outline_fn(){
    return new CSG.Path2D([[85.5,-114.5],[85.5,-68.5]]).appendPoint([345.5,-68.5]).appendPoint([345.5,-114.5]).appendPoint([274,-114.5]).appendPoint([274,-148.5]).appendPoint([157,-148.5]).appendPoint([157,-114.5]).appendPoint([85.5,-114.5]).close().innerToCAG()
.extrude({ offset: [0, 0, 1.6] });
}


function innerCase_extrude_1_6_outline_fn(){
    return new CSG.Path2D([[87.5,-112.5],[87.5,-70.5]]).appendPoint([343.5,-70.5]).appendPoint([343.5,-112.5]).appendPoint([272,-112.5]).appendPoint([272,-146.5]).appendPoint([159,-146.5]).appendPoint([159,-112.5]).appendPoint([87.5,-112.5]).close().innerToCAG()
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

        