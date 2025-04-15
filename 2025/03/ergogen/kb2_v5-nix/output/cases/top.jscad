function topPlate_extrude_2_2_outline_fn(){
    return new CSG.Path2D([[139.5,-114.5],[139.5,-107.15]]).appendPoint([143.15,-107.15]).appendPoint([143.15,-92.85]).appendPoint([139.5,-92.85]).appendPoint([139.5,-90.15]).appendPoint([143.15,-90.15]).appendPoint([143.15,-75.85]).appendPoint([139.5,-75.85]).appendPoint([139.5,-68.5]).appendPoint([237.5,-68.5]).appendPoint([237.5,-75.85]).appendPoint([233.85,-75.85]).appendPoint([233.85,-90.15]).appendPoint([237.5,-90.15]).appendPoint([237.5,-92.85]).appendPoint([233.85,-92.85]).appendPoint([233.85,-107.15]).appendPoint([237.5,-107.15]).appendPoint([237.5,-114.5]).appendPoint([202,-114.5]).appendPoint([202,-148.5]).appendPoint([157,-148.5]).appendPoint([153.1088235,-141.15]).appendPoint([161.15,-141.15]).appendPoint([161.15,-126.85]).appendPoint([146.85,-126.85]).appendPoint([146.85,-129.3277778]).appendPoint([139,-114.5]).appendPoint([139.5,-114.5]).close().innerToCAG()
.subtract(
    new CSG.Path2D([[179.85,-141.15],[194.15,-141.15]]).appendPoint([194.15,-126.85]).appendPoint([179.85,-126.85]).appendPoint([179.85,-141.15]).close().innerToCAG()
.union(
    new CSG.Path2D([[215.85,-90.15],[230.15,-90.15]]).appendPoint([230.15,-75.85]).appendPoint([215.85,-75.85]).appendPoint([215.85,-90.15]).close().innerToCAG()
).union(
    new CSG.Path2D([[215.85,-107.15],[230.15,-107.15]]).appendPoint([230.15,-92.85]).appendPoint([215.85,-92.85]).appendPoint([215.85,-107.15]).close().innerToCAG()
).union(
    new CSG.Path2D([[197.85,-90.15],[212.15,-90.15]]).appendPoint([212.15,-75.85]).appendPoint([197.85,-75.85]).appendPoint([197.85,-90.15]).close().innerToCAG()
).union(
    new CSG.Path2D([[197.85,-107.15],[212.15,-107.15]]).appendPoint([212.15,-92.85]).appendPoint([197.85,-92.85]).appendPoint([197.85,-107.15]).close().innerToCAG()
).union(
    new CSG.Path2D([[146.85,-90.15],[161.15,-90.15]]).appendPoint([161.15,-75.85]).appendPoint([146.85,-75.85]).appendPoint([146.85,-90.15]).close().innerToCAG()
).union(
    new CSG.Path2D([[146.85,-107.15],[161.15,-107.15]]).appendPoint([161.15,-92.85]).appendPoint([146.85,-92.85]).appendPoint([146.85,-107.15]).close().innerToCAG()
).union(
    new CSG.Path2D([[161.85,-141.15],[179.15,-141.15]]).appendPoint([179.15,-126.85]).appendPoint([161.85,-126.85]).appendPoint([161.85,-141.15]).close().innerToCAG()
).union(
    new CSG.Path2D([[179.85,-90.15],[197.15,-90.15]]).appendPoint([197.15,-75.85]).appendPoint([179.85,-75.85]).appendPoint([179.85,-90.15]).close().innerToCAG()
).union(
    new CSG.Path2D([[179.85,-107.15],[197.15,-107.15]]).appendPoint([197.15,-92.85]).appendPoint([179.85,-92.85]).appendPoint([179.85,-107.15]).close().innerToCAG()
).union(
    new CSG.Path2D([[161.85,-90.15],[179.15,-90.15]]).appendPoint([179.15,-75.85]).appendPoint([161.85,-75.85]).appendPoint([161.85,-90.15]).close().innerToCAG()
).union(
    new CSG.Path2D([[161.85,-107.15],[179.15,-107.15]]).appendPoint([179.15,-92.85]).appendPoint([161.85,-92.85]).appendPoint([161.85,-107.15]).close().innerToCAG()
)).extrude({ offset: [0, 0, 2.2] });
}


function screwHoles_extrude_1_6_outline_fn(){
    return CAG.circle({"center":[171.5,-134],"radius":2})
.union(
    CAG.circle({"center":[151.5,-134],"radius":2})
).union(
    CAG.circle({"center":[178,-117],"radius":2})
).union(
    CAG.circle({"center":[136,-117],"radius":2})
).union(
    CAG.circle({"center":[214,-92],"radius":2})
).union(
    CAG.circle({"center":[196,-92],"radius":2})
).union(
    CAG.circle({"center":[109,-92],"radius":2})
).union(
    CAG.circle({"center":[163,-92],"radius":2})
).extrude({ offset: [0, 0, 1.6] });
}


function microcontroller_extrude_3_8000000000000003_outline_fn(){
    return new CSG.Path2D([[127,-108.5],[127,-68.5]]).appendPoint([178,-68.5]).appendPoint([178,-108.5]).appendPoint([127,-108.5]).close().innerToCAG()
.extrude({ offset: [0, 0, 3.8000000000000003] });
}


function outerCase_extrude_1_6_outline_fn(){
    return new CSG.Path2D([[139.5,-114.5],[139.5,-68.5]]).appendPoint([237.5,-68.5]).appendPoint([237.5,-114.5]).appendPoint([202,-114.5]).appendPoint([202,-148.5]).appendPoint([157,-148.5]).appendPoint([139,-114.5]).appendPoint([139.5,-114.5]).close().innerToCAG()
.extrude({ offset: [0, 0, 1.6] });
}


function innerCase_extrude_1_6_outline_fn(){
    return new CSG.Path2D([[141.5,-112.5],[141.5,-70.5]]).appendPoint([235.5,-70.5]).appendPoint([235.5,-112.5]).appendPoint([200,-112.5]).appendPoint([200,-146.5]).appendPoint([159,-146.5]).appendPoint([141,-112.5]).appendPoint([141.5,-112.5]).close().innerToCAG()
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

        