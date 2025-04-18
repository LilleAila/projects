function outerCase_extrude_1_outline_fn(){
    return new CSG.Path2D([[85.5,-114.5],[85.5,-68.5]]).appendPoint([348.5,-68.5]).appendPoint([348.5,-114.5]).appendPoint([277,-114.5]).appendPoint([277,-148.5]).appendPoint([157,-148.5]).appendPoint([157,-114.5]).appendPoint([85.5,-114.5]).close().innerToCAG()
.extrude({ offset: [0, 0, 1] });
}


function standoffs_extrude_3_outline_fn(){
    return CAG.circle({"center":[217,-134],"radius":2.9})
.union(
    CAG.circle({"center":[253,-125.5],"radius":2.9})
).union(
    CAG.circle({"center":[235,-117],"radius":2.9})
).union(
    CAG.circle({"center":[235,-91.5],"radius":2.9})
).union(
    CAG.circle({"center":[253,-108.5],"radius":2.9})
).union(
    CAG.circle({"center":[253,-91.5],"radius":2.9})
).union(
    CAG.circle({"center":[325,-91.5],"radius":2.9})
).union(
    CAG.circle({"center":[181,-125.5],"radius":2.9})
).union(
    CAG.circle({"center":[199,-117],"radius":2.9})
).union(
    CAG.circle({"center":[199,-91.5],"radius":2.9})
).union(
    CAG.circle({"center":[181,-108.5],"radius":2.9})
).union(
    CAG.circle({"center":[181,-91.5],"radius":2.9})
).union(
    CAG.circle({"center":[109,-91.5],"radius":2.9})
).extrude({ offset: [0, 0, 3] });
}


function mountingHoles_extrude_3_outline_fn(){
    return CAG.circle({"center":[217,-134],"radius":1.65})
.union(
    CAG.circle({"center":[253,-125.5],"radius":1.65})
).union(
    CAG.circle({"center":[235,-117],"radius":1.65})
).union(
    CAG.circle({"center":[235,-91.5],"radius":1.65})
).union(
    CAG.circle({"center":[253,-108.5],"radius":1.65})
).union(
    CAG.circle({"center":[253,-91.5],"radius":1.65})
).union(
    CAG.circle({"center":[325,-91.5],"radius":1.65})
).union(
    CAG.circle({"center":[181,-125.5],"radius":1.65})
).union(
    CAG.circle({"center":[199,-117],"radius":1.65})
).union(
    CAG.circle({"center":[199,-91.5],"radius":1.65})
).union(
    CAG.circle({"center":[181,-108.5],"radius":1.65})
).union(
    CAG.circle({"center":[181,-91.5],"radius":1.65})
).union(
    CAG.circle({"center":[109,-91.5],"radius":1.65})
).extrude({ offset: [0, 0, 3] });
}


function outerCase_extrude_3_outline_fn(){
    return new CSG.Path2D([[85.5,-114.5],[85.5,-68.5]]).appendPoint([348.5,-68.5]).appendPoint([348.5,-114.5]).appendPoint([277,-114.5]).appendPoint([277,-148.5]).appendPoint([157,-148.5]).appendPoint([157,-114.5]).appendPoint([85.5,-114.5]).close().innerToCAG()
.extrude({ offset: [0, 0, 3] });
}


function innerCase_extrude_3_outline_fn(){
    return new CSG.Path2D([[87.5,-112.5],[87.5,-70.5]]).appendPoint([346.5,-70.5]).appendPoint([346.5,-112.5]).appendPoint([275,-112.5]).appendPoint([275,-146.5]).appendPoint([159,-146.5]).appendPoint([159,-112.5]).appendPoint([87.5,-112.5]).close().innerToCAG()
.extrude({ offset: [0, 0, 3] });
}




                function _standoffs_case_fn() {
                    

                // creating part 0 of case _standoffs
                let _standoffs__part_0 = standoffs_extrude_3_outline_fn();

                // make sure that rotations are relative
                let _standoffs__part_0_bounds = _standoffs__part_0.getBounds();
                let _standoffs__part_0_x = _standoffs__part_0_bounds[0].x + (_standoffs__part_0_bounds[1].x - _standoffs__part_0_bounds[0].x) / 2
                let _standoffs__part_0_y = _standoffs__part_0_bounds[0].y + (_standoffs__part_0_bounds[1].y - _standoffs__part_0_bounds[0].y) / 2
                _standoffs__part_0 = translate([-_standoffs__part_0_x, -_standoffs__part_0_y, 0], _standoffs__part_0);
                _standoffs__part_0 = rotate([0,0,0], _standoffs__part_0);
                _standoffs__part_0 = translate([_standoffs__part_0_x, _standoffs__part_0_y, 0], _standoffs__part_0);

                _standoffs__part_0 = translate([0,0,0], _standoffs__part_0);
                let result = _standoffs__part_0;
                
            

                // creating part 1 of case _standoffs
                let _standoffs__part_1 = mountingHoles_extrude_3_outline_fn();

                // make sure that rotations are relative
                let _standoffs__part_1_bounds = _standoffs__part_1.getBounds();
                let _standoffs__part_1_x = _standoffs__part_1_bounds[0].x + (_standoffs__part_1_bounds[1].x - _standoffs__part_1_bounds[0].x) / 2
                let _standoffs__part_1_y = _standoffs__part_1_bounds[0].y + (_standoffs__part_1_bounds[1].y - _standoffs__part_1_bounds[0].y) / 2
                _standoffs__part_1 = translate([-_standoffs__part_1_x, -_standoffs__part_1_y, 0], _standoffs__part_1);
                _standoffs__part_1 = rotate([0,0,0], _standoffs__part_1);
                _standoffs__part_1 = translate([_standoffs__part_1_x, _standoffs__part_1_y, 0], _standoffs__part_1);

                _standoffs__part_1 = translate([0,0,0], _standoffs__part_1);
                result = result.subtract(_standoffs__part_1);
                
            
                    return result;
                }
            
            

                function _bottomWall_case_fn() {
                    

                // creating part 0 of case _bottomWall
                let _bottomWall__part_0 = outerCase_extrude_3_outline_fn();

                // make sure that rotations are relative
                let _bottomWall__part_0_bounds = _bottomWall__part_0.getBounds();
                let _bottomWall__part_0_x = _bottomWall__part_0_bounds[0].x + (_bottomWall__part_0_bounds[1].x - _bottomWall__part_0_bounds[0].x) / 2
                let _bottomWall__part_0_y = _bottomWall__part_0_bounds[0].y + (_bottomWall__part_0_bounds[1].y - _bottomWall__part_0_bounds[0].y) / 2
                _bottomWall__part_0 = translate([-_bottomWall__part_0_x, -_bottomWall__part_0_y, 0], _bottomWall__part_0);
                _bottomWall__part_0 = rotate([0,0,0], _bottomWall__part_0);
                _bottomWall__part_0 = translate([_bottomWall__part_0_x, _bottomWall__part_0_y, 0], _bottomWall__part_0);

                _bottomWall__part_0 = translate([0,0,0], _bottomWall__part_0);
                let result = _bottomWall__part_0;
                
            

                // creating part 1 of case _bottomWall
                let _bottomWall__part_1 = innerCase_extrude_3_outline_fn();

                // make sure that rotations are relative
                let _bottomWall__part_1_bounds = _bottomWall__part_1.getBounds();
                let _bottomWall__part_1_x = _bottomWall__part_1_bounds[0].x + (_bottomWall__part_1_bounds[1].x - _bottomWall__part_1_bounds[0].x) / 2
                let _bottomWall__part_1_y = _bottomWall__part_1_bounds[0].y + (_bottomWall__part_1_bounds[1].y - _bottomWall__part_1_bounds[0].y) / 2
                _bottomWall__part_1 = translate([-_bottomWall__part_1_x, -_bottomWall__part_1_y, 0], _bottomWall__part_1);
                _bottomWall__part_1 = rotate([0,0,0], _bottomWall__part_1);
                _bottomWall__part_1 = translate([_bottomWall__part_1_x, _bottomWall__part_1_y, 0], _bottomWall__part_1);

                _bottomWall__part_1 = translate([0,0,0], _bottomWall__part_1);
                result = result.subtract(_bottomWall__part_1);
                
            
                    return result;
                }
            
            

                function bottom_case_fn() {
                    

                // creating part 0 of case bottom
                let bottom__part_0 = outerCase_extrude_1_outline_fn();

                // make sure that rotations are relative
                let bottom__part_0_bounds = bottom__part_0.getBounds();
                let bottom__part_0_x = bottom__part_0_bounds[0].x + (bottom__part_0_bounds[1].x - bottom__part_0_bounds[0].x) / 2
                let bottom__part_0_y = bottom__part_0_bounds[0].y + (bottom__part_0_bounds[1].y - bottom__part_0_bounds[0].y) / 2
                bottom__part_0 = translate([-bottom__part_0_x, -bottom__part_0_y, 0], bottom__part_0);
                bottom__part_0 = rotate([0,0,0], bottom__part_0);
                bottom__part_0 = translate([bottom__part_0_x, bottom__part_0_y, 0], bottom__part_0);

                bottom__part_0 = translate([0,0,0], bottom__part_0);
                let result = bottom__part_0;
                
            

                // creating part 1 of case bottom
                let bottom__part_1 = _standoffs_case_fn();

                // make sure that rotations are relative
                let bottom__part_1_bounds = bottom__part_1.getBounds();
                let bottom__part_1_x = bottom__part_1_bounds[0].x + (bottom__part_1_bounds[1].x - bottom__part_1_bounds[0].x) / 2
                let bottom__part_1_y = bottom__part_1_bounds[0].y + (bottom__part_1_bounds[1].y - bottom__part_1_bounds[0].y) / 2
                bottom__part_1 = translate([-bottom__part_1_x, -bottom__part_1_y, 0], bottom__part_1);
                bottom__part_1 = rotate([0,0,0], bottom__part_1);
                bottom__part_1 = translate([bottom__part_1_x, bottom__part_1_y, 0], bottom__part_1);

                bottom__part_1 = translate([0,0,1], bottom__part_1);
                result = result.union(bottom__part_1);
                
            

                // creating part 2 of case bottom
                let bottom__part_2 = _bottomWall_case_fn();

                // make sure that rotations are relative
                let bottom__part_2_bounds = bottom__part_2.getBounds();
                let bottom__part_2_x = bottom__part_2_bounds[0].x + (bottom__part_2_bounds[1].x - bottom__part_2_bounds[0].x) / 2
                let bottom__part_2_y = bottom__part_2_bounds[0].y + (bottom__part_2_bounds[1].y - bottom__part_2_bounds[0].y) / 2
                bottom__part_2 = translate([-bottom__part_2_x, -bottom__part_2_y, 0], bottom__part_2);
                bottom__part_2 = rotate([0,0,0], bottom__part_2);
                bottom__part_2 = translate([bottom__part_2_x, bottom__part_2_y, 0], bottom__part_2);

                bottom__part_2 = translate([0,0,1], bottom__part_2);
                result = result.union(bottom__part_2);
                
            
                    return result;
                }
            
            
        
            function main() {
                return bottom_case_fn();
            }

        