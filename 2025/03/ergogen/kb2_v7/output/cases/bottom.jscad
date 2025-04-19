function outerCase_extrude_1_outline_fn(){
    return new CSG.Path2D([[86.5,-110],[86.5,-73]]).appendArc([89.5,-70],{"radius":3,"clockwise":true,"large":false}).appendPoint([344.5,-70]).appendArc([347.5,-73],{"radius":3,"clockwise":true,"large":false}).appendPoint([347.5,-110]).appendArc([344.5,-113],{"radius":3,"clockwise":true,"large":false}).appendPoint([278.5,-113]).appendArc([275.5,-116],{"radius":3,"clockwise":false,"large":false}).appendPoint([275.5,-144]).appendArc([272.5,-147],{"radius":3,"clockwise":true,"large":false}).appendPoint([161.5,-147]).appendArc([158.5,-144],{"radius":3,"clockwise":true,"large":false}).appendPoint([158.5,-116]).appendArc([155.5,-113],{"radius":3,"clockwise":false,"large":false}).appendPoint([89.5,-113]).appendArc([86.5,-110],{"radius":3,"clockwise":true,"large":false}).close().innerToCAG()
.extrude({ offset: [0, 0, 1] });
}


function caseWalls_extrude_5_outline_fn(){
    return new CSG.Path2D([[86.5,-110],[86.5,-73]]).appendArc([89.5,-70],{"radius":3,"clockwise":true,"large":false}).appendPoint([344.5,-70]).appendArc([347.5,-73],{"radius":3,"clockwise":true,"large":false}).appendPoint([347.5,-110]).appendArc([344.5,-113],{"radius":3,"clockwise":true,"large":false}).appendPoint([278.5,-113]).appendArc([275.5,-116],{"radius":3,"clockwise":false,"large":false}).appendPoint([275.5,-144]).appendArc([272.5,-147],{"radius":3,"clockwise":true,"large":false}).appendPoint([161.5,-147]).appendArc([158.5,-144],{"radius":3,"clockwise":true,"large":false}).appendPoint([158.5,-116]).appendArc([155.5,-113],{"radius":3,"clockwise":false,"large":false}).appendPoint([89.5,-113]).appendArc([86.5,-110],{"radius":3,"clockwise":true,"large":false}).close().innerToCAG()
.subtract(
    new CSG.Path2D([[88.5,-108],[88.5,-75]]).appendArc([91.5,-72],{"radius":3,"clockwise":true,"large":false}).appendPoint([342.5,-72]).appendArc([345.5,-75],{"radius":3,"clockwise":true,"large":false}).appendPoint([345.5,-108]).appendArc([342.5,-111],{"radius":3,"clockwise":true,"large":false}).appendPoint([276.5,-111]).appendArc([273.5,-114],{"radius":3,"clockwise":false,"large":false}).appendPoint([273.5,-142]).appendArc([270.5,-145],{"radius":3,"clockwise":true,"large":false}).appendPoint([163.5,-145]).appendArc([160.5,-142],{"radius":3,"clockwise":true,"large":false}).appendPoint([160.5,-114]).appendArc([157.5,-111],{"radius":3,"clockwise":false,"large":false}).appendPoint([91.5,-111]).appendArc([88.5,-108],{"radius":3,"clockwise":true,"large":false}).close().innerToCAG()
).extrude({ offset: [0, 0, 5] });
}


function lip_extrude_3_outline_fn(){
    return new CSG.Path2D([[88.5,-108],[88.5,-75]]).appendArc([91.5,-72],{"radius":3,"clockwise":true,"large":false}).appendPoint([342.5,-72]).appendArc([345.5,-75],{"radius":3,"clockwise":true,"large":false}).appendPoint([345.5,-108]).appendArc([342.5,-111],{"radius":3,"clockwise":true,"large":false}).appendPoint([276.5,-111]).appendArc([273.5,-114],{"radius":3,"clockwise":false,"large":false}).appendPoint([273.5,-142]).appendArc([270.5,-145],{"radius":3,"clockwise":true,"large":false}).appendPoint([163.5,-145]).appendArc([160.5,-142],{"radius":3,"clockwise":true,"large":false}).appendPoint([160.5,-114]).appendArc([157.5,-111],{"radius":3,"clockwise":false,"large":false}).appendPoint([91.5,-111]).appendArc([88.5,-108],{"radius":3,"clockwise":true,"large":false}).close().innerToCAG()
.subtract(
    new CSG.Path2D([[91,-105.5],[91,-77.5]]).appendArc([94,-74.5],{"radius":3,"clockwise":true,"large":false}).appendPoint([340,-74.5]).appendArc([343,-77.5],{"radius":3,"clockwise":true,"large":false}).appendPoint([343,-105.5]).appendArc([340,-108.5],{"radius":3,"clockwise":true,"large":false}).appendPoint([274,-108.5]).appendArc([271,-111.5],{"radius":3,"clockwise":false,"large":false}).appendPoint([271,-139.5]).appendArc([268,-142.5],{"radius":3,"clockwise":true,"large":false}).appendPoint([166,-142.5]).appendArc([163,-139.5],{"radius":3,"clockwise":true,"large":false}).appendPoint([163,-111.5]).appendArc([160,-108.5],{"radius":3,"clockwise":false,"large":false}).appendPoint([94,-108.5]).appendArc([91,-105.5],{"radius":3,"clockwise":true,"large":false}).close().innerToCAG()
).extrude({ offset: [0, 0, 3] });
}


function standoffs_extrude_3_outline_fn(){
    return CAG.circle({"center":[235,-117],"radius":2.9})
.subtract(
    CAG.circle({"center":[235,-117],"radius":1.65})
).union(
    CAG.circle({"center":[235,-91.5],"radius":2.9})
.subtract(
    CAG.circle({"center":[235,-91.5],"radius":1.65})
)).union(
    CAG.circle({"center":[325,-91.5],"radius":2.9})
.subtract(
    CAG.circle({"center":[325,-91.5],"radius":1.65})
)).union(
    CAG.circle({"center":[253,-125.5],"radius":2.9})
.subtract(
    CAG.circle({"center":[253,-125.5],"radius":1.65})
)).union(
    CAG.circle({"center":[181,-125.5],"radius":2.9})
.subtract(
    CAG.circle({"center":[181,-125.5],"radius":1.65})
)).union(
    CAG.circle({"center":[199,-117],"radius":2.9})
.subtract(
    CAG.circle({"center":[199,-117],"radius":1.65})
)).union(
    CAG.circle({"center":[199,-91.5],"radius":2.9})
.subtract(
    CAG.circle({"center":[199,-91.5],"radius":1.65})
)).union(
    CAG.circle({"center":[109,-91.5],"radius":2.9})
.subtract(
    CAG.circle({"center":[109,-91.5],"radius":1.65})
)).extrude({ offset: [0, 0, 3] });
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
                let bottom__part_1 = caseWalls_extrude_5_outline_fn();

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
                let bottom__part_2 = lip_extrude_3_outline_fn();

                // make sure that rotations are relative
                let bottom__part_2_bounds = bottom__part_2.getBounds();
                let bottom__part_2_x = bottom__part_2_bounds[0].x + (bottom__part_2_bounds[1].x - bottom__part_2_bounds[0].x) / 2
                let bottom__part_2_y = bottom__part_2_bounds[0].y + (bottom__part_2_bounds[1].y - bottom__part_2_bounds[0].y) / 2
                bottom__part_2 = translate([-bottom__part_2_x, -bottom__part_2_y, 0], bottom__part_2);
                bottom__part_2 = rotate([0,0,0], bottom__part_2);
                bottom__part_2 = translate([bottom__part_2_x, bottom__part_2_y, 0], bottom__part_2);

                bottom__part_2 = translate([0,0,1], bottom__part_2);
                result = result.union(bottom__part_2);
                
            

                // creating part 3 of case bottom
                let bottom__part_3 = standoffs_extrude_3_outline_fn();

                // make sure that rotations are relative
                let bottom__part_3_bounds = bottom__part_3.getBounds();
                let bottom__part_3_x = bottom__part_3_bounds[0].x + (bottom__part_3_bounds[1].x - bottom__part_3_bounds[0].x) / 2
                let bottom__part_3_y = bottom__part_3_bounds[0].y + (bottom__part_3_bounds[1].y - bottom__part_3_bounds[0].y) / 2
                bottom__part_3 = translate([-bottom__part_3_x, -bottom__part_3_y, 0], bottom__part_3);
                bottom__part_3 = rotate([0,0,0], bottom__part_3);
                bottom__part_3 = translate([bottom__part_3_x, bottom__part_3_y, 0], bottom__part_3);

                bottom__part_3 = translate([0,0,1], bottom__part_3);
                result = result.union(bottom__part_3);
                
            
                    return result;
                }
            
            
        
            function main() {
                return bottom_case_fn();
            }

        