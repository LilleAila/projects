function topPlate_extrude_2_2_outline_fn(){
    return new CSG.Path2D([[86.5,-110],[86.5,-73]]).appendArc([89.5,-70],{"radius":3,"clockwise":true,"large":false}).appendPoint([197.85,-70]).appendArc([200.85,-73],{"radius":3,"clockwise":true,"large":false}).appendPoint([200.85,-105.5]).appendArc([203.85,-108.5],{"radius":3,"clockwise":false,"large":false}).appendPoint([230.15,-108.5]).appendArc([233.15,-105.5],{"radius":3,"clockwise":false,"large":false}).appendPoint([233.15,-73]).appendArc([236.15,-70],{"radius":3,"clockwise":true,"large":false}).appendPoint([344.5,-70]).appendArc([347.5,-73],{"radius":3,"clockwise":true,"large":false}).appendPoint([347.5,-110]).appendArc([344.5,-113],{"radius":3,"clockwise":true,"large":false}).appendPoint([278.5,-113]).appendArc([275.5,-116],{"radius":3,"clockwise":false,"large":false}).appendPoint([275.5,-144]).appendArc([272.5,-147],{"radius":3,"clockwise":true,"large":false}).appendPoint([161.5,-147]).appendArc([158.5,-144],{"radius":3,"clockwise":true,"large":false}).appendPoint([158.5,-116]).appendArc([155.5,-113],{"radius":3,"clockwise":false,"large":false}).appendPoint([89.5,-113]).appendArc([86.5,-110],{"radius":3,"clockwise":true,"large":false}).close().innerToCAG()
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


function caseWalls_extrude_0_4_outline_fn(){
    return new CSG.Path2D([[86.5,-110],[86.5,-73]]).appendArc([89.5,-70],{"radius":3,"clockwise":true,"large":false}).appendPoint([344.5,-70]).appendArc([347.5,-73],{"radius":3,"clockwise":true,"large":false}).appendPoint([347.5,-110]).appendArc([344.5,-113],{"radius":3,"clockwise":true,"large":false}).appendPoint([278.5,-113]).appendArc([275.5,-116],{"radius":3,"clockwise":false,"large":false}).appendPoint([275.5,-144]).appendArc([272.5,-147],{"radius":3,"clockwise":true,"large":false}).appendPoint([161.5,-147]).appendArc([158.5,-144],{"radius":3,"clockwise":true,"large":false}).appendPoint([158.5,-116]).appendArc([155.5,-113],{"radius":3,"clockwise":false,"large":false}).appendPoint([89.5,-113]).appendArc([86.5,-110],{"radius":3,"clockwise":true,"large":false}).close().innerToCAG()
.subtract(
    new CSG.Path2D([[88.5,-108],[88.5,-75]]).appendArc([91.5,-72],{"radius":3,"clockwise":true,"large":false}).appendPoint([342.5,-72]).appendArc([345.5,-75],{"radius":3,"clockwise":true,"large":false}).appendPoint([345.5,-108]).appendArc([342.5,-111],{"radius":3,"clockwise":true,"large":false}).appendPoint([276.5,-111]).appendArc([273.5,-114],{"radius":3,"clockwise":false,"large":false}).appendPoint([273.5,-142]).appendArc([270.5,-145],{"radius":3,"clockwise":true,"large":false}).appendPoint([163.5,-145]).appendArc([160.5,-142],{"radius":3,"clockwise":true,"large":false}).appendPoint([160.5,-114]).appendArc([157.5,-111],{"radius":3,"clockwise":false,"large":false}).appendPoint([91.5,-111]).appendArc([88.5,-108],{"radius":3,"clockwise":true,"large":false}).close().innerToCAG()
).extrude({ offset: [0, 0, 0.4] });
}


function screwHoles_extrude_1_6_outline_fn(){
    return CAG.circle({"center":[235,-117],"radius":2})
.union(
    CAG.circle({"center":[235,-91.5],"radius":2})
).union(
    CAG.circle({"center":[325,-91.5],"radius":2})
).union(
    CAG.circle({"center":[253,-125.5],"radius":2})
).union(
    CAG.circle({"center":[181,-125.5],"radius":2})
).union(
    CAG.circle({"center":[199,-117],"radius":2})
).union(
    CAG.circle({"center":[199,-91.5],"radius":2})
).union(
    CAG.circle({"center":[109,-91.5],"radius":2})
).extrude({ offset: [0, 0, 1.6] });
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

                top__part_0 = translate([0,0,0], top__part_0);
                let result = top__part_0;
                
            

                // creating part 1 of case top
                let top__part_1 = caseWalls_extrude_0_4_outline_fn();

                // make sure that rotations are relative
                let top__part_1_bounds = top__part_1.getBounds();
                let top__part_1_x = top__part_1_bounds[0].x + (top__part_1_bounds[1].x - top__part_1_bounds[0].x) / 2
                let top__part_1_y = top__part_1_bounds[0].y + (top__part_1_bounds[1].y - top__part_1_bounds[0].y) / 2
                top__part_1 = translate([-top__part_1_x, -top__part_1_y, 0], top__part_1);
                top__part_1 = rotate([0,0,0], top__part_1);
                top__part_1 = translate([top__part_1_x, top__part_1_y, 0], top__part_1);

                top__part_1 = translate([0,0,0], top__part_1);
                result = result.subtract(top__part_1);
                
            

                // creating part 2 of case top
                let top__part_2 = screwHoles_extrude_1_6_outline_fn();

                // make sure that rotations are relative
                let top__part_2_bounds = top__part_2.getBounds();
                let top__part_2_x = top__part_2_bounds[0].x + (top__part_2_bounds[1].x - top__part_2_bounds[0].x) / 2
                let top__part_2_y = top__part_2_bounds[0].y + (top__part_2_bounds[1].y - top__part_2_bounds[0].y) / 2
                top__part_2 = translate([-top__part_2_x, -top__part_2_y, 0], top__part_2);
                top__part_2 = rotate([0,0,0], top__part_2);
                top__part_2 = translate([top__part_2_x, top__part_2_y, 0], top__part_2);

                top__part_2 = translate([0,0,0], top__part_2);
                result = result.subtract(top__part_2);
                
            
                    return result;
                }
            
            
        
            function main() {
                return top_case_fn();
            }

        