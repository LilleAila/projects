function board_extrude_1_6_outline_fn(){
    return new CSG.Path2D([[89,-107.5],[89,-75.5]]).appendArc([92,-72.5],{"radius":3,"clockwise":true,"large":false}).appendPoint([342,-72.5]).appendArc([345,-75.5],{"radius":3,"clockwise":true,"large":false}).appendPoint([345,-107.5]).appendArc([342,-110.5],{"radius":3,"clockwise":true,"large":false}).appendPoint([276,-110.5]).appendArc([273,-113.5],{"radius":3,"clockwise":false,"large":false}).appendPoint([273,-141.5]).appendArc([270,-144.5],{"radius":3,"clockwise":true,"large":false}).appendPoint([164,-144.5]).appendArc([161,-141.5],{"radius":3,"clockwise":true,"large":false}).appendPoint([161,-113.5]).appendArc([158,-110.5],{"radius":3,"clockwise":false,"large":false}).appendPoint([92,-110.5]).appendArc([89,-107.5],{"radius":3,"clockwise":true,"large":false}).close().innerToCAG()
.extrude({ offset: [0, 0, 1.6] });
}


function _standoffsInner_extrude_1_6_outline_fn(){
    return CAG.circle({"center":[235,-117],"radius":1.65})
.union(
    CAG.circle({"center":[235,-91.5],"radius":1.65})
).union(
    CAG.circle({"center":[325,-91.5],"radius":1.65})
).union(
    CAG.circle({"center":[253,-125.5],"radius":1.65})
).union(
    CAG.circle({"center":[181,-125.5],"radius":1.65})
).union(
    CAG.circle({"center":[199,-117],"radius":1.65})
).union(
    CAG.circle({"center":[199,-91.5],"radius":1.65})
).union(
    CAG.circle({"center":[109,-91.5],"radius":1.65})
).extrude({ offset: [0, 0, 1.6] });
}




                function pcb_case_fn() {
                    

                // creating part 0 of case pcb
                let pcb__part_0 = board_extrude_1_6_outline_fn();

                // make sure that rotations are relative
                let pcb__part_0_bounds = pcb__part_0.getBounds();
                let pcb__part_0_x = pcb__part_0_bounds[0].x + (pcb__part_0_bounds[1].x - pcb__part_0_bounds[0].x) / 2
                let pcb__part_0_y = pcb__part_0_bounds[0].y + (pcb__part_0_bounds[1].y - pcb__part_0_bounds[0].y) / 2
                pcb__part_0 = translate([-pcb__part_0_x, -pcb__part_0_y, 0], pcb__part_0);
                pcb__part_0 = rotate([0,0,0], pcb__part_0);
                pcb__part_0 = translate([pcb__part_0_x, pcb__part_0_y, 0], pcb__part_0);

                pcb__part_0 = translate([0,0,0], pcb__part_0);
                let result = pcb__part_0;
                
            

                // creating part 1 of case pcb
                let pcb__part_1 = _standoffsInner_extrude_1_6_outline_fn();

                // make sure that rotations are relative
                let pcb__part_1_bounds = pcb__part_1.getBounds();
                let pcb__part_1_x = pcb__part_1_bounds[0].x + (pcb__part_1_bounds[1].x - pcb__part_1_bounds[0].x) / 2
                let pcb__part_1_y = pcb__part_1_bounds[0].y + (pcb__part_1_bounds[1].y - pcb__part_1_bounds[0].y) / 2
                pcb__part_1 = translate([-pcb__part_1_x, -pcb__part_1_y, 0], pcb__part_1);
                pcb__part_1 = rotate([0,0,0], pcb__part_1);
                pcb__part_1 = translate([pcb__part_1_x, pcb__part_1_y, 0], pcb__part_1);

                pcb__part_1 = translate([0,0,0], pcb__part_1);
                result = result.subtract(pcb__part_1);
                
            
                    return result;
                }
            
            
        
            function main() {
                return pcb_case_fn();
            }

        