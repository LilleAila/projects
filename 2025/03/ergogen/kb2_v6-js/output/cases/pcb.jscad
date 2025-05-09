function board_extrude_1_6_outline_fn(){
    return new CSG.Path2D([[89,-110.5],[89,-72.5]]).appendPoint([345,-72.5]).appendPoint([345,-110.5]).appendPoint([273,-110.5]).appendPoint([273,-144.5]).appendPoint([161,-144.5]).appendPoint([161,-110.5]).appendPoint([89,-110.5]).close().innerToCAG()
.extrude({ offset: [0, 0, 1.6] });
}


function mountingHoles_extrude_1_6_outline_fn(){
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
                let pcb__part_1 = mountingHoles_extrude_1_6_outline_fn();

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

        