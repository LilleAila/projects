function board_extrude_1_6_outline_fn(){
    return new CSG.Path2D([[143,-110.5],[143,-72.5]]).appendPoint([234,-72.5]).appendPoint([234,-110.5]).appendPoint([198,-110.5]).appendPoint([198,-144.5]).appendPoint([161,-144.5]).appendPoint([143,-110.5]).close().innerToCAG()
.extrude({ offset: [0, 0, 1.6] });
}


function mountingHoles_extrude_1_6_outline_fn(){
    return CAG.circle({"center":[171.5,-134],"radius":1.65})
.union(
    CAG.circle({"center":[151.5,-134],"radius":1.65})
).union(
    CAG.circle({"center":[178,-117],"radius":1.65})
).union(
    CAG.circle({"center":[136,-117],"radius":1.65})
).union(
    CAG.circle({"center":[214,-92],"radius":1.65})
).union(
    CAG.circle({"center":[196,-92],"radius":1.65})
).union(
    CAG.circle({"center":[109,-92],"radius":1.65})
).union(
    CAG.circle({"center":[163,-92],"radius":1.65})
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

        