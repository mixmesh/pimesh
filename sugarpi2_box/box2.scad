include <MCAD/boxes.scad>;
include <globals.scad>

$fa=1;
$fs=0.4;

collector_hole_width=25;
collector_hole_height=13;
collector_hole_y=box_wall_thickness+box_circuit_padding+
                pizero_first_micro_usb_y;
collector_hole_z=15;

// Box
difference() {
    // Outer box
    translate([box_outer_width/2,box_outer_height/2,
               box_outer_depth/2])
        roundedBox(size=[box_outer_width,box_outer_height,
                         box_outer_depth],
                   radius=box_corner_radius,
                   sidesonly=true);
    // Inner box
    translate([box_inner_width/2+box_wall_thickness,
               box_inner_height/2+box_wall_thickness,
               box_outer_depth/2+box_wall_thickness])
        roundedBox(size=[box_inner_width,box_inner_height,
                         box_inner_depth],
                   radius=box_corner_radius,
                   sidesonly=true);        
    // Connector hole
    translate([box_wall_thickness+box_inner_width-0.001,
               collector_hole_y, collector_hole_z])
        cube([box_wall_thickness*2,collector_hole_width,
              collector_hole_height]); 
}

nut_case_wall_thickness=0.75;
nut_case_width=box_nut_width+nut_case_wall_thickness*2;
nut_case_height=box_circuit_padding+lid_screw_padding+
                box_nut_width/2;
nut_case_thickness=box_nut_thickness+
                   nut_case_wall_thickness*2;
nut_case_z=16;

// Lower left nut case
nutCase(box_wall_thickness+lid_screw_padding-
        nut_case_width/2,
        box_wall_thickness-0.001,
        nut_case_z, false);
// Lower right nut case
nutCase(box_outer_width-
        (box_wall_thickness+lid_screw_padding+       
         nut_case_width/2),
        box_wall_thickness-0.001,
        nut_case_z, false);
// Upper left nut case
nutCase(box_wall_thickness+lid_screw_padding-
        nut_case_width/2,
        box_outer_height-(box_wall_thickness+nut_case_height)+0.001,
        nut_case_z, true);
// Upper right nut case
nutCase(box_outer_width-
        (box_wall_thickness+lid_screw_padding+       
         nut_case_width/2),
        box_outer_height-(box_wall_thickness+nut_case_height)+0.001,
        nut_case_z, true);

// FIXME: Rewrite. Can be much shorter. Lamer mistakes.
module nutCase(x,y,z,up=true) {
    if(up) {
        difference() {
            union() {
                // Nut case cube
                translate([x,y,z])
                    cube([nut_case_width,nut_case_height,
                          nut_case_thickness]);
                // Nut case support
                translate([x,y,z])
                rotate([0, 90, 0])
                linear_extrude(height=nut_case_width)
                    polygon([[0,0],
                             [0,nut_case_height],
                             [nut_case_thickness, // height
                              nut_case_height]]);
            }
            // Nut case screw
            translate([x+nut_case_width/2,
                       y+box_nut_width/2,
                       z-nut_case_thickness/2])
               cylinder(r=lid_screw_radius,
                        h=nut_case_thickness*2);             
            // Nut case hole
            translate([x+(nut_case_width-box_nut_width)/2,
                         y-0.001,
                       z+(nut_case_thickness-
                          box_nut_thickness)/2])
                cube([box_nut_width,box_nut_width,
                      box_nut_thickness]);           
        }
    } else {
       difference() {
            union() {
                // Nut case cube
                translate([x,y,z])
                    cube([nut_case_width,nut_case_height,
                          nut_case_thickness]);
                // Nut case support
                translate([x+nut_case_width,
                           y+nut_case_height,
                           z])
                rotate([180, 90, 0])
                linear_extrude(height=nut_case_width)
                    polygon([[0,0],
                             [0,nut_case_height],
                             [nut_case_thickness, // height,
                              nut_case_height]]);
            }
            // Nut case screw
            translate([x+nut_case_width/2,
                       y+nut_case_height-box_nut_width/2,
                       z-nut_case_thickness/2])
                cylinder(r=lid_screw_radius,
                         h=nut_case_thickness*2);
            // Nut case hole
            translate([x+(nut_case_width-box_nut_width)/2,
                       y+(nut_case_height-box_nut_width/2),
                       z+(nut_case_thickness-
                          box_nut_thickness)/2])
               cube([box_nut_width,box_nut_width,
                     box_nut_thickness]);
        }
    }
}
