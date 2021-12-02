use <MCAD/boxes.scad>
include <globals.scad>

$fa=1;
$fs=0.4;

difference() {
    // Lid
    translate([lid_width/2,lid_height/2,lid_thickness/2])
        roundedBox(size=[lid_width,lid_height,lid_thickness],
                   radius=box_corner_radius,
                   sidesonly=true);
    // Slot
    translate([(lid_width-lid_slot_width)/2,
               (lid_height-lid_slot_height)/2,
               lid_slot_floor_thickness])
        cube([lid_slot_width, lid_slot_height,
              lid_slot_depth*2]);
    // Film glue frame
    translate([(lid_width-(lid_slot_width+                 
                           lid_film_glue_frame_width*2))/2,
               (lid_height-(lid_slot_height+
                            lid_film_glue_frame_width*2))/2,
               lid_thickness-lid_film_thickness])
        cube([lid_slot_width+lid_film_glue_frame_width*2,
              lid_slot_height+lid_film_glue_frame_width*2,
              lid_film_thickness+0.001]);
    
    echo("Hålets vidd (exklusive limmram): ", lid_slot_width, "mm");
    echo("Hålets höjd (exklusive limmram): ", lid_slot_height, "mm");
    echo("Limmramen på varje sida: ", lid_film_glue_frame_width, "mm");
    
    // Hole
    translate([lid_width/2-lid_slot_hole_width/2,
               (lid_height-lid_slot_height)/2+lid_slot_hole_y,
               -lid_slot_floor_thickness/4])
        cube([lid_slot_hole_width,lid_slot_hole_height,
              lid_slot_floor_thickness*2]);
    // Left bottom screw hole
//    translate([box_wall_thickness+lid_screw_padding,
//               box_wall_thickness+lid_screw_padding,
//               -lid_thickness/2])
//        cylinder(r=lid_screw_radius,
//                 h=lid_thickness*2);
    // Right bottom screw hole
    translate([lid_width-
               (box_wall_thickness+lid_screw_padding),
               box_wall_thickness+lid_screw_padding, 
               -lid_thickness/2])
        cylinder(r=lid_screw_radius,
                 h=lid_thickness*2);
    // Left top screw hole
    translate([box_wall_thickness+lid_screw_padding,
               lid_height-
               (box_wall_thickness+lid_screw_padding),
               -lid_thickness/2])
        cylinder(r=lid_screw_radius,
                 h=lid_thickness*2);
    // Right top screw hole
//    translate([lid_width-
//               (box_wall_thickness+lid_screw_padding),
//               lid_height-
//               (box_wall_thickness+lid_screw_padding),
//               -lid_thickness/2])
//    cylinder(r=lid_screw_radius,h=lid_thickness*2);
}
