use <MCAD/boxes.scad>
include <globals.scad>

$fa=1;
$fs=0.4;

difference() {
    cube([keyboard_circuit_width,
          keyboard_circuit_height,
          lid_mask_thickness]);
    // Button row 1
    button(button_x0,button_y0);
    button(button_x1,button_y0);
    button(button_x2,button_y0);
    // Button row 2
    button(button_x0,button_y1);
    button(button_x1,button_y1);
    button(button_x2,button_y1);
    // Button row 3
    button(button_x0,button_y2);
    button(button_x1,button_y2);
    button(button_x2,button_y2);
    // Button row 4
    button(button_x0,button_y3);
    button(button_x1,button_y3);
    button(button_x2,button_y3);
    // LED holes
    ledHole(led_d1_x,led_d1_7_y, 1,"circle",-1);
    ledHole(led_d2_x,led_d1_7_y, 1,"circle",-1);
    ledHole(led_d3_x,led_d1_7_y, 1,"circle",4);
    ledHole(led_d4_x,led_d1_7_y, 1,"circle",4);
    ledHole(led_d5_x,led_d1_7_y, 1,"circle",6);
    ledHole(led_d6_x,led_d1_7_y, 1,"circle",6);
    ledHole(led_d7_x,led_d1_7_y, 1, "square");
    ledHole(led_d8_x,led_d8_y, 1.5, "square");
}

module button(x,y) {
    translate([keyboard_circuit_width-x,y,
               -lid_mask_thickness/2])
        cylinder(h=lid_mask_thickness*2,r=button_radius);
}

module ledHole(x,y,radius,type="circle",fn=-1) {
//    translate([keyboard_circuit_width-x,y,
//               -lid_mask_thickness/2])
//        cylinder(h=lid_mask_thickness*2,r=radius);

    if (type == "circle" && fn == -1) {
        translate([keyboard_circuit_width-x,y,
                   -lid_mask_thickness/2])
          linear_extrude(height=lid_mask_thickness*2)
          circle(d=radius*2);
    } else if (type == "circle") {
        translate([keyboard_circuit_width-x,y,
                   -lid_mask_thickness/2])
          linear_extrude(height=lid_mask_thickness*2)
          circle(d=radius*2,$fn=fn);
    } else if(type == "square") {
        translate([keyboard_circuit_width-x,y,
                   -lid_mask_thickness/2])
          linear_extrude(height=lid_mask_thickness*2)
          square(size=radius*2,center=true);
    }
}

// Pillars
pillar_row(lid_slot_height-button_x0/4,
           [true,true,true,true]);
pillar_row(38, [true,true,true,true]);
pillar_row(button_y2+(button_y3-button_y2)/2,
           [true,true,true,true]);
pillar_row(button_y1+(button_y2-button_y1)/2,
           [true,false,true,true]);
pillar_row(button_y0+(button_y1-button_y0)/2,
           [true,false,true,true]);
pillar_row(6.9, [true,false,true,true]);
pillar_row(button_x0/4, [true,true,true,true]);

module pillar_row(y, is_there) {
    if (is_there[0]) {
        pillar(button_x0/4,y);
    }
    if (is_there[1]) {
        pillar(button_x0+(button_x1-button_x0)/2,y);
    }
    if (is_there[2]) {
        pillar(button_x1+(button_x2-button_x1)/2,y);
    }
    if (is_there[3]) {
        pillar(button_x2+(keyboard_circuit_width-button_x2)/
               4*3,y);
    }
}

module pillar(x,y) {
    translate([keyboard_circuit_width-x,y,
               lid_mask_thickness-0.001])
        cylinder(h=1.05,r=0.75);
}
