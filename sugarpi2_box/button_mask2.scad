use <MCAD/boxes.scad>
include <globals.scad>

$fa=1;
$fs=0.4;
width=27.97;
height=46.01;
button_radius=3.55;
button_x0=6;
button_x1=width/2;
button_x2=width-6;
button_y0=10.75; // ~
button_y1=18.4; // ~
button_y2=26.32; // ~
button_y3=34.6; // ~

difference() {
    cube([width,height,lid_mask_thickness]);
    // Button row 1
    button(button_x0, button_y0);
    button(button_x1, button_y0);
    button(button_x2, button_y0);
    // Button row 2
    button(button_x0, button_y1);
    button(button_x1, button_y1);
    button(button_x2, button_y1);
    // Button row 3
    button(button_x0, button_y2);
    button(button_x1, button_y2);
    button(button_x2, button_y2);
    // Button row 4
    button(button_x0, button_y3);
    button(button_x1, button_y3);
    button(button_x2, button_y3);
}

module button(x,y) {
    translate([x,y,-lid_mask_thickness/2])
        cylinder(h=lid_mask_thickness*2,r=button_radius);
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
        pillar(button_x2+(width-button_x2)/4*3,y);
    }
}

module pillar(x,y) {
    translate([x,y,lid_mask_thickness-0.001])
        cylinder(h=1.05,r=0.75);
}
