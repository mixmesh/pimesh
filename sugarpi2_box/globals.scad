// PI Zero
pizero_width = 30;
pizero_height = 65;
pizero_screw_padding=3.5;
pizero_first_micro_usb_y=35;

// Keyboard
keyboard_circuit_width=27.97;
keyboard_circuit_height=46.01;
keyboard_circuit_thickness=0.95;
keyboard_pressed_button_thickness=1.35;
keyboard_released_button_thickness=1.5;
keyboard_tca8418_thickness=0.85;
button_radius=3.55;
button_x0=6;
button_x1=14;
button_x2=22;
button_y0=10.6;
button_y1=18.6;
button_y2=26.6;
button_y3=34.6;
led_d1_7_y=41.7;
led_d1_x=4.1;
led_d2_x=6.7;
led_d3_x=9.3;
led_d4_x=11.9;
led_d5_x=14.5;
led_d6_x=20.7;
led_d7_x=23.4;
led_d8_y=4;
led_d8_x=6;

// Box
box_circuit_padding=2;
box_inner_width=pizero_width+box_circuit_padding*2;
box_inner_height=pizero_height+box_circuit_padding*2;
box_inner_depth=30;
box_wall_thickness=1;
box_outer_width=box_inner_width+box_wall_thickness*2;
box_outer_height=box_inner_height+box_wall_thickness*2;
box_outer_depth=box_inner_depth+box_wall_thickness;
box_corner_radius=3;
box_nut_tolerance=0.25;
box_nut_width=5+box_nut_tolerance;
box_nut_thickness=2+box_nut_tolerance;

// Lid
lid_width=box_outer_width;
lid_height=box_outer_height;
lid_mask_thickness=0.5;
lid_mask_pillars_thickness=1.3;
lid_mask_pillars_radius=1;
lid_film_thickness=0.75;
lid_film_glue_frame_width=2;
lid_slot_padding=0.5;
lid_slot_width=keyboard_circuit_width+lid_slot_padding;
lid_slot_height=keyboard_circuit_height+lid_slot_padding;
lid_slot_depth=keyboard_circuit_thickness+
               keyboard_tca8418_thickness+
               lid_mask_pillars_thickness+
               lid_mask_thickness+
               lid_film_thickness;
lid_slot_floor_thickness=0.5;
lid_thickness=lid_slot_floor_thickness+lid_slot_depth;
lid_slot_hole_y=26;
lid_slot_hole_width=14;
lid_slot_hole_height=11;
lid_screw_padding=box_circuit_padding+pizero_screw_padding;
lid_screw_tolerance=0.5;
lid_screw_radius=2.4/2+lid_screw_tolerance;
