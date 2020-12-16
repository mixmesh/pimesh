
int pin_state[14];

//    2   3   4   6
// 7 |1| |4| |7| |*|
// 8 |2| |5| |8| |0|
// 5 |3| |6| |9| |#|
//

// pin -> grid line (+1)
int ver[14] = { 0,0,1,2,3,0,4,0,0,0,0,0,0,0 };
int hor[14] = { 0,0,0,0,0,3,0,1,2,0,0,0,0,0 };

char button[4][3] =
  { { '1', '2', '3' },
    { '4', '5', '6' },
    { '7', '8', '9' },
    { '*', '0', '#' } };

int h;
int v;

void setup() {
  int i;
  Serial.begin (9600);
  while (!Serial) {
    ; // wait for serial port to connect. Needed for native USB port only
  }
  Serial.println ("Keyboard7 started");
  
  for (i = 2; i <= 8; i++) {
    pinMode(i, INPUT);
    pin_state[i] = digitalRead(i);
  }
  h = 0;
  v = 0;
}

void loop() 
{
  int i;

  for (i = 2; i <= 8; i++) {
    int s = digitalRead(i);
    if (s != pin_state[i]) {
      int j;
      pin_state[i] = s;
      if ((j = hor[i]) != 0) h = j;
      else if ((j = ver[i]) != 0) v = j;
      if ((v != 0) && (h != 0)) {
        if (s == LOW)
          Serial.print("PRESS ");
        else 
          Serial.print("RELEASE ");
        Serial.println(button[v-1][h-1]);
        h = v = 0;
      }
    }
  }
}
