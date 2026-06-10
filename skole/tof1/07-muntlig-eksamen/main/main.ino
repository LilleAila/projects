const int TEMT6000_PIN = A0;
const int LDR_PIN = A1;
const int TRIG_PIN = 9;
const int ECHO_PIN = 10;

long duration;
float distance;
const int samples = 5;

void setup() {
  delay(5000);

  Serial.begin(115200);
  while (!Serial);
  Serial.println("---------- BOOT ----------");

  // TEMT6000 (phototransistor) and LDR (photoresistor)
  pinMode(TEMT6000_PIN, INPUT);
  pinMode(LDR_PIN, INPUT);

  // Ultrasonic sensor
  pinMode(TRIG_PIN, OUTPUT);
  pinMode(ECHO_PIN, INPUT);
}

void loop() {
  // unsigned long now = millis();

  int temt6000Value = analogRead(TEMT6000_PIN);
  int ldrValue = analogRead(LDR_PIN);

  float distances = 0.0f;
  int validSamples = 0;
  for (int i = 0; i < samples; ++i) {
    // Trigger a measurement from ultrasonic sensor
    digitalWrite(TRIG_PIN, LOW);
    delayMicroseconds(2);
    digitalWrite(TRIG_PIN, HIGH);
    delayMicroseconds(10);
    digitalWrite(TRIG_PIN, LOW);

    // Read the value and convert to cm using speed of sound
    // Timeout limited to 30k microseconds ≈ 500cm
    duration = pulseIn(ECHO_PIN, HIGH, 30000);
    if (duration > 0) {
      distances += duration * 0.0343f / 2.0f;
      ++validSamples;
    }

    delay(10);
  }

  if (validSamples > 0) {
    distance = distances / validSamples;
  } else {
    distance = -1.0f;
  }

  // Log values
  Serial.print("TEMT6000:");
  Serial.print(temt6000Value);

  Serial.print(",");

  Serial.print("LDR:");
  Serial.print(ldrValue);

  Serial.print(",");
  Serial.print("Distance:");
  Serial.print(distance);

  Serial.println();

  // delay(50); // Implicitly delayed from the distance loop
}
