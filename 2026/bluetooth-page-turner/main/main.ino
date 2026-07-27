#include <BleKeyboard.h>

const int buttonPin = 4;
bool lastButtonState = HIGH;

BleKeyboard bleKeyboard;

void setup() {
  delay(500);

  Serial.begin(115200);
  unsigned long start = millis();
  while (!Serial && millis() - start < 3000) {}
  Serial.println("Initialized Serial");

  pinMode(buttonPin, INPUT_PULLUP);
  Serial.println("Initialized Button");

  bleKeyboard.begin();
}

void loop() {
  bool buttonState = digitalRead(buttonPin);
  unsigned long now = millis();

  if (lastButtonState == HIGH && buttonState == LOW) {
    // Button pressed
    Serial.println("Button pressed");
  }

  if (lastButtonState == LOW && buttonState == HIGH) {
    // Button released
    Serial.println("Button released");
    bleKeyboard.print(" ");
  }

  lastButtonState = buttonState;
}
