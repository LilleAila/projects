#include <HijelHID_BLEKeyboard.h>

const int buttonPin = 4;
const unsigned long pairingThreshold = 5000;
bool lastButtonState = HIGH;
unsigned long pressedTime = 0;

HijelHID_BLEKeyboard keyboard("ESP32-C6 Page Turner", "ESP32", 100);

void setup() {
  delay(500);

  Serial.begin(115200);
  unsigned long start = millis();
  while (!Serial && millis() - start < 3000) {}
  Serial.println("Initialized Serial");

  pinMode(buttonPin, INPUT_PULLUP);
  Serial.println("Initialized Button");

  keyboard.setDebugLevel(HIDLogLevel::Normal);
  keyboard.begin();
  Serial.println("Initialized BLE Keyboard");
}

void loop() {
  bool buttonState = digitalRead(buttonPin);
  unsigned long now = millis();
  unsigned long duration = now - pressedTime;

  // Button pressed
  if (lastButtonState == HIGH && buttonState == LOW) {
    Serial.println("Button pressed");
    pressedTime = now;
  }

  // Button released
  if (lastButtonState == LOW && buttonState == HIGH) {
    Serial.print("Button released after ");
    Serial.print(duration);
    Serial.println("ms");

    if (duration < pairingThreshold) {
      // Turn page
      if (keyboard.isConnected()) {
        Serial.println("Turning page");
        keyboard.tap(KEY_SPACE);
      }
    } else {
      // Enter pairing mode
      Serial.println("Entering pairing mode");
      keyboard.clearBonds();
    }
  }

  lastButtonState = buttonState;
}
