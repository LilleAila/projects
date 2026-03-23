#include <BleCompositeHID.h>
#include <KeyboardDevice.h>

const int buttonPin = 4;
bool lastButtonState = HIGH;

KeyboardDevice* keyboard;
BLEHostConfiguration bleHostConfig;
BleCompositeHID compositeHID("ESP32-C6 Page Turner", "Espressif", 100);

void setup() {
  delay(500);

  Serial.begin(115200);
  unsigned long start = millis();
  while (!Serial && millis() - start < 3000) {}
  Serial.println("Initialized Serial");

  pinMode(buttonPin, INPUT_PULLUP);
  Serial.println("Initialized Button");

  bleHostConfig.setHidType(HID_KEYBOARD);
  keyboard = new KeyboardDevice();
  compositeHID.addDevice(keyboard);
  compositeHID.begin(bleHostConfig);
  Serial.println("Initialized BLE HID");
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
    Serial.print("Button released");

    // Turn page
    if (compositeHID.isConnected()) {
      Serial.println("Turning page");
      keyboard->keyPress(KEY_SPACE);
      keyboard->keyRelease(KEY_SPACE);
    }
  }

  lastButtonState = buttonState;
}
