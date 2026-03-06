#include <NimBLEDevice.h>

uint8_t keyReport[8] = {0};

void setup() {
  delay(500);

  Serial.begin(115200);
  unsigned long start = millis();
  while (!Serial && millis() - start < 3000) {}
  Serial.println("Initialized Serial");

  NimBLEDevice::init("ESP32-C6 Page Turner");
  NimBLEServer *pServer = NimBLEDevice::createServer();
  NimBLEService *pService = pServer->createService(NimBLEUUID((uint16_t)0x1812));
  NimBLECharacteristic *pInput = pService->createCharacteristic(
      NimBLEUUID((uint16_t)0x2A4D),
      NIMBLE_PROPERTY::READ | NIMBLE_PROPERTY::NOTIFY
  );
  pInput->setValue((uint8_t*)keyReport, sizeof(keyReport));
  pService->start();
  NimBLEAdvertising *pAdvertising = NimBLEDevice::getAdvertising();
  pAdvertising->addServiceUUID(pService->getUUID());
  pAdvertising->setName("ESP32-C6 Page Turner");
  pAdvertising->start();
  Serial.println("Initialized BLE HID");
}

void loop() {
  static unsigned long lastPress = 0;
  if (millis() - lastPress > 5000) {
    lastPress = millis();

    keyReport[2] = 0x2C;
    NimBLEDevice::getAdvertising()->getServer()->getServiceByUUID(0x1812)
        ->getCharacteristicByUUID(0x2A4D)->setValue(keyReport, sizeof(keyReport));
    NimBLEDevice::getAdvertising()->getServer()->getServiceByUUID(0x1812)
        ->getCharacteristicByUUID(0x2A4D)->notify();

    delay(100);

    keyReport[2] = 0x00;
    NimBLEDevice::getAdvertising()->getServer()->getServiceByUUID(0x1812)
        ->getCharacteristicByUUID(0x2A4D)->setValue(keyReport, sizeof(keyReport));
    NimBLEDevice::getAdvertising()->getServer()->getServiceByUUID(0x1812)
        ->getCharacteristicByUUID(0x2A4D)->notify();

    Serial.println("Page Down sent");
  }
}
