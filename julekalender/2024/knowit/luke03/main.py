class Tallerken:
    def __init__(self):
        self.food = {
            "ris": 100,
            "erter": 100,
            "gulrøtter": 100,
            "reinsdyrkjøtt": 100,
            "julekringle": 100,
        }

        self.refill = {
            "ris": [0, 0, 1, 0, 0, 2],
            "erter": [0, 3, 0, 0],
            "gulrøtter": [0, 1, 0, 0, 0, 8],
            "reinsdyrkjøtt": [100, 80, 40, 20, 10],
            "julekringle": [0],
        }

        self.refill_status = {
            "ris": 0,
            "erter": 0,
            "gulrøtter": 0,
            "reinsdyrkjøtt": 0,
            "julekringle": 0,
        }

        self.seconds = 0
        self.seconds_since_reinsdyrkjøtt = 0

    def eat_food(self):
        eaten = 0
        for food, amount in self.food.items():
            if food == "reinsdyrkjøtt" and amount >= 2:
                if eaten == 0:
                    self.food[food] -= 2
                    break
            elif food == "julekringle" and amount >= 1:
                if eaten == 0:
                    self.food[food] -= 1
                    break
            elif food != "reinsdyrkjøtt" and food != "julekringle":
                grams_to_eat = 5 if eaten == 0 else 3
                if amount >= grams_to_eat:
                    self.food[food] -= grams_to_eat
                    eaten += 1
                if eaten >= 2:
                    break

    def refill_food(self):
        for food, status in self.refill_status.items():
            refills = self.refill[food]
            next_refill = refills[status % len(refills)]

            if food == "gulrøtter":
                if self.seconds >= 30:
                    self.food[food] += next_refill
                    self.refill_status[food] += 1
            elif food == "reinsdyrkjøtt":
                if self.food[food] <= 0:
                    if self.seconds_since_reinsdyrkjøtt >= 50 and status < len(refills):
                        self.food[food] += next_refill
                        self.seconds_since_reinsdyrkjøtt = 0
                        self.refill_status[food] += 1
                    else:
                        self.seconds_since_reinsdyrkjøtt += 1
            elif food == "julekringle":
                pass
            else:
                self.food[food] += next_refill
                self.refill_status[food] += 1

    def tick(self):
        self.eat_food()
        self.refill_food()
        self.seconds += 1


if __name__ == "__main__":
    tallerken = Tallerken()
    while tallerken.food["julekringle"] > 99:
        tallerken.tick()
        print(tallerken.seconds)
        print(tallerken.food)
    print(tallerken.seconds)
