import datetime


class Kernel:
    def __init__(self):
        self.working_updates = 2
        self.target = datetime.date(2024, 12, 12)
        self.date = datetime.date(2020, 4, 1)
        self.prev1, self.prev2 = 0, 1
        self.bookmarks = 0

    def is_sunday(self) -> bool:
        return self.date.isoweekday() == 7

    def is_advent(self) -> bool:
        year = self.date.year
        christmas = datetime.date(year, 12, 25)
        sunday = christmas.weekday() + 1
        fourth_sunday = christmas - datetime.timedelta(days=sunday + 21)
        advent_start = fourth_sunday
        advent_end = datetime.date(year, 12, 24)
        return advent_start <= self.date <= advent_end

    def is_problem_day(
        self,
    ) -> bool:
        if not self.is_sunday():
            return False
        if self.is_advent() or self.date.month == 7:
            self.working_updates = 2
            return True
        if self.working_updates > 0:
            self.working_updates -= 1
            return False
        return True

    def next_day(self, days: int = 1):
        self.date += datetime.timedelta(days=days)

    def handle_day(self) -> None:
        print(self.prev1, self.prev2)
        if self.is_problem_day():
            print("Problem day!", self.date)
            # One for the current day, one for the next.
            self.bookmarks += 1
            self.prev1, self.prev2 = 0, 1
            self.next_day()  # Jump two days
            self.working_updates = 2
        else:
            self.bookmarks += self.prev1 + self.prev2
            self.prev1, self.prev2 = self.prev2, self.prev1 + self.prev2
            self.next_day()

    def total_bookmarks(self) -> int:
        while self.date <= self.target:
            self.handle_day()
        return self.bookmarks


if __name__ == "__main__":
    kernel = Kernel()
    result = kernel.total_bookmarks()
    print(result)
    # print(kernel.bookmarks)
    # kernel.handle_day()
    # kernel.handle_day()
    # kernel.handle_day()
    # kernel.handle_day()
    # kernel.handle_day()
    # kernel.handle_day()
    # kernel.handle_day()
    # kernel.handle_day()
    # kernel.handle_day()
    # kernel.handle_day()
    # kernel.handle_day()
    # kernel.handle_day()
    # kernel.handle_day()1962658
    # kernel.handle_day()
    # kernel.handle_day()
    # kernel.handle_day()
    # kernel.handle_day()
    # kernel.handle_day()
    # kernel.handle_day()
    # kernel.handle_day()
    # kernel.handle_day()
    # kernel.handle_day()
