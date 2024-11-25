import datetime

type ISBN = int

type Genre = str
GENRES: list[Genre] = ["Action", "Humor", "Science Fiction", "Mystery", "Nonfiction"]


class Author(str):
    __slots__ = "__names"

    def __init__(self, name: str) -> None:
        # Ex. Jo Bjørnar Hausnes -> ["Jo", "Bjørnar", "Hausnes"]
        self.__names = name.split()

    @property
    def last_name(self) -> str:
        return self.__names[-1].capitalize()

    @property
    def first_names(self) -> list[str]:
        return [name.capitalize() for name in self.__names[:-1]]

    @property
    def full_name(self) -> str:
        first_names = " ".join(self.first_names)
        return f"{self.last_name}, {first_names}"

    @property
    def short_name(self) -> str:
        first_names = ", ".join([f"{name[0]}." for name in self.first_names])
        return f"{self.last_name}, {first_names}"

    def __str__(self) -> str:
        return self.full_name

    def __repr__(self) -> str:
        return self.full_name


class Book:
    # Possible additions: language, genre
    __slots__ = (
        "__title",
        "__authors",
        "__isbn",
        "__published",
        "__genres",
        "__borrow_date",
        "__due_date",
        "__borrowed",
    )

    def __init__(
        self,
        title: str,
        authors: list[Author],
        isbn: ISBN,
        published: datetime.date,
        genres: list[Genre] = [],
        borrow_date: None | datetime.date = None,
    ) -> None:
        self.__title = title.capitalize()
        self.__authors = authors
        self.__isbn = isbn
        self.__published = published
        self.__genres = genres
        self.__borrow_date = borrow_date
        if borrow_date is not None:
            self.borrow(borrow_date)
        else:
            self.__borrow_date = None
            self.__due_date = None
            self.__borrowed = False

    @property
    def title(self) -> str:
        return self.__title

    @property
    def authors(self) -> list[Author]:
        return self.__authors

    @property
    def isbn(self) -> ISBN:
        return self.__isbn

    @property
    def published(self) -> datetime.date:
        return self.__published

    @property
    def genres(self) -> list[Genre]:
        return self.__genres

    @property
    def borrow_date(self) -> datetime.date:
        assert isinstance(
            self.__borrow_date, datetime.date
        ), "Attempted to get borrow date on non-borrowed book"
        return self.__borrow_date

    @property
    def due_date(self) -> datetime.date:
        assert isinstance(
            self.__due_date, datetime.date
        ), "Attempted to get due date on non-borrowed book"
        return self.__due_date

    @property
    def borrowed(self) -> bool:
        return self.__borrowed

    def past_due(self, date: datetime.date) -> bool:
        return self.borrowed and self.due_date < date

    def borrow(self, date: datetime.date) -> bool:
        if self.borrowed:
            return False
        self.__borrowed = True
        self.__borrow_date = date
        self.__due_date = self.__borrow_date + datetime.timedelta(days=60)
        return True

    def return_book(self):
        assert self.borrowed, "Cannot return non-borrowed book"
        self.__borrowed = False
        self.__borrow_date = None
        self.__due_date = None

    def __repr__(self):
        authors = " & ".join(self.authors)
        if self.borrowed:
            return f"{self.title} - {authors}\nBorrowed {str(self.borrow_date)}, Due {str(self.due_date)}"
        else:
            return f"{self.title} - {authors}\nNot borrowed"


class Library:
    __slots__ = "__books"

    def __init__(self):
        pass


test_book = Book(
    "The hitchhiker's guide to the galaxy",
    [Author("Douglas Adams")],
    9780307417138,
    datetime.date(1979, 10, 15),
    genres=[GENRES[1], GENRES[2]],
)

# Ulike tester for å forsikre meg om at koden fungerer
assert test_book.borrow(datetime.date.today()) == True
assert not test_book.past_due(datetime.date.today())
assert test_book.past_due(test_book.due_date + datetime.timedelta(days=1))
test_book.return_book()
