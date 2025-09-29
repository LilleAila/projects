import datetime

type ISBN = int

type Genre = str
GENRES: list[Genre] = ["Action", "Humor", "Science Fiction", "Mystery", "Nonfiction"]

type Edition = str
EDITIONS: list[Edition] = ["Physical", "Digital", "Audiobook"]


class Author:
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
        "__edition",
        "__genre",
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
        genre: Genre | None = None,
        edition: Edition = EDITIONS[0],
        borrow_date: None | datetime.date = None,
    ) -> None:
        self.__title = title.capitalize()
        self.__authors = authors
        self.__isbn = isbn
        self.__published = published
        self.__edition = edition
        self.__genre = genre
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
    def genre(self) -> Genre | None:
        return self.__genre

    @property
    def edition(self) -> Edition:
        return self.__edition

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

    def return_book(self) -> None:
        assert self.borrowed, "Cannot return non-borrowed book"
        self.__borrowed = False
        self.__borrow_date = None
        self.__due_date = None

    def __repr__(self) -> str:
        authors = " & ".join(map(str, self.authors))
        if self.borrowed:
            return f"{self.edition}, {self.genre} - {self.title} - {authors} - Borrowed {str(self.borrow_date)}, Due {str(self.due_date)}"
        else:
            return f"{self.edition}, {self.genre} - {self.title} - {authors} - Not borrowed"


class Library:
    __slots__ = "__books"

    def __init__(self) -> None:
        self.__books = [
            Book(
                "The hitchhiker's guide to the galaxy",
                [Author("Douglas Adams")],
                9780307417138,
                datetime.date(1979, 10, 15),
                GENRES[1],
                EDITIONS[0],
            )
        ]

    @property
    def books(self) -> list[Book]:
        return self.__books

    def add_book(self, book: Book) -> None:
        self.__books.append(book)

    def search(self, query: str) -> list[Book]:
        return [book for book in self.books if query.lower() in book.title.lower()]

    def __repr__(self) -> str:
        num_books = len(self.books)
        s = "s" if num_books > 1 else ""
        return f"Library with {num_books} book{s}"


class TUI:
    __slots__ = ("__running", "__library")

    def __init__(self) -> None:
        self.__running = False
        self.__library = Library()

    def __input(self, prompt: str, predicate=lambda x: len(x) > 0) -> str:
        while True:
            result = input(prompt)
            if predicate(result):
                return result

    def __input_int(self, prompt: str) -> int:
        while True:
            try:
                return int(self.__input(prompt))
            except ValueError:
                pass

    def __input_date(self, prompt):
        while True:
            result = input(prompt)
            try:
                return datetime.datetime.strptime(result, "%Y-%m-%d")
            except ValueError:
                print(f"Not a valid date: {result}")

    def __input_authors(self) -> list[Author]:
        authors = []
        print("Input the names of the authors. To finish, press enter.")
        while True:
            result = input("Author's full name: ")
            if len(result) > 0:
                authors.append(Author(result))
            else:
                return authors

    def __choose_action(self, actions: dict[str, str]) -> str:
        actions_prompt = "\n".join(
            [f"{key}: {action}" for key, action in actions.items()]
        )
        print()
        while True:
            result = input(actions_prompt + "\nChoose an action: ")
            print()
            if result in actions:
                return result
            else:
                print("Invalid answer! Try again.")

    def __list_books(self, books) -> None:
        books = map(str, self.__library.books)
        print("\n".join(books))

    def __add_book(self) -> None:
        title = self.__input("Book title: ")
        authors = self.__input_authors()
        isbn = self.__input_int("Book ISBN: ")
        published = self.__input_date("Publishing date in format YYYY-MM-DD: ")
        genres = ", ".join(GENRES)
        genre = self.__input(
            f"{genres}\nChoose one of the above genres: ",
            predicate=lambda x: x in GENRES,
        )
        editions = ", ".join(EDITIONS)
        edition = self.__input(
            f"{editions}\nChoose one of the above editions: ",
            predicate=lambda x: x in EDITIONS,
        )
        book = Book(title, authors, isbn, published, genre, edition)
        self.__library.add_book(book)

    def __search_book(self) -> None:
        query = self.__input("Search query:")
        print("Books matching your query:")
        result = self.__library.search(query)
        self.__list_books(result)

    def start(self) -> None:
        self.__running = True
        print("Welcome to the library!")
        try:
            while self.__running:
                action = self.__choose_action(
                    {
                        "l": "List books",
                        "a": "Add book",
                        "s": "Search for book",
                    }
                )
                match action:
                    case "l":
                        self.__list_books(self.__library.books)
                    case "a":
                        self.__add_book()
                    case "s":
                        self.__search_book()
        except (KeyboardInterrupt, EOFError):
            self.__running = False


if __name__ == "__main__":
    tui = TUI()
    tui.start()
