# Jeg fikk ikke testet og kommentert denne koden skikkelig :(

import csv
import os
import random


class Game:
    def _read_file(self, filename):
        """Lese spørsmål fra en csv-fil"""
        dir = os.path.dirname(__file__)
        with open(os.path.join(dir, filename)) as file:
            reader = csv.reader(file)
            data = list(reader)
            # Jeg kunne laget en egen klasse for spørsmål, noe som hadde gjort koden videre mye mer lesbar enn det jeg gjør nå, ved å manuelt velge indeks 0 og 1 for spørsmål og svar.
            self._questions[data[0][0]] = {  # navnet på kategorien
                # Strengen med tekst som skal stilles til brukeren
                "question": data[0][1],
                # Alle rader utenom den første er selve spørsmålene
                "answers": data[1:],
            }

    def _init_answers(self):
        """Laste inn alle filene med spørsmål og svar"""
        self._questions = {}

        # lese fra disse filene
        self._read_file("hovedstader.csv")
        self._read_file("innbyggere.csv")
        self._read_file("naboland.csv")

    def __init__(self):
        """Sette variabler relatert til spillet"""
        self._init_answers()
        self._score = 0
        self._rounds = 0
        self._user_answers = []

    def _trim_str(self, text):
        """Fjerne ekstra mellomrom og gjøre til små bokstaver"""
        return text.strip().lower()

    def ask_question(self):
        """Velge et tilfeldig spørsmål og stille det til brukeren"""
        # Velg tilfeldig spørsmål og kategori
        category = random.choice(list(self._questions.keys()))
        questions = self._questions[category]
        question = random.choice(questions["answers"])
        # En ekstra sjekk for om det er denne typen spørsmål
        if category == "naboland":
            print("Skriv naboland separert med komma:")
        # Spør bruker om svaret
        user_question = f"{questions['question']} {question[0].capitalize()}? "
        user_answer = self._trim_str(
            input(user_question)
        )
        answer = question[1]

        correct = False

        # Gå gjennom de ulike kategoriene, siden hver av dem har litt ulik logikk
        match category:
            case "innbyggere":
                try:
                    user_answer = int(user_answer)
                    answer = int(answer)
                    # Lar brukeren være opp til 10% feil med svaret sitt
                    if abs(user_answer / answer) - 1 < 0.10:
                        correct = True
                        print("Det var nesten riktig svar.")
                    if user_answer == answer:
                        print("Det var helt riktig!")
                        print("Du er jammen god med tall!")
                except ValueError:
                    # Jeg bestemmer at det er feil svar hvis brukeren ikke sier et tall.
                    pass
            case "hovedstader":
                if user_answer == answer:
                    correct = True
                    print("Det var riktig svar!")
            case "naboland":
                valid_answers = answer.split(";")
                user_answers = [self._trim_str(a) for a in user_answer.split(",")]
                # correct = user_answer in valid_answers
                num_correct = sum([i in valid_answers for i in user_answers])
                num_answers = len(valid_answers)
                if num_answers == 2 and num_correct == 2:
                    correct = True
                else:
                    correct = num_correct >= min(num_answers - 1, 3)

                if correct:
                    print("Det var riktig svar!")

        if correct:
            self._score += 1
            print(f"Nå har du fått {self._score} riktige svar.")
        else:
            print(f"Det var feil svar! Du har {self._score} riktige svar.")

        self._user_answers.append({
            "correct": correct,
            "question": user_question,
            "answer": user_answer
        })

    def game(self):
        print("Velkommen til dette geografi-quiz spillet!")
        while self._rounds < 3:
            self._rounds += 1
            print(f"Dette er runde {self._rounds}.")
            self.ask_question()
            # if self._trim_str(input("Vil du avslutte spillet? (j/n)")) == "j":
            #     break
        print(
            f"Spillet er avsluttet! Du fikk {self._score} poent på {self._rounds} runder!"
        )

        match self._score:
            case 0:
                print("Det var dårlig")
            case 1:
                print("Det var mindre bra")
            case 2:
                print("Det var bra")
            case 3:
                print("Det var kjempebra")

        correct = [i for i in self._user_answers if i["correct"]]
        incorrect = [i for i in self._user_answers if not i["correct"]]
        if len(correct) > 0:
            print("Du fikk riktig på:")
            [print(i["question"], i["answer"]) for i in correct]
        if len(incorrect) > 0:
            print("Du fikk feil på:")
            [print(i["question"], i["answer"]) for i in incorrect]


if __name__ == "__main__":
    game = Game()
    try:
        game.game()
    except (EOFError, KeyboardInterrupt):
        print("\nneiiiiii ikke stopp spillet :(")
