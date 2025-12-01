with open("input.txt", "r") as file:
    commands = [i.strip() for i in file.readlines()]

stack = []

key = ""

for c in commands:
    args = c.split()
    match args[0]:
        case "ADD":
            stack.append(args[1])
        case "PROCESS":
            item = stack.pop(0)
            key += item[0]
        case "COUNT":
            key += str(len(stack))[-1]

print(key)
