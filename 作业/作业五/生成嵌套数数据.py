import random


def do():
    a = random.randint(0, 6)
    if a == 0 or a == 1 or a == 2:
        print("(integer ", random.randint(-10, 10), ")", sep="", end="")
    elif a == 3 or a == 4:
        print("(rational ", end="")
        do()
        print(" ", end="")
        do()
        print(")", end="")
    elif a == 5 or a == 6:
        print("(complex ", end="")
        do()
        print(" ", end="")
        do()
        print(")", end="")


do()
