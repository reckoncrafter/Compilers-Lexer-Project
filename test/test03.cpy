x: int = 5

def foo_void(y : int):
    x: int = 5
    z : int = 0

    z = x * 2




class Fooc(object):

    m0: int = 0

    def m1() -> int:
        i : int = 0

        j : int = 0

        l : [int] = None

        l = [1, 2, 3]

        for i in l:
            j = j + 1

        return j

f: Fooc = None

inp: str = ""

f = Fooc()
inp = input()
x = len(inp)
print("Hello", f.m1())
