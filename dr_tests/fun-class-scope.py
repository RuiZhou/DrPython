def f(x):
    class C:
        def m(self):
            return a
        a = x
    return C

y = f(3)()
print(y.a)
print(y.m())
