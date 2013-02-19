class MyClass(object):
    i = 4
    if(True):
        a = 5
        print("^_^")
    else:
        b = 6
    def f(self):
        return 'hello world'
    def __init__(this):
        this.b = 1
    #c = lambda x: print(x)
    def d(x):
        print(x)
    c = lambda x: print(x)

a = MyClass()
a.c = lambda x: print(x)
a.c(2)

del(a.c)

a.c(2)

#del(a.c)

#a.c(2)

#a.d(2)
