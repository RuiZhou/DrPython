class A(object):
	number=1


a=A()


print(a.number)
a.number=2;
print(a.number)
del(a.number)
print(a.number)


print(A.number)
A.number=2;
print(A.number)
del(A.number)
print(A.number)
