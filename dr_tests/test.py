#!/course/cs173/python/Python-3.2.3/python
print ("\ntest 1\n")
import random

class Account(object):
	num_accounts=0
	def __init__(self, name, balance):
		self.name=name
		self.balance=balance
		Account.num_accounts+=1
	def __del__(self):
		Account.num_accounts-=1
	def deposit(self,amt):
		self.balance+=amt
	def inquiry(self):
		return self.balance
		


a=Account('ray', 10000)
print(a.inquiry())
a.deposit(100)
print(a.inquiry())



class EvilAccount(Account):
	def inquiry(self):
		if random.randint(0,4)==1:
			return self.balance * 1.10
		else:
			return self.balance


c= EvilAccount("bb",500);
c.deposit(100);
print(c.inquiry())



print ("\ntest 2\n")
class Foo(object):
	def bar(self):
		print("hello")
	def spam(self):
		#bar(self)
		self.bar()
		Foo.bar(self)
		print("__________")

a=Foo()
a.bar()
a.spam()
Foo.bar(a)
Foo.spam(a)

