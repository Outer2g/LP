from math import *

def absValue(x):
  if x>=0:
    return x
  else:
    return -x

def power(x,p):
  if p==0: return 1
  else: return x*power(x,p-1)

def isPrime(x):
  if x<=1 : return False
  aux=int(sqrt(x))
  for i in range(aux+1)[2:]:
    if (x%i==0): return False
  return True

def slowFib(n):
  if n==0: return 0
  elif n==1: return 1
  else: return slowFib(n-1)+slowFib(n-2)

def quickFib(n):
  return fib(n)[0]

def fib(n):
  if n==0: return (0,0)
  elif n==1: return (1,0)
  else : 
      fn = fib(n-1)
      return (fn[0]+fn[1],fn[0])  