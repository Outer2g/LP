def myLength(L):
  if L==[]: return 0
  else : return 1+myLength(L[1:])

def myMaximum(L):
  cMax = L[0]
  for i in L[1:]:
    if i>cMax : cMax= i
  return cMax

def average(L):
  cSum = 0.0
  for i in L:
    cSum = cSum + i
  return float(cSum/myLength(L))

def buildPalindrome(L):
  x = myLength(L)
  if x == 1 : return [L[0]]+[L[0]]
  else : return [L[x-1]]+ buildPalindrome(L[:x-1]) + [L[x-1]]

def remove(L1,L2):
  if L2 == [] : return L1
  else : 
    try:
      L1.remove(L2[0])
      return remove(L1,L2)
    except ValueError :
      return remove(L1,L2[1:])
    
def flatten(L):
  if L!=[] and isinstance(L[0],list) : return flatten(L[0]) + flatten(L[1:])
  elif L ==[] : return [4]
  else : return L