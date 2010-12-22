'''
Created on Oct 30, 2010

@author: Olander
'''

from overloading import overload

def removeLeadingZero(n):
    return (n%100) > 9

def triangleNumber (n):
    return n*(n+1)/2

def pentagonalNumber(n):
    return n*(3*n-1)/2

def hexagonalNumber(n):
    return n*(2*n-1)

def heptagonalNumber(n):
    return n*(5*n-3)/2

def octagonalNumber(n):
    return n*(3*n-2)

tri = [x for x in filter(removeLeadingZero, [triangleNumber(x) for x in range(45, 141)])]

quad = [x for x in filter(removeLeadingZero, [x*x for x in range(32, 100)])]

pent = [x for x in filter(removeLeadingZero, [pentagonalNumber(x) for x in range(26, 82)])]

hex = [x for x in filter(removeLeadingZero, [hexagonalNumber(x) for x in range(23, 71)])]

hept = [x for x in filter(removeLeadingZero, [heptagonalNumber(x) for x in range(21, 64)])]

oct = [x for x in filter(removeLeadingZero, [octagonalNumber(x) for x in range(19, 59)])]
 
[(x, x*x) for x in range(32, 100)]

[(x, pentagonalNumber(x)) for x in range(26, 82)]

[(x, hexagonalNumber(x)) for x in range(23, 71)]

[(x, heptagonalNumber(x)) for x in range(21, 64)]

[(x, octagonalNumber(x)) for x in range(19, 59)]

def startlist():
    return [(x, 32) for x in filter(removeLeadingZero, [octagonalNumber(x) for x in range(19, 59)])]

def newStartList():
    return [(x, 32) for x in oct]

def valueList():
    l = [(x, 16) for x in filter(removeLeadingZero, [heptagonalNumber(x) for x in range(21, 64)])]
    l.extend([(x, 8) for x in filter(removeLeadingZero, [hexagonalNumber(x) for x in range(23, 71)])])
    l.extend([(x, 4) for x in filter(removeLeadingZero, [pentagonalNumber(x) for x in range(26, 82)])])
    l.extend([(x, 2) for x in filter(removeLeadingZero, [x*x for x in range(32, 100)])])
    l.extend([(x, 1) for x in filter(removeLeadingZero, [triangleNumber(x) for x in range(45, 141)])])
    return l   

def newValueList():
    l = [(x, 16) for x in hept]
    l.extend([(x, 8) for x in hex])
    l.extend([(x, 4) for x in pent])
    l.extend([(x, 2) for x in quad])
    l.extend([(x, 1) for x in tri])     
    return l 

def startlist1():
    return [(x, 4) for x in filter(removeLeadingZero, [pentagonalNumber(x) for x in range(26, 82)])]

def valueList1():
    l = [(x, 2) for x in filter(removeLeadingZero, [x*x for x in range(32, 100)])]
    l.extend([(x, 1) for x in filter(removeLeadingZero, [triangleNumber(x) for x in range(45, 141)])])
    return l          

def showit(l):
    print l
    return True

def allowed(a, b):
    return a[1] != b[1] and a[0]%100 == b[0]/100

def allowed3(a, b, c):
    return allowed(a, b) and allowed(b, c) and a[1] != c[1]

def allowed4(a, b, c, d):
    return allowed3(a, b, c) and allowed3(b, c, d) and a[1] != d[1]


def remSet(l):
    return set([x%100 for x in l])

def divSet(l):
    return set([x/100 for x in l])

notTriSet = set(quad).union(pent, hex, hept, oct)

notQuadSet = set(tri).union(pent, hex, hept, oct)

notPentSet = set(tri).union(quad, hex, hept, oct)

notHexSet = set(tri).union(quad, pent, hept, oct)

notHeptSet = set(tri).union(quad, pent, hex, oct)

notOctSet = set(tri).union(quad, pent, hex, hept)

def removeElements(l1, l2):
    s1 = remSet(l1) - divSet(l2)
    s2 = divSet(l1) - remSet(l2)
    r = [x for x in l1 if x%100 not in s1]
    return [x for x in r if x/100 not in s2]

def makeAllowed(s, e):
    return list(set([y for x in s for y in e if allowed(x, y)]))

def makeAllowedR(s, e):
    return list(set([x for x in s for y in e if allowed(x, y)]))

def makeAllowed3(s, m, e):
    return list(set([z for x in s for y in m for z in e if allowed3(x, y, z)]))

def makeAllowedM3(s, m, e):
    return list(set([y for x in s for y in m for z in e if allowed3(x, y, z)]))

def makeAllowedR3(s, m, e):
    return list(set([x for x in s for y in m for z in e if allowed3(x, y, z)]))

def makeAllowed4(s, m1, m2, e):
    return list(set([z for w in s for x in m1 for y in m2 for z in e if allowed4(w, x, y, z)]))

[(a,b,c,d,e,f) for a in valueList() for b in valueList() for c in valueList() for d in valueList() for e in valueList() for f in valueList() 
if sum([a[1],b[1],c[1],d[1],e[1],f[1]]) == 63 and allowed(a[0],b[0]) and allowed(b[0],c[0]) and allowed(c[0],d[0]) and allowed(d[0],e[0]) and allowed(e[0],f[0]) and allowed(f[0],a[0])]

[(a,b,c,d,e,f) for a in newStartList() for b in newValueList() for c in newValueList() for d in newValueList() for e in newValueList() for f in newValueList() 
if sum([a[1],b[1],c[1],d[1],e[1],f[1]]) == 63 and allowed(a[0],b[0]) and allowed(b[0],c[0]) and allowed(c[0],d[0]) and allowed(d[0],e[0]) and allowed(e[0],f[0]) and allowed(f[0],a[0]) and showit([a,b,c,d,e,f])]

[(a,b,c,d,e,f) for f in vlist5 for e in vlist4 for d in vlist3  for c in vlist2 for b in vlist1 for a in newStartList()
if sum([a[1],b[1],c[1],d[1],e[1],f[1]]) == 63 and allowed(a,b) and allowed(b,c) and showit([a,b,c]) and allowed(c,d) and showit([a,b,c,d]) and allowed(d,e) and showit([a,b,c,d,e]) and allowed(e,f) and allowed(f,a) and showit([a,b,c,d,e,f])]

#[(a,b,c) for a in startlist1() for b in valueList1() for c in valueList1() 
#if sum([a[1],b[1],c[1]]) == 7 and allowed(a[0],b[0]) and allowed(b[0],c[0]) and allowed(c[0],a[0]) and showit([a,b,c])]

#[triangleNumber(x) for x in range(int(math.sqrt(1000)), int(math.sqrt(10000)))]

if __name__ == '__main__':
    pass



