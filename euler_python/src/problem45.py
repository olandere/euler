'''
Created on Nov 7, 2010

@author: Olander
'''

def triangleNumber (n):
    return n*(n+1)/2

def pentagonalNumber(n):
    return n*(3*n-1)/2

def hexagonalNumber(n):
    return n*(2*n-1)

def findNum(n) :
    hex = set([hexagonalNumber(x) for x in range(1, n)])
    pent = set([pentagonalNumber(x) for x in range(1, n)])
    return hex.intersection(pent)

findNum(100000)