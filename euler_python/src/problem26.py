'''
Created on Oct 30, 2010

@author: Olander
'''

def getQuotRem(n, d):
    return ((n*10)/d, (n*10)%d )

def getSeqLength(d):
    t = getQuotRem(1, d)
    counter = 0
    map = dict()
    while (t not in map):
        map[t] = counter
        counter += 1
        (q, r) = t
        t = getQuotRem(r, d)
    return len(map) - map[t]
    
def tuplemax((a, b), (c, d)):
    if (b > d):
        return (a, b)
    return (c, d)

reduce(tuplemax, [(x, getSeqLength(x)) for x in range(1,1000)])

if __name__ == '__main__':
    pass