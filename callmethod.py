from time import time



def invoke(v):
    return v
    
veccall = map(lambda x: invoke, range(20))


def testvec(x, y):
    return veccall[x](y)
    
    
def testif(x, y):
    if x == 0:
        return invoke(x)
    if x == 1:
        return invoke(x)
    if x == 2:
        return invoke(x)
    if x == 3:
        return invoke(x)
    if x == 4:
        return invoke(x)
    if x == 5:
        return invoke(x)
    if x == 6:
        return invoke(x)
    if x == 7:
        return invoke(x)
    if x == 8:
        return invoke(x)
    if x == 9:
        return invoke(x)
    if x == 10:
        return invoke(x)
    if x == 11:
        return invoke(x)
    if x == 12:
        return invoke(x)
    if x == 13:
        return invoke(x)
    if x == 14:
        return invoke(x)
    if x == 15:
        return invoke(x)
    if x == 16:
        return invoke(x)
    if x == 17:
        return invoke(x)
    if x == 18:
        return invoke(x)
    if x == 19:
        return invoke(x)
nax = 10000000


st = time()

for x in range(nax):
    for x in range(3):
        testif(x, x)
    
print time() - st


st = time()
for x in range(nax):
    for x in range(3):
        testvec(x, x)
print time() - st

