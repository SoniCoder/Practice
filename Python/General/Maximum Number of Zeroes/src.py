t = int(input())

for t_i in range(t):
    n = int(input())
    lst = input().split()
    maxzero = -1
    maxnum = -1
    for numstr in lst:
        thiszero = 0
        for ch in numstr:
            if ch == '0':
                thiszero += 1
        if thiszero == maxzero and int(numstr) > maxnum:
            maxnum = int(numstr)
        elif thiszero > maxzero:
            maxzero = thiszero
            maxnum = int(numstr)
    print(maxnum)
