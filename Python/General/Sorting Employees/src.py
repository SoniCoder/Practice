t = int(input())

for t_i in range(t):
    n = int(input())
    inp = input().split()
    emps = []
    for inp_i in range(0, len(inp), 2):
        emps.append((int(inp[inp_i+1]), inp[inp_i]))
    emps = sorted(emps)
    #print(emps)
    for emp in emps:
        print(emp[1],emp[0], end=' ')
    print()
