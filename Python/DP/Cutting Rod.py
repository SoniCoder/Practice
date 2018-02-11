#https://practice.geeksforgeeks.org/problems/rod-cutting/0

t = int(input())

for t_i in range(t):
    n = int(input())
    price = list(map(int, input().split()))
    best_price = [0] + price
    for n_i in range(2, n+1):
        best_yet = best_price[n_i]
        for test_cut in range(1, n_i):
            thisprice = best_price[test_cut] + best_price[n_i-test_cut]
            if best_yet < thisprice: best_yet = thisprice
        best_price[n_i] = best_yet
    print(best_price[n])
