def nzz(n: int, k: int, l: int) -> int:
    if k == 0 or l == 0:
        return 1
    if n == 0 or n < k+l-1:
        return 1
    return nzz(n-1, k, l) + nzz(n-l, k-1, l)

print(nzz(4, 3, 2))