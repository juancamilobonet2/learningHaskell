def sieve_of_sundaram(n):
    """The sieve of Sundaram is a simple deterministic algorithm for finding all the prime numbers up to a specified integer."""
    k = (n - 2) // 2
    integers_list = [True] * (k + 1)
    for i in range(1, k + 1):
        j = i
        while i + j + 2 * i * j <= k:
            integers_list[i + j + 2 * i * j] = False
            j += 1
    if n > 2:
        print(2, end=' ')
    for i in range(1, k + 1):
        if integers_list[i]:
            print(2 * i + 1, end=' ')

if __name__ == "__main__":
    sieve_of_sundaram(10)