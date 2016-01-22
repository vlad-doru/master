import fractions
import itertools
import math

def gcd(a, b):
    return fractions.gcd(a, b)

def egcd(a, b):
    if a == 0:
        return (b, 0, 1)
    else:
        g, y, x = egcd(b % a, a)
        return (g, x - (b // a) * y, y)

# Find the modular inverse.
def modinv(a, m):
    a = a % m
    g, x, y = egcd(a, m)
    if g != 1:
        raise Exception('modular inverse does not exist')
    else:
        return x % m

def chinese_remainder(n, a):
    sum = 0
    prod = reduce(lambda a, b: a*b, n)

    for n_i, a_i in zip(n, a):
        p = prod / n_i
        sum += a_i * modinv(p, n_i) * p
        print("Termenul ", a_i, modinv(p, n_i), p)
    return sum % prod

def all_chinese_solutions(n, a):
    print(itertools.product(a))

def pow(a, b, m):
    result = 1
    pow_value = a
    while b > 0:
        if b & 1 :
            result = (result * pow_value) % m
        b = b >> 1
        pow_value = (pow_value * pow_value) % m
    return result

def primes_sieve(limit):
    limitn = limit+1
    not_prime = set()
    primes = []

    for i in range(2, limitn):
        if i in not_prime:
            continue
        for f in range(i*2, limitn, i):
            not_prime.add(f)
        primes.append(i)
    return primes

def fact(n):
    return math.factorial(n)
