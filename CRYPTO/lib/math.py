import fractions
import itertools
import numpy
import functools

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
    prod = functools.reduce(lambda a, b: a*b, n)

    for n_i, a_i in zip(n, a):
        p = prod / n_i
        sum += a_i * modinv(p, n_i) * p
        print("Termenul ", a_i, modinv(p, n_i), p)
    return sum % prod

def pow(a, b, m):
    result = 1
    pow_value = a
    b = int(b)
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
    return numpy.math.factorial(n)

def sqrt(n):
    return numpy.sqrt(n)

def solve_quadratic(a, b, c):
    d = b**2-4*a*c # discriminant

    if d < 0:
        print ("This equation has no real solution")
    elif d == 0:
        x = (-b+sqrt(b**2-4*a*c))/2*a
        print ("This equation has one solutions: "), x
        return x
    else:
        x1 = (-b+sqrt((b**2)-(4*(a*c))))/(2*a)
        x2 = (-b-sqrt((b**2)-(4*(a*c))))/(2*a)
        print ("This equation has two solutions: ", x1, " or", x2)
        return x1, x2

def is_prime(n):
    i = 2
    while i * i <= n:
        if n % i == 0:
            return False
        i += 1
    return True

def all_chinese_solutions(n, ais):
    sols = []
    for a in itertools.product(*ais):
        sol = chinese_remainder(n, a)
        print("Sistemul " + str(a) + " cu solutia " + str(sol))
        sols.append(sol)
    return sols

def factorization(n):
    assert (n == int(n) and n > 0)
    d, p = (2, 0)
    while n != 1:
        while n % d == 0:
            p += 1
            n /= d
        if p > 0:
            print ("%d^%d" % (d, p))
            p = 0
        d += 1

