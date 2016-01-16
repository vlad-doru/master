import fractions

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

def pow(a, b, m):
    result = 1
    pow_value = a
    while b > 0:
        if b & 1 :
            result = (result * pow_value) % m
        b = b >> 1
        pow_value = (pow_value * pow_value) % m
    return result
        
