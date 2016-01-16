from lib import math
from lib import input

p = input.read_int('p')
alpha = input.read_int('alpha')
beta = input.read_int('beta')
x1 = input.read_int('x1')
x2 = input.read_int('x2')
x3 = input.read_int('x3')
x4 = input.read_int('x4')

q = (p - 1)/2
print("q = ", q)

def check(alpha, beta, log_value):
    check = 1
    for i in range(0, log_value):
        check = check * alpha
        check = check % p
    if (check == beta):
        print("Raspunsul {0} verifica!!!!".format(log_value))

print("Incepem rezolvarea:")
print("----------------------")
print("Calculam d = (x4 - x2, p - 1)")
d = math.gcd(x4 - x2, p - 1)
print("d = {0}".format(d))
if (d == 1):
    print("Calculam y = (x4 - x2)^-1(mod p - 1)")
    y = math.modinv(x4 - x2, p - 1)
    print("y = {0}".format(y))
    print("Calculam log_alpha(beta) = (x1 - x3) * y (mod p - 1)")
    log_value = ((x1 - x3) * y) % (p - 1)
    print("Verificam logaritmului: {0}".format(log_value))
    checl(alpha, beta, log_value)
elif (d == 2):
    print("Calculam y = (x4 - x2)^-1(mod q)")
    y = math.modinv(x4 - x2, q)
    print("y = {0}".format(y))
    print("Calculam posibilitatea 1 pt log_alpha(beta) = (x1 - x3) * y (mod p - 1)")
    log_value = ((x1 - x3) * y) % (p - 1)
    print("Verficam posibilitatea 1 pentru log_alpha(beta)", log_value)
    check(alpha, beta, log_value)
    print("Calculam posibilitatea 1 pt log_alpha(beta) = (x1 - x3) * y  + q (mod p - 1)")
    log_value = (log_value + q) % (p - 1)
    print("Verficam posibilitatea 2 pentru log_alpha(beta)", log_value)
    check(alpha, beta, log_value)
else:
    print ("Ceva e gresit, d poate fi doar 1 sau 2")

