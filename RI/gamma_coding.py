def gamma(code):
    print(code)
    binary = []
    while code:
        bit = code & 1
        code = code >> 1
        binary = [bit] + binary
    print binary

for code in [13, 777, 17743, 294068, 31251336]:
    gamma(code)

gamma(130)
