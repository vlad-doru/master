import numpy.random as rand

def main():
    for size, name in [(1000000, '1M'), (10000000, '10M'), (50000000, '50M')]:
        print(size, name)
        with open('numbers_{0}.in'.format(name), 'w') as f:
            nums = [str(rand.randint(0, 2000000000)) for x in range(size)]
            print(str(size) + '\n', file = f)
            print(' '.join(nums), file = f)
if __name__ == '__main__':
    main()
