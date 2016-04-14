import numpy.random as rand

def main():
    for size, name in [(1000, '1K'), (10000, '10K'), (100000, '100K'), (1000000, '1M'), (10000000, '10M')]:
        print(size, name)
        with open('numbers_{0}.in'.format(name), 'w') as f:
            nums = [str(rand.randint(0, 1 << 31)) for x in range(size)]
            print(' '.join(nums), file = f)
if __name__ == '__main__':
    main()
