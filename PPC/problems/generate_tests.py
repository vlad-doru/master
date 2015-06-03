import numpy
import random
import sys
from math import sqrt

def distance(a, b):
    x_a, y_a = a
    x_b, y_b = b
    return  sqrt((x_a -x_b)*(x_a - x_b) + (y_a - y_b)*(y_a - y_b))


for n in xrange(9, 13):
    f = open("input_{0}.txt".format(n), "w")
    f.write(str(n) + "\n")
    points = [(random.randint(0, 30), random.randint(0, 30)) for i in xrange(n)]
    for i in xrange(n):
        for j in xrange(n):
            f.write(str(round(distance(points[i], points[j]), 2)) + "\t")
        f.write("\n")
    f.close()

