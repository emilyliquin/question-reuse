# Adapted from https://github.com/anselmrothe/EIG, Copyright (c) 2018, MIT license

from ..program import DataType
import numpy as np

def equal(node, a, b):
    return a == b

def set_equal(node, s):
    if isinstance(s, set):
        return len(s) == 1
    elif len(s) == 0:
        return True
    else:
        t = s[0]
        for i in s:
            if not i == t: return False
        return True
            
def greater(node, a, b):
    return a > b

def less(node, a, b):
    return a < b

def plus(node, a, b):
    return a + b

def minus(node, a, b):
    return a - b

def sum_op(node, s):
    return sum(s)

def and_op(node, a, b):
    return a and b

def or_op(node, a, b):
    return a or b

def not_op(node, a):
    return not a

def set_size(node, s):
    return len(s)

def legs_fn(node, hypothesis, o):
    for object in hypothesis.objects:
        if object.object_label == o:
            return object.size
    raise RuntimeError("Object {} not found!".format(o))

def shape_fn(node, hypothesis, o):
    for object in hypothesis.objects:
        if object.object_label == o:
            return object.shape
    raise RuntimeError("Object {} not found!".format(o))

def shape_objects_fn(node, hypothesis, s):
    objects = []
    for object in hypothesis.objects:
            if object.shape == s:
                objects = np.append(objects, object)
    return set([o.object_label for o in objects])

def legs_objects_fn(node, hypothesis, s):
    objects = []
    for object in hypothesis.objects:
            if object.size == s:
                objects = np.append(objects, object)
    return set([o.object_label for o in objects])

def any_op(node, s):
    return any(s)

def all_op(node, s):
    return all(s)

def map_op(node, func, s):
    return [func(x) for x in s]

def set_op(node, hypothesis, *args):
    if len(args) == 0: return []
    if node.dtype == DataType.SET_O:
        return set(args)
    else:
        return list(args)

def union(node, s1, s2):
    if isinstance(s1, list): s1 = set(s1)
    if isinstance(s2, list): s2 = set(s2)
    return s1 | s2

def intersect(node, s1, s2):
    if isinstance(s1, list): s1 = set(s1)
    if isinstance(s2, list): s2 = set(s2)
    return s1 & s2

def unique(node, s):
    return set(s)