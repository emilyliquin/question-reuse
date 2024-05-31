# Adapted from https://github.com/anselmrothe/EIG, Copyright (c) 2018, MIT license

import numpy as np
from itertools import permutations, product

from ...hypothesis import HypothesisSpace

class Object:

    def __init__(self, object_label, color, shape, size):
        self.shape = shape
        self.size = size
        self.color = color
        self.object_label = object_label


class ObjectSetHypothesis:

    def __init__(self, objects):
        self.objects = objects
        self.object_cnt = len(objects)
        set = np.zeros(len(objects)*2).reshape(len(objects),2)
        for object in objects:
            # fill in board with object properties
            if object.color == "red":
                set[0] = [object.shape, object.size]
            if object.color == "blue":
                set[1] = [object.shape, object.size]
            if object.color == "purple":
                set[2] = [object.shape, object.size]
        self.set = set



class ObjectSetHypothesisSpace(HypothesisSpace):

    def create_hypothesis_space(self, object_colors, object_sizes, object_shapes, observation=None):
        """
        object_colors, object_sizes, object_shapes: list
        """
        if observation:
            raise KeyError("Argument 'observation' is not supported in Python version. Please install the cython version")


        def object_generator():
                for size in object_sizes:
                    for shape in object_shapes:
                        yield (shape, size)
        
        hypotheses = []
        for perm in product(object_generator(), repeat = len(object_colors)):

            objects = []
            for label, config in zip(object_colors, perm):
                objects.append(Object(label[0], label, *config))

            try:
                hypothesis = ObjectSetHypothesis(objects)
            except ValueError:
                continue
            hypotheses.append(hypothesis)
        return hypotheses

    def match(self, i, observation):
        """
        The observation is a numpy array representing a board, where -1 indicates hidden.
        We need to check all locations except -1, and see if the hypothesis agrees with 
        the observation.
        """
        # both hypothesis.set and observation are numpy arrays, so we can use element-wise 
        # logical operations in numpy to efficiently compare them.
        hypothesis = self.hypotheses[i].set
        return np.all(np.logical_or(observation < 0, np.equal(hypothesis, observation)))

    def execute_on_subspace(self, executor, subset_id):
        answers = []
        for id in subset_id:
            answers.append(executor.execute(self.hypotheses[id]))
        return answers


