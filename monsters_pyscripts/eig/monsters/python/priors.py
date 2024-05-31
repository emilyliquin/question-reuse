# Adapted from https://github.com/anselmrothe/EIG, Copyright (c) 2018, MIT license

from ...bayes import Distribution, normalize
import numpy as np

class UniformDistribution(Distribution):
    def __init__(self):
        pass

    def __call__(self, hypotheses):
        size = len(hypotheses)
        return np.ones(size) / size