# Adapted from https://github.com/anselmrothe/EIG, Copyright (c) 2018, MIT license

"""
Program structured as a tree.
"""
from enum import Enum

LABELS = {'Square': 1, 'Circle': 2}

class NodeConfig:
    """
    Configs of each kind of node,
    including node type, number of parameters, evaluated data type, accepted data type of parameters

    params_dtypes accepts two formats:
    a tuple (e.g. (DataType.NUMBER, ) or (DataType.LAMBDA_X, DataType.BOOLEAN)), whose length is 1 or the same
            as number of params, means types of params must be exactly in correspondance of types in the tuple.
            Length 1 means all parameters have the same type;
    a list of tuples (e.g. [(DataType.LAMBDA_FXB, DataType.SET_B), (DataType.LAMBDA_FXN, DataType.SET_N)]) 
            means the combination of params types can be any of the tuples in the list.

    dtype also accepts two formats:
    a single DataType object, means this function always evaluates to this type;
    a list of DataType objects. This can only be used when params_dtypes is a list, and the length of this tuple
            should be the same with param_dtypes. This means the evaluted type is determined by type of paramters.
            The i-th combination of paramters in list param_dtype will result in the i-th type in list dtype.
    """
    def __init__(self, ntype, param_num, dtype, param_dtypes):
        self.ntype = ntype
        self.param_num = param_num
        self.dtype = dtype
        self.param_dtypes = param_dtypes


class DataType(Enum):
    NUMBER = 1
    BOOLEAN = 2
    SHAPE = 3
    OBJECT = 4
    LAMBDA_X = 5
    LAMBDA_FXB = 6
    LAMBDA_FXN = 7
    LAMBDA_FXS = 8
    SET_S = 9
    SET_B = 10
    SET_N = 11
    SET_O = 12
    SET_LITERAL_O = 13


# mapping from ntype to config
NODES = {
    # basic functions
    'equal': NodeConfig('equal', 2, DataType.BOOLEAN, [(DataType.BOOLEAN, ), (DataType.NUMBER, ), 
                                                       (DataType.SHAPE, ), ]),
    'set_equal': NodeConfig('set_equal', 1, DataType.BOOLEAN, [(DataType.SET_N, ),
    (DataType.SET_S, )]),
    'greater': NodeConfig('greater', 2, DataType.BOOLEAN, (DataType.NUMBER, )),
    'less': NodeConfig('less', 2, DataType.BOOLEAN, (DataType.NUMBER, )),
    'plus': NodeConfig('plus', 2, DataType.NUMBER, [(DataType.NUMBER, ), (DataType.BOOLEAN, )]),
    'minus': NodeConfig('minus', 2, DataType.NUMBER, (DataType.NUMBER, )),
    'sum_op': NodeConfig('sum_op', 1, DataType.NUMBER, [(DataType.SET_N, ), (DataType.SET_B, )]),
    'and_op': NodeConfig('and_op', 2, DataType.BOOLEAN, (DataType.BOOLEAN, )),
    'or_op': NodeConfig('or_op', 2, DataType.BOOLEAN, (DataType.BOOLEAN, )),
    'not_op': NodeConfig('not_op', 1, DataType.BOOLEAN, (DataType.BOOLEAN, )),


    # board functions
    'legs_fn': NodeConfig('legs_fn', 1, DataType.NUMBER, (DataType.OBJECT, )),
    'shape_fn': NodeConfig('shape_fn', 1, DataType.SHAPE, (DataType.OBJECT, )),
    'shape_objects_fn': NodeConfig('shape_objects_fn', 1, DataType.SET_O, (DataType.SHAPE, )),
    'legs_objects_fn': NodeConfig('legs_objects_fn', 1, DataType.SET_O, (DataType.NUMBER, )),

    # set functions
    'any_op': NodeConfig('any_op', 1, DataType.BOOLEAN, (DataType.SET_B, )),
    'all_op': NodeConfig('all_op', 1, DataType.BOOLEAN, (DataType.SET_B, )),
    'map_op': NodeConfig('map_op', 2, [DataType.SET_B, DataType.SET_N, DataType.SET_S], 
                    [(DataType.LAMBDA_FXB, DataType.SET_O), (DataType.LAMBDA_FXN, DataType.SET_O), (DataType.LAMBDA_FXS, DataType.SET_O)]),
    'set_op': NodeConfig('set_op', -1, [DataType.SET_O, DataType.SET_O], 
                    [(DataType.SET_LITERAL_O, ), (DataType.OBJECT, )]),
    'set_size': NodeConfig('set_size', 1, DataType.NUMBER, [(DataType.SET_O, ), (DataType.SET_N, ), (DataType.SET_S, )]),
    'union': NodeConfig('union', 2, DataType.SET_O, (DataType.SET_O, )),
    'intersect': NodeConfig('intersect', 2, DataType.SET_O, (DataType.SET_O, )),
    'unique': NodeConfig('unique', 1, [DataType.SET_O, DataType.SET_N, DataType.SET_S], [(DataType.SET_O, ), (DataType.SET_N, ), (DataType.SET_S, )]),

    'lambda_op': NodeConfig('lambda_op', 2, [DataType.LAMBDA_FXB, DataType.LAMBDA_FXN, DataType.LAMBDA_FXS], 
                    [(DataType.LAMBDA_X, DataType.BOOLEAN),
                     (DataType.LAMBDA_X, DataType.NUMBER),
                     (DataType.LAMBDA_X, DataType.SHAPE)]),

    # literals
    'number': NodeConfig('number', 0, DataType.NUMBER, None), 
    'boolean': NodeConfig('boolean', 0, DataType.BOOLEAN, None), 
    'shape': NodeConfig('shape', 0, DataType.SHAPE, None),
    'object': NodeConfig('object', 0, DataType.OBJECT, None),
    'lambda_x': NodeConfig('lambda_x', 0, DataType.LAMBDA_X, None),
}

# mapping from symbol to ntype for functions
FUNC_NTYPES = {
    # basic functions
    '==': 'equal',
    '===': 'set_equal',
    '>': 'greater',
    '<': 'less',
    '+': 'plus',
    '-': 'minus',
    '++': 'sum_op',
    'and': 'and_op',
    'or': 'or_op',
    'not': 'not_op',

    # board functions
    'legs': 'legs_fn',
    'shape': 'shape_fn',
    'shapeObjects': 'shape_objects_fn',
    'legsObjects': 'legs_objects_fn',

    # set functions
    'any': 'any_op',
    'all': 'all_op',
    'map': 'map_op',
    'set': 'set_op',
    'setSize': 'set_size',
    'union': 'union',
    'intersection': 'intersect',
    'unique': 'unique',
    'lambda': 'lambda_op',
}

class ProgramSyntaxError(Exception):
    def __init__(self, program, error_msg="Syntax Error"):
        self.program = program
        self.error_msg = error_msg

    def __str__(self):
        # TODO: Better program error location suggestion
        # maybe raise a location during recursive parsing, and package the real exception at top level
        return "Error found in the program: \n {} around '{}'".format(self.error_msg, self.program)


class Node:
    def __init__(self, ntype, children, prog):
        self.ntype = ntype
        self.children = children
        self.prog = prog

    def to_dict(self):
        return {'type': self.ntype,
                'children': [c.to_dict() for c in self.children]}


class LiteralNode(Node):
    def __init__(self, ntype, value, prog=None):
        super().__init__(ntype, None, prog or value)
        self.value = value

    def to_dict(self):
        return {'type': self.ntype,
                'value': self.value}


class LambdaVarNode(Node):
    def __init__(self, var, value):
        super().__init__('lambda_{}'.format(var), None, value)
        self.value = value
    
    def to_dict(self):
        return {'type': self.ntype,
                'value': self.value}
