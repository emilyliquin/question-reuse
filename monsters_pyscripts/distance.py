import zss
from eig.monsters import Parser
from eig.monsters.program import Node

def strdist(a, b):
    if a == b:
        return 0
    else:
        return 1

class CustomNode(object):
    def __init__(self, label):
        self.my_label = label
        self.my_children = list()

    @staticmethod
    def get_children(node):
        return node.my_children

    @staticmethod
    def get_label(node):
        return node.my_label

    def addkid(self, node, before=False):
        if before:  self.my_children.insert(0, node)
        else:   self.my_children.append(node)
        return self

## function to add children
def add_children(node, children):
    if(children == None):
        return
    for child in children:
        node.addkid(CustomNode(child.ntype))
        add_children(node.my_children[-1], child.children)

#function to make a whole tree
def make_tree(q):
    parsed_q = Parser.parse(q)
    tree_q = (CustomNode(parsed_q.ntype)) # root node 
    add_children(tree_q, parsed_q.children)
    return tree_q

def get_sim(question1, question2):
    dist = zss.simple_distance(
        make_tree(question1), make_tree(question2), CustomNode.get_children, CustomNode.get_label, strdist)
    return dist