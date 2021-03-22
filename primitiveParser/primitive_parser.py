# Yacc example

import ply.yacc as yacc
import pygraphviz as pgv
import sys
# Get the token map from the lexer.  This is required.
from lexer import tokens

def new_node():
    global itr
    G.add_node(itr)
    n = G.get_node(itr)
    itr -= 1
    return n

class attribute:
    def __init__(self,dataType):
        self.dataType = dataType

class Node:
    def __init__(self,label,children=None,leaf=None):
        self.label = label
        # self.attribute = attribute
        self.node = None
        if children:
            self.children = children
        else:
            self.children = []
        self.leaf = leaf
    
    def print_val(self):
        for child in self.children:
            child.print_val()
        print(self.label)

def p_expression_plus(p):
    '''expression : expression '+' term'''
    # str(p[0].node) = str(p[1].node) + str(p[3].node)
    p[0] = Node('+',[p[1],p[3]])
    p[0].node = new_node()
    p[0].node.attr['label'] = '+'
    G.add_edge(p[0].node,p[1].node)
    G.add_edge(p[0].node,p[3].node)
    G.add_edge(p[1].node,p[3].node,style='invis')
    G.add_subgraph([p[1].node,p[3].node], rank='same')


def p_expression_minus(p):
    '''expression : expression '-' term'''
    # str(p[0].node) = str(p[1].node) - str(p[3].node)
    p[0] = Node('-',[p[1],p[3]])
    p[0].node = new_node()
    p[0].node.attr['label'] = '-'
    G.add_edge(p[0].node,p[1].node)
    G.add_edge(p[0].node,p[3].node)
    G.add_edge(p[1].node,p[3].node,style='invis')
    G.add_subgraph([p[1].node,p[3].node], rank='same')

def p_expression_term(p):
    '''expression : term'''
    # str(p[0].node) = str(p[1].node)
    p[0] = p[1]

def p_term_times(p):
    '''term : term '*' factor'''
    # str(p[0].node) = str(p[1].node) * str(p[3].node)
    p[0] = Node('*',[p[1],p[3]])
    p[0].node = new_node()
    p[0].node.attr['label'] = '*'

    G.add_edge(p[0].node,p[1].node)
    G.add_edge(p[0].node,p[3].node)
    G.add_edge(p[1].node,p[3].node,style='invis')
    G.add_subgraph([p[1].node,p[3].node], rank='same')
    for child in p[0].children: # if you want to remove the node
        G.remove_node(child.node.name)

def p_term_div(p):
    '''term : term '/' factor'''
    # str(p[0].node) = str(p[1].node) / str(p[3].node)
    p[0] = Node('/',[p[1],p[3]])
    p[0].node = new_node()
    p[0].node.attr['label'] = '/'
    G.add_edge(p[0].node,p[1].node)
    G.add_edge(p[0].node,p[3].node)
    G.add_edge(p[1].node,p[3].node,style='invis')
    G.add_subgraph([p[1].node,p[3].node], rank='same')

def p_term_factor(p):
    'term : factor'
    # str(p[0].node) = str(p[1].node)
    p[0] = p[1]

def p_factor_num(p):
    'factor : CONSTANT'
    # str(p[0].node) = str(p[1].node)
    p[0] = Node(str(p[1]))
    p[0].node = new_node()
    p[0].node.attr['label'] = str(p[1])

def p_factor_expr(p):
    '''factor : '(' expression ')' '''
    # str(p[0].node) = str(p[2].node)
    p[0] = p[2]

def p_start_state(p):
    ''' S : expression '''
    p[0] = p[1]
    p[0].print_val()

# Error rule for syntax errors
def p_error(p):
    print("Syntax error in input!")

# Build the parser
parser = yacc.yacc(start='S')

#driver code
G = pgv.AGraph(strict=True, directed=True)
itr = 0
file = open(sys.argv[1], 'r')
data = file.read()
result = parser.parse(data)
G.layout(prog="circo")
G.write("dot/test.dot")