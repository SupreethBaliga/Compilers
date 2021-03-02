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

def p_expression_plus(p):
    '''expression : expression '+' term'''
    # str(p[0]) = str(p[1]) + str(p[3])
    
    p[0] = new_node()
    p[0].attr['label'] = 'expression'
    
    
    p[2] = new_node()
    p[2].attr['label'] ='+'
    
    G.add_edge(p[0],p[1])
    G.add_edge(p[0],p[2])
    G.add_edge(p[0],p[3])
    G.add_edge(p[1],p[2],style='invis')
    G.add_edge(p[2],p[3],style='invis')
    G.add_subgraph([p[1],p[2],p[3]], rank='same')

def p_expression_minus(p):
    '''expression : expression '-' term'''
    # str(p[0]) = str(p[1]) - str(p[3])
    p[0] = new_node()
    p[0].attr['label'] = 'expression'
    
    
    p[2] = new_node()
    p[2].attr['label'] ='-'
    
    G.add_edge(p[0],p[1])
    G.add_edge(p[0],p[2])
    G.add_edge(p[0],p[3])
    G.add_edge(p[1],p[2],style='invis')
    G.add_edge(p[2],p[3],style='invis')
    G.add_subgraph([p[1],p[2],p[3]], rank='same')

def p_expression_term(p):
    '''expression : term'''
    # str(p[0]) = str(p[1])
    p[0] = new_node()
    p[0].attr['label'] = 'expression'
    
    G.add_edge(p[0],p[1])

def p_term_times(p):
    '''term : term '*' factor'''
    # str(p[0]) = str(p[1]) * str(p[3])
    p[0] = new_node()
    p[0].attr['label'] = 'term'
    
    
    p[2] = new_node()
    p[2].attr['label'] ='*'
    
    G.add_edge(p[0],p[1])
    G.add_edge(p[0],p[2])
    G.add_edge(p[0],p[3])
    G.add_edge(p[1],p[2],style='invis')
    G.add_edge(p[2],p[3],style='invis')
    G.add_subgraph([p[1],p[2],p[3]], rank='same')

def p_term_div(p):
    '''term : term '/' factor'''
    # str(p[0]) = str(p[1]) / str(p[3])
    p[0] = new_node()
    p[0].attr['label'] = 'term'
    
    
    p[2] = new_node()
    p[2].attr['label'] ='/'
    
    G.add_edge(p[0],p[1])
    G.add_edge(p[0],p[2])
    G.add_edge(p[0],p[3])
    G.add_edge(p[1],p[2],style='invis')
    G.add_edge(p[2],p[3],style='invis')
    G.add_subgraph([p[1],p[2],p[3]], rank='same')

def p_term_factor(p):
    'term : factor'
    # str(p[0]) = str(p[1])
    p[0] = new_node()
    p[0].attr['label'] = 'term'

    G.add_edge(p[0],p[1])

def p_factor_num(p):
    'factor : CONSTANT'
    # str(p[0]) = str(p[1])
    p[0] = new_node()
    p[0].attr['label'] = 'factor'
    
    p1val = p[1]
    p[1] = new_node()
    p[1].attr['label'] = str(p1val)
    
    G.add_edge(p[0],p[1])
    

def p_factor_expr(p):
    '''factor : '(' expression ')' '''
    # str(p[0]) = str(p[2])
    p[0] = new_node()
    p[0].attr['label'] = 'expression'
    
    
    p[1] = new_node()
    p[1].attr['label'] ='('
    
    p[3] = new_node()
    p[3].attr['label'] = ')'

    G.add_edge(p[0],p[1])
    G.add_edge(p[0],p[2])
    G.add_edge(p[0],p[3])
    G.add_edge(p[1],p[2],style='invis')
    G.add_edge(p[2],p[3],style='invis')
    G.add_subgraph([p[1],p[2],p[3]], rank='same')

# Error rule for syntax errors
def p_error(p):
    print("Syntax error in input!")

# Build the parser
parser = yacc.yacc()

#driver code
G = pgv.AGraph(strict=True, directed=True)
itr = 0
file = open(sys.argv[1], 'r')
data = file.read()
result = parser.parse(data)
G.layout(prog="circo")
G.write("dot/test1.dot")