# import necessary libraries
import ply.yacc as yacc
import pygraphviz as pgv
import sys

# Get the token map from lexer
from lexerClass import CLexer
from SymbolTable import SymbolTable
from TypeTable import TypeTable

############## Helper Functions ###########
def new_node():
    global itr
    G.add_node(itr)
    n = G.get_node(itr)
    itr += 1
    return n

def remove_node(graphNode):
    G.remove_node(graphNode)

########### Classes Required ###########

# This class denotes the Node of our Functional AST
class Node:
    def __init__(self,label,children=None,node=None,attributes=None,createAST = True, type=None, isvar = False):
        self.label = label
        self.createAST = createAST
        self.node = node
        self.type = type
        self.isvar = isvar
        self.isTerminal = False
        if children is None:
            self.isTerminal = True
        if children:
            self.children = children
        else:
            self.children = []

        if attributes:
            self.attributes = attributes
        else:
            self.attributes = {}
        
        self.attributes["err"] = False  # determines if AST subtree has an error

        if (self.createAST == True) :
            self.makeGraph()
    
        self.variables = dict()
        # The key of the dictionary  will be variable name and the value will be a tuple consisting of type
        self.extraValues = []
    def addTypeInDict(self,type):
        '''
        Add "type" to all variables in the dictionary
        '''
        for key in self.variables.keys():
            self.variables[key].append(type)
    # def print_val(self):
    #     for child in self.children:
    #         child.print_val()
    #     print(self.label)
    
    # def should_make_node(self):
    #     for child in self.children:
    #         if child.node:
    #             return True
    #     return False
    def removeGraph(self):
        for child in self.children:
            if child.node :
                child.removeGraph()
        remove_node(self.node)
        self.node = None
    
    def makeGraph(self): # for creating the dot dump
        if self.isTerminal:
            self.node = new_node()
            self.node.attr['label'] = self.label
            return

        newchildren = []
        for child in self.children:
            if ((child is not None) and (child.node is not None)):
                newchildren.append(child)
        self.children = newchildren

        if self.children:
            self.node = new_node()
            self.node.attr['label'] = self.label
            listNode = []
            for child in self.children:
                G.add_edge(self.node,child.node)
                listNode.append(child.node)
            for i in range(0,len(self.children)-1):
                G.add_edge(self.children[i].node,self.children[i+1].node,style='invis')

            G.add_subgraph(listNode,rank='same')

# This denotes an entry of the symbol table
# class SymTabEntry:

#     def __init__(self, name, type=None, attributes=None):
#         self.name = name
#         if type:
#             self.type = type
#         else:
#             self.type = None
        
#         if attributes:
#             self.attributes = attributes
#         else:
#             attributes = {}

# ######## Important Global Variables

# symtab = {}  # right now a global var. If class based parser, then it will become an attribute
ast_root = None # this will contain the root of the AST after it is built
############## Grammar Rules ##############
### Might have to convert it into class based code
ST = SymbolTable()
TT = TypeTable()

def p_primary_expression_1(p):
    '''
    primary_expression : ID
    '''
    found, entry = ST.ReturnSymTabEntry(p[1]['lexeme'], p.lineno(1))
    if found: # Change this accordingly
        
        p[0] = Node(str(p[1]['lexeme']))
        type_list = entry['type']
        p[0].isvar = 1

        p[0].type = []
        if 'long' in type_list:
            p[0].type.append('long int')
            for single_type in type_list:
                if single_type != 'long' and single_type != 'int':
                    p[0].type.append(single_type)
        
        elif 'int' in type_list:
            p[0].type.append('int')
            for single_type in type_list:
                if single_type != 'int':
                    p[0].type.append(single_type)
        
        elif 'char' in type_list:
            p[0].type.append('char')
            for single_type in type_list:
                if single_type != 'char':
                    p[0].type.append(single_type)
        
        elif 'str' in type_list:
            p[0].type.append('str')
            for single_type in type_list:
                if single_type != 'str':
                    p[0].type.append(single_type)
        
        elif 'float' in type_list:
            p[0].type.append('float')
            for single_type in type_list:
                if single_type != 'float':
                    p[0].type.append(single_type)
        
        elif 'double' in type_list:
            p[0].type.append('double')
            for single_type in type_list:
                if single_type != 'double':
                    p[0].type.append(single_type)



def p_primary_expression(p):
    '''
    primary_expression : IntegerConst
                       | FloatConst
                       | CharConst
                       | StringConst
                       | '(' expression ')'
    '''
    # AST Done
    if (len(p) == 2):
        p[0] = p[1]
    elif (len(p) == 4):
        p[0] = p[2]

def p_identifer(p):
    '''
    identifier : ID
    '''
    p[0] = Node(str(p[1]['lexeme']))
    p[0].variables[p[0].label] = []
    p[0].isvar = 1
    ST.InsertSymbol(p[1]['lexeme'], p[1]['additional']['line'])
    ST.ModifySymbol(p[1]['lexeme'], "check", "VAR")


def p_IntegerConst(p):
    '''
    IntegerConst : INT_CONSTANT
    '''
    p[0] = Node(str(p[1]))
    p[0].type = ['int']
    


def p_FloatConst(p):
    '''
    FloatConst : FLOAT_CONSTANT
    '''
    p[0] = Node(str(p[1]))
    p[0].type = ['float']



def p_CharConst(p):
    '''
    CharConst : CHAR_CONSTANT
    '''
    p[0] = Node(str(p[1]))
    p[0].type = ['char']




def p_StringConst(p):
    '''
    StringConst : STRING_LITERAL
    '''
    p[0] = Node(str(p[1]))
    p[0].type = ['str']



def p_postfix_expression(p):
    '''
    postfix_expression : primary_expression
                       | postfix_expression INC_OP
                       | postfix_expression DEC_OP
                       | postfix_expression '.' ID
                       | postfix_expression '(' ')'
                       | postfix_expression PTR_OP ID
                       | postfix_expression '[' expression ']'
                       | postfix_expression '(' argument_expression_list ')'
    '''
    # AST Done - see sheet for rules 2-postinc,3-postdec 5,7 and 8
    if (len(p) == 2):
        p[0] = p[1]
    elif (len(p) == 3):
        global isError
        if p[1].type == None:
            isError = 1
            print(f'Cannot increase/decrease value of expression at line {p.lineno(2)}')

        elif 'const' in p[1].type:
            isError = 1
            # print(p[1].isTerminal)
            print(f'Cannot increase/decrease value of read only variable at line {p.lineno(2)}')

        elif p[1].type[0]!= 'int' and p[1].type[0]!= 'long int' and p[1].type[0]!= 'char':
            isError = 1
            # print(p[1].isTerminal)
            print(f'Cannot use increment/decrement operator on non-integral at line {p.lineno(2)}')
        elif p[1].isTerminal == False:
            isError = 1
            print(f'Cannot use increment/decrement operator on expression at line {p.lineno(2)}')
        elif p[1].isvar == 0:
            isError = 1
            print(f'Cannot use increment/decrement operator on constant at line {p.lineno(2)}')
        else:
            p[0] = Node('POST' + str(p[2]),[p[1]])
            p[0].type = p[1].type

    elif (len(p) == 4):
        if p[2] == '.':
            p3val = p[3]['lexeme']
            p[3] = Node(str(p3val))

            p[0] = Node('.',[p[1],p[3]])
            # type of p[0]?

        elif p[2] == '(':
            p[0] = Node('FuncCall',[p[1]])
            # type of p[0]?

        elif p[2] == '->':
            p3val = p[3]['lexeme']
            p[3] = Node(str(p3val))

            p[0] = Node('->',[p[1],p[3]])
            # type of p[0]?

    elif (len(p) == 5):
        if p[2] == '(':
            p[0] = Node('FuncCall',[p[1],p[3]])
        elif p[2] == '[':
            p[0] = Node('ArrSub',[p[1],p[3]])
            # type of p[0]?

def p_argument_expression_list(p):
    '''
    argument_expression_list : assignment_expression
	                         | argument_expression_list ',' assignment_expression
    '''
    # AST Done
    if (len(p) == 2):
        p[0] = p[1]
    elif (len(p) == 4):
        p[0] = Node(',',[p[1],p[3]])

def p_unary_expression(p):
    '''
    unary_expression : postfix_expression
                     | INC_OP unary_expression
                     | DEC_OP unary_expression
                     | SIZEOF unary_expression
                     | unary_operator cast_expression
                     | SIZEOF '(' type_name ')'
    '''
    # AST DONE - check sheet for rule 2- preinc,3- predec,5
    if (len(p) == 2):
        p[0] = p[1]
    elif (len(p) == 3):
        if p[1] == '++' or p[1] == '--':
            global isError
            if p[2].type == None:
                isError = 1
                print(f'Cannot increase/decrease value of expression at line {p.lineno(1)}')
            elif 'const' in p[2].type:
                isError = 1
                # print(p[1].isTerminal)
                print(f'Cannot increase/decrease value of read only variable at line {p.lineno(1)}')

            elif p[2].type[0]!= 'int' and p[2].type[0]!= 'long int' and p[2].type[0]!= 'char':
                isError = 1
                # print(p[1].isTerminal)
                print(f'Cannot use increment/decrement operator on non-integral at line {p.lineno(1)}')
            elif p[2].isTerminal == False:
                isError = 1
                print(f'Cannot use increment/decrement operator on expression at line {p.lineno(1)}')
            elif p[2].isvar == 0:
                isError = 1
                print(f'Cannot use increment/decrement operator on constant at line {p.lineno(1)}')
            else:
                p[0] = Node('PRE' + str(p[1]),[p[2]])
                p[0].type = p[2].type

        elif p[1] == 'sizeof':
            p[0] = Node('SIZEOF',[p[2]])
            p[0].type = ['int']
        else:
            p[0] = p[1]
            if ((p[2] is not None) and (p[2].node is not None)):
                p[0].children.append(p[2])
                G.add_edge(p[0].node,p[2].node)
            # Stuff to be added here, what should the type of p[0] be

    elif (len(p) == 5):
        p[0] = Node('SIZEOF',[p[3]])
        p[0].type = ['int']

def p_unary_operator(p):
    '''
    unary_operator : '&'
                   | '*'
                   | '+'
                   | '-'
                   | '~'
                   | '!'
    '''
    # AST DONE
    p[0] = Node('UNARY' + str(p[1]))

def p_cast_expression(p):
    '''
    cast_expression : unary_expression
	                | '(' type_name ')' cast_expression
    '''
    #AST DONE - rule for 2 in sheet
    if (len(p) == 2):
        p[0] = p[1]
    elif (len(p) == 5):
        p[0] = Node('CAST',[p[2],p[4]])

        # Change this for pointers
        p[0].type = p[2].type

def p_mulitplicative_expression(p):
    '''
    multiplicative_expression : cast_expression
	                          | multiplicative_expression '*' cast_expression
	                          | multiplicative_expression '/' cast_expression
	                          | multiplicative_expression '%' cast_expression
    '''
    #AST DOne
    if (len(p) == 2):
        p[0] = p[1]
    elif (len(p) == 4):
        p[0] = Node(str(p[2]),[p[1],p[3]])
        
        global isError
        if p[1].type == None or p[3].type == None:
            isError = 1
            print(f'Cannot perform multiplicative operation between expressions on line {p.lineno(2)}')

        elif p[1].type[0] in ['int', 'long int', 'char', 'float', 'double'] and p[3].type[0] in ['int', 'long int', 'char', 'float', 'double']:
            if p[1].type[0] == 'double' or p[3].type[0] == 'double':
                p[0].type = 'double'
            elif p[1].type[0] == 'float' or p[3].type[0] == 'float':
                p[0].type = 'float'
            elif p[1].type[0] == 'long int' or p[3].type[0] == 'long int':
                p[0].type = 'long int'
            elif p[1].type[0] == 'int' or p[3].type[0] == 'int':
                p[0].type = 'int'
            elif p[1].type[0] == 'char' or p[3].type[0] == 'char':
                p[0].type = 'char'
            else:
                isError = 1
                print(f'Cannot perform multiplicative operation between expressions on line {p.lineno(2)}')

            p[0].label = p[0].label + p[0].type
            p[0].node.attr['label'] = p[0].label
            p[0].type = [p[0].type]
        else :
            isError = 1
            print(f'Multiplictaive operation between incompatible types {p[1].type} and {p[3].type} on line {p.lineno(2)}')
        


        


def p_additive_expression(p):
    '''
    additive_expression : multiplicative_expression
	                    | additive_expression '+' multiplicative_expression
	                    | additive_expression '-' multiplicative_expression
    '''
    # AST DOne
    
    if (len(p) == 2):
        p[0] = p[1]
    elif (len(p) == 4):
        p[0] = Node(str(p[2]),[p[1],p[3]])
        
        global isError
        if p[1].type == None or p[3].type == None:
            isError = 1
            print(f'Cannot perform additive operation between expressions on line {p.lineno(2)}')

        elif p[1].type[0] in ['int', 'long int', 'char', 'float', 'double'] and p[3].type[0] in ['int', 'long int', 'char', 'float', 'double']:
            if p[1].type[0] == 'double' or p[3].type[0] == 'double':
                p[0].type = 'double'
            elif p[1].type[0] == 'float' or p[3].type[0] == 'float':
                p[0].type = 'float'
            elif p[1].type[0] == 'long int' or p[3].type[0] == 'long int':
                p[0].type = 'long int'
            elif p[1].type[0] == 'int' or p[3].type[0] == 'int':
                p[0].type = 'int'
            elif p[1].type[0] == 'char' or p[3].type[0] == 'char':
                p[0].type = 'char'
            else:
                isError = 1
                print(f'Cannot perform additive operation between expressions on line {p.lineno(2)}')

            p[0].label = p[0].label + p[0].type
            p[0].node.attr['label'] = p[0].label
            p[0].type = [p[0].type]
        else :
            isError = 1
            print(f'Additive operation between incompatible types {p[1].type} and {p[3].type} on line {p.lineno(2)}')
        

def p_shift_expression(p):
    '''
    shift_expression : additive_expression
	                 | shift_expression LEFT_OP additive_expression
	                 | shift_expression RIGHT_OP additive_expression
    '''
    #AST DOne
    if (len(p) == 2):
        p[0] = p[1]
    elif (len(p) == 4):
        
        

        global isError
        if p[1].type == None or p[3].type == None:
            isError = 1
            print(f'Cannot perform bitshift operation between expressions on line {p.lineno(2)}')

        elif p[1].type[0] in ['int', 'long int', 'char'] and p[3].type[0] in ['int', 'long int', 'char']:
            p[0] = Node(str(p[2]),[p[1],p[3]])
            p[0].type = ['int']

        else:
            isError = 1
            print(f'Bitshift operation between incompatible types {p[1].type} and {p[3].type} on line {p.lineno(2)}')




def p_relational_expression(p):
    '''
    relational_expression : shift_expression
	                      | relational_expression '<' shift_expression
	                      | relational_expression '>' shift_expression
	                      | relational_expression LE_OP shift_expression
	                      | relational_expression GE_OP shift_expression
    '''
    # AST Done
    if (len(p) == 2):
        p[0] = p[1]
    elif (len(p) == 4):
        global isError
        if p[1].type == None or p[3].type == None:
            isError = 1
            print(f'Cannot perform relational operation between expressions on line {p.lineno(2)}')

        elif p[1].type[0] in ['int', 'long int', 'char', 'float', 'double'] and p[3].type[0] in ['int', 'long int', 'char', 'float', 'double'] :
            p[0] = Node(str(p[2]),[p[1],p[3]])
            p[0].type = ['int']
            if p[1].type[0] in ['float', 'double'] or p[3].type[0] in ['float', 'double']:
                if p[1].type[0] == 'double' or p[3].type[0] == 'double':
                    p[0].type = ['double']
                else:
                    p[0].type = ['float']
        elif p[1].type[0] == 'str' and p[3].type[0] == 'str':
            p[0] = Node(str(p[2]),[p[1],p[3]])
            p[0].type = ['int']

        else:
            isError = 1
            print(f'Relational operation between incompatible types {p[1].type} and {p[3].type} on line {p.lineno(2)}')


# 10 rules done till here

def p_equality_expression(p):
    '''
    equality_expression : relational_expression
	                    | equality_expression EQ_OP relational_expression
	                    | equality_expression NE_OP relational_expression
    '''
    # AST Done
    if (len(p) == 2):
        p[0] = p[1]
    elif (len(p) == 4):
        global isError
        if p[1].type == None or p[3].type == None:
            isError = 1
            print(f'Cannot perform equality check operation between expressions on line {p.lineno(2)}')

        elif p[1].type[0] in ['int', 'long int', 'char', 'float', 'double'] and p[3].type[0] in ['int', 'long int', 'char', 'float', 'double'] :
            p[0] = Node(str(p[2]),[p[1],p[3]])
            p[0].type = ['int']
            if p[1].type[0] in ['float', 'double'] or p[3].type[0] in ['float', 'double']:
                if p[1].type[0] == 'double' or p[3].type[0] == 'double':
                    p[0].type = ['double']
                else:
                    p[0].type = ['float']
        elif p[1].type[0] == 'str' and p[3].type[0] == 'str':
            p[0] = Node(str(p[2]),[p[1],p[3]])
            p[0].type = ['int']

        else:
            isError = 1
            print(f'Equality check operation between incompatible types {p[1].type} and {p[3].type} on line {p.lineno(2)}')

def p_and_expression(p):
    '''
    and_expression : equality_expression
	               | and_expression '&' equality_expression
    '''
    #AST done
    if (len(p) == 2):
        p[0] = p[1]
    elif (len(p) == 4):
        global isError
        if p[1].type == None or p[3].type == None:
            isError = 1
            print(f'Cannot perform bitiwise and between expressions on line {p.lineno(2)}')

        elif p[1].type[0] in ['int', 'long int', 'char'] and p[3].type[0] in ['int', 'long int', 'char']:
            p[0] = Node(str(p[2]),[p[1],p[3]])
            p[0].type = ['int']

        else:
            isError = 1
            print(f'Bitwise and operation between incompatible types {p[1].type} and {p[3].type} on line {p.lineno(2)}')


def p_exclusive_or_expression(p):
    '''
    exclusive_or_expression : and_expression
	                        | exclusive_or_expression '^' and_expression
    '''
    #AST done
    if (len(p) == 2):
        p[0] = p[1]
    elif (len(p) == 4):
        global isError
        if p[1].type == None or p[3].type == None:
            isError = 1
            print(f'Cannot perform bitwise xor between expressions on line {p.lineno(2)}')

        elif p[1].type[0] in ['int', 'long int', 'char'] and p[3].type[0] in ['int', 'long int', 'char']:
            p[0] = Node(str(p[2]),[p[1],p[3]])
            p[0].type = ['int']

        else:
            isError = 1
            print(f'Bitwise xor operation between incompatible types {p[1].type} and {p[3].type} on line {p.lineno(2)}')
def p_inclusive_or_expression(p):
    '''
    inclusive_or_expression : exclusive_or_expression
	                        | inclusive_or_expression '|' exclusive_or_expression
    '''
    #AST done
    if (len(p) == 2):
        p[0] = p[1]
    elif (len(p) == 4):
        global isError
        if p[1].type == None or p[3].type == None:
            isError = 1
            print(f'Cannot perform bitwise or between expressions on line {p.lineno(2)}')

        elif p[1].type[0] in ['int', 'long int', 'char'] and p[3].type[0] in ['int', 'long int', 'char']:
            p[0] = Node(str(p[2]),[p[1],p[3]])
            p[0].type = ['int']

        else:
            isError = 1
            print(f'Bitwise or operation between incompatible types {p[1].type} and {p[3].type} on line {p.lineno(2)}')
def p_logical_and_expression(p):
    '''
    logical_and_expression : inclusive_or_expression
	                       | logical_and_expression AND_OP inclusive_or_expression
    '''
    #AST done
    if (len(p) == 2):
        p[0] = p[1]
    elif (len(p) == 4):
        global isError
        if p[1].type == None or p[3].type == None:
            isError = 1
            print(f'Cannot perform logical and between expressions on line {p.lineno(2)}')

        elif p[1].type[0] in ['int', 'long int', 'char', 'float', 'double', 'str'] and p[3].type[0] in ['int', 'long int', 'char', 'float', 'double', 'str']:
            p[0] = Node(str(p[2]),[p[1],p[3]])
            p[0].type = ['int']

        else:
            isError = 1
            print(f'Logical and operation between incompatible types {p[1].type} and {p[3].type} on line {p.lineno(2)}')
def p_logical_or_expression(p):
    '''
    logical_or_expression : logical_and_expression
	                      | logical_or_expression OR_OP logical_and_expression
    '''
    #AST done
    if (len(p) == 2):
        p[0] = p[1]
    elif (len(p) == 4):
        global isError
        if p[1].type == None or p[3].type == None:
            isError = 1
            print(f'Cannot perform logical or between expressions on line {p.lineno(2)}')

        elif p[1].type[0] in ['int', 'long int', 'char', 'float', 'double', 'str'] and p[3].type[0] in ['int', 'long int', 'char', 'float', 'double', 'str']:
            p[0] = Node(str(p[2]),[p[1],p[3]])
            p[0].type = ['int']

        else:
            isError = 1
            print(f'Logical or operation between incompatible types {p[1].type} and {p[3].type} on line {p.lineno(2)}')

def p_conditional_expression(p):
    '''
    conditional_expression : logical_or_expression
	                       | logical_or_expression '?' expression ':' conditional_expression
    '''
    # AST Done
    if (len(p) == 2):
        p[0] = p[1]
    elif (len(p) == 6):
        p[0] = Node('TERNARY',[p[1],p[3],p[5]])

def p_assignment_expression(p):
    '''
    assignment_expression : conditional_expression
	                      | unary_expression assignment_operator assignment_expression
    '''
    # AST Done
    if (len(p) == 2):
        p[0] = p[1]
    elif (len(p) == 4):
        p[0] = p[2]
        if ((p[1] is not None) and (p[1].node is not None)):
            if ((p[3] is not None) and (p[3].node is not None)):

                
                
                global isError
                if p[1].isvar == 0:
                    isError = 1
                    print(f'Left hand side has to be a variable at line {p[2].lineno}')

                elif 'const' in p[1].type:
                    isError = 1
                    print(f'Cannot assign value to read only variable at line {p[2].lineno}')

                # Type mismatch arrays etc.
                # elif p[1] is array/pointer, then no assignment

                else:
                    G.add_edge(p[0].node,p[1].node)
                    G.add_edge(p[0].node,p[3].node)

                    G.add_edge(p[1].node,p[3].node,style='invis')
                    G.add_subgraph([p[1].node,p[3].node], rank='same')
                    p[0].children.append(p[1])
                    p[0].children.append(p[3])

            else:
                G.add_edge(p[0].node,p[1].node)
                p[0].children.append(p[1])
                # Complete when p[3] may be None

        else:
            if ((p[3] is not None) and (p[3].node is not None)):
                G.add_edge(p[0].node,p[3].node)
                p[0].children.append(p[3])
                # Complete when p[1] may be None
                
def p_assignment_operator(p):
    '''
    assignment_operator : '='
	                    | MUL_ASSIGN
	                    | DIV_ASSIGN
	                    | MOD_ASSIGN
	                    | ADD_ASSIGN
	                    | SUB_ASSIGN
	                    | LEFT_ASSIGN
	                    | RIGHT_ASSIGN
	                    | AND_ASSIGN
	                    | XOR_ASSIGN
	                    | OR_ASSIGN
    '''
    # AST Done
    p[0] = Node(str(p[1]))
    p[0].lineno = p.lineno(1)

def p_expression(p):
    '''
    expression : assignment_expression
	           | expression ',' assignment_expression
    '''
    # AST done
    if (len(p) == 2):
        p[0] = p[1]
    elif (len(p) == 4):
        p[0] = Node(',',[p[1],p[3]])

# 20 done here

def p_constant_expression(p):
    '''
    constant_expression : conditional_expression
    '''
    p[0] = p[1]

## grammar for all expressions done

def p_declaration(p):
    '''
    declaration : declaration_specifiers ';'
	            | declaration_specifiers init_declarator_list ';'
    '''
    if (len(p) == 3):
        p[0] = Node('TypeDecl')
    elif (len(p) == 4):
        p[0] = Node('TypeDecl',[p[2]])
    p[1].removeGraph()
    # Need to remove the nodes for declaration_specifiers

def p_declaration_specifiers(p):
    '''
    declaration_specifiers : storage_class_specifier
	                       | storage_class_specifier declaration_specifiers
	                       | type_specifier
	                       | type_specifier declaration_specifiers
	                       | type_qualifier
	                       | type_qualifier declaration_specifiers
    '''
    if (len(p) == 2):
        p[0] = p[1]
    elif (len(p) == 3):
        p[0] = p[1]
        if ((p[2] is not None) and (p[2].node is not None)):
            G.add_edge(p[0].node, p[2].node)
            p[0].children.append(p[2])

        p[0].extraValues += p[2].extraValues

def p_init_declarator_list(p):
    '''
    init_declarator_list : init_declarator
	                     | init_declarator_list ',' InitM1 init_declarator
    '''
    #  Marker Here
    if (len(p) == 2):
        p[0] = p[1]
    elif (len(p) == 5):
        p[0] = Node(',',[p[1],p[4]])
    p[0].extraValues = p[-1].extraValues

def p_InitM1(p):
    '''
    InitM1 : 
    '''
    p[0] = Node('',createAST=False)
    p[0].extraValues = p[-2].extraValues


def p_init_declarator(p):
    '''
    init_declarator : declarator
	                | declarator '=' initializer
    '''
    if (len(p) == 2):
        p[1].removeGraph()
        p[0] = p[1]
    elif (len(p) == 4):
        p[0] = Node('=',[p[1],p[3]])
        p[0].variables = p[1].variables
    
    # Code to add types to variable
    p[0].extraValues = p[-1].extraValues
    for val in p[0].extraValues:
        p[0].addTypeInDict(val)
    # for key in p[0].variables.keys():
    #     print("The key is: " + key)
    #     print(p[0].variables[key])
    
    # Types added
    for var_name in p[0].variables:
        ST.ModifySymbol(var_name, "type", p[0].variables[var_name],p.lineno(1))
    # <---------------XXXXX------------------>


def p_storage_class_specifier(p):
    '''
    storage_class_specifier : TYPEDEF
	                        | EXTERN
	                        | STATIC
	                        | AUTO
	                        | REGISTER
    '''
    p[0] = Node(str(p[1]))

def p_type_specifier(p):
    '''
    type_specifier : VOID
	               | CHAR
	               | SHORT
	               | INT
	               | LONG
	               | FLOAT
                   | BOOL
	               | DOUBLE
	               | SIGNED
	               | UNSIGNED
	               | struct_or_union_specifier
    '''
    if str(p[1]) in ['void' , 'char', 'int', 'long', 'float', 'bool', 'double', 'signed', 'unsigned']:
        p[0] = Node(str(p[1]))
        p[0].extraValues.append(str(p[1]))
    else:
        p[0] = p[1]

def p_struct_or_union_specifier(p):
    '''
    struct_or_union_specifier : struct_or_union ID '{' markerStructFlag2 struct_declaration_list '}' markerStructFlag0
	                          | struct_or_union ID
    '''
    p[0] = p[1]
    if (len(p) == 8):
        p2val = p[2]['lexeme']
        p[2] = Node(str(p2val))

        p[0].node.attr['label'] = p[0].node.attr['label'] + '{}'
        p[0].label = p[0].node.attr['label']

        if ((p[2] is not None) and (p[2].node is not None)):
            if ((p[5] is not None) and (p[5].node is not None)):
                G.add_edge(p[0].node,p[2].node)
                G.add_edge(p[0].node,p[5].node)

                G.add_edge(p[2].node,p[5].node,style='invis')
                G.add_subgraph([p[2].node,p[5].node], rank='same')
                p[0].children.append(p[2])
                p[0].children.append(p[5])
            else:
                G.add_edge(p[0].node,p[2].node)
                p[0].children.append(p[2])
        else:
            if ((p[5] is not None) and (p[5].node is not None)):
                G.add_edge(p[0].node,p[5].node)
                p[0].children.append(p[5])

    elif (len(p) == 7):
        p[0].node.attr['label'] = p[0].node.attr['label'] + '{}'
        p[0].label = p[0].node.attr['label']
    
        if ((p[3] is not None) and (p[3].node is not None)):
            G.add_edge(p[0].node, p[3].node)
            p[0].children.append(p[3])
            # print("Hello")


    elif (len(p) == 3):
        # This rule is used when declaring a struct type variable 
        # Eg:
        # struct tmp a;
        # While Type Checking check here whether 
        # the struct of given type exists or not
        p2val = p[2]['lexeme']
        p[2] = Node(str(p2val))
        p[0].extraValues.append(p[1].label)
        p[0].extraValues.append(p2val)
        G.add_edge(p[0].node, p[2].node)
        p[0].children.append(p[2])
        # print("Hello")
    
def p_markerStructFlag2(p):
    '''
    markerStructFlag2 :
    '''
    iden = p[-2]['lexeme']
    type_name = p[-3].label.upper()
    line_num = p[-2]['additional']['line']
    ST.flag = 1
    ST.InsertSymbol(iden, line_num, type_name)
    ST.flag = 2

def p_markerStructFlag0(p):
    '''
    markerStructFlag0 :
    '''
    ST.flag = 0

def p_struct_or_union(p):
    '''
    struct_or_union : STRUCT
	                | UNION
    '''
    p[0] = Node(str(p[1]))

def p_struct_declaration_list(p):
    '''
    struct_declaration_list : struct_declaration
	                        | struct_declaration_list struct_declaration
    '''

    if (len(p) == 2):
        p[0] = p[1]
    elif (len(p) == 3):
        p[0] = p[2]

        if ((p[1] is not None) and (p[1].node is not None)):
            G.add_edge(p[0].node, p[1].node)
            p[0].children.append(p[1])

def p_struct_declaration(p):
    '''
    struct_declaration : specifier_qualifier_list struct_declarator_list ';'
    '''
    p[0] = Node('StructOrUnionDec',[p[1],p[2]])

    # Here p[1] has the datatypes like int, float ......

def p_specifier_qualifier_list(p):
    '''
    specifier_qualifier_list : type_specifier specifier_qualifier_list
	                         | type_specifier
	                         | type_qualifier specifier_qualifier_list
	                         | type_qualifier
    '''
    # AST done
    if (len(p) == 2):
        p[0] = p[1]
    elif (len(p) == 3):
        p[0] = p[1]

        if ((p[2] is not None) and (p[2].node is not None)):
            G.add_edge(p[0].node, p[2].node)
            p[0].children.append(p[2])
            p[0].extraValues += p[2].extraValues

def p_struct_declarator_list(p):
    '''
    struct_declarator_list : struct_declarator
	                       | struct_declarator_list ',' structDeclaratorMarker1 struct_declarator
    '''
    # AST done
    if (len(p) == 2):
        p[0] = p[1]
    elif (len(p) == 5):
        p[0] = Node(',',[p[1],p[4]])
    p[0].extraValues = p[-1].extraValues
        
def p_structDeclaratorMarker1(p):
    '''
    structDeclaratorMarker1 :
    '''
    p[0] = Node('',createAST=False)
    p[0].extraValues = p[-2].extraValues

def p_struct_declarator(p):
    '''
    struct_declarator : declarator
	                  | ':' constant_expression
	                  | declarator ':' constant_expression
    '''
    #AST done
    if (len(p) == 2):
        p[0] = p[1]
    elif (len(p) == 3):
        p[0] = Node(':',[p[2]])
    elif (len(p) == 4):
        p[0] = Node(':',[p[1],p[3]])
        p[0].variables = p[1].variables
    
    p[0].extraValues = p[-1].extraValues
    # Here in p[0].extravalues we have all the types like int, float

    for val in p[0].extraValues:
        p[0].addTypeInDict(val)
    # for key in p[0].variables.keys():
    #     print("The key is: " + key)
    #     print(p[0].variables[key])

    # Here the name of the variable acts as a key of the dictionary p[0].variables
    # The type of the variable is a list that is the value of the key
    # Add after this comment, the above print statement is for checking purposes
    
    
    for key in p[0].variables.keys():
        ST.ModifySymbol(key, 'type', p[0].variables[key], p.lineno(0))
    
    # <--------------XXXXXXX---------------->

def p_type_qualifier(p):
    '''
    type_qualifier : CONST
	               | VOLATILE
    '''
    # AST done
    p[0] = Node(str(p[1]))
    p[0].extraValues.append(str(p[1]))

# To be done from here

def p_declarator(p):
    '''
    declarator : direct_declarator
	           | pointer direct_declarator
    '''
    #AST done
    if (len(p) == 2):
        p[0] = p[1]
    elif (len(p) == 3):
        p[0] = Node('Decl',[p[1],p[2]])
        p[0].variables = p[2].variables
        for val in p[1].extraValues:
            p[0].addTypeInDict(val)

def p_function_declarator(p):
    '''
    function_declarator : direct_declarator
	                    | pointer direct_declarator
    '''
    #AST done
    if (len(p) == 2):
        p[0] = p[1]
    elif (len(p) == 3):
        p[0] = Node('Decl',[p[1],p[2]])
        p[0].variables = p[2].variables
        p[0].extraValues += p[1].extraValues

def p_direct_declarator(p):
    '''
    direct_declarator : identifier
	                  | '(' declarator ')'
	                  | direct_declarator '[' ']'
	                  | direct_declarator '(' markerFuncPush ')'
	                  | direct_declarator '[' constant_expression ']'
	                  | direct_declarator '(' markerFuncPush parameter_type_list ')'
	                  | direct_declarator '(' identifier_list ')'
    '''
    # AST doubt - # to be added or not for rule 3, 4, 5, 6, 7
    if (len(p) == 2):
        # ID
        p[0] = p[1]
    elif (len(p) == 4):
        if (p[1] == '('):
            p[0] = p[2]
        elif (p[2] == '['):
            p[0] = Node('DDArrSub',[p[1]])
            p[0].variables = p[1].variables
            p[0].addTypeInDict("[]")
    elif (len(p) == 5):
        if (p[2] == '('):
            if(p[3] == None):
                # direct_declarator '(' M1 ')'
                # this is a function, I have to pass the name of the function,
                # that is create an entry in global symbol table to 
                # to create this function and then pass the variables
                p[0] = Node('DDFuncCall',[p[1]])
                p[0].variables = p[1].variables
                p[0].addTypeInDict("Function Name")
            else:
                # Do not know what this rule is for
                p[0] = Node('DDFuncCallWithIdList',[p[1],p[3]])
        elif (p[2] == '['):
            p[0] = Node('DDArrSub',[p[1],p[3]])
            type1 = "[" + str(p[3].label) + "]"
            p[0].variables = p[1].variables
            p[0].addTypeInDict(type1)
    elif (len(p) == 6):
            # This has last rule
            p[0] = Node('DDFuncCall',[p[1],p[4]])
            p[0].variables = p[4].variables
            p[0].variables[p[1].label] = ["Function Name"]

# correct till here

def p_markerFuncPush(p):
    '''
    markerFuncPush :
    '''
    p[0] = None
    ST.PushScope()

def p_pointer(p):
    '''
    pointer : '*'
	        | '*' type_qualifier_list
	        | '*' pointer
	        | '*' type_qualifier_list pointer
    '''
    # AST done
    if (len(p) == 2):
        p[0] = Node('PTR')
        p[0].extraValues.append("*")
    elif (len(p) == 3):
        p[0] = Node('PTR',[p[2]])
        p[0].extraValues = p[2].extraValues
        p[0].extraValues.append("*")
    elif (len(p) == 4):
        p[0] = Node('PTR',[p[2],p[3]])
        p[0].extraValues = p[2].extraValues + p[3].extraValues
        p[0].extraValues.append("*")

def p_type_qualifier_list(p):
    '''
    type_qualifier_list : type_qualifier
	                    | type_qualifier_list type_qualifier
    '''
    # AST doubt
    if len(p) == 2:
        p[0] = p[1]
    elif len(p) == 3:
        p[0] = p[2]
        if ((p[1] is not None) and (p[1].node is not None)):
            G.add_edge(p[0].node, p[1].node)
            p[0].children.append(p[1])
        p[0].extraValues += p[1].extraValues

def p_parameter_type_list(p):
    '''
    parameter_type_list : parameter_list
	                    | parameter_list ',' ELLIPSIS
    '''
    # AST Done
    if (len(p) == 2):
        p[0] = p[1]
    else:
        # Current design choice: parent operator : ',...'
            # Single child : parameter list

        # Alternative design choice: parent operator ','
            # Left child : parameter_list
            # Right child : ELLIPSIS 
        p[0] = Node('ELLIPSIS',[p[1]])
        p[0].variables = p[1].variables
        p[0].variables["Ellipses"] = []

def p_parameter_list(p):
    '''
    parameter_list : parameter_declaration
	               | parameter_list ',' parameter_declaration
    '''
    # AST Done
    if (len(p) == 2):
        p[0] = p[1]
    else:
        p[0] = Node(',',[p[1],p[3]])
        p[0].variables = {**p[1].variables, **p[3].variables}

def p_parameter_declaration_1(p):
    '''
    parameter_declaration : declaration_specifiers abstract_declarator
	                      | declaration_specifiers
    '''
    # AST done
    if len(p) == 2:
        # Doubt here
        p[0] = Node('ParDeclWithoutDeclarator',[p[1]])
    elif len(p) == 3:
        p[0] = Node('ParDecl',[p[1],p[2]])

def p_parameter_declaration_2(p):
    '''
    parameter_declaration : declaration_specifiers declarator
    '''
    # AST done
    p[0] = Node('ParDecl',[p[1],p[2]])
    p[0].variables = p[2].variables
    for val in p[1].extraValues:
        p[0].addTypeInDict(val)
     
def p_identifier_list(p):
    '''
    identifier_list : ID
	                | identifier_list ',' ID
    '''
    # AST Done
    if (len(p) == 2):
        p[0] = Node(str(p[1]['lexeme']))
    else:
        p3val = p[3]['lexeme']
        p[3] = Node(str(p3val))
        p[0] = Node(',',[p[1],p[3]])

def p_type_name(p):
    '''
    type_name : specifier_qualifier_list
	          | specifier_qualifier_list abstract_declarator
    '''
    # AST done

    if len(p) == 2:
        p[0] = Node('TypeName',[p[1]])
    else:
        p[0] = Node('TypeName',[p[1],p[2]])

def p_abstract_declarator(p):
    '''
    abstract_declarator : pointer
	                    | direct_abstract_declarator
	                    | pointer direct_abstract_declarator
    '''
    # AST done

    if len(p) == 2:
        p[0] = Node('AbsDecl',[p[1]])
    else:
        p[0] = Node('AbsDecl',[p[1],p[2]])

def p_direct_abstract_declarator(p):
    '''
	direct_abstract_declarator : '[' ']'
	                           | '(' ')'
                               | '(' abstract_declarator ')'
	                           | '(' parameter_type_list ')'
	                           | '[' constant_expression ']'
	                           | direct_abstract_declarator '[' ']'
	                           | direct_abstract_declarator '(' ')'
	                           | direct_abstract_declarator '[' constant_expression ']'
	                           | direct_abstract_declarator '(' parameter_type_list ')'
    '''
    # AST done

    if (len(p) == 3):
        if(p[1] == '('):
            p[0] = Node('DAD()')
        elif(p[1] == '['):
            p[0] = Node('DAD[]')

    if (len(p) == 4):
        if(p[1] == '('):
            p[0] = Node('DAD()',[p[2]])
        elif(p[1] == '['):
            p[0] = Node('DAD[]',[p[2]])
        elif(p[2] == '('):
            p[0] = Node('POSTDAD()',[p[1]])
        elif(p[2] == '['):
            p[0] = Node('POSTDAD[]',[p[1]])

    elif (len(p) == 5):
        if (p[2] == '('):
            p[0] = Node('DAD()',[p[1],p[3]])
        elif (p[2] == '['):
            p[0] = Node('DAD[]',[p[1],p[3]])

#correct till here

def p_initializer(p):
    '''
    initializer : assignment_expression
	            | '{' initializer_list '}'
                | '{' initializer_list ',' '}'
    '''
    # AST done
    if len(p) == 2:
        p[0] = p[1]
    elif len(p) == 4 or len(p) == 5:
        p[0] = Node('{}',[p[2]])

def p_initializer_list(p):
    '''
    initializer_list : initializer
	                 | initializer_list ',' initializer
    '''
    # AST done
    if len(p) == 2:
        p[0] = p[1]
    else:
        p[0] = Node(',',[p[1],p[3]])

def p_statement(p):
    '''
    statement : labeled_statement
	          | compound_statement
	          | expression_statement
	          | selection_statement
	          | iteration_statement
	          | jump_statement
    '''
    # AST Done
    p[0] = p[1]

def p_labeled_statement(p):
    '''
    labeled_statement : ID ':' statement
	                  | CASE constant_expression ':' statement
	                  | DEFAULT ':' statement
    '''
    # AST Done
    if (len(p) == 4):
        if (p[1] == 'default'):
            p[0] = Node('DEFAULT:',[p[3]])
        else:
            p1val = p[1]['lexeme']
            p[1] = Node(str(p1val))
            p[0] = Node('ID:',[p[1],p[3]])
    else:
        p[0] = Node('CASE:',[p[2],p[4]])

def p_compound_statement(p):
    '''
    compound_statement : '{' markerCompStatPush '}' markerCompStatPop
	                   | '{' markerCompStatPush block_item_list '}' markerCompStatPop
    '''
    if (len(p) == 5):
        p[0] = Node('EmptySCOPE')
    elif (len(p) == 6):
        p[0] = Node('SCOPE',[p[3]])

def p_markerCompStatPush(p):
    '''
    markerCompStatPush :
    '''
    ST.PushScope()

def p_markerCompStatPop(p):
    '''
    markerCompStatPop :
    '''
    ST.PopScope()

def p_block_item_list(p):
    '''
    block_item_list : block_item
                    | block_item_list block_item
    '''
    # AST done

    if (len(p) == 2):
        p[0] = Node(';',[p[1]])
    elif (len(p) == 3):
        p[0] = Node(';',[p[1],p[2]])

def p_block_item(p):
    '''
    block_item : declaration
	            | statement
    '''
    # AST Done
    if (len(p) == 2):
        p[0] = p[1]

def p_expression_statement(p):
    '''
    expression_statement : ';'
	                     | expression ';'
    '''
    # AST Done
    if len(p) == 2:
        p[0] = Node('EmptyExprStmt')
    if (len(p) == 3):
        p[0] = p[1]

def p_selection_statement(p):
    '''
    selection_statement : IF '(' expression ')' statement
	                    | IF '(' expression ')' statement ELSE statement
	                    | SWITCH '(' expression ')' statement
    '''
    # AST done
    if(len(p) == 6):
        p[0] = Node(str(p[1]).upper(),[p[3],p[5]])
    else:
        p[0] = Node('IF-ELSE',[p[3],p[5],p[7]])

# Correct till here

def p_iteration_statement(p):
    '''
    iteration_statement : WHILE '(' expression ')' statement
	                    | DO statement WHILE '(' expression ')' ';'
	                    | FOR '(' expression_statement expression_statement ')' statement
	                    | FOR '(' expression_statement expression_statement expression ')' statement
	                    | FOR '(' markerForPush declaration expression_statement ')' statement markerForPop
	                    | FOR '(' markerForPush declaration expression_statement expression ')' statement markerForPop
    '''
    # AST done
    if len(p) == 6:
        p[0] = Node('WHILE',[p[3],p[5]])
    elif len(p) == 7:
        p[0] = Node('FOR',[p[3],p[4],p[6]])
    elif len(p) == 8:
        if (p[1] == 'do'):
            p[0] = Node('DO-WHILE',[p[2],p[5]])
        else:
            p[0] = Node('FOR',[p[3],p[4],p[5],p[7]])
    elif len(p) == 9:
        p[0] = Node('FOR', [p[4], p[5], p[7]])
    else:
        p[0] = Node('FOR', [p[4], p[5], p[6], p[8]])

# Markers for FOR loops
def p_markerForPush(p):
    '''
    markerForPush :
    '''
    ST.PushScope()

def p_markerForPop(p):
    '''
    markerForPop :
    '''
    ST.PopScope()

def p_jump_statement(p):
    '''
    jump_statement : GOTO ID ';'
	               | CONTINUE ';'
	               | BREAK ';'
	               | RETURN ';'
	               | RETURN expression ';'
    '''
    # AST done
    if (len(p) == 3):
        p[0] = Node(str(p[1]).upper())
    else:
        if(p[1] == 'return'):
            p[0] = Node('RETURN',[p[2]])
        else:
            p2val = p[2]['lexeme']
            p[2] = Node(str(p2val))
            p[0] = Node('GOTO',[p[2]])

def p_start(p):
    '''
    start : translation_unit
    '''
    p[0] = p[1]
    ST.StoreResults()

def p_translation_unit(p):
    '''
    translation_unit : external_declaration
	                 | translation_unit external_declaration
    '''
    # AST done
    # Here
    # Hack to restrict single source node
    # <------XXXXX-------> do this once the type adding thing done
    p[0] = 'SourceNode'

    if (len(p) == 2):
        if ((p[1] is not None) and (p[1].node is not None)):
            G.add_edge(p[0] , p[1].node)
    elif (len(p) == 3):
        if ((p[2] is not None) and (p[2].node is not None)):
            G.add_edge(p[0], p[2].node)

def p_external_declaration(p):
    '''
    external_declaration : function_definition
	                     | declaration
    '''
    # AST Done
    p[0] = p[1]

def p_function_definition(p):
    '''
    function_definition : declaration_specifiers function_declarator declaration_list '{' markerFunc1 '}' markerFuncPop
                        | declaration_specifiers function_declarator declaration_list '{' markerFunc1 block_item_list '}' markerFuncPop
                        | declaration_specifiers function_declarator '{' markerFunc2 '}' markerFuncPop
                        | declaration_specifiers function_declarator '{' markerFunc2 block_item_list '}' markerFuncPop
    '''
    # AST doubt
    if (len(p) == 7):
        # Add AST Node for EMPTY SCOPE? (check other places too)
        p[0] = Node('FUNC',[p[1],p[2]])
    elif (len(p) == 8):
        if p[3] == '{':
            p[0] = Node('FUNC',[p[1],p[2],Node('SCOPE', [p[5]])])
        else:
            p[0] = Node('FUNC',[p[1],p[2],p[3]])
    elif len(p) == 9:
        p[0] = Node('FUNC',[p[1],p[2],p[3],Node('SCOPE', [p[6]])])

def p_markerFunc1(p):
    '''
    markerFunc1 : 
    '''
    # ST.PopScope()

    p[0] = Node('',createAST=False)
    p[0].variables = p[-3].variables
    function_name = str()
    for key in p[0].variables.keys():
        if(p[0].variables[key][0] == "Function Name"):
            function_name = key
            break
    p[0].variables[key] += p[-4].extraValues + p[-3].extraValues

    # print("This is start of the function in funcpop1")
    # for key in p[0].variables.keys():
    #     print("The key is: " + key)
    #     print(p[0].variables[key])
    # print('This is end of the function')

    ST.ModifySymbol(function_name, 'check', "FUNC") # says that this entry is a function
    for key in p[0].variables.keys():
        if not key == function_name:
            ST.ModifySymbol(key, "type", p[0].variables[key])
        else:
            ST.ModifySymbol(key, "type", p[0].variables[key][1:])
    # Add code before this
    #  <----------------------XXXXXX------------------>


def p_markerFunc2(p):
    '''
    markerFunc2 : 
    '''
    # ST.PopScope()
    p[0] = Node('',createAST=False)
    p[0].variables = p[-2].variables
    function_name = str()
    for key in p[0].variables.keys():
        if(p[0].variables[key][0] == "Function Name"):
            function_name = key
            break
    p[0].variables[key] += p[-3].extraValues + p[-2].extraValues

    # print("This is start of the function in funcpop2")
    # for key in p[0].variables.keys():
    #     print("The key is: " + key)
    #     print(p[0].variables[key])
    # print('This is end of the function')
    # Here the function name is a key and has a type "Function Name" in the value list
    # The first item in the list will be "Function Name" and thereafter the rest of the
    # items in the list will be return type.

    ST.ModifySymbol(function_name, 'check', "FUNC") # says that this entry is a function
    for key in p[0].variables.keys():
        if not key == function_name:
            ST.ModifySymbol(key, "type", p[0].variables[key])
        else:
            ST.ModifySymbol(key, "type", p[0].variables[key][1:])
    #  <----------------------XXXX------------------>

def p_markerFuncPop(p):
    '''
    markerFuncPop :
    '''
    ST.PopScope();

def p_declaration_list(p):
    '''
    declaration_list : declaration
	                 | declaration_list declaration
    '''
    # AST done
    if (len(p) == 2):
        p[0] = Node(';',[p[1]])
    elif (len(p) == 3):
        p[0] = Node(';',[p[1],p[2]])

def p_error(p):
    print('Error found while parsing!')
    global isError
    isError = 1

def _lex_on_lbrace_func():
    ST.PushScope()

def _lex_on_rbrace_func():
    ST.PopScope()

def error_func():
    pass

def type_lookup_func():
    pass

isError = 0
if len(sys.argv) == 1:
    print('No file given as input')
    sys.exit(1)
file = open(sys.argv[1], 'r')
data = file.read()

# Lexer driver code
clex = CLexer( error_func=error_func, type_lookup_func=type_lookup_func, on_lbrace_func=_lex_on_lbrace_func, on_rbrace_func=_lex_on_rbrace_func)
clex.build()
clex.lexer.input(data)
tokens = clex.tokens
clex.lexer.lineno = 1

# driver code
parser = yacc.yacc(start='start', outputdir='./tmp')

G = pgv.AGraph(strict=False, directed=True)
G.layout(prog='circo')

itr = 0 # Global var to give unique IDs to nodes of the graph


result = parser.parse(data)

fileNameCore = str(sys.argv[1]).split('/')[-1].split('.')[0]
outputFile = 'dot/' + fileNameCore + '.dot'

if isError == 1:
    print(f'Error found. Aborting parsing of {sys.argv[1]}....')
    sys.exit(1)
elif ST.error:
    sys.exit(1) 
else:
    print('Output file is: ' + fileNameCore + '.ps')
    G.write(outputFile)
    ST.PrintTable()
