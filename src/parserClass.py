# import necessary libraries
import ply.yacc as yacc
import pygraphviz as pgv
import sys
import struct
import json
import copy

# Get the token map from lexer
from lexerClass import CLexer
from SymbolTable import SymbolTable
from TAC import TAC
# from codeGen import CG

############## Helper Functions ###########
def new_node():
    global itr
    G.add_node(itr)
    n = G.get_node(itr)
    itr += 1
    return n

def remove_node(graphNode):
    G.remove_node(graphNode)

# This class denotes the Node of our Functional AST
class Node:
    def __init__(self,label,children=None,node=None,attributes=None,createAST = True, type=None, isvar = False):
        self.label = label
        self.createAST = createAST
        self.node = node
        self.type = type
        self.isvar = isvar
        self.isTerminal = False
        self.temp = ''
        self.varname = []
        # TAC lists
        self.truelist = []
        self.falselist = []
        self.nextlist = []
        self.breaklist = []
        self.continuelist = []
        self.testlist = []
        self.arglist = []
        self.type = []
        self.totype = None
        self.param_nums = None
        self.params = None
        self.numdef = 0
        self.arrlvl = 0
        # This field is only for Marker nodes used in TAC
        self.quad = None
        self.dimensionList = None
        self.addr = None

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

    def print_val(self):
        for child in self.children:
            child.print_val()
        self.node.attr['label'] += '\n' + str(self.temp)
    
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
    
    def onlyAddEdge(self,extraChildren):
        # This function only adds edge from the parent to the given
        # node and adds the given nodes to the children list
        # This function is different from makrGraph()
        # and is not called by constructor of the class
        listNode = []
        for child in extraChildren:
            G.add_edge(self.node,child.node)
            listNode.append(child.node)

        for i in range(0,len(extraChildren)-1):
            G.add_edge(extraChildren[i].node,extraChildren[i+1].node,style='invis')
        G.add_subgraph(listNode,rank='same')

        self.children += extraChildren
        
dit = ['char', 'short', 'int', 'long int']
dft = ['float', 'double', 'long double']
iit = ['bool', 'char', 'short', 'int', 'long int']
aat = ['bool', 'char', 'short', 'int', 'long int',
    'float', 'double', 'long double']
adt = ['bool', 'char', 'short', 'int', 'long int',
    'float', 'double', 'long double',
    'str', 'void']
sizes = {
    'int': 4,
    'char': 1,
    'short': 2,
    'long int': 8,
    'long': 8,
    'float': 4,
    'double': 8,
    'long double':10,
    'PTR': 4,
    'bool': 1,
    'void': 0,
    'int unsigned': 4,
    'unsigned int': 4,
    'str': 4 # 'str' is same as int unsigned
}
class CParser():
    tokens = CLexer.tokens
    tokens.remove("ERROR")
    tokens.remove("HEXA_CONSTANT")
    tokens.remove("OCTAL_CONSTANT")
    def __init__(self):
        self.ST = SymbolTable()
        self.TAC = TAC()
        self.AST_ROOT = Node("SourceNode")
        self.isError = 0
        # self.CG = CG()
        self.strList = []

    def updateSizeInSymTab(self, variables, var_name):
        multiplier = 1
        new_list = []
        for var in variables:
            new_list = new_list + var.split(' ')
        variables = new_list
        if '*' in variables:
            self.ST.ModifySymbol(var_name, "sizeAllocInBytes", sizes["PTR"])
        elif 'struct' in variables :
            struct_size = 0
            found = self.ST.TT.ReturnTypeTabEntry(variables[1], "struct")
            struct_size = found["sizeAllocInBytes"]
            self.ST.ModifySymbol(var_name, "sizeAllocInBytes", multiplier*struct_size)
        elif 'union' in variables:
            struct_size = 0
            found = self.ST.TT.ReturnTypeTabEntry(variables[1], "union")
            struct_size = found["sizeAllocInBytes"]
            self.ST.ModifySymbol(var_name, "sizeAllocInBytes", multiplier*struct_size)
        elif 'long' in variables:
            if 'int' in variables:
                self.ST.ModifySymbol(var_name, "sizeAllocInBytes", multiplier*sizes["long int"])
            elif 'double' in variables:
                self.ST.ModifySymbol(var_name, "sizeAllocInBytes", multiplier*sizes["long double"])
            else:
                self.ST.ModifySymbol(var_name, "sizeAllocInBytes", multiplier*sizes["long"])
        elif 'float' in variables:
            self.ST.ModifySymbol(var_name, "sizeAllocInBytes", multiplier*sizes["float"])
        elif 'double' in variables:
            self.ST.ModifySymbol(var_name, "sizeAllocInBytes", multiplier*sizes["double"])
        elif 'short' in variables:
            self.ST.ModifySymbol(var_name, "sizeAllocInBytes", multiplier*sizes["short"])
        elif 'int' in variables:
            self.ST.ModifySymbol(var_name, "sizeAllocInBytes", multiplier*sizes["int"])
        elif 'char' in variables:
            self.ST.ModifySymbol(var_name, "sizeAllocInBytes", multiplier*sizes["char"])
        elif 'bool' in variables:
            self.ST.ModifySymbol(var_name, "sizeAllocInBytes", multiplier*sizes["bool"])
        else:
            self.ST.ModifySymbol(var_name, "sizeAllocInBytes", multiplier*sizes["void"])
    
    def convertFloatRepToLong(self, val):
        float_rep = ''.join(bin(c).replace('0b', '').rjust(8, '0') for c in struct.pack('!f', val))
        long_rep = int(float_rep,2)
        self.TAC.floatvals.append(long_rep)
        return len(self.TAC.floatvals)-1

    def build(self):
        # Debug is kept true only while testing
        self.parser = yacc.yacc(module=self, start='start', outputdir='./tmp',debug=False)

    def p_primary_expression_1(self,p):
        '''
        primary_expression : ID
        '''
        if self.isError :
            return
        found, entry = self.ST.ReturnSymTabEntry(p[1]['lexeme'], p.lineno(1))
        if found:

            try :
                entry['type']
            except:
                self.ST.error = 1
                print(f'Self referencing variables not allowed at line {p.lineno(1)}')
                return

            if entry['check'] == 'FUNC':
                p[0] = Node(str(p[1]['lexeme']))
                p[0].type = []
                p[0].type.append('func')
                p[0].ret_type = []
                type_list = entry['type']
                isarr = 0
 
                for i in range(len(entry['type'])):
                    if entry['type'][i][0]=='[' and entry['type'][i][-1] == ']':
                        isarr += 1
                        p[0].arrlvl += 1

                if 'unsigned' in type_list or 'signed' in type_list:
                    if 'bool' not in type_list and 'char' not in type_list and 'short' not in type_list:
                        type_list.append('int')

                if 'long' in type_list and 'int' in type_list:
                    p[0].ret_type.append('long int')
                    for single_type in type_list:
                        if single_type != 'long' and single_type != 'int':
                            p[0].ret_type.append(single_type)
                
                elif 'long' in type_list and 'double' in type_list:
                    p[0].ret_type.append('long double')
                    for single_type in type_list:
                        if single_type != 'long' and single_type != 'double':
                            p[0].ret_type.append(single_type)
                
                elif 'long' in type_list:
                    p[0].ret_type.append('long int')
                    for single_type in type_list:
                        if single_type != 'long':
                            p[0].ret_type.append(single_type)

                elif 'int' in type_list:
                    p[0].ret_type.append('int')
                    for single_type in type_list:
                        if single_type != 'int':
                            p[0].ret_type.append(single_type)

                elif 'short' in type_list:
                    p[0].ret_type.append('short')
                    for single_type in type_list:
                        if single_type != 'short':
                            p[0].ret_type.append(single_type)
                
                elif 'char' in type_list:
                    p[0].ret_type.append('char')
                    for single_type in type_list:
                        if single_type != 'char':
                            p[0].ret_type.append(single_type)
                
                elif 'bool' in type_list:
                    p[0].ret_type.append('bool')
                    for single_type in type_list:
                        if single_type != 'bool':
                            p[0].ret_type.append(single_type)
                
                elif 'str' in type_list:
                    p[0].ret_type.append('str')
                    for single_type in type_list:
                        if single_type != 'str':
                            p[0].ret_type.append(single_type)
                
                elif 'float' in type_list:
                    p[0].ret_type.append('float')
                    for single_type in type_list:
                        if single_type != 'float':
                            p[0].ret_type.append(single_type)

                elif 'double' in type_list:
                    p[0].ret_type.append('double')
                    for single_type in type_list:
                        if single_type != 'double':
                            p[0].ret_type.append(single_type)

                if isarr > 0:
                    temp_type = []
                    temp_type.append(p[0].ret_type[0])
                    for i in range(isarr):
                        temp_type[0] += ' *'

                    for i in range(len(p[0].ret_type)):
                        if i>isarr:
                            temp_type.append(p[0].ret_type[i])
                    p[0].ret_type = temp_type
                    p[0].ret_type.append('arr')
                    for i in range(len(type_list)):
                        if type_list[len(type_list)-i-1][0] == '[' and type_list[len(type_list)-i-1][-1] == ']':  
                            p[0].ret_type.append(type_list[len(type_list)-i-1])

                if 'struct' in type_list:
                    p[0].ret_type.append('struct')
                    for single_type in type_list:
                        if single_type != 'struct':
                                p[0].ret_type.append(single_type)     

                if 'union' in type_list:
                    p[0].ret_type.append('union')
                    for single_type in type_list:
                        if single_type != 'union':
                                p[0].ret_type.append(single_type)     

                if 'void' in type_list:
                    p[0].ret_type.append('void')
                    for single_type in type_list:
                        if single_type != 'void':
                                p[0].ret_type.append(single_type)     

                if '*' in type_list:
                    temp_type = []
                    temp_type.append(p[0].ret_type[0])
                    for i in range(1, len(p[0].ret_type)):
                            if p[0].ret_type[i] == '*':
                                temp_type[0] += ' *'
                            else:
                                temp_type.append(p[0].ret_type[i])
                    p[0].ret_type = temp_type

                p[0].param_nums = entry['PARAM_NUMS']
                p[0].params = []
                if '#scope' in entry.keys():
                    for var in entry['#scope'][0]:
                        if var == '#StructOrUnion':
                            p[0].structorunion = entry['#scope'][0][var]
                            continue
                        if var == '#scope':
                            continue
                        if var == '#scopeNum':
                            continue
                        if entry['#scope'][0][var]['check'] == 'PARAM':
                            p[0].params.append(entry['#scope'][0][var])

                if self.ST.error:
                    return
                p[0].truelist.append(self.TAC.nextstat)
                p[0].falselist.append(self.TAC.nextstat+1)
                return

            isarr = 0
            for i in range(len(entry['type'])):
                if entry['type'][i][0]=='[' and entry['type'][i][-1] == ']':
                    isarr += 1
            
            p[0] = Node(str(p[1]['lexeme']))
            type_list = entry['type']
            if type_list is None or set(type_list) == {'*'}:
                type_list = []
            if entry['check'] == 'VAR' or entry['check'] == 'PARAM':
                p[0].isvar = 1

            if 'unsigned' in type_list or 'signed' in type_list:
                if 'bool' not in type_list and 'char' not in type_list and 'short' not in type_list:
                    type_list.append('int')

            if 'long' in type_list and 'int' in type_list:
                p[0].type.append('long int')
                for single_type in type_list:
                    if single_type != 'long' and single_type != 'int':
                        p[0].type.append(single_type)
            
            elif 'long' in type_list and 'double' in type_list:
                p[0].type.append('long double')
                for single_type in type_list:
                    if single_type != 'long' and single_type != 'double':
                        p[0].type.append(single_type)
            
            elif 'long' in type_list:
                p[0].type.append('long int')
                for single_type in type_list:
                    if single_type != 'long':
                        p[0].type.append(single_type)

            elif 'int' in type_list:
                p[0].type.append('int')
                for single_type in type_list:
                    if single_type != 'int':
                        p[0].type.append(single_type)

            elif 'short' in type_list:
                p[0].type.append('short')
                for single_type in type_list:
                    if single_type != 'short':
                        p[0].type.append(single_type)
            
            elif 'char' in type_list:
                p[0].type.append('char')
                for single_type in type_list:
                    if single_type != 'char':
                        p[0].type.append(single_type)
            
            elif 'bool' in type_list:
                p[0].type.append('bool')
                for single_type in type_list:
                    if single_type != 'bool':
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


            if 'struct' in type_list:
                p[0].type.append('struct')
                for single_type in type_list:
                    if single_type != 'struct':
                            p[0].type.append(single_type)     

            if 'union' in type_list:
                p[0].type.append('union')
                for single_type in type_list:
                    if single_type != 'union':
                            p[0].type.append(single_type)     

            if isarr > 0:
                temp_type = []
                temp_type.append(p[0].type[0])
                for i in range(isarr):
                    temp_type[0] += ' *'

                for i in range(len(p[0].type)):
                    if i>isarr:
                        temp_type.append(p[0].type[i])
                p[0].type = temp_type
                p[0].type.append('arr')
                for i in range(len(type_list)):
                    if type_list[len(type_list)-i-1][0] == '[' and type_list[len(type_list)-i-1][-1] == ']':  
                        p[0].type.append(type_list[len(type_list)-i-1])

            if 'void' in type_list:
                p[0].type.append('void')
                for single_type in type_list:
                    if single_type != 'void':
                            p[0].type.append(single_type)     

            if '*' in type_list:
                temp_type = []
                temp_type.append(p[0].type[0])
                for i in range(1, len(p[0].type)):
                        if p[0].type[i] == '*':
                            temp_type[0] += ' *'
                        else:
                            temp_type.append(p[0].type[i])
                p[0].type = temp_type

            if 'struct' in p[0].type[0] or 'union' in p[0].type[0]:
                p[0].vars = entry['vars']

        else:
            p[0] = Node('error')
        
        if self.ST.error:
            return
        
        p[0].varname.append(p[1]['lexeme'])
        p[0].temp = self.TAC.get_sym(self.ST, p[0].label)
        p[0].truelist.append(self.TAC.nextstat)
        p[0].falselist.append(self.TAC.nextstat+1)
        self.TAC.emit('ifnz goto','',p[0].temp,'')
        self.TAC.emit('goto','','','')

    def p_primary_expression(self,p):
        '''
        primary_expression : IntegerConst
                        | FloatConst
                        | CharConst
                        | StringConst
                        | '(' expression ')'
        '''
        if self.isError :
            return
        if (len(p) == 2):
            p[0] = p[1]
            if self.ST.error:
                return
            p[0].truelist.append(self.TAC.nextstat)
            p[0].falselist.append(self.TAC.nextstat+1)
            self.TAC.emit('ifnz goto','',p[0].temp,'')
            self.TAC.emit('goto','','','')
        elif (len(p) == 4):
            p[0] = p[2]

    def p_identifier(self,p):
        '''
        identifier : ID
        '''
        if self.isError :
            return
        p[0] = Node(str(p[1]['lexeme']))
        p[0].variables[p[0].label] = []
        p[0].isvar = 1
        self.ST.InsertSymbol(p[1]['lexeme'], p[1]['additional']['line'])
        self.ST.ModifySymbol(p[1]['lexeme'], "check", "VAR")
        
        if self.ST.error:
            return
                
        self.ST.ModifySymbol(p[1]['lexeme'], "temp", p[1]['lexeme'])
        
    def p_IntegerConst(self,p):
        '''
        IntegerConst : INT_CONSTANT
        '''
        if self.isError :
            return
        p[0] = Node(str(p[1]))
        p[0].type = ['int']
        if self.ST.error:
            return
        p[0].temp = self.TAC.newtemp()
        self.ST.InsertSymbol(p[0].temp, 0)
        self.ST.ModifySymbol(p[0].temp, "type", p[0].type)
        self.ST.ModifySymbol(p[0].temp, "check", "TEMP")
        self.updateSizeInSymTab(p[0].type, p[0].temp)
        if self.ST.isGlobal(p[0].temp):
            self.ST.ModifySymbol(p[0].temp, "varclass", "Global")
        else :
            self.ST.ModifySymbol(p[0].temp, "varclass", "Local")
            found, entry = self.ST.ReturnSymTabEntry(p[0].temp)
            var_size = found['sizeAllocInBytes']
            if found["varclass"] == "Local":
                # self.TAC.emit('-_int', '%esp', '%esp', f'${var_size}')
                if found["offset"] >0:
                    self.ST.ModifySymbol(p[0].temp, 'temp', f'-{found["offset"] + found["sizeAllocInBytes"] }(%ebp)')
                else:
                    self.ST.ModifySymbol(p[0].temp, 'temp', f'{-found["offset"] - found["sizeAllocInBytes"] }(%ebp)')
            p[0].temp = found['temp']
        self.TAC.emit('=_int', p[0].temp, f'${p[1]}')

    def p_FloatConst(self,p):
        '''
        FloatConst : FLOAT_CONSTANT
        '''
        if self.isError :
            return
        p[0] = Node(str(p[1]))
        idx = self.convertFloatRepToLong(p[1])
        p[0].type = ['float']
        if self.ST.error:
            return
        p[0].temp = self.TAC.newtemp()
        self.ST.InsertSymbol(p[0].temp, 0)
        self.ST.ModifySymbol(p[0].temp, "type", p[0].type)
        self.ST.ModifySymbol(p[0].temp, "check", "TEMP")
        self.updateSizeInSymTab(p[0].type, p[0].temp)
        if self.ST.isGlobal(p[0].temp):
            self.ST.ModifySymbol(p[0].temp, "varclass", "Global")
        else :
            self.ST.ModifySymbol(p[0].temp, "varclass", "Local")
            found, entry = self.ST.ReturnSymTabEntry(p[0].temp)
            var_size = found['sizeAllocInBytes']
            if found["varclass"] == "Local":
                # self.TAC.emit('-_int', '%esp', '%esp', f'${var_size}')
                if found["offset"] >0:
                    self.ST.ModifySymbol(p[0].temp, 'temp', f'-{found["offset"] + found["sizeAllocInBytes"] }(%ebp)')
                else:
                    self.ST.ModifySymbol(p[0].temp, 'temp', f'{-found["offset"] - found["sizeAllocInBytes"] }(%ebp)')
            p[0].temp = found['temp']
        self.TAC.emit('load_float', f'.LF{idx}', p[0].temp, '')

    def p_CharConst(self,p):
        '''
        CharConst : CHAR_CONSTANT
        '''
        if self.isError :
            return
        p[0] = Node(str(p[1]))
        p[0].type = ['char']
        if self.ST.error:
            return
        p[0].temp = self.TAC.newtemp()
        self.ST.InsertSymbol(p[0].temp, 0)
        self.ST.ModifySymbol(p[0].temp, "type", p[0].type)
        self.ST.ModifySymbol(p[0].temp, "check", "TEMP")
        self.updateSizeInSymTab(p[0].type, p[0].temp)
        if self.ST.isGlobal(p[0].temp):
            self.ST.ModifySymbol(p[0].temp, "varclass", "Global")
        else :
            self.ST.ModifySymbol(p[0].temp, "varclass", "Local")
            found, entry = self.ST.ReturnSymTabEntry(p[0].temp)
            var_size = found['sizeAllocInBytes']
            if found["varclass"] == "Local":
                # self.TAC.emit('-_int', '%esp', '%esp', f'${var_size}')
                if found["offset"] >0:
                    self.ST.ModifySymbol(p[0].temp, 'temp', f'-{found["offset"] + found["sizeAllocInBytes"] }(%ebp)')
                else:
                    self.ST.ModifySymbol(p[0].temp, 'temp', f'{-found["offset"] - found["sizeAllocInBytes"] }(%ebp)')
            p[0].temp = found['temp']
        self.TAC.emit('=_char', p[0].temp, f'${p[1]}')

    def p_StringConst(self,p):
        '''
        StringConst : STRING_LITERAL
        '''
        if self.isError :
            return
        p[0] = Node(str(p[1]))
        self.TAC.strList.append(str(p[1]))
        p[0].type = ['str']
        if self.ST.error:
            return
        idx = self.TAC.findStringIdx(p[1])
        p[0].temp = f'$.LC{idx}'

    def p_postfix_expression(self,p):
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
        if self.isError :
            return
        if (len(p) == 2):
            p[0] = p[1]

            if p[1] == None or p[1].type == None:
                self.ST.error = 1
                return

            for i in range(len(p[1].type) - 1, 0, -1):
                if p[1].type[i][0] != '[':
                    break
                else:
                    if p[0].dimensionList is None:
                        p[0].dimensionList = []
                    if len(p[0].dimensionList) == 0 and len(p[1].type[i][1:-1]) == 0:
                        p[0].dimensionList.append(0)
                    else:
                        try:
                            p[0].dimensionList.append(int(p[1].type[i][1:-1]))
                        except:
                            p[0].dimensionList.append(p[1].type[i][1:-1])
            if p[0].dimensionList is not None:
                p[0].dimensionList.reverse()        
                p[0].dimensionList.append('isFirstAccess')
                var = p[0].temp.split('(')[0]
                if var == '' or var is None:
                    return
                if var[0] != '-':
                    var = '+' + var
                p[0].addr = f'%ebp{var}'
                if var[0] == '-':
                    p[0].temp = p[0].addr

        elif (len(p) == 3):
            if p[1] == None or p[1].type == None or p[1].type == []:
                self.ST.error = 1
                print(f'Cannot increase/decrease value of expression at line {p.lineno(2)}')

            elif 'const' in p[1].type:
                self.ST.error = 1
                print(f'Cannot increase/decrease value of read only variable at line {p.lineno(2)}')

            elif p[1].type[0] not in iit and p[1].type[0][-1] != '*':
                self.ST.error = 1
                print(f'Cannot use increment/decrement operator on non-integral at line {p.lineno(2)}')

            elif p[1].isTerminal == False and p[1].isvar == False:
                self.ST.error = 1
                print(f'Cannot use increment/decrement operator on expression at line {p.lineno(2)}')

            elif p[1].isvar == 0 and p[1].type[0][-1] != '*':
                self.ST.error = 1
                print(f'Cannot use increment/decrement operator on constant at line {p.lineno(2)}')

            elif p[1].type[0][-1] == '*' and 'arr' in p[1].type:
                self.ST.error = 1
                print(f'Cannot use increment/decrement operator on array type at line {p.lineno(2)}')         

            else:
                p[0] = Node('POST' + str(p[2]),[p[1]])

                p[0].type = p[1].type

                #tac
                if self.ST.error:
                    return
                
                p[0].varname = p[1].varname
                p[0].temp = self.TAC.newtemp()
                self.ST.InsertSymbol(p[0].temp, 0)
                self.ST.ModifySymbol(p[0].temp, "type", p[0].type)
                self.ST.ModifySymbol(p[0].temp, "check", "TEMP")
                self.updateSizeInSymTab(p[0].type, p[0].temp)
                if self.ST.isGlobal(p[0].temp):
                    self.ST.ModifySymbol(p[0].temp, "varclass", "Global")
                else :
                    self.ST.ModifySymbol(p[0].temp, "varclass", "Local")
                    found, entry = self.ST.ReturnSymTabEntry(p[0].temp)
                    var_size = found['sizeAllocInBytes']
                    if found["varclass"] == "Local":
                        # self.TAC.emit('-_int', '%esp', '%esp', f'${var_size}')
                        if found["offset"] >0:
                            self.ST.ModifySymbol(p[0].temp, 'temp', f'-{found["offset"] + found["sizeAllocInBytes"] }(%ebp)')
                        else:
                            self.ST.ModifySymbol(p[0].temp, 'temp', f'{-found["offset"] - found["sizeAllocInBytes"] }(%ebp)')
                    p[0].temp = found['temp']
                self.TAC.emit('=_int', p[0].temp, p[1].temp, '')
                if str(p[2]) == '++':
                    self.TAC.emit('+_int', p[1].temp, p[1].temp, f'$1')
                else:
                    self.TAC.emit('-_int', p[1].temp, p[1].temp, f'$1')
                p[0].truelist.append(self.TAC.nextstat)
                p[0].falselist.append(self.TAC.nextstat+1)
                self.TAC.emit('ifnz goto','',p[0].temp,'')
                self.TAC.emit('goto','','','')

        elif (len(p) == 4):
            if p[2] == '.':
                p3val = p[3]['lexeme']
                p[3] = Node(str(p3val))
                p[0] = Node('.',[p[1],p[3]])

                if p[1] == None or p[1].type == None or p[1].type == []:
                    self.ST.error = 1
                    print(f'Invalid request for member of object that is not a structure/union at line {p.lineno(2)}')
                    return

                if 'struct' not in p[1].type and 'union' not in p[1].type:
                    self.ST.error = 1
                    print(f'Invalid request for member of object that is not a structure/union at line {p.lineno(2)}')
                    return
                if not hasattr(p[1], "vars"):
                    self.ST.error = 1
                    return
                elif p3val not in p[1].vars:
                    self.ST.error = 1
                    print(f'Invalid request for member of object that does not belong to the structure/union at {p.lineno(2)}')
                else:
                    old_type_list = p[1].vars[p3val]['type']
                    isarr = 0
                    for i in range(len(old_type_list)):
                        if old_type_list[i][0]=='[' and old_type_list[i][-1] == ']':
                            isarr += 1
                    
                    type_list = old_type_list
                    if type_list is None or set(type_list) == {'*'}:
                        type_list = []
                    p[0].type = []
                    if 'long' in type_list and 'int' in type_list:
                        p[0].type.append('long int')
                        for single_type in type_list:
                            if single_type != 'long' and single_type != 'int':
                                p[0].type.append(single_type)
                    
                    elif 'long' in type_list and 'double' in type_list:
                        p[0].type.append('long double')
                        for single_type in type_list:
                            if single_type != 'long' and single_type != 'double':
                                p[0].type.append(single_type)
                    
                    elif 'long' in type_list:
                        p[0].type.append('long int')
                        for single_type in type_list:
                            if single_type != 'long':
                                p[0].type.append(single_type)

                    elif 'int' in type_list:
                        p[0].type.append('int')
                        for single_type in type_list:
                            if single_type != 'int':
                                p[0].type.append(single_type)

                    elif 'short' in type_list:
                        p[0].type.append('short')
                        for single_type in type_list:
                            if single_type != 'short':
                                p[0].type.append(single_type)
                    
                    elif 'char' in type_list:
                        p[0].type.append('char')
                        for single_type in type_list:
                            if single_type != 'char':
                                p[0].type.append(single_type)
                    
                    elif 'bool' in type_list:
                        p[0].type.append('bool')
                        for single_type in type_list:
                            if single_type != 'bool':
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

                    if 'struct' in type_list:
                        p[0].type.append('struct')
                        for single_type in type_list:
                            if single_type != 'struct':
                                    p[0].type.append(single_type)     

                    if 'union' in type_list:
                        p[0].type.append('union')
                        for single_type in type_list:
                            if single_type != 'union':
                                    p[0].type.append(single_type)    

                    if isarr > 0:
                        temp_type = []
                        temp_type.append(p[0].type[0])
                        for i in range(isarr):
                            temp_type[0] += ' *'

                        for i in range(len(p[0].type)):
                            if i>isarr:
                                temp_type.append(p[0].type[i])
                        p[0].type = temp_type
                        p[0].type.append('arr')
                        for i in range(len(type_list)):
                            if type_list[len(type_list)-i-1][0] == '[' and type_list[len(type_list)-i-1][-1] == ']':  
                                p[0].type.append(type_list[len(type_list)-i-1])

                    if 'void' in type_list:
                        p[0].type.append('void')
                        for single_type in type_list:
                            if single_type != 'void':
                                    p[0].type.append(single_type)     

                    if '*' in type_list:
                        temp_type = []
                        temp_type.append(p[0].type[0])
                        for i in range(1, len(p[0].type)):
                            if p[0].type[i] == '*':
                                temp_type[0] += ' *'
                            else:
                                temp_type.append(p[0].type[i])
                        p[0].type = temp_type

                    if 'struct' in p[0].type[0] or 'union' in p[0].type[0]:

                        strtype = ''
                        if 'struct' in  p[0].type[0]:
                            strtype = 'struct'
                        elif 'union' in p[0].type[0]:
                            strtype = 'union'

                        typet = self.ST.TT.ReturnTypeTabEntry(p[0].type[1], strtype, p.lineno(3))
                        p[0].vars = typet['vars']

                        # self.ST.TT.ReturnTypeTabEntry(p2val, p[1].type[0], p.lineno(1))

                    if 'struct' not in p[0].type and 'union' not in p[0].type:
                        p[0].isvar = 1

                if self.ST.error:
                    return

                p[0].varname = p[1].varname + [p3val]
                found, entry = self.ST.ReturnSymTabEntry(p[0].varname[0], p.lineno(1))
                if found is not None :
                    if p[0].varname[1] in found["vars"].keys():
                        ptr_flag = 0
                        p[0].temp = p[1].temp
                        if p[0].temp[0] == '(':
                            ptr_flag = 1
                            p0_offset = 0
                        else:
                            p0_offset = int(p[0].temp.split('(')[0])
                        if len(p[0].varname) == 2:
                            tmp_offset = 0
                            for item in found["vars"]:
                                if item == p[0].varname[1]:
                                    break
                                tmp_offset += found["vars"][item]["sizeAllocInBytes"]
                            p0_offset += tmp_offset
                            if ptr_flag == 1:
                                self.TAC.emit("+_int", p[0].temp[1:-1], p[0].temp[1:-1], f'${p0_offset}')
                            else:
                                p[0].temp = f'{p0_offset}(%ebp)'
                        else:
                            type_to_check = found["vars"][p[0].varname[1]]["type"]
                            idx = 2
                            while "struct" in type_to_check:
                                tmp_type = copy.deepcopy(type_to_check)
                                tmp_type.remove('struct')
                                if '*' in tmp_type:
                                    tmp_type.remove('*')
                                found2 = self.ST.TT.ReturnTypeTabEntry(tmp_type[0] , 'struct')
                                if idx == len(p[0].varname) - 1:
                                    tmp_offset = 0
                                    for item in found2["vars"]:
                                        if item == p[0].varname[idx]:
                                            break
                                        tmp_offset += found2["vars"][item]["sizeAllocInBytes"]
                                    p0_offset += tmp_offset
                                    if ptr_flag == 1:
                                        self.TAC.emit("+_int", p[0].temp[1:-1], p[0].temp[1:-1], f'${p0_offset}')
                                    else:
                                        p[0].temp = f'{p0_offset}(%ebp)'
                                    break
                                type_to_check = found2["vars"][p[0].varname[idx]]['type']
                                idx += 1
                
                # if p[1].label == 'UNARY*':
                #     p[0].temp = p[1].temp[1:-1]

                #     # # Uncomment if you need entry from type table instead. But note that type table does not have offset
                #     # strtype = ''
                #     # if 'struct' in  p[1].type[0]:
                #     #     strtype = 'struct'
                #     # elif 'union' in p[1].type[0]:
                #     #     strtype = 'union'
                #     # found = self.ST.TT.ReturnTypeTabEntry(p[1].type[1], strtype)
                #     found, entry = self.ST.ReturnSymTabEntry(p[1].varname[0])

                #     self.TAC.emit('+_int', p[0].temp, p[0].temp, f"${found['vars'][p[3].label]['offset']}")
                    # p[0].temp = f'({p[0].temp})'

                # self.TAC.emit('.', p[0].temp, p[1].temp, p[3].label)
                p[0].truelist.append(self.TAC.nextstat)
                p[0].falselist.append(self.TAC.nextstat+1)
                self.TAC.emit('ifnz goto','',p[0].temp,'')
                self.TAC.emit('goto','','','')

            elif p[2] == '(':
                p[0] = Node('FuncCall',[p[1]])
                if p[1].type is None:
                    p[1].type = []
                
                if 'func' not in p[1].type:
                    self.ST.error = 1
                    print(f'Cannot call non-function at line {p.lineno(2)}')
                    return

                elif p[1].param_nums != 0:
                    self.ST.error = 1
                    print(f'{p[1].param_nums} Parameters required to call function at line {p.lineno(2)} ')
                    return

                else:
                    p[0].type = p[1].ret_type
                
                p[0].varname = p[1].varname
                if self.ST.error:
                    return

                p[0].temp = self.TAC.newtemp()
                self.ST.InsertSymbol(p[0].temp, 0)
                self.ST.ModifySymbol(p[0].temp, "type", p[0].type)
                self.ST.ModifySymbol(p[0].temp, "check", "TEMP")
                self.updateSizeInSymTab(p[0].type, p[0].temp)
                if self.ST.isGlobal(p[0].temp):
                    self.ST.ModifySymbol(p[0].temp, "varclass", "Global")
                else :
                    self.ST.ModifySymbol(p[0].temp, "varclass", "Local")
                    found, entry = self.ST.ReturnSymTabEntry(p[0].temp)
                    var_size = found['sizeAllocInBytes']
                    if found["varclass"] == "Local":
                        # self.TAC.emit('-_int', '%esp', '%esp', f'${var_size}')
                        if found["offset"] >0:
                            self.ST.ModifySymbol(p[0].temp, 'temp', f'-{found["offset"] + found["sizeAllocInBytes"] }(%ebp)')
                        else:
                            self.ST.ModifySymbol(p[0].temp, 'temp', f'{-found["offset"] - found["sizeAllocInBytes"] }(%ebp)')
                    p[0].temp = found['temp']

                found, entry = self.ST.ReturnSymTabEntry(p[1].label)
                if (("struct" in found["type"]) and ("*" not in found["type"])):
                    self.TAC.emit('callq_struct', p[0].temp, p[1].label , '0')
                elif found["type"] == ['void']:
                    self.TAC.emit('callq', '', p[1].label, '0')
                elif (("char" in found["type"]) and ("*" not in found["type"])):
                    self.TAC.emit('callq_char', p[0].temp, p[1].label , '0')
                else:
                    self.TAC.emit('callq', p[0].temp, p[1].label , '0')
                    p[0].truelist.append(self.TAC.nextstat)
                    p[0].falselist.append(self.TAC.nextstat+1)
                    self.TAC.emit('ifnz goto','',p[0].temp,'')
                    self.TAC.emit('goto','','','')

            elif p[2] == '->':
                p3val = p[3]['lexeme']
                p[3] = Node(str(p3val))

                p[0] = Node('->',[p[1],p[3]])

                if p[1] == None or p[1].type == None or p[1].type == []:
                    self.ST.error = 1
                    print(f'Invalid request for member of object that is not a structure/union at line {p.lineno(2)}')
                    return
                
                if p[1].type is None:
                    p[1].type = []

                if 'struct *' not in p[1].type and 'union *' not in p[1].type:
                    self.ST.error = 1
                    print(f'Invalid request for member of object that is not a pointer to a structure or union at line {p.lineno(2)}')
                elif p3val not in p[1].vars:
                    self.ST.error = 1
                    print(f'Invalid request for member of object that does not belong to the structure or union at {p.lineno(2)}')
                else:
                    old_type_list = p[1].vars[p3val]['type']
                    isarr = 0

                    for i in range(len(old_type_list)):
                        if old_type_list[i][0]=='[' and old_type_list[i][-1] == ']':
                            isarr += 1
                    
                    type_list = old_type_list

                    if type_list is None or set(type_list) == {'*'}:
                        type_list = []

                    p[0].type = []
                    if 'long' in type_list and 'int' in type_list:
                        p[0].type.append('long int')
                        for single_type in type_list:
                            if single_type != 'long' and single_type != 'int':
                                p[0].type.append(single_type)
                    
                    elif 'long' in type_list and 'double' in type_list:
                        p[0].type.append('long double')
                        for single_type in type_list:
                            if single_type != 'long' and single_type != 'double':
                                p[0].type.append(single_type)
                    
                    elif 'long' in type_list:
                        p[0].type.append('long int')
                        for single_type in type_list:
                            if single_type != 'long':
                                p[0].type.append(single_type)

                    elif 'int' in type_list:
                        p[0].type.append('int')
                        for single_type in type_list:
                            if single_type != 'int':
                                p[0].type.append(single_type)

                    elif 'short' in type_list:
                        p[0].type.append('short')
                        for single_type in type_list:
                            if single_type != 'short':
                                p[0].type.append(single_type)
                    
                    elif 'char' in type_list:
                        p[0].type.append('char')
                        for single_type in type_list:
                            if single_type != 'char':
                                p[0].type.append(single_type)
                    
                    elif 'bool' in type_list:
                        p[0].type.append('bool')
                        for single_type in type_list:
                            if single_type != 'bool':
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

                    if 'struct' in type_list:
                        p[0].type.append('struct')
                        for single_type in type_list:
                            if single_type != 'struct':
                                    p[0].type.append(single_type)     
                    if 'union' in type_list:
                        p[0].type.append('union')
                        for single_type in type_list:
                            if single_type != 'union':
                                    p[0].type.append(single_type)  

                    if isarr > 0:
                        temp_type = []
                        temp_type.append(p[0].type[0])
                        for i in range(isarr):
                            temp_type[0] += ' *'

                        for i in range(len(p[0].type)):
                            if i>isarr:
                                temp_type.append(p[0].type[i])
                        p[0].type = temp_type
                        p[0].type.append('arr')
                        for i in range(len(type_list)):
                            if type_list[len(type_list)-i-1][0] == '[' and type_list[len(type_list)-i-1][-1] == ']':  
                                p[0].type.append(type_list[len(type_list)-i-1])

                    if 'void' in type_list:
                        p[0].type.append('void')
                        for single_type in type_list:
                            if single_type != 'void':
                                    p[0].type.append(single_type)     

                    if '*' in type_list:
                        temp_type = []
                        temp_type.append(p[0].type[0])
                        for i in range(1, len(p[0].type)):
                            if p[0].type[i] == '*':
                                temp_type[0] += ' *'
                            else:
                                temp_type.append(p[0].type[i])
                        p[0].type = temp_type
                    
                    if 'struct' in p[0].type[0] or 'union' in p[0].type[0]:

                        strtype = ''
                        if 'struct' in  p[0].type[0]:
                            strtype = 'struct'
                        elif 'union' in p[0].type[0]:
                            strtype = 'union'

                        typet = self.ST.TT.ReturnTypeTabEntry(p[0].type[1], strtype, p.lineno(3))
                        p[0].vars = typet['vars']

                    if 'struct' not in p[0].type and 'union' not in p[0].type:
                        p[0].isvar = 1

                if self.ST.error:
                    return

                p[0].varname = p[1].varname + [p3val]
                p[0].temp = self.TAC.newtemp()
                self.ST.InsertSymbol(p[0].temp, 0)
                self.ST.ModifySymbol(p[0].temp, "type", ["int", "long"])
                self.ST.ModifySymbol(p[0].temp, "check", "TEMP")
                self.updateSizeInSymTab(["int", "long"], p[0].temp)
                if self.ST.isGlobal(p[0].temp):
                    self.ST.ModifySymbol(p[0].temp, "varclass", "Global")
                else :
                    self.ST.ModifySymbol(p[0].temp, "varclass", "Local")
                    found, entry = self.ST.ReturnSymTabEntry(p[0].temp)
                    var_size = found['sizeAllocInBytes']
                    if found["varclass"] == "Local":
                        # self.TAC.emit('-_int', '%esp', '%esp', f'${var_size}')
                        if found["offset"] >0:
                            self.ST.ModifySymbol(p[0].temp, 'temp', f'-{found["offset"] + found["sizeAllocInBytes"] }(%ebp)')
                        else:
                            self.ST.ModifySymbol(p[0].temp, 'temp', f'{-found["offset"] - found["sizeAllocInBytes"] }(%ebp)')
                    p[0].temp = found['temp']
                
                
                # # Uncomment if you need entry from type table instead. But note that type table does not have offset
                # strtype = ''
                # if 'struct' in  p[1].type[0]:
                #     strtype = 'struct'
                # elif 'union' in p[1].type[0]:
                #     strtype = 'union'
                # found = self.ST.TT.ReturnTypeTabEntry(p[1].type[1], strtype)

                found, entry = self.ST.ReturnSymTabEntry(p[0].varname[0], p.lineno(1))
                if found is not False :
                    if p[0].varname[1] in found["vars"].keys():
                        p0_offset = 0
                        if len(p[0].varname) == 2:
                            tmp_offset = 0
                            for item in found["vars"]:
                                if item == p[0].varname[1]:
                                    break
                                tmp_offset += found["vars"][item]["sizeAllocInBytes"]
                            p0_offset += tmp_offset
                            self.TAC.emit("+_int", p[0].temp, p[1].temp, f'${p0_offset}')
                        else:
                            type_to_check = found["vars"][p[0].varname[1]]["type"]
                            idx = 2
                            while "struct" in type_to_check:
                                tmp_type = copy.deepcopy(type_to_check)
                                tmp_type.remove('struct')
                                if '*' in tmp_type:
                                    tmp_type.remove('*')
                                found2 = self.ST.TT.ReturnTypeTabEntry(tmp_type[0] , 'struct')
                                if idx == len(p[0].varname) - 1:
                                    tmp_offset = 0
                                    for item in found2["vars"]:
                                        if item == p[0].varname[idx]:
                                            break
                                        tmp_offset += found2["vars"][item]["sizeAllocInBytes"]
                                    p0_offset += tmp_offset
                                    self.TAC.emit("+_int", p[0].temp, p[1].temp, f'${p0_offset}')
                                    break
                                type_to_check = found2["vars"][p[0].varname[idx]]['type']
                                idx += 1
                
                p[0].temp = f'({p[0].temp})'
                p[0].truelist.append(self.TAC.nextstat)
                p[0].falselist.append(self.TAC.nextstat+1)
                self.TAC.emit('ifnz goto','',p[0].temp,'')
                self.TAC.emit('goto','','','')

        elif (len(p) == 5):
            if p[2] == '(':
                p[0] = Node('FuncCall',[p[1],p[3]])
                if p[1] is None or p[1].type is None or p[1].param_nums is None or p[3] is None or p[3].param_nums is None or p[1].type == [] or p[1].params is None:
                    self.ST.error = 1
                    print(f'Cannot perform function call at line {p.lineno(2)}')
                    return               

                elif 'func' not in p[1].type:
                    self.ST.error = 1
                    print(f'Cannot call non-function at line {p.lineno(2)}')
                    return

                elif p[3].param_nums != p[1].param_nums:
                    self.ST.error = 1
                    print(f'Incorrect number of parameters (given: {p[3].param_nums}, required: {p[1].param_nums}) at line  {p.lineno(2)}')
                    return
                else:
                    ctr = -1
                    for param in p[1].params:
                        ctr += 1
                        entry = param
                        isarr = 0
                        for i in range(len(entry['type'])):
                            if entry['type'][i][0]=='[' and entry['type'][i][-1] == ']':
                                isarr += 1

                        type_list = entry['type']
                        if 'unsigned' in type_list or 'signed' in type_list:
                            if 'bool' not in type_list and 'char' not in type_list and 'short' not in type_list:
                                type_list.append('int')
                        paramtype = []
                        if 'long' in type_list and 'int' in type_list:
                            paramtype.append('long int')
                            for single_type in type_list:
                                if single_type != 'long' and single_type != 'int':
                                    paramtype.append(single_type)
                        
                        elif 'long' in type_list and 'double' in type_list:
                            paramtype.append('long double')
                            for single_type in type_list:
                                if single_type != 'long' and single_type != 'double':
                                    paramtype.append(single_type)
                        
                        elif 'long' in type_list:
                            paramtype.append('long int')
                            for single_type in type_list:
                                if single_type != 'long':
                                    paramtype.append(single_type)

                        elif 'int' in type_list:
                            paramtype.append('int')
                            for single_type in type_list:
                                if single_type != 'int':
                                    paramtype.append(single_type)

                        elif 'short' in type_list:
                            paramtype.append('short')
                            for single_type in type_list:
                                if single_type != 'short':
                                    paramtype.append(single_type)
                        
                        elif 'char' in type_list:
                            paramtype.append('char')
                            for single_type in type_list:
                                if single_type != 'char':
                                    paramtype.append(single_type)
                        
                        elif 'bool' in type_list:
                            paramtype.append('bool')
                            for single_type in type_list:
                                if single_type != 'bool':
                                    paramtype.append(single_type)
                        
                        elif 'str' in type_list:
                            paramtype.append('str')
                            for single_type in type_list:
                                if single_type != 'str':
                                    paramtype.append(single_type)
                        
                        elif 'float' in type_list:
                            paramtype.append('float')
                            for single_type in type_list:
                                if single_type != 'float':
                                    paramtype.append(single_type)

                        elif 'double' in type_list:
                            paramtype.append('double')
                            for single_type in type_list:
                                if single_type != 'double':
                                    paramtype.append(single_type)

                        if 'struct' in type_list:
                            paramtype.append('struct')
                            for single_type in type_list:
                                if single_type != 'struct':
                                        paramtype.append(single_type)     

                        if 'union' in type_list:
                            paramtype.append('union')
                            for single_type in type_list:
                                if single_type != 'union':
                                        paramtype.append(single_type)     

                        if isarr > 0:
                            temp_type = []
                            temp_type.append(paramtype[0])
                            for i in range(isarr):
                                temp_type[0] += ' *'

                            for i in range(len(paramtype)):
                                if i>isarr:
                                    temp_type.append(paramtype[i])
                            paramtype = temp_type
                            paramtype.append('arr')
                            for i in range(len(type_list)):
                                if type_list[len(type_list)-i-1][0] == '[' and type_list[len(type_list)-i-1][-1] == ']':  
                                    paramtype.append(type_list[len(type_list)-i-1])

                        if 'void' in type_list:
                            paramtype.append('void')
                            for single_type in type_list:
                                if single_type != 'void':
                                        paramtype.append(single_type)     

                        if '*' in type_list:
                            temp_type = []
                            temp_type.append(paramtype[0])
                            for i in range(1, len(paramtype)):
                                if paramtype[i] == '*':
                                    temp_type[0] += ' *'
                                else:
                                    temp_type.append(paramtype[i])
                            paramtype = temp_type

                        if p[3] is None or paramtype is None or p[3].params[ctr] is None or paramtype == [] or p[3].params[ctr] == []:
                            self.ST.error = 1
                            print(f'Cannot call function at line {p.lineno(2)}')
                            return

                        if ('struct' in paramtype or 'union' in paramtype) and ('struct' not in p[3].params[ctr] and 'union' not in p[3].params[ctr]):
                            self.ST.error = 1;
                            print(f'Need struct/union value {paramtype} to call function but got non- struct/union type {p[3].params[ctr]} at line {p.lineno(2)}')
                            return

                        if ('struct' not in paramtype and 'union' not in paramtype) and ('struct'  in p[3].params[ctr] or 'union'  in p[3].params[ctr]):
                            self.ST.error = 1;
                            print(f'Need non-struct/union value {paramtype} to call function but got struct/union type {p[3].params[ctr]} at line {p.lineno(2)}')
                            return

                        if 'struct' in paramtype and 'struct' in p[3].params[ctr] and paramtype[1] != p[3].params[ctr][1]:
                            self.ST.error = 1;
                            print(f'Incompatible struct types to call function at line {p.lineno(2)}')
                            return
                        
                        if 'union' in paramtype and 'union' in p[3].params[ctr] and paramtype[1] != p[3].params[ctr][1]:
                            self.ST.error = 1;
                            print(f'Incompatible union types to call function at line {p.lineno(2)}')
                            return
                        
                        if paramtype[0] in aat and p[3].params[ctr][0] not in aat and p[3].params[ctr][0][-1] != '*':
                            self.ST.error = 1
                            print(f'Invalid parameter type to call function at line {p.lineno(2)}')
                            return

                        if paramtype[0] not in aat and paramtype[0][-1] != '*' and p[3].params[ctr][0] in aat:
                            self.ST.error = 1
                            print(f'Invalid parameter type to call function at line {p.lineno(2)}')
                            return

                        
                        if paramtype[0][-1] == '*' and p[3].params[ctr][0] not in iit and p[3].params[ctr][0][-1] != '*' and 'str' not in p[3].params[ctr]:    
                            self.ST.error = 1
                            print(f'Incompatible assignment between pointer and {p[3].params[ctr]} at line {p.lineno(2)}')
                            return

                        if self.ST.error:
                            return
                        
                        isin = True
                        p3totype = []
                        for single_type in paramtype:
                            if single_type != 'arr'  and single_type[0] != '[' and single_type[-1] != ']':
                                p3totype.append(single_type)
                                if single_type not in p[3].params[ctr]:
                                    isin = False

                        if isin == False: 
                            
                            p3temp = self.TAC.newtemp()
                            self.ST.InsertSymbol(p3temp, 0)
                            self.ST.ModifySymbol(p3temp, "type", p3totype)
                            self.ST.ModifySymbol(p3temp, "check", "TEMP")
                            self.updateSizeInSymTab(p3totype, p3temp)
                            if self.ST.isGlobal(p3temp):
                                self.ST.ModifySymbol(p3temp, "varclass", "Global")
                            else :
                                self.ST.ModifySymbol(p3temp, "varclass", "Local")
                                found, entry = self.ST.ReturnSymTabEntry(p3temp)
                                var_size = found['sizeAllocInBytes']
                                if found["varclass"] == "Local":
                                    # self.TAC.emit('-_int', '%esp', '%esp', f'${var_size}')
                                    if found["offset"] >0:
                                        self.ST.ModifySymbol(p3temp, 'temp', f'-{found["offset"] + found["sizeAllocInBytes"] }(%ebp)')
                                    else:
                                        self.ST.ModifySymbol(p3temp, 'temp', f'{-found["offset"] - found["sizeAllocInBytes"] }(%ebp)')
                                p3temp = found['temp']  

                            fromtype = p[3].params[ctr]
                            currtype = []
                            for single_type in fromtype:
                                if single_type != 'arr'  and single_type[0] != '[' and single_type[-1] != ']':
                                    currtype.append(single_type)
                            currtyprstr =',' +  ' '.join(currtype).replace(' ','_')

                            self.TAC.emit('cast',p3temp, p[3].arglist[ctr][0], ' '.join(p3totype).replace(' ','_') + currtyprstr) 
                            p[3].arglist[ctr] = [p3temp , p3totype]

                    p[0].type = p[1].ret_type

                p[0].varname = p[1].varname
                if self.ST.error:
                    return

                p[0].temp = self.TAC.newtemp()
                self.ST.InsertSymbol(p[0].temp, 0)
                self.ST.ModifySymbol(p[0].temp, "type", p[0].type)
                self.ST.ModifySymbol(p[0].temp, "check", "TEMP")
                self.updateSizeInSymTab(p[0].type, p[0].temp)
                if self.ST.isGlobal(p[0].temp):
                    self.ST.ModifySymbol(p[0].temp, "varclass", "Global")
                else :
                    self.ST.ModifySymbol(p[0].temp, "varclass", "Local")
                    found, entry = self.ST.ReturnSymTabEntry(p[0].temp)
                    var_size = found['sizeAllocInBytes']
                    if found["varclass"] == "Local":
                        # self.TAC.emit('-_int', '%esp', '%esp', f'${var_size}')
                        if found["offset"] >0:
                            self.ST.ModifySymbol(p[0].temp, 'temp', f'-{found["offset"] + found["sizeAllocInBytes"] }(%ebp)')
                        else:
                            self.ST.ModifySymbol(p[0].temp, 'temp', f'{-found["offset"] - found["sizeAllocInBytes"] }(%ebp)')
                    p[0].temp = found['temp']


                math_funcs_list_single = ["sqrt", "ceil", "floor", "fabs", "log", "log10", "exp", "cos" ,"sin", "acos", "asin", "tan", "atan"]
                math_funcs_list_double = ["pow", 'fmod']
                for arg in reversed(p[3].arglist):
                    if p[1].label == 'printf':
                        if arg[0][0] == '$':
                            self.TAC.emit('param', arg[0],'','')
                        else:
                            if 'float' in arg[1]:
                                self.TAC.emit('printf_push_float', arg[0])
                            elif 'char' in arg[1]:
                                self.TAC.emit('printf_push_char', arg[0])
                            else:
                                self.TAC.emit('param', arg[0],'','')
                    elif p[1].label in math_funcs_list_single:
                        if 'float' in arg[1]:
                            self.TAC.emit('math_func_push_float', arg[0])
                        elif 'int' in arg[1]:
                            self.TAC.emit('math_func_push_int', arg[0])
                        elif 'char' in arg[1]:
                            self.TAC.emit('push_char', arg[0])
                        else:
                            self.TAC.emit('math_func_push_int', arg[0])
                    elif p[1].label in math_funcs_list_double:
                        if 'float' in arg[1]:
                            self.TAC.emit('pow_func_push_float', arg[0])
                        elif 'int' in arg[1]:
                            self.TAC.emit('pow_func_push_int', arg[0])
                        elif 'char' in arg[1]:
                            self.TAC.emit('push_char', arg[0])
                        else:
                            self.TAC.emit('pow_func_push_int', arg[0])
                    else:  
                        new_p2_list = []
                        for elem in arg[1]:
                            if elem != 'arr' and elem[0] != '[' and elem[-1] != ']':
                                new_p2_list = new_p2_list + elem.split(' ')
                        req_type = 'void'
                        if 'char' in arg[1]:
                            self.TAC.emit('push_char', arg[0])
                        else:
                            if '*' in new_p2_list:
                                req_type = 'PTR'
                            else:
                                req_type = ' '.join(new_p2_list)
                            if req_type in sizes:
                                if 'struct' in new_p2_list and '*' not in new_p2_list:
                                    to_print = self.recurse_struct(new_p2_list, arg[0])
                                    to_print.reverse()
                                    for item in to_print:
                                        if item[0] == 1:
                                            self.TAC.emit('push_char', item[1])
                                        else:
                                            self.TAC.emit('param', item[1], '$4')
                                else:
                                    self.TAC.emit('param', arg[0], f'${sizes[req_type]}')
                            else:
                                self.ST.error = 1
                                print(f'Invalid type given in line number {p.lineno(4)}')

                found, entry = self.ST.ReturnSymTabEntry(p[1].label)
                if (("struct" in found["type"]) and ("*" not in found["type"])):
                    self.TAC.emit('callq_struct', p[0].temp, p[1].label, len(p[3].arglist))
                elif found["type"] == ['void']:
                    self.TAC.emit('callq', '', p[1].label, len(p[3].arglist))
                elif (("char" in found["type"]) and ("*" not in found["type"])):
                    self.TAC.emit('callq_char', p[0].temp, p[1].label, len(p[3].arglist))
                else:
                    self.TAC.emit('callq', p[0].temp, p[1].label , len(p[3].arglist))
                    p[0].truelist.append(self.TAC.nextstat)
                    p[0].falselist.append(self.TAC.nextstat+1)
                    self.TAC.emit('ifnz goto','',p[0].temp,'')
                    self.TAC.emit('goto','','','')
            elif p[2] == '[':

                if p[3] is None or p[3].type is None or p[3].type == [] or p[1] is None or p[1].type is None or p[1].type == []:
                    self.ST.error = 1
                    print(f'Invalid call to access array element at line {p.lineno(2)}')
                    return

                flag = 0
                if p[3].type[0] in iit:
                    flag = 1    

                if flag==0:
                    self.ST.error = 1
                    print(f'Invalid array subscript of type {p[3].type} at line {p.lineno(2)}')
                    return
                else:
                    if p[1].type[0][-1] != '*':
                        self.ST.error = 1
                        print(f'Expression of type {p[1].type} not an array at line {p.lineno(2)}')
                        return
                    else:        

                        p[0] = Node('ArrSub',[p[1],p[3]])
                        p[0].type = []
                        for single_type in p[1].type:
                            if single_type[0] == '[' and single_type[-1] == ']':
                                pass
                            else:
                                p[0].type.append(single_type)

                        p[0].type[0] = p[0].type[0][0:-2]
                        p[0].arrlvl = p[1].arrlvl - 1

                        if p[0].type[0][-1] != '*':
                            p[0].isvar = 1
                        elif 'arr' not in p[0].type:
                            p[0].isvar = 1
                        for i in range(len(p[1].type)):
                            if p[1].type[i][0] == '[' and p[1].type[i][-1] == ']':
                                p[0].type.append(p[1].type[i])

                        if 'struct' in p[0].type[0] or 'union' in p[0].type[0]:
                            p[0].vars = p[1].vars

                p[0].varname = p[1].varname
                p[0].temp = self.TAC.newtemp()
                self.ST.InsertSymbol(p[0].temp, 0)
                self.ST.ModifySymbol(p[0].temp, "type", ["int"])
                self.ST.ModifySymbol(p[0].temp, "check", "TEMP")
                self.updateSizeInSymTab(["int"], p[0].temp)
                if self.ST.isGlobal(p[0].temp):
                    self.ST.ModifySymbol(p[0].temp, "varclass", "Global")
                else :
                    self.ST.ModifySymbol(p[0].temp, "varclass", "Local")
                    found, entry = self.ST.ReturnSymTabEntry(p[0].temp)
                    var_size = found['sizeAllocInBytes']
                    if found["varclass"] == "Local":
                        # self.TAC.emit('-_int', '%esp', '%esp', f'${var_size}')
                        if found["offset"] >0:
                            self.ST.ModifySymbol(p[0].temp, 'temp', f'-{found["offset"] + found["sizeAllocInBytes"] }(%ebp)')
                        else:
                            self.ST.ModifySymbol(p[0].temp, 'temp', f'{-found["offset"] - found["sizeAllocInBytes"] }(%ebp)')
                    p[0].temp = found['temp']


                p[0].dimensionList = p[1].dimensionList
                isFirstAccess = False

                if p[0].dimensionList is None:

                    arrtemp = self.TAC.newtemp()
                    self.ST.InsertSymbol(arrtemp, 0)
                    self.ST.ModifySymbol(arrtemp, "type", ["int"])
                    self.ST.ModifySymbol(arrtemp, "check", "TEMP")
                    self.updateSizeInSymTab(["int"], arrtemp)
                    if self.ST.isGlobal(arrtemp):
                        self.ST.ModifySymbol(arrtemp, "varclass", "Global")
                    else :
                        self.ST.ModifySymbol(arrtemp, "varclass", "Local")
                        found, entry = self.ST.ReturnSymTabEntry(arrtemp)
                        var_size = found['sizeAllocInBytes']
                        if found["varclass"] == "Local":
                            # self.TAC.emit('-_int', '%esp', '%esp', f'${var_size}')
                            if found["offset"] >0:
                                self.ST.ModifySymbol(arrtemp, 'temp', f'-{found["offset"] + found["sizeAllocInBytes"] }(%ebp)')
                            else:
                                self.ST.ModifySymbol(arrtemp, 'temp', f'{-found["offset"] - found["sizeAllocInBytes"] }(%ebp)')
                        arrtemp = found['temp']

                    var = 0
                    if '*' in (' '.join(p[0].type)).split(' '):
                        var = 4
                    else:
                        var = sizes[' '.join(p[0].type)]
                    
                    self.TAC.emit('*_int', arrtemp , p[3].temp , f'${var}')

                    if p[1].addr is not None:
                        var = p[1].addr.split('(')[0]
                        if var[0] != '-':
                            var = '+' + var
                        self.TAC.emit('+_int', p[0].temp , f'%ebp{var}', arrtemp)
                    else:
                        self.TAC.emit('+_int', p[0].temp, p[1].temp, arrtemp)

                    self.TAC.emit('UNARY*', p[0].temp , p[0].temp , '')


                    p[0].temp = f'({p[0].temp})'

                    p[0].truelist.append(self.TAC.nextstat)
                    p[0].falselist.append(self.TAC.nextstat+1)
                    self.TAC.emit('ifnz goto','',p[0].temp,'')
                    self.TAC.emit('goto','','','')
                    p[0].addr = None

                else:
                    if len(p[0].dimensionList) > 0 and p[0].dimensionList[-1] == 'isFirstAccess':
                        isFirstAccess = True
                        p[0].dimensionList.pop()

                    if isFirstAccess:
                        self.TAC.emit('=_int', p[0].temp, p[3].temp , '') 
                    else:
                        if len(p[0].dimensionList) == 0:
                            self.ST.error = 1
                            return
                        curDimension = p[0].dimensionList[-1]
                        self.TAC.emit('*_int', p[0].temp, p[1].temp , f'${curDimension}')
                        self.TAC.emit('+_int', p[0].temp, p[0].temp, p[3].temp)
                    
                    p[0].dimensionList.pop()

                    if len(p[0].dimensionList) == 0:
                        if(p[0].type[0][-1] == '*'):
                            self.TAC.emit('*_int', p[0].temp, p[0].temp, '$4')
                        else:
                            if 'struct' == p[0].type[0] or 'union' == p[0].type[0]:
                                strtype =  p[0].type[0] + ' ' + p[0].type[1] 
                                self.TAC.emit('*_int', p[0].temp, p[0].temp, f'${sizes[ strtype ]}')
                            else:
                                self.TAC.emit('*_int', p[0].temp, p[0].temp, f'${sizes[p[0].type[0]]}')

                        if p[1].addr is None or p[1].addr == '':
                            self.ST.error = 1
                            return
                        var = p[1].addr[4]
                        p1_addr = p[1].addr
                        if var == '+':
                            p1_addr = f'{p[1].addr[5:]}(%ebp)'
                        self.TAC.emit('+_int', p[0].temp, p1_addr, p[0].temp)
                        p[0].temp = f'({p[0].temp})'
                        
                        p[0].truelist.append(self.TAC.nextstat)
                        p[0].falselist.append(self.TAC.nextstat+1)
                        self.TAC.emit('ifnz goto','',p[0].temp,'')
                        self.TAC.emit('goto','','','') 

                p[0].addr = p[1].addr         

    def recurse_struct(self, new_p2_list, arg):
        to_print = []
        new_p2_list.remove('struct')
        found = self.ST.TT.ReturnTypeTabEntry(new_p2_list[0], 'struct')
        curr_offset = int(arg.split('(')[0])
        if found:
            for item in found["vars"]:
                if 'struct' in found['vars'][item]['type'] and '*' not in found['vars'][item]['type']:
                    ret_list = self.recurse_struct(found['vars'][item]['type'], f'{curr_offset}(%ebp)')
                    to_print.extend(ret_list)
                    curr_offset += found['vars'][item]['sizeAllocInBytes']
                else:
                    if '*' in found['vars'][item]['type']:
                        to_print.append([4, f'{curr_offset}(%ebp)'])
                    to_print.append([found['vars'][item]['sizeAllocInBytes'], f'{curr_offset}(%ebp)'])
                    curr_offset += found['vars'][item]['sizeAllocInBytes']
            return to_print

    def p_argument_expression_list(self,p):
        '''
        argument_expression_list : assignment_expression
                                | argument_expression_list ',' assignment_expression
        '''
        if self.isError :
            return
        if (len(p) == 2):
            p[0] = p[1]
            if p[1] is None:
                return
            p[0].param_nums = 1
            p[0].params = []
            p[0].params.append(p[1].type) 
            if p[1].type is None:
                p[1].type = ['Dummy']
            p[0].arglist =[[p[1].temp, p[1].type]]

        elif (len(p) == 4):
            p[0] = Node(',',[p[1],p[3]])
            if p[1] is None or p[1].param_nums is None or p[1].arglist is None or p[1].params is None or p[3] is None or p[3].temp is None or p[3].type is None or p[3].type == []:
                return
            p[0].param_nums = p[1].param_nums + 1
            p[0].params = p[1].params
            p[0].params.append(p[3].type)
            p[0].arglist = p[1].arglist
            p[0].arglist.append([p[3].temp,p[3].type])

    def p_unary_expression(self,p):
        '''
        unary_expression : postfix_expression
                        | INC_OP unary_expression
                        | DEC_OP unary_expression
                        | SIZEOF unary_expression
                        | unary_operator cast_expression
                        | SIZEOF '(' type_name ')'
        '''
        
        if self.isError :
            return
        if (len(p) == 2):
            p[0] = p[1]
        elif (len(p) == 3):

            if p[1] == '++' or p[1] == '--':
                if p[2] is None or p[2].type is None or p[2].type == []:
                    self.ST.error = 1
                    print(f'Cannot increase/decrease value of expression at line {p.lineno(1)}')
                elif 'const' in p[2].type:
                    self.ST.error = 1
                    print(f'Cannot increase/decrease value of read only variable at line {p.lineno(1)}')
                elif p[2].type[0] not in iit and p[2].type[0][-1] != '*':
                    self.ST.error = 1
                    print(f'Cannot use increment/decrement operator on non-integral at line {p.lineno(1)}')
                elif p[2].isTerminal == False and p[2].isvar==False:
                    self.ST.error = 1
                    print(f'Cannot use increment/decrement operator on expression at line {p.lineno(1)}')
                elif p[2].isvar == 0 and p[2].type[0][-1] != '*':
                    self.ST.error = 1
                    print(f'Cannot use increment/decrement operator on constant at line {p.lineno(2)}')
                elif p[2].type[0][-1] == '*' and 'arr' in p[2].type:
                    self.ST.error = 1
                    print(f'Cannot use increment/decrement operator on array type at line {p.lineno(2)}')  
                else:
                    p[0] = Node('PRE' + str(p[1]),[p[2]])
                    if p[2].type is None:
                        p[2].type = []
                    
                    p[0].type = p[2].type

                if self.ST.error:
                    return
                
                p[0].varname = p[2].varname
                p[0].temp = self.TAC.newtemp()
                self.ST.InsertSymbol(p[0].temp, 0)
                self.ST.ModifySymbol(p[0].temp, "type", p[0].type)
                self.ST.ModifySymbol(p[0].temp, "check", "TEMP")
                self.updateSizeInSymTab(p[0].type, p[0].temp)
                if self.ST.isGlobal(p[0].temp):
                    self.ST.ModifySymbol(p[0].temp, "varclass", "Global")
                else :
                    self.ST.ModifySymbol(p[0].temp, "varclass", "Local")
                    found, entry = self.ST.ReturnSymTabEntry(p[0].temp)
                    var_size = found['sizeAllocInBytes']
                    if found["varclass"] == "Local":
                        # self.TAC.emit('-_int', '%esp', '%esp', f'${var_size}')
                        if found["offset"] >0:
                            self.ST.ModifySymbol(p[0].temp, 'temp', f'-{found["offset"] + found["sizeAllocInBytes"] }(%ebp)')
                        else:
                            self.ST.ModifySymbol(p[0].temp, 'temp', f'{-found["offset"] - found["sizeAllocInBytes"] }(%ebp)')
                    p[0].temp = found['temp']
                self.TAC.emit('=_int', p[0].temp, p[2].temp, '')
                if str(p[1]) == '++':
                    self.TAC.emit('+_int', p[0].temp, p[0].temp, f'$1')
                    self.TAC.emit('+_int', p[2].temp, p[2].temp, f'$1')
                else:
                    self.TAC.emit('-_int', p[0].temp, p[0].temp, f'$1')
                    self.TAC.emit('-_int', p[2].temp, p[2].temp, f'$1')
                p[0].truelist.append(self.TAC.nextstat)
                p[0].falselist.append(self.TAC.nextstat+1)
                self.TAC.emit('ifnz goto','',p[0].temp,'')
                self.TAC.emit('goto','','','')

            elif p[1] == 'sizeof':
                p[0] = Node('SIZEOF',[p[2]])
                p[0].type = ['int']
                # tac
                if self.ST.error:
                    return
                p[0].temp = self.TAC.newtemp()
                self.ST.InsertSymbol(p[0].temp, 0)
                self.ST.ModifySymbol(p[0].temp, "type", p[0].type)
                self.ST.ModifySymbol(p[0].temp, "check", "TEMP")
                self.updateSizeInSymTab(p[0].type, p[0].temp)
                if self.ST.isGlobal(p[0].temp):
                    self.ST.ModifySymbol(p[0].temp, "varclass", "Global")
                else :
                    self.ST.ModifySymbol(p[0].temp, "varclass", "Local")
                    found, entry = self.ST.ReturnSymTabEntry(p[0].temp)
                    var_size = found['sizeAllocInBytes']
                    if found["varclass"] == "Local":
                        # self.TAC.emit('-_int', '%esp', '%esp', f'${var_size}')
                        if found["offset"] >0:
                            self.ST.ModifySymbol(p[0].temp, 'temp', f'-{found["offset"] + found["sizeAllocInBytes"] }(%ebp)')
                        else:
                            self.ST.ModifySymbol(p[0].temp, 'temp', f'{-found["offset"] - found["sizeAllocInBytes"] }(%ebp)')
                    p[0].temp = found['temp']

                p[0].varname = p[2].varname
                if p[2].type is None or p[2].type == []:
                    self.ST.error = 1
                    print(f'Invalid type given in line number {p.lineno(1)}')
                    return
                new_p2_list = []
                multiplier = 1
                for elem in p[2].type:
                    if elem != 'arr' and elem[0] != '[' and elem[-1] != ']':
                        new_p2_list = new_p2_list + elem.split(' ')

                    elif elem[0] == '[' and elem[-1] == ']':
                        multiplier *= int(elem[1:-1])
                
                req_type = 'void'
                if '*' in new_p2_list:
                    req_type = 'PTR'
                else:
                    req_type = ' '.join(new_p2_list)
                if req_type in sizes:
                    self.TAC.emit('=_int', p[0].temp, f'${multiplier*sizes[req_type]}')
                else:
                    self.ST.error = 1
                    print(f'Invalid type given in line number {p.lineno(1)}')
                return
                p[0].truelist.append(self.TAC.nextstat)
                p[0].falselist.append(self.TAC.nextstat+1)
                self.TAC.emit('ifnz goto','',p[0].temp,'')
                self.TAC.emit('goto','','','')

            else:
                p[0] = p[1]
                if ((p[2] is not None) and (p[2].node is not None)):

                    if p[2].type is None or p[2].type == []:
                        self.ST.error = 1
                        print(f'Cannot perform unary operation at line {p[1].lineno}')
                        return

                    if p[1].label[-1] in ['+', '-', '!']:
                        if len(p[2].type)>0 and p[2].type[0] in aat:
                            p[0].type = [p[2].type[0]]
                            if (p[2].type[0] in iit and p[2].type[0] not in dft and p[2].type[0] != 'long int' )  or p[1].label[-1] == '!':
                                p[0].type = ['int']
                            else:
                                pass

                            if p[0].label[-1] != '!':
                                if p[0].type != p[2].type:
                                    p[2].totype = p[0].type
                                    p2str = 'to'
                                    for single_type in p[0].type:
                                        p2str += '_' + single_type
                                    p2 = Node(p2str, [p[2]])
                                else:
                                    p[2].totype = None
                                    p2 = p[2]

                                for single_type in p[0].type:
                                    p[0].label += '_' + single_type
                                p[0].label = p[0].label.replace(' ','_')
                                p[0].node.attr['label'] = p[0].label

                        else:
                            self.ST.error = 1
                            print(f'Invalid Unary operator for operand type {p[2].type} at line {p[1].lineno}')
                            return

                    elif p[1].label[-1] == '~':
                        if len(p[2].type)>0 and p[2].type[0] in iit:
                            p[0].type = ['int']
                            if p[2].type[0] == 'long int':
                                p[0].type = ['long int']
                            else:
                                pass
                            if p[0].type != p[2].type:
                                p[2].totype = p[0].type
                                p2str = 'to'
                                for single_type in p[0].type:
                                    p2str += '_' + single_type
                                p2 = Node(p2str, [p[2]])
                            else:
                                p[2].totype = None
                                p2 = p[2]

                            for single_type in p[0].type:
                                p[0].label += '_' + single_type
                            p[0].label = p[0].label.replace(' ','_')
                            p[0].node.attr['label'] = p[0].label

                        else:
                            self.ST.error = 1
                            print(f'Invalid Unary operator for operand type {p[2].type} at line {p[1].lineno}')
                            return

                    elif p[1].label[-1] == '*':

                        if p[2] is None or p[2].type is None or p[2].type ==[]:
                            self.ST.error = 1
                            print(f'Cannot perform unary operation * at line {p[1].lineno}')
                            return              

                        elif len(p[2].type)>0 and p[2].type[0][-1] != '*' and ('*' not in p[2].type):
                            self.ST.error = 1
                            print(f'Invalid Unary operator for operand type {p[2].type} at line {p[1].lineno}')
                            return
                        else:
                            p[0].isvar = 1
                            p[0].type = p[2].type
                            p[0].type[0] = p[0].type[0][:-1]
                            if len(p[0].type)>0 and p[0].type[0][-1] == ' ':
                                p[0].type[0] = p[0].type[0][:-1]
                            try:
                                p[0].vars = p[2].vars
                            except:
                                pass

                    elif p[1].label[-1] == '&':

                        if p[2] is None or p[2].type is None or p[2].type ==[]:
                            self.ST.error = 1
                            print(f'Cannot perform unary operation * at line {p[1].lineno}')
                            return        

                        elif len(p[2].type)>0 and 'struct' != p[2].type[0] and 'union' != p[2].type[0] and p[2].isvar==0:
                            self.ST.error = 1
                            print(f'Cannot find pointer for non variable {p[2].type} at line {p[1].lineno}')
                            return
                        elif len(p[2].type)>0 and 'struct' == p[2].type[0] or 'union' == p[2].type[0]:
                            p[0].type = p[2].type
                            p[0].type[0] += ' *'
                            p[0].vars = p[2].vars
                        else:
                            p[0].type = ['int', 'unsigned']
                            # How to check if this is pointer

                    try:
                        p[0].onlyAddEdge([p2])
                    except:
                        p[0].onlyAddEdge([p[2]])
                    
                if self.ST.error:
                    return

                if p[2].totype is not None and p[2].totype != p[2].type:

                    p2.temp = self.TAC.newtemp()
                    self.ST.InsertSymbol(p2.temp, 0)
                    self.ST.ModifySymbol(p2.temp, "type", p[2].totype)
                    self.ST.ModifySymbol(p2.temp, "check", "TEMP")
                    self.updateSizeInSymTab(p[2].totype, p2.temp)
                    if self.ST.isGlobal(p2.temp):
                        self.ST.ModifySymbol(p2.temp, "varclass", "Global")
                    else :
                        self.ST.ModifySymbol(p2.temp, "varclass", "Local")
                        found, entry = self.ST.ReturnSymTabEntry(p2.temp)
                        var_size = found['sizeAllocInBytes']
                        if found["varclass"] == "Local":
                            # self.TAC.emit('-_int', '%esp', '%esp', f'${var_size}')
                            if found["offset"] >0:
                                self.ST.ModifySymbol(p2.temp, 'temp', f'-{found["offset"] + found["sizeAllocInBytes"] }(%ebp)')
                            else:
                                self.ST.ModifySymbol(p2.temp, 'temp', f'{-found["offset"] - found["sizeAllocInBytes"] }(%ebp)')
                        p2.temp = found['temp']  
                    
                    fromtype = p[2].type
                    currtype = []
                    for single_type in fromtype:
                        if single_type != 'arr'  and single_type[0] != '[' and single_type[-1] != ']':
                            currtype.append(single_type)
                    cstr =',' +  ' '.join(currtype).replace(' ','_')

                    self.TAC.emit('cast',p2.temp,p[2].temp,' '.join(p[2].totype).replace(' ','_') + cstr) 

                else:

                    try:
                        p2.temp = p[2].temp
                    except:
                        pass

                p[0].varname = p[2].varname
                p[0].temp = self.TAC.newtemp()
                # if p[1].label == 'UNARY*':
                #     p[0].type = ['*'] + p[0].type
                self.ST.InsertSymbol(p[0].temp, 0)
                self.ST.ModifySymbol(p[0].temp, "type", p[0].type)
                self.ST.ModifySymbol(p[0].temp, "check", "TEMP")

                if p[1].label == 'UNARY*':
                    found, entry = self.ST.ReturnSymTabEntry(p[2].varname[0])
                    var_size = found['sizeAllocInBytes']
                    self.ST.ModifySymbol(p[0].temp, "sizeAllocInBytes", var_size)
                else:
                    self.updateSizeInSymTab(p[0].type, p[0].temp)
                if self.ST.isGlobal(p[0].temp):
                    self.ST.ModifySymbol(p[0].temp, "varclass", "Global")
                else :
                    self.ST.ModifySymbol(p[0].temp, "varclass", "Local")
                    found, entry = self.ST.ReturnSymTabEntry(p[0].temp)
                    var_size = found['sizeAllocInBytes']
                    if found["varclass"] == "Local":
                        # self.TAC.emit('-_int', '%esp', '%esp', f'${var_size}')
                        if found["offset"] >0:
                            self.ST.ModifySymbol(p[0].temp, 'temp', f'-{found["offset"] + found["sizeAllocInBytes"] }(%ebp)')
                        else:
                            self.ST.ModifySymbol(p[0].temp, 'temp', f'{-found["offset"] - found["sizeAllocInBytes"] }(%ebp)')
                    p[0].temp = found['temp']
                try:
                    self.TAC.emit(p[0].label, p[0].temp, p2.temp)
                except:
                    self.TAC.emit(p[0].label, p[0].temp, p[2].temp)

                if p[1].label == 'UNARY*':
                    p[0].temp = f'({p[0].temp})'
                    
                if(p[1].label[-1] == '!'):
                    p[0].truelist = p[1].falselist
                    p[0].falselist = p[1].truelist
                else:
                    p[0].truelist.append(self.TAC.nextstat)
                    p[0].falselist.append(self.TAC.nextstat+1)
                    self.TAC.emit('ifnz goto','',p[0].temp,'')
                    self.TAC.emit('goto','','','')

        elif (len(p) == 5):
            p[0] = Node('SIZEOF',[p[3]])
            p[0].type = ['int']
            
            if self.ST.error:
                return
            # if len(p[3].temp) > 0 and p[3].temp[0] == '_':
            #     p[0].temp = p[3].temp
            # else:
            p[0].temp = self.TAC.newtemp()
            self.ST.InsertSymbol(p[0].temp, 0)
            self.ST.ModifySymbol(p[0].temp, "type", p[0].type)
            self.ST.ModifySymbol(p[0].temp, "check", "TEMP")
            self.updateSizeInSymTab(p[0].type, p[0].temp)
            if self.ST.isGlobal(p[0].temp):
                self.ST.ModifySymbol(p[0].temp, "varclass", "Global")
            else :
                self.ST.ModifySymbol(p[0].temp, "varclass", "Local")
                found, entry = self.ST.ReturnSymTabEntry(p[0].temp)
                var_size = found['sizeAllocInBytes']
                if found["varclass"] == "Local":
                    # self.TAC.emit('-_int', '%esp', '%esp', f'${var_size}')
                    if found["offset"] >0:
                        self.ST.ModifySymbol(p[0].temp, 'temp', f'-{found["offset"] + found["sizeAllocInBytes"] }(%ebp)')
                    else:
                        self.ST.ModifySymbol(p[0].temp, 'temp', f'{-found["offset"] - found["sizeAllocInBytes"] }(%ebp)')
                p[0].temp = found['temp']

            if p[3].type is None or p[3].type == []:
                self.ST.error = 1
                print(f'Invalid type given in line number {p.lineno(1)}')
                return
            req_type = 'void'
            if '*' in p[3].type:
                req_type = 'PTR'
            else:
                req_type = ' '.join(p[3].type)

            if req_type in sizes:
                self.TAC.emit('=_int', p[0].temp, f'${sizes[req_type]}')
            else:
                self.ST.error = 1
                print(f'Invalid type given in line number {p.lineno(1)}')
                return
            p[0].truelist.append(self.TAC.nextstat)
            p[0].falselist.append(self.TAC.nextstat+1)
            self.TAC.emit('ifnz goto','',p[0].temp,'')
            self.TAC.emit('goto','','','')

    def p_unary_operator(self, p):
        '''
        unary_operator : '&'
                    | '*'
                    | '+'
                    | '-'
                    | '~'
                    | '!'
        '''
        if self.isError :
            return
        # AST DONE
        p[0] = Node('UNARY' + str(p[1]))
        p[0].lineno = p.lineno(1)

    def p_cast_expression(self,p):
        '''
        cast_expression : unary_expression
                        | '(' type_name ')' cast_expression
        '''
        if self.isError :
            return
        if (len(p) == 2):
            p[0] = p[1]
        elif (len(p) == 5):
            p[0] = Node('CAST',[p[2],p[4]])
            if p[2] is None or p[2].type is None or p[2].type == [] or p[4] is None or p[4].type is None or p[4].type == []:
                self.ST.error = 1
                print(f'Cannot perform casting at line {p.lineno(1)}')
                return
            temp_type_list = []
            temp2_type_list = []
            for single_type in p[2].type:
                if len(single_type)>0 and single_type != '*':
                    temp_type_list.append(single_type)
                    if single_type[0] != '[' or single_type[-1] != ']':
                        temp2_type_list.append(single_type)

                if len(single_type)>0 and single_type[0] == '[' and single_type[-1] == ']':
                    if single_type[1:-1] == '':
                        self.ST.error = 1
                        print('Cannot have empty indices for array declarations at line', p.lineno(1))
                        return
                    elif int(single_type[1:-1]) <= 0:
                        self.ST.error = 1
                        print('Cannot have non-positive integers for array declarations at line', p.lineno(1))
                        return

            if len(temp2_type_list) != len(set(temp2_type_list)):
                self.ST.error = 1
                print('variables cannot have duplicating type of declarations at line', p.lineno(1))
                return

            if 'long' in p[2].type and 'short' in p[2].type:
                self.ST.error = 1
                print('variable cannot be both long and short at line', p.lineno(1))
                return
            elif 'unsigned' in p[2].type and 'signed' in p[2].type:
                self.ST.error = 1
                print('variable cannot be both signed and unsigned at line', p.lineno(1))
                return
            else:
                data_type_count = 0
                if p[2].type is None:
                    p[2].type = []

                if 'int' in p[2].type or 'short' in p[2].type  or 'unsigned' in p[2].type or 'signed' in p[2].type:
                    data_type_count += 1
                if 'char' in  p[2].type:
                    data_type_count += 1               
                if 'bool' in  p[2].type:
                    data_type_count += 1
                if 'float' in p[2].type:
                    data_type_count += 1
                if 'double' in p[2].type:
                    data_type_count += 1
                if 'void' in p[2].type:
                    data_type_count += 1
                if 'struct' in p[2].type:
                    data_type_count += 1
                if 'union' in p[2].type:
                    data_type_count += 1
                if data_type_count > 1:    
                    self.ST.error = 1
                    print('Two or more conflicting data types specified for variable at line', p.lineno(1)) 
                    return
                if 'long' in p[2].type:
                    if 'char' in p[2].type or 'bool' in  p[2].type or 'float' in  p[2].type or 'void' in  p[2].type:
                        self.ST.error = 1
                        print('Two or more conflicting data types specified for variable at line', p.lineno(1))
                        return

            isarr = 0
            for i in range(len(p[2].type)):
                if len(p[2].type[i])>0 and p[2].type[i][0]=='[' and p[2].type[i][-1] == ']':
                    isarr += 1
            
            type_list = p[2].type
            if type_list is None:
                type_list=[]

            p[0].type = []
                                         
            if 'unsigned' in type_list or 'signed' in type_list:
                if 'bool' not in type_list and 'char' not in type_list and 'short' not in type_list:
                    type_list.append('int')
                                             
            if 'long' in type_list and 'int' in type_list:
                p[0].type.append('long int')
                for single_type in type_list:
                    if single_type != 'long' and single_type != 'int':
                        p[0].type.append(single_type)
            
            elif 'long' in type_list and 'double' in type_list:
                p[0].type.append('long double')
                for single_type in type_list:
                    if single_type != 'long' and single_type != 'double':
                        p[0].type.append(single_type)
            
            elif 'long' in type_list:
                p[0].type.append('long int')
                for single_type in type_list:
                    if single_type != 'long':
                        p[0].type.append(single_type)

            elif 'int' in type_list:
                p[0].type.append('int')
                for single_type in type_list:
                    if single_type != 'int':
                        p[0].type.append(single_type)

            elif 'short' in type_list:
                p[0].type.append('short')
                for single_type in type_list:
                    if single_type != 'short':
                        p[0].type.append(single_type)
            
            elif 'char' in type_list:
                p[0].type.append('char')
                for single_type in type_list:
                    if single_type != 'char':
                        p[0].type.append(single_type)
            
            elif 'bool' in type_list:
                p[0].type.append('bool')
                for single_type in type_list:
                    if single_type != 'bool':
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

            if 'struct' in type_list:
                p[0].type.append('struct')
                for single_type in type_list:
                    if single_type != 'struct':
                            p[0].type.append(single_type)     

            if 'union' in type_list:
                p[0].type.append('union')
                for single_type in type_list:
                    if single_type != 'union':
                            p[0].type.append(single_type)     

            if isarr > 0:
                temp_type = []
                temp_type.append(p[0].type[0])
                for i in range(isarr):
                    temp_type[0] += ' *'

                for i in range(len(p[0].type)):
                    if i>isarr:
                        temp_type.append(p[0].type[i])
                p[0].type = temp_type
                p[0].type.append('arr')
                for i in range(len(type_list)):
                    if type_list[len(type_list)-i-1][0] == '[' and type_list[len(type_list)-i-1][-1] == ']':  
                        p[0].type.append(type_list[len(type_list)-i-1])

            if 'void' in type_list:
                p[0].type.append('void')
                for single_type in type_list:
                    if single_type != 'void':
                            p[0].type.append(single_type)     

            if '*' in type_list:
                temp_type = []
                temp_type.append(p[0].type[0])
                for i in range(1,len(p[0].type)):
                    if p[0].type[i] == '*':
                        temp_type[0] += ' *'
                    else:
                        temp_type.append(p[0].type[i])
                p[0].type = temp_type

            if p[2].type is None or p[4].type is None or p[0].type is None:
                self.ST.error = 1
                print(f'Cannot perform casting at line {p.lineno(1)}')

            elif 'struct' in p[2].type and '*' not in p[2].type and 'struct' not in p[4].type:
                self.ST.error = 1
                print(f'Cannot cast non-struct value {p[4].type} to struct type {p[2].type} at line {p.lineno(1)}')

            elif 'struct' in p[2].type and 'struct' in p[4].type and p[4].type[1] not in p[2].type:
                self.ST.error = 1
                print(f'Incompatible struct types to perform casting at line {p.lineno(1)}')

            elif 'union' in p[2].type and '*' not in p[2].type and 'union' not in p[4].type:
                self.ST.error = 1
                print(f'Cannot cast non-union value {p[4].type} to union type {p[2].type} at line {p.lineno(1)}')

            elif 'union' in p[2].type and 'union' in p[4].type and p[4].type[1] not in p[2].type:
                self.ST.error = 1
                print(f'Incompatible union types to perform casting at line {p.lineno(1)}')

            elif p[0].type[0] in aat and p[4].type[0] not in aat and p[4].type[0][-1] != '*':
                self.ST.error = 1
                print(f'Type mismatch while casting value at line {p.lineno(1)}')
            
            elif '*' in p[2].type and p[4].type[0] not in iit and p[4].type[0][-1] != '*':    
                self.ST.error = 1
                print(f'Incompatible casting between pointer and {p[4].type} at line {p.lineno(1)}')

            p[4].totype = p[0].type
            
            if self.ST.error:
                return
            p[0].temp = self.TAC.newtemp()
            self.ST.InsertSymbol(p[0].temp, 0)
            self.ST.ModifySymbol(p[0].temp, "type", p[0].type)
            self.ST.ModifySymbol(p[0].temp, "check", "TEMP")
            self.updateSizeInSymTab(p[0].type, p[0].temp)
            if self.ST.isGlobal(p[0].temp):
                self.ST.ModifySymbol(p[0].temp, "varclass", "Global")
            else :
                self.ST.ModifySymbol(p[0].temp, "varclass", "Local")
                found, entry = self.ST.ReturnSymTabEntry(p[0].temp)
                var_size = found['sizeAllocInBytes']
                if found["varclass"] == "Local":
                    # self.TAC.emit('-_int', '%esp', '%esp', f'${var_size}')
                    if found["offset"] >0:
                        self.ST.ModifySymbol(p[0].temp, 'temp', f'-{found["offset"] + found["sizeAllocInBytes"] }(%ebp)')
                    else:
                        self.ST.ModifySymbol(p[0].temp, 'temp', f'{-found["offset"] - found["sizeAllocInBytes"] }(%ebp)')
                p[0].temp = found['temp']

            fromtype = p[4].type
            currtype = []
            for single_type in fromtype:
                if single_type != 'arr'  and single_type[0] != '[' and single_type[-1] != ']':
                    currtype.append(single_type)
            cstr =',' +  ' '.join(currtype).replace(' ','_')

            self.TAC.emit('cast',p[0].temp, p[4].temp, ' '.join(p[4].totype).replace(' ','_') + cstr)
            p[0].truelist.append(self.TAC.nextstat)
            p[0].falselist.append(self.TAC.nextstat+1)
            self.TAC.emit('ifnz goto','',p[0].temp,'')
            self.TAC.emit('goto','','','')

    def p_mulitplicative_expression(self,p):
        '''
        multiplicative_expression : cast_expression
                                | multiplicative_expression '*' cast_expression
                                | multiplicative_expression '/' cast_expression
                                | multiplicative_expression '%' cast_expression
        '''
        if self.isError :
            return
        #AST DOne
        if (len(p) == 2):
            p[0] = p[1]
        elif (len(p) == 4):
            if p[1] is None or p[3] is None or p[1].type is None or p[3].type is None or p[1].type == [] or p[3].type == []:
                self.ST.error = 1
                print(f'Cannot perform multiplicative operation between expressions on line {p.lineno(2)}')
                return

            elif str(p[2]) == '%':
                if p[1].type[0] not in iit or p[3].type[0] not in iit:
                    self.ST.error = 1
                    print(f'Cannot perform modulo operation between expressions of type {p[1].type} and {p[3].type} on line {p.lineno(2)}')
                    return

                p0type = ['int']

                if 'long int' in p[1].type or 'long int' in p[3].type:
                    p0type[0] = 'long int'

                if 'unsigned' in p[1].type or 'unsigned' in p[3].type:
                    p0type.append('unsigned')

                p0typestr = "to"
                for single_type in p0type:
                    p0typestr += '_' + single_type
                p0typestr = p0typestr.replace(' ','_')

                isin = True
                for single_type in p0type:
                    if single_type not in p[1].type:
                        isin = False
                if isin == False:
                    p[1].totype = p0type
                    p1 = Node(p0typestr,[p[1]])
                else:
                    p1 = p[1]

                isin = True
                for single_type in p0type:
                    if single_type not in p[3].type:
                        isin = False
                if isin == False:
                    p[3].totype = p0type
                    p3 = Node(p0typestr,[p[3]])
                else:
                    p3 = p[3]

                p[0] = Node(str(p[2]),[p1,p3])
                p[0].type = p0type
                
                p[0].label = p[0].label + '_' + p[0].type[0]
                if len(p[0].type)==2:
                    p[0].label = p[0].label + '_' +  p[0].type[1]

                p[0].label = p[0].label.replace(" ", "_")
                p[0].node.attr['label'] = p[0].label
            
            elif len(p[1].type)>0  and p[1].type[0] in aat and len(p[3].type)>0 and p[3].type[0] in aat:
                p0type = []
                p0type.append(aat[max(aat.index(p[1].type[0]), aat.index(p[3].type[0]))])
                if ('unsigned' in p[1].type or 'unsigned' in p[3].type) and max(aat.index(p[1].type[0]), aat.index(p[3].type[0])) <= 4 :
                    p0type.append('unsigned')

                p0typestr = "to"
                for single_type in p0type:
                    p0typestr += '_' + single_type

                p0typestr = p0typestr.replace(' ','_')
                
                isin = True
                for single_type in p0type:
                    if single_type not in p[1].type:
                        isin = False
                if isin == False:
                    p[1].totype = p0type
                    p1 = Node(p0typestr,[p[1]])
                else:
                    p1 = p[1]

                isin = True
                for single_type in p0type:
                    if single_type not in p[3].type:
                        isin = False
                if isin == False:
                    p[3].totype = p0type
                    p3 = Node(p0typestr,[p[3]])
                else:
                    p3 = p[3]

                p[0] = Node(str(p[2]),[p1,p3])
                p[0].type = p0type
                
                p[0].label = p[0].label + '_' + p[0].type[0]
                if len(p[0].type)==2:
                    p[0].label = p[0].label + '_' +  p[0].type[1]

                p[0].label = p[0].label.replace(" ", "_")
                p[0].node.attr['label'] = p[0].label
            else :
                self.ST.error = 1
                print(f'Multiplictaive operation between incompatible types {p[1].type} and {p[3].type} on line {p.lineno(2)}')
            
            if self.ST.error:
                return

            if p[1].totype is not None and p[1].totype != p[1].type:
                p1.temp = self.TAC.newtemp()
                self.ST.InsertSymbol(p1.temp, 0)
                self.ST.ModifySymbol(p1.temp, "type", p[1].totype)
                self.ST.ModifySymbol(p1.temp, "check", "TEMP")
                self.updateSizeInSymTab(p[1].totype, p1.temp)
                if self.ST.isGlobal(p1.temp):
                    self.ST.ModifySymbol(p1.temp, "varclass", "Global")
                else :
                    self.ST.ModifySymbol(p1.temp, "varclass", "Local")
                    found, entry = self.ST.ReturnSymTabEntry(p1.temp)
                    var_size = found['sizeAllocInBytes']
                    if found["varclass"] == "Local":
                        # self.TAC.emit('-_int', '%esp', '%esp', f'${var_size}')
                        if found["offset"] >0:
                            self.ST.ModifySymbol(p1.temp, 'temp', f'-{found["offset"] + found["sizeAllocInBytes"] }(%ebp)')
                        else:
                            self.ST.ModifySymbol(p1.temp, 'temp', f'{-found["offset"] - found["sizeAllocInBytes"] }(%ebp)')
                    p1.temp = found['temp']  
            
                fromtype = p[1].type
                currtype = []
                for single_type in fromtype:
                    if single_type != 'arr'  and single_type[0] != '[' and single_type[-1] != ']':
                        currtype.append(single_type)
                cstr =',' +  ' '.join(currtype).replace(' ','_')

                self.TAC.emit('cast',p1.temp,p[1].temp,' '.join(p[1].totype).replace(' ','_') + cstr) 

            else:
                p1.temp = p[1].temp

            if p[3].totype is not None and p[3].totype != p[3].type:
                p3.temp = self.TAC.newtemp()
                self.ST.InsertSymbol(p3.temp, 0)
                self.ST.ModifySymbol(p3.temp, "type", p[3].totype)
                self.ST.ModifySymbol(p3.temp, "check", "TEMP")
                self.updateSizeInSymTab(p[3].totype, p3.temp)
                if self.ST.isGlobal(p3.temp):
                    self.ST.ModifySymbol(p3.temp, "varclass", "Global")
                else :
                    self.ST.ModifySymbol(p3.temp, "varclass", "Local")
                    found, entry = self.ST.ReturnSymTabEntry(p3.temp)
                    var_size = found['sizeAllocInBytes']
                    if found["varclass"] == "Local":
                        # self.TAC.emit('-_int', '%esp', '%esp', f'${var_size}')
                        if found["offset"] >0:
                            self.ST.ModifySymbol(p3.temp, 'temp', f'-{found["offset"] + found["sizeAllocInBytes"] }(%ebp)')
                        else:
                            self.ST.ModifySymbol(p3.temp, 'temp', f'{-found["offset"] - found["sizeAllocInBytes"] }(%ebp)')
                    p3.temp = found['temp']  

                fromtype = p[3].type
                currtype = []
                for single_type in fromtype:
                    if single_type != 'arr'  and single_type[0] != '[' and single_type[-1] != ']':
                        currtype.append(single_type)
                cstr =',' +  ' '.join(currtype).replace(' ','_')

                self.TAC.emit('cast',p3.temp,p[3].temp,' '.join(p[3].totype).replace(' ','_') + cstr) 

            else:
                p3.temp = p[3].temp
                
            p[0].temp = self.TAC.newtemp()
            self.ST.InsertSymbol(p[0].temp, 0)
            self.ST.ModifySymbol(p[0].temp, "type", p[0].type)
            self.ST.ModifySymbol(p[0].temp, "check", "TEMP")
            self.updateSizeInSymTab(p[0].type, p[0].temp)
            if self.ST.isGlobal(p[0].temp):
                self.ST.ModifySymbol(p[0].temp, "varclass", "Global")
            else :
                self.ST.ModifySymbol(p[0].temp, "varclass", "Local")
                found, entry = self.ST.ReturnSymTabEntry(p[0].temp)
                var_size = found['sizeAllocInBytes']
                if found["varclass"] == "Local":
                    # self.TAC.emit('-_int', '%esp', '%esp', f'${var_size}')
                    if found["offset"] >0:
                        self.ST.ModifySymbol(p[0].temp, 'temp', f'-{found["offset"] + found["sizeAllocInBytes"] }(%ebp)')
                    else:
                        self.ST.ModifySymbol(p[0].temp, 'temp', f'{-found["offset"] - found["sizeAllocInBytes"] }(%ebp)')
                p[0].temp = found['temp']

            self.TAC.emit(p[0].label, p[0].temp, p1.temp, p3.temp)

            p[0].truelist.append(self.TAC.nextstat)
            p[0].falselist.append(self.TAC.nextstat+1)
            self.TAC.emit('ifnz goto','',p[0].temp,'')
            self.TAC.emit('goto','','','')

    def p_additive_expression(self, p):
        '''
        additive_expression : multiplicative_expression
                            | additive_expression '+' multiplicative_expression
                            | additive_expression '-' multiplicative_expression
        '''
        if self.isError :
            return
        
        if (len(p) == 2):
            p[0] = p[1]
        elif (len(p) == 4):
            
            if p[1] is None or p[3] is None or p[1].type is None or p[3].type is None or p[1].type == [] or p[3].type == []:
                self.ST.error = 1
                print(f'Cannot perform additive operation between expressions on line {p.lineno(2)}')

            elif len(p[1].type)>0 and p[1].type[0] in aat and len(p[3].type)>0 and p[3].type[0] in aat:
                p0type = []
                p0type.append(aat[max(aat.index(p[1].type[0]), aat.index(p[3].type[0]))])
                if ('unsigned' in p[1].type or 'unsigned' in p[3].type) and max(aat.index(p[1].type[0]), aat.index(p[3].type[0])) <= 4 :
                    p0type.append('unsigned')

                p0typestr = "to"
                for single_type in p0type:
                    p0typestr += '_' + single_type

                p0typestr = p0typestr.replace(' ','_')

                isin = True
                for single_type in p0type:
                    if single_type not in p[1].type:
                        isin = False
                if isin == False:
                    p[1].totype = p0type
                    p1 = Node(p0typestr,[p[1]])
                else:
                    p1 = p[1]

                isin = True
                for single_type in p0type:
                    if single_type not in p[3].type:
                        isin = False
                if isin == False:
                    p[3].totype = p0type
                    p3 = Node(p0typestr,[p[3]])
                else:
                    p3 = p[3]

                p[0] = Node(str(p[2]),[p1,p3])
                p[0].type = p0type       

                p[0].label = p[0].label + '_' +  p[0].type[0]
                if len(p[0].type)==2:
                    p[0].label = p[0].label + '_' +  p[0].type[1]

                p[0].label = p[0].label.replace(" ", "_")

                p[0].node.attr['label'] = p[0].label
                
            elif len(p[1].type)>0 and p[1].type[0][-1] == '*' and len(p[3].type)>0  and p[3].type[0] in iit:
                self.ST.error = 1
                print(f'Pointer Arithmetic not allowed at line {p.lineno(2)}')

                # p[0] = Node(str(p[2]),[p[1],p[3]])
                # p[0].label = p[0].label + '_' + p[1].type[0]
                # p[0].label = p[0].label.replace(" ", "_")
                # p[0].node.attr['label'] = p[0].label
                # p[0].type = p[1].type
                # p1 = p[1]
                # p3 = p[3]
                # try:
                #     p[0].vars = p[1].vars
                # except:
                #     pass   
                    
            elif len(p[3].type)>0 and p[3].type[0][-1] == '*' and len(p[1].type)>0 and p[1].type[0] in iit and str(p[2])=='+':
                self.ST.error = 1
                print(f'Pointer Arithmetic not allowed at line {p.lineno(2)}')
                
                # p[0] = Node(str(p[2]),[p[1],p[3]])
                # p[0].label = p[0].label + '_' + p[1].type[0]
                # p[0].label = p[0].label.replace(" ", "_")
                # p[0].node.attr['label'] = p[0].label
                # p[0].type = p[3].type
                # p1 = p[1]
                # p3 = p[3]
                # try:
                #     p[0].vars = p[1].vars
                # except:
                #     pass
                
            elif len(p[3].type)>0 and p[3].type[0][-1] == '*' and len(p[1].type)>0 and p[1].type[0] in iit and str(p[2])=='-':
                self.ST.error = 1
                print(f'Invalid binary - operation between incompatible types {p[1].type} and {p[3].type} on line {p.lineno(2)}')

            else :
                self.ST.error = 1
                print(f'Additive operation between incompatible types {p[1].type} and {p[3].type} on line {p.lineno(2)}')

            if self.ST.error:
                return

            if p[1].totype is not None and p[1].totype != p[1].type:
                p1.temp = self.TAC.newtemp()
                self.ST.InsertSymbol(p1.temp, 0)
                self.ST.ModifySymbol(p1.temp, "type", p[1].totype)
                self.ST.ModifySymbol(p1.temp, "check", "TEMP")
                self.updateSizeInSymTab(p[1].totype, p1.temp)
                if self.ST.isGlobal(p1.temp):
                    self.ST.ModifySymbol(p1.temp, "varclass", "Global")
                else :
                    self.ST.ModifySymbol(p1.temp, "varclass", "Local")
                    found, entry = self.ST.ReturnSymTabEntry(p1.temp)
                    var_size = found['sizeAllocInBytes']
                    if found["varclass"] == "Local":
                        # self.TAC.emit('-_int', '%esp', '%esp', f'${var_size}')
                        if found["offset"] >0:
                            self.ST.ModifySymbol(p1.temp, 'temp', f'-{found["offset"] + found["sizeAllocInBytes"] }(%ebp)')
                        else:
                            self.ST.ModifySymbol(p1.temp, 'temp', f'{-found["offset"] - found["sizeAllocInBytes"] }(%ebp)')
                    p1.temp = found['temp']  

                fromtype = p[1].type
                currtype = []
                for single_type in fromtype:
                    if single_type != 'arr'  and single_type[0] != '[' and single_type[-1] != ']':
                        currtype.append(single_type)
                cstr =',' +  ' '.join(currtype).replace(' ','_')

                self.TAC.emit('cast',p1.temp,p[1].temp,' '.join(p[1].totype).replace(' ','_') + cstr) 

            else:
                p1.temp = p[1].temp

            if p[3].totype is not None and p[3].totype != p[3].type:
                p3.temp = self.TAC.newtemp()
                self.ST.InsertSymbol(p3.temp, 0)
                self.ST.ModifySymbol(p3.temp, "type", p[3].totype)
                self.ST.ModifySymbol(p3.temp, "check", "TEMP")
                self.updateSizeInSymTab(p[3].totype, p3.temp)
                if self.ST.isGlobal(p3.temp):
                    self.ST.ModifySymbol(p3.temp, "varclass", "Global")
                else :
                    self.ST.ModifySymbol(p3.temp, "varclass", "Local")
                    found, entry = self.ST.ReturnSymTabEntry(p3.temp)
                    var_size = found['sizeAllocInBytes']
                    if found["varclass"] == "Local":
                        # self.TAC.emit('-_int', '%esp', '%esp', f'${var_size}')
                        if found["offset"] >0:
                            self.ST.ModifySymbol(p3.temp, 'temp', f'-{found["offset"] + found["sizeAllocInBytes"] }(%ebp)')
                        else:
                            self.ST.ModifySymbol(p3.temp, 'temp', f'{-found["offset"] - found["sizeAllocInBytes"] }(%ebp)')
                    p3.temp = found['temp']  

                fromtype = p[3].type
                currtype = []
                for single_type in fromtype:
                    if single_type != 'arr'  and single_type[0] != '[' and single_type[-1] != ']':
                        currtype.append(single_type)
                cstr =',' +  ' '.join(currtype).replace(' ','_')

                self.TAC.emit('cast',p3.temp,p[3].temp,' '.join(p[3].totype).replace(' ','_') + cstr) 

            else:
                p3.temp = p[3].temp
                
            p[0].temp = self.TAC.newtemp()
            self.ST.InsertSymbol(p[0].temp, 0)
            self.ST.ModifySymbol(p[0].temp, "type", p[0].type)
            self.ST.ModifySymbol(p[0].temp, "check", "TEMP")
            self.updateSizeInSymTab(p[0].type, p[0].temp)
            if self.ST.isGlobal(p[0].temp):
                self.ST.ModifySymbol(p[0].temp, "varclass", "Global")
            else :
                self.ST.ModifySymbol(p[0].temp, "varclass", "Local")
                found, entry = self.ST.ReturnSymTabEntry(p[0].temp)
                var_size = found['sizeAllocInBytes']
                if found["varclass"] == "Local":
                    # self.TAC.emit('-_int', '%esp', '%esp', f'${var_size}')
                    if found["offset"] >0:
                        self.ST.ModifySymbol(p[0].temp, 'temp', f'-{found["offset"] + found["sizeAllocInBytes"] }(%ebp)')
                    else:
                        self.ST.ModifySymbol(p[0].temp, 'temp', f'{-found["offset"] - found["sizeAllocInBytes"] }(%ebp)')
                p[0].temp = found['temp']

            self.TAC.emit(p[0].label, p[0].temp, p1.temp, p3.temp)
            
            p[0].truelist.append(self.TAC.nextstat)
            p[0].falselist.append(self.TAC.nextstat+1)
            self.TAC.emit('ifnz goto','',p[0].temp,'')
            self.TAC.emit('goto','','','')

    def p_shift_expression(self, p):
        '''
        shift_expression : additive_expression
                        | shift_expression LEFT_OP additive_expression
                        | shift_expression RIGHT_OP additive_expression
        '''
        if self.isError :
            return
        if (len(p) == 2):
            p[0] = p[1]
        elif (len(p) == 4):
            if p[1] is None or p[3] is None or p[1].type is None or p[3].type is None or p[1].type == [] or p[3].type == []:
                self.ST.error = 1
                print(f'Cannot perform bitshift operation between expressions on line {p.lineno(2)}')

            elif len(p[1].type)>0 and p[1].type[0] in iit and len(p[3].type)>0 and p[3].type[0] in iit:
                p0type = []
                p0label = str(p[2])
                p0typestr = 'to'
                
                if iit.index(p[1].type[0]) <= 3:
                    p0type = ['int']
                    p0label += '_int'
                    p0typestr += '_int'
                else:
                    p0type = ['long int']
                    p0label += '_long_int'
                    p0typestr += '_long_int'

                if 'unsigned' in p[1].type:
                    p0type.append('unsigned')
                    p0label += '_unsigned'
                    p0typestr += '_unsigned'

                isin = True
                for single_type in p0type:
                    if single_type not in p[1].type:
                        isin = False
                if isin == False:
                    p[1].totype = p0type
                    p1 = Node(p0typestr,[p[1]])
                else:
                    p1 = p[1]

                isin = True
                for single_type in p0type:
                    if single_type not in p[3].type:
                        isin = False
                if isin == False:
                    p[3].totype = p0type
                    p3 = Node(p0typestr,[p[3]])
                else:
                    p3 = p[3]

                p[0] = Node(str(p[2]),[p1,p[3]])
                p[0].type = p0type
                p[0].label = p0label

                p[0].label = p[0].label.replace(" ", "_")

                p[0].node.attr['label'] = p[0].label

            else:
                self.ST.error = 1
                print(f'Bitshift operation between incompatible types {p[1].type} and {p[3].type} on line {p.lineno(2)}')
            
            if self.ST.error:
                return
            if p[1].totype is not None and p[1].totype != p[1].type:
                p1.temp = self.TAC.newtemp()
                self.ST.InsertSymbol(p1.temp, 0)
                self.ST.ModifySymbol(p1.temp, "type", p[1].totype)
                self.ST.ModifySymbol(p1.temp, "check", "TEMP")
                self.updateSizeInSymTab(p[1].totype, p1.temp)
                if self.ST.isGlobal(p1.temp):
                    self.ST.ModifySymbol(p1.temp, "varclass", "Global")
                else :
                    self.ST.ModifySymbol(p1.temp, "varclass", "Local")
                    found, entry = self.ST.ReturnSymTabEntry(p1.temp)
                    var_size = found['sizeAllocInBytes']
                    if found["varclass"] == "Local":
                        # self.TAC.emit('-_int', '%esp', '%esp', f'${var_size}')
                        if found["offset"] >0:
                            self.ST.ModifySymbol(p1.temp, 'temp', f'-{found["offset"] + found["sizeAllocInBytes"] }(%ebp)')
                        else:
                            self.ST.ModifySymbol(p1.temp, 'temp', f'{-found["offset"] - found["sizeAllocInBytes"] }(%ebp)')
                    p1.temp = found['temp']  

                fromtype = p[1].type
                currtype = []
                for single_type in fromtype:
                    if single_type != 'arr'  and single_type[0] != '[' and single_type[-1] != ']':
                        currtype.append(single_type)
                cstr =',' +  ' '.join(currtype).replace(' ','_')

                self.TAC.emit('cast',p1.temp,p[1].temp,' '.join(p[1].totype).replace(' ','_') + cstr) 

            else:
                p1.temp = p[1].temp

            if p[3].totype is not None and p[3].totype != p[3].type:
                p3.temp = self.TAC.newtemp()
                self.ST.InsertSymbol(p3.temp, 0)
                self.ST.ModifySymbol(p3.temp, "type", p[3].totype)
                self.ST.ModifySymbol(p3.temp, "check", "TEMP")
                self.updateSizeInSymTab(p[3].totype, p3.temp)
                if self.ST.isGlobal(p3.temp):
                    self.ST.ModifySymbol(p3.temp, "varclass", "Global")
                else :
                    self.ST.ModifySymbol(p3.temp, "varclass", "Local")
                    found, entry = self.ST.ReturnSymTabEntry(p3.temp)
                    var_size = found['sizeAllocInBytes']
                    if found["varclass"] == "Local":
                        # self.TAC.emit('-_int', '%esp', '%esp', f'${var_size}')
                        if found["offset"] >0:
                            self.ST.ModifySymbol(p3.temp, 'temp', f'-{found["offset"] + found["sizeAllocInBytes"] }(%ebp)')
                        else:
                            self.ST.ModifySymbol(p3.temp, 'temp', f'{-found["offset"] - found["sizeAllocInBytes"] }(%ebp)')
                    p3.temp = found['temp']  

                fromtype = p[3].type
                currtype = []
                for single_type in fromtype:
                    if single_type != 'arr'  and single_type[0] != '[' and single_type[-1] != ']':
                        currtype.append(single_type)
                cstr =',' +  ' '.join(currtype).replace(' ','_')

                self.TAC.emit('cast',p3.temp,p[3].temp,' '.join(p[3].totype).replace(' ','_') + cstr) 

            else:
                p3.temp = p[3].temp
                
            p[0].temp = self.TAC.newtemp()
            self.ST.InsertSymbol(p[0].temp, 0)
            self.ST.ModifySymbol(p[0].temp, "type", p[0].type)
            self.ST.ModifySymbol(p[0].temp, "check", "TEMP")
            self.updateSizeInSymTab(p[0].type, p[0].temp)
            if self.ST.isGlobal(p[0].temp):
                self.ST.ModifySymbol(p[0].temp, "varclass", "Global")
            else :
                self.ST.ModifySymbol(p[0].temp, "varclass", "Local")
                found, entry = self.ST.ReturnSymTabEntry(p[0].temp)
                var_size = found['sizeAllocInBytes']
                if found["varclass"] == "Local":
                    # self.TAC.emit('-_int', '%esp', '%esp', f'${var_size}')
                    if found["offset"] >0:
                        self.ST.ModifySymbol(p[0].temp, 'temp', f'-{found["offset"] + found["sizeAllocInBytes"] }(%ebp)')
                    else:
                        self.ST.ModifySymbol(p[0].temp, 'temp', f'{-found["offset"] - found["sizeAllocInBytes"] }(%ebp)')
                p[0].temp = found['temp']

            self.TAC.emit(p[0].label, p[0].temp, p1.temp, p3.temp)

            p[0].truelist.append(self.TAC.nextstat)
            p[0].falselist.append(self.TAC.nextstat+1)
            self.TAC.emit('ifnz goto','',p[0].temp,'')
            self.TAC.emit('goto','','','')

    def p_relational_expression(self, p):
        '''
        relational_expression : shift_expression
                            | relational_expression '<' shift_expression
                            | relational_expression '>' shift_expression
                            | relational_expression LE_OP shift_expression
                            | relational_expression GE_OP shift_expression
        '''
        if self.isError :
            return
        if (len(p) == 2):
            p[0] = p[1]
        elif (len(p) == 4):
            if p[1] is None or p[3] is None or p[1].type is None or p[3].type is None or p[1].type == [] or p[3].type == []:
                self.ST.error = 1
                print(f'Cannot perform relational operation between expressions on line {p.lineno(2)}')

            elif len(p[1].type)>0 and p[1].type[0] in aat and len(p[3].type)>0 and p[3].type[0] in aat :
                p0type = ['int']
                p0label = str(p[2]) + '_' +  aat[max(aat.index(p[1].type[0]), aat.index(p[3].type[0]))]
                flag = 0
                if 'unsigned' in p[1].type or 'unsigned' in p[3].type and max(aat.index(p[1].type[0]), aat.index(p[3].type[0])) > 0 and max(aat.index(p[1].type[0]), aat.index(p[3].type[0])) < 5:
                    flag = 1
                    p0label = p0label + '_' +  'unsigned'

                p0label = p0label.replace(" ", "_")

                p[1].totype = None
                p[3].totype = None

                if aat[max(aat.index(p[1].type[0]), aat.index(p[3].type[0]))] not in p[1].type: 
                    p[1].totype = [aat[max(aat.index(p[1].type[0]), aat.index(p[3].type[0]))]]
                    if flag:
                        p[1].totype.append('unsigned')
                elif flag and 'unsigned' not in p[1].type:
                    p[1].totype = [aat[max(aat.index(p[1].type[0]), aat.index(p[3].type[0]))], 'unsigned']

                if p[1].totype != None and p[1].totype != p[1].type:
                    p1str = 'to'
                    for single_type in p[1].totype:
                        p1str += '_' + single_type
                    p1 = Node(p1str, [p[1]])
                else:
                    p1 = p[1]

                if aat[max(aat.index(p[1].type[0]), aat.index(p[3].type[0]))] not in p[3].type:
                    p[3].totype = [aat[max(aat.index(p[1].type[0]), aat.index(p[3].type[0]))]]
                    if flag:
                        p[3].totype.append('unsigned')
                elif flag and 'unsigned' not in p[3].type:
                    p[3].totype = [aat[max(aat.index(p[1].type[0]), aat.index(p[3].type[0]))], 'unsigned']
                
                if p[3].totype != None and p[3].totype != p[3].type:
                    p3str = 'to'
                    for single_type in p[3].totype:
                        p3str += '_' + single_type
                    p3 = Node(p3str, [p[3]])
                else:
                    p3 = p[3]

                p[0] = Node(str(p[2]), [p1, p3])
                p[0].type = p0type
                p[0].label = p0label
                p[0].node.attr['label'] = p[0].label

            elif len(p[1].type)>0 and p[1].type[0] == 'str' and len(p[3].type)>0 and p[3].type[0] == 'str':
                p[0] = Node(str(p[2]),[p[1],p[3]])
                p[0].type = ['int']
                p[0].label += '_str'
                p[0].label = p[0].label.replace(" ", "_")
                p[0].node.attr['label'] = p[0].label

            elif len(p[1].type)>0 and p[1].type[0][-1] == '*' and len(p[3].type)>0 and p[3].type[0] in dft:
                self.ST.error = 1
                print(f'Relational operation between incompatible types {p[1].type} and {p[3].type} on line {p.lineno(2)}')

            elif len(p[3].type)>0 and p[3].type[0][-1] == '*' and len(p[1].type)>0 and p[1].type[0] in dft:
                self.ST.error = 1
                print(f'Relational operation between incompatible types {p[1].type} and {p[3].type} on line {p.lineno(2)}')

            elif ((len(p[1].type)>0 and p[1].type[0][-1] == '*') or (len(p[3].type)>0 and p[3].type[0][-1] == '*')) and 'struct' not in p[1].type and 'struct' not in p[3].type and 'union' not in p[1].type and 'union' not in p[3].type:
                p[1].totype = ['int', 'unsigned']
                p1 = Node('to_int_unsigned',[p[1]] )      
                p[3].totype = ['int', 'unsigned'] 
                p3 = Node('to_int_unsigned',[p[3]] )
                p[0] = Node(str(p[2]),[p1,p3])
                p[0].type = ['int']
                p[0].label += '_*'
                p[0].label = p[0].label.replace(" ", "_")
                p[0].node.attr['label'] = p[0].label      

            else:
                self.ST.error = 1
                print(f'Relational operation between incompatible types {p[1].type} and {p[3].type} on line {p.lineno(2)}')

            if self.ST.error:
                return
            if p[1].totype is not None and p[1].totype != p[1].type:
                p1.temp = self.TAC.newtemp()
                self.ST.InsertSymbol(p1.temp, 0)
                self.ST.ModifySymbol(p1.temp, "type", p[1].totype)
                self.ST.ModifySymbol(p1.temp, "check", "TEMP")
                self.updateSizeInSymTab(p[1].totype, p1.temp)
                if self.ST.isGlobal(p1.temp):
                    self.ST.ModifySymbol(p1.temp, "varclass", "Global")
                else :
                    self.ST.ModifySymbol(p1.temp, "varclass", "Local")
                    found, entry = self.ST.ReturnSymTabEntry(p1.temp)
                    var_size = found['sizeAllocInBytes']
                    if found["varclass"] == "Local":
                        # self.TAC.emit('-_int', '%esp', '%esp', f'${var_size}')
                        if found["offset"] >0:
                            self.ST.ModifySymbol(p1.temp, 'temp', f'-{found["offset"] + found["sizeAllocInBytes"] }(%ebp)')
                        else:
                            self.ST.ModifySymbol(p1.temp, 'temp', f'{-found["offset"] - found["sizeAllocInBytes"] }(%ebp)')
                    p1.temp = found['temp']  

                fromtype = p[1].type
                currtype = []
                for single_type in fromtype:
                    if single_type != 'arr'  and single_type[0] != '[' and single_type[-1] != ']':
                        currtype.append(single_type)
                cstr =',' +  ' '.join(currtype).replace(' ','_')

                self.TAC.emit('cast',p1.temp,p[1].temp,' '.join(p[1].totype).replace(' ','_') + cstr) 

            else:
                p1.temp = p[1].temp

            if p[3].totype is not None and p[3].totype != p[3].type:
                p3.temp = self.TAC.newtemp()
                self.ST.InsertSymbol(p3.temp, 0)
                self.ST.ModifySymbol(p3.temp, "type", p[3].totype)
                self.ST.ModifySymbol(p3.temp, "check", "TEMP")
                self.updateSizeInSymTab(p[3].totype, p3.temp)
                if self.ST.isGlobal(p3.temp):
                    self.ST.ModifySymbol(p3.temp, "varclass", "Global")
                else :
                    self.ST.ModifySymbol(p3.temp, "varclass", "Local")
                    found, entry = self.ST.ReturnSymTabEntry(p3.temp)
                    var_size = found['sizeAllocInBytes']
                    if found["varclass"] == "Local":
                        # self.TAC.emit('-_int', '%esp', '%esp', f'${var_size}')
                        if found["offset"] >0:
                            self.ST.ModifySymbol(p3.temp, 'temp', f'-{found["offset"] + found["sizeAllocInBytes"] }(%ebp)')
                        else:
                            self.ST.ModifySymbol(p3.temp, 'temp', f'{-found["offset"] - found["sizeAllocInBytes"] }(%ebp)')
                    p3.temp = found['temp']  

                fromtype = p[3].type
                currtype = []
                for single_type in fromtype:
                    if single_type != 'arr'  and single_type[0] != '[' and single_type[-1] != ']':
                        currtype.append(single_type)
                cstr =',' +  ' '.join(currtype).replace(' ','_')

                self.TAC.emit('cast',p3.temp,p[3].temp,' '.join(p[3].totype).replace(' ','_') + cstr) 
            else:
                p3.temp = p[3].temp
                
            p[0].temp = self.TAC.newtemp()
            self.ST.InsertSymbol(p[0].temp, 0)
            self.ST.ModifySymbol(p[0].temp, "type", p[0].type)
            self.ST.ModifySymbol(p[0].temp, "check", "TEMP")
            self.updateSizeInSymTab(p[0].type, p[0].temp)
            if self.ST.isGlobal(p[0].temp):
                self.ST.ModifySymbol(p[0].temp, "varclass", "Global")
            else :
                self.ST.ModifySymbol(p[0].temp, "varclass", "Local")
                found, entry = self.ST.ReturnSymTabEntry(p[0].temp)
                var_size = found['sizeAllocInBytes']
                if found["varclass"] == "Local":
                    # self.TAC.emit('-_int', '%esp', '%esp', f'${var_size}')
                    if found["offset"] >0:
                        self.ST.ModifySymbol(p[0].temp, 'temp', f'-{found["offset"] + found["sizeAllocInBytes"] }(%ebp)')
                    else:
                        self.ST.ModifySymbol(p[0].temp, 'temp', f'{-found["offset"] - found["sizeAllocInBytes"] }(%ebp)')
                p[0].temp = found['temp']

            self.TAC.emit(p[0].label, p[0].temp, p1.temp, p3.temp)

            p[0].truelist.append(self.TAC.nextstat)
            p[0].falselist.append(self.TAC.nextstat+1)
            self.TAC.emit('ifnz goto','',p[0].temp,'')
            self.TAC.emit('goto','','','')

    def p_equality_expression(self, p):
        '''
        equality_expression : relational_expression
                            | equality_expression EQ_OP relational_expression
                            | equality_expression NE_OP relational_expression
        '''
        if self.isError :
            return
        if (len(p) == 2):
            p[0] = p[1]
        elif (len(p) == 4):
            if p[1] is None or p[3] is None or p[1].type is None or p[3].type is None or p[1].type == [] or p[3].type == []:
                self.ST.error = 1
                print(f'Cannot perform Equality check operation between expressions on line {p.lineno(2)}')

            elif len(p[1].type)>0 and p[1].type[0] in aat and len(p[3].type)>0 and p[3].type[0] in aat :
                p0type = ['int']
                p0label = str(p[2]) + '_' +  aat[max(aat.index(p[1].type[0]), aat.index(p[3].type[0]))]
                
                flag = 0
                if 'unsigned' in p[1].type or 'unsigned' in p[3].type and max(aat.index(p[1].type[0]), aat.index(p[3].type[0])) > 0 and max(aat.index(p[1].type[0]), aat.index(p[3].type[0])) < 5:
                    flag = 1
                    p0label = p0label + '_' +  'unsigned'
                
                p0label = p0label.replace(" ", "_")
                    # p[0].node.attr['label'] = p[0].label

                p[1].totype = None
                p[3].totype = None
                
                if aat[max(aat.index(p[1].type[0]), aat.index(p[3].type[0]))] not in p[1].type: 
                    p[1].totype = [aat[max(aat.index(p[1].type[0]), aat.index(p[3].type[0]))]]
                    if flag:
                        p[1].totype.append('unsigned')
                elif flag and 'unsigned' not in p[1].type:
                    p[1].totype = [aat[max(aat.index(p[1].type[0]), aat.index(p[3].type[0]))], 'unsigned']
                
                if p[1].totype != None and p[1].totype != p[1].type:
                    p1str = 'to'
                    for single_type in p[1].totype:
                        p1str += '_' + single_type
                    p1 = Node(p1str, [p[1]])
                else:
                    p1 = p[1]
                
                if aat[max(aat.index(p[1].type[0]), aat.index(p[3].type[0]))] not in p[3].type:
                    p[3].totype = [aat[max(aat.index(p[1].type[0]), aat.index(p[3].type[0]))]]
                    if flag:
                        p[3].totype.append('unsigned')
                elif flag and 'unsigned' not in p[3].type:
                    p[3].totype = [aat[max(aat.index(p[1].type[0]), aat.index(p[3].type[0]))], 'unsigned']
                
                if p[3].totype != None and p[3].totype != p[3].type:
                    p3str = 'to'
                    for single_type in p[3].totype:
                        p3str += '_' + single_type
                    p3 = Node(p3str, [p[3]])
                else:
                    p3 = p[3]
            
                p[0] = Node(str(p[2]), [p1, p3])
                p[0].type = p0type
                p[0].label = p0label
                p[0].node.attr['label'] = p[0].label

            elif len(p[1].type)>0 and p[1].type[0] == 'str' and len(p[3].type)>0 and p[3].type[0] == 'str':
                p[0] = Node(str(p[2]),[p[1],p[3]])
                p[0].type = ['int']
                p[0].label += '_str'
                p[0].label = p[0].label.replace(" ", "_")
                p[0].node.attr['label'] = p[0].label

            elif len(p[1].type)>0 and p[1].type[0][-1] == '*' and len(p[3].type)>0 and p[3].type[0] in dft:
                self.ST.error = 1
                print(f'Relational operation between incompatible types {p[1].type} and {p[3].type} on line {p.lineno(2)}')

            elif len(p[3].type)>0 and p[3].type[0][-1] == '*' and len(p[1].type)>0 and p[1].type[0] in dft:
                self.ST.error = 1
                print(f'Relational operation between incompatible types {p[1].type} and {p[3].type} on line {p.lineno(2)}')

            elif ((len(p[1].type)>0 and p[1].type[0][-1] == '*' ) or (len(p[3].type)>0 and p[3].type[0][-1] == '*')) and 'struct' not in p[1].type and 'struct' not in p[3].type and 'union' not in p[1].type and 'union' not in p[3].type:

                p[1].totype = ['int', 'unsigned']
                p1 = Node('to_int_unsigned',[p[1]] )      
                p[3].totype = ['int', 'unsigned'] 
                p3 = Node('to_int_unsigned',[p[3]] )
                
                p[0] = Node(str(p[2]),[p1,p3])
                p[0].type = ['int']
                p[0].label += '_*'
                p[0].label = p[0].label.replace(" ", "_")
                p[0].node.attr['label'] = p[0].label      

            else:
                self.ST.error = 1
                print(f'Equality check operation between incompatible types {p[1].type} and {p[3].type} on line {p.lineno(2)}')

            if self.ST.error:
                return
            if p[1].totype is not None and p[1].totype != p[1].type:
                p1.temp = self.TAC.newtemp()
                self.ST.InsertSymbol(p1.temp, 0)
                self.ST.ModifySymbol(p1.temp, "type", p[1].totype)
                self.ST.ModifySymbol(p1.temp, "check", "TEMP")
                self.updateSizeInSymTab(p[1].totype, p1.temp)
                if self.ST.isGlobal(p1.temp):
                    self.ST.ModifySymbol(p1.temp, "varclass", "Global")
                else :
                    self.ST.ModifySymbol(p1.temp, "varclass", "Local")
                    found, entry = self.ST.ReturnSymTabEntry(p1.temp)
                    var_size = found['sizeAllocInBytes']
                    if found["varclass"] == "Local":
                        # self.TAC.emit('-_int', '%esp', '%esp', f'${var_size}')
                        if found["offset"] >0:
                            self.ST.ModifySymbol(p1.temp, 'temp', f'-{found["offset"] + found["sizeAllocInBytes"] }(%ebp)')
                        else:
                            self.ST.ModifySymbol(p1.temp, 'temp', f'{-found["offset"] - found["sizeAllocInBytes"] }(%ebp)')
                    p1.temp = found['temp']  

                fromtype = p[1].type
                currtype = []
                for single_type in fromtype:
                    if single_type != 'arr'  and single_type[0] != '[' and single_type[-1] != ']':
                        currtype.append(single_type)
                cstr =',' +  ' '.join(currtype).replace(' ','_')

                self.TAC.emit('cast',p1.temp,p[1].temp,' '.join(p[1].totype).replace(' ','_') + cstr) 

            else:
                p1.temp = p[1].temp

            if p[3].totype is not None and p[3].totype != p[3].type:
                p3.temp = self.TAC.newtemp()
                self.ST.InsertSymbol(p3.temp, 0)
                self.ST.ModifySymbol(p3.temp, "type", p[3].totype)
                self.ST.ModifySymbol(p3.temp, "check", "TEMP")
                self.updateSizeInSymTab(p[3].totype, p3.temp)
                if self.ST.isGlobal(p3.temp):
                    self.ST.ModifySymbol(p3.temp, "varclass", "Global")
                else :
                    self.ST.ModifySymbol(p3.temp, "varclass", "Local")
                    found, entry = self.ST.ReturnSymTabEntry(p3.temp)
                    var_size = found['sizeAllocInBytes']
                    if found["varclass"] == "Local":
                        # self.TAC.emit('-_int', '%esp', '%esp', f'${var_size}')
                        if found["offset"] >0:
                            self.ST.ModifySymbol(p3.temp, 'temp', f'-{found["offset"] + found["sizeAllocInBytes"] }(%ebp)')
                        else:
                            self.ST.ModifySymbol(p3.temp, 'temp', f'{-found["offset"] - found["sizeAllocInBytes"] }(%ebp)')
                    p3.temp = found['temp']  

                fromtype = p[3].type
                currtype = []
                for single_type in fromtype:
                    if single_type != 'arr'  and single_type[0] != '[' and single_type[-1] != ']':
                        currtype.append(single_type)
                cstr =',' +  ' '.join(currtype).replace(' ','_')

                self.TAC.emit('cast',p3.temp,p[3].temp,' '.join(p[3].totype).replace(' ','_') + cstr) 

            else:
                p3.temp = p[3].temp
                
            p[0].temp = self.TAC.newtemp()
            self.ST.InsertSymbol(p[0].temp, 0)
            self.ST.ModifySymbol(p[0].temp, "type", p[0].type)
            self.ST.ModifySymbol(p[0].temp, "check", "TEMP")
            self.updateSizeInSymTab(p[0].type, p[0].temp)
            if self.ST.isGlobal(p[0].temp):
                self.ST.ModifySymbol(p[0].temp, "varclass", "Global")
            else :
                self.ST.ModifySymbol(p[0].temp, "varclass", "Local")
                found, entry = self.ST.ReturnSymTabEntry(p[0].temp)
                var_size = found['sizeAllocInBytes']
                if found["varclass"] == "Local":
                    # self.TAC.emit('-_int', '%esp', '%esp', f'${var_size}')
                    if found["offset"] >0:
                        self.ST.ModifySymbol(p[0].temp, 'temp', f'-{found["offset"] + found["sizeAllocInBytes"] }(%ebp)')
                    else:
                        self.ST.ModifySymbol(p[0].temp, 'temp', f'{-found["offset"] - found["sizeAllocInBytes"] }(%ebp)')
                p[0].temp = found['temp']

            self.TAC.emit(p[0].label, p[0].temp, p1.temp, p3.temp)

            p[0].truelist.append(self.TAC.nextstat)
            p[0].falselist.append(self.TAC.nextstat+1)
            self.TAC.emit('ifnz goto','',p[0].temp,'')
            self.TAC.emit('goto','','','')

    def p_and_expression(self, p):
        '''
        and_expression : equality_expression
                    | and_expression '&' equality_expression
        '''
        if self.isError :
            return
        if (len(p) == 2):
            p[0] = p[1]
        elif (len(p) == 4):
            if p[1] is None or p[3] is None or p[1].type is None or p[3].type is None or p[1].type == [] or p[3].type == []:
                self.ST.error = 1
                print(f'Cannot perform bitwise and between expressions on line {p.lineno(2)}')

            elif len(p[1].type)>0 and p[1].type[0] in iit and len(p[3].type)>0 and p[3].type[0] in iit:
                p0type = ['int']
                p0label = str(p[2])
                if max(iit.index(p[1].type[0]), iit.index(p[3].type[0])) == 4:
                    p0type = ['long int']
                    p0label += '_long_int'
                else:
                    p0label += '_int'

                if 'unsigned' in p[1].type or 'unsigned' in p[3].type:
                    p0type.append('unsigned')
                    p0label += '_unsigned'

                p0label = p0label.replace(" ", "_")
                isin = True
                for single_type in p0type:
                    if single_type not in p[1].type:
                        isin = False
                if isin == False:
                    p[1].totype = p0type
                    strp1 = 'to_'
                    for single_type in p[1].totype:
                        strp1 += single_type
                    p1 = Node(strp1 , [p[1]])
                else:
                    p1 = p[1]
                isin = True
                for single_type in p0type:
                    if single_type not in p[3].type:
                        isin = False
                if isin == False:
                    p[3].totype = p0type   
                    strp3 = 'to_'
                    for single_type in p[3].totype:
                        strp3 += single_type
                    p3 = Node(strp3 , [p[3]])

                else:
                    p3 = p[3]

                p[0] = Node(str(p[2]),[p1,p3])
                p[0].type = p0type
                p[0].label = p0label
                p[0].node.attr['label'] = p[0].label 

            else:
                self.ST.error = 1
                print(f'Bitwise and operation between incompatible types {p[1].type} and {p[3].type} on line {p.lineno(2)}')

            if self.ST.error:
                return
            if p[1].totype is not None and p[1].totype != p[1].type:
                p1.temp = self.TAC.newtemp()
                self.ST.InsertSymbol(p1.temp, 0)
                self.ST.ModifySymbol(p1.temp, "type", p[1].totype)
                self.ST.ModifySymbol(p1.temp, "check", "TEMP")
                self.updateSizeInSymTab(p[1].totype, p1.temp)
                if self.ST.isGlobal(p1.temp):
                    self.ST.ModifySymbol(p1.temp, "varclass", "Global")
                else :
                    self.ST.ModifySymbol(p1.temp, "varclass", "Local")
                    found, entry = self.ST.ReturnSymTabEntry(p1.temp)
                    var_size = found['sizeAllocInBytes']
                    if found["varclass"] == "Local":
                        # self.TAC.emit('-_int', '%esp', '%esp', f'${var_size}')
                        if found["offset"] >0:
                            self.ST.ModifySymbol(p1.temp, 'temp', f'-{found["offset"] + found["sizeAllocInBytes"] }(%ebp)')
                        else:
                            self.ST.ModifySymbol(p1.temp, 'temp', f'{-found["offset"] - found["sizeAllocInBytes"] }(%ebp)')
                    p1.temp = found['temp']  

                fromtype = p[1].type
                currtype = []
                for single_type in fromtype:
                    if single_type != 'arr'  and single_type[0] != '[' and single_type[-1] != ']':
                        currtype.append(single_type)
                cstr =',' +  ' '.join(currtype).replace(' ','_')
                self.TAC.emit('cast',p1.temp,p[1].temp, ' '.join(p[1].totype).replace(' ','_') + cstr) 

            else:
                p1.temp = p[1].temp

            if p[3].totype is not None and p[3].totype != p[3].type:
                p3.temp = self.TAC.newtemp()
                self.ST.InsertSymbol(p3.temp, 0)
                self.ST.ModifySymbol(p3.temp, "type", p[3].totype)
                self.ST.ModifySymbol(p3.temp, "check", "TEMP")
                self.updateSizeInSymTab(p[3].totype, p3.temp)
                if self.ST.isGlobal(p3.temp):
                    self.ST.ModifySymbol(p3.temp, "varclass", "Global")
                else :
                    self.ST.ModifySymbol(p3.temp, "varclass", "Local")
                    found, entry = self.ST.ReturnSymTabEntry(p3.temp)
                    var_size = found['sizeAllocInBytes']
                    if found["varclass"] == "Local":
                        # self.TAC.emit('-_int', '%esp', '%esp', f'${var_size}')
                        if found["offset"] >0:
                            self.ST.ModifySymbol(p3.temp, 'temp', f'-{found["offset"] + found["sizeAllocInBytes"] }(%ebp)')
                        else:
                            self.ST.ModifySymbol(p3.temp, 'temp', f'{-found["offset"] - found["sizeAllocInBytes"] }(%ebp)')
                    p3.temp = found['temp']  
                
                fromtype = p[3].type
                currtype = []
                for single_type in fromtype:
                    if single_type != 'arr'  and single_type[0] != '[' and single_type[-1] != ']':
                        currtype.append(single_type)
                cstr =',' +  ' '.join(currtype).replace(' ','_')
                self.TAC.emit('cast',p3.temp,p[3].temp,' '.join(p[3].totype).replace(' ','_') + cstr) 

            else:
                p3.temp = p[3].temp

            p[0].temp = self.TAC.newtemp()
            self.ST.InsertSymbol(p[0].temp, 0)
            self.ST.ModifySymbol(p[0].temp, "type", p[0].type)
            self.ST.ModifySymbol(p[0].temp, "check", "TEMP")
            self.updateSizeInSymTab(p[0].type, p[0].temp)
            if self.ST.isGlobal(p[0].temp):
                self.ST.ModifySymbol(p[0].temp, "varclass", "Global")
            else :
                self.ST.ModifySymbol(p[0].temp, "varclass", "Local")
                found, entry = self.ST.ReturnSymTabEntry(p[0].temp)
                var_size = found['sizeAllocInBytes']
                if found["varclass"] == "Local":
                    # self.TAC.emit('-_int', '%esp', '%esp', f'${var_size}')
                    if found["offset"] >0:
                        self.ST.ModifySymbol(p[0].temp, 'temp', f'-{found["offset"] + found["sizeAllocInBytes"] }(%ebp)')
                    else:
                        self.ST.ModifySymbol(p[0].temp, 'temp', f'{-found["offset"] - found["sizeAllocInBytes"] }(%ebp)')
                p[0].temp = found['temp']

            self.TAC.emit(p[0].label, p[0].temp, p1.temp, p3.temp)

            p[0].truelist.append(self.TAC.nextstat)
            p[0].falselist.append(self.TAC.nextstat+1)
            self.TAC.emit('ifnz goto','',p[0].temp,'')
            self.TAC.emit('goto','','','')

    def p_exclusive_or_expression(self, p):
        '''
        exclusive_or_expression : and_expression
                                | exclusive_or_expression '^' and_expression
        '''
        if self.isError :
            return
        if (len(p) == 2):
            p[0] = p[1]
        elif (len(p) == 4):
            if p[1] is None or p[3] is None or p[1].type is None or p[3].type is None or p[1].type == [] or p[3].type == []:
                self.ST.error = 1
                print(f'Cannot perform bitwise xor between expressions on line {p.lineno(2)}')

            elif len(p[1].type)>0 and p[1].type[0] in iit and len(p[3].type)>0 and p[3].type[0] in iit:
                p0type = ['int']
                p0label = str(p[2])
                if max(iit.index(p[1].type[0]), iit.index(p[3].type[0])) == 4:
                    p0type = ['long int']
                    p0label += '_long_int'
                else:
                    p0label += '_int'

                if 'unsigned' in p[1].type or 'unsigned' in p[3].type:
                    p0type.append('unsigned')
                    p0label += '_unsigned'

                p0label = p0label.replace(" ", "_")
                
                isin = True
                for single_type in p0type:
                    if single_type not in p[1].type:
                        isin = False
                if isin == False:
                    p[1].totype = p0type
                    strp1 = 'to_'
                    for single_type in p[1].totype:
                        strp1 += single_type
                    p1 = Node(strp1 , [p[1]])
                else:
                    p1 = p[1]
                isin = True
                for single_type in p0type:
                    if single_type not in p[3].type:
                        isin = False
                if isin == False:
                    p[3].totype = p0type   
                    strp3 = 'to_'
                    for single_type in p[3].totype:
                        strp3 += single_type
                    p3 = Node(strp3 , [p[3]])

                else:
                    p3 = p[3]

                p[0] = Node(str(p[2]),[p1,p3])
                p[0].type = p0type
                p[0].label = p0label
                p[0].node.attr['label'] = p[0].label  

            else:
                self.ST.error = 1
                print(f'Bitwise xor operation between incompatible types {p[1].type} and {p[3].type} on line {p.lineno(2)}')
    
            if self.ST.error:
                return
            if p[1].totype is not None and p[1].totype != p[1].type:
                p1.temp = self.TAC.newtemp()
                self.ST.InsertSymbol(p1.temp, 0)
                self.ST.ModifySymbol(p1.temp, "type", p[1].totype)
                self.ST.ModifySymbol(p1.temp, "check", "TEMP")
                self.updateSizeInSymTab(p[1].totype, p1.temp)
                if self.ST.isGlobal(p1.temp):
                    self.ST.ModifySymbol(p1.temp, "varclass", "Global")
                else :
                    self.ST.ModifySymbol(p1.temp, "varclass", "Local")
                    found, entry = self.ST.ReturnSymTabEntry(p1.temp)
                    var_size = found['sizeAllocInBytes']
                    if found["varclass"] == "Local":
                        # self.TAC.emit('-_int', '%esp', '%esp', f'${var_size}')
                        if found["offset"] >0:
                            self.ST.ModifySymbol(p1.temp, 'temp', f'-{found["offset"] + found["sizeAllocInBytes"] }(%ebp)')
                        else:
                            self.ST.ModifySymbol(p1.temp, 'temp', f'{-found["offset"] - found["sizeAllocInBytes"] }(%ebp)')
                    p1.temp = found['temp']  

                fromtype = p[1].type
                currtype = []
                for single_type in fromtype:
                    if single_type != 'arr'  and single_type[0] != '[' and single_type[-1] != ']':
                        currtype.append(single_type)
                cstr =',' +  ' '.join(currtype).replace(' ','_')

                self.TAC.emit('cast',p1.temp,p[1].temp, ' '.join(p[1].totype).replace(' ','_') + cstr) 

            else:
                p1.temp = p[1].temp

            if p[3].totype is not None and p[3].totype != p[3].type:
                p3.temp = self.TAC.newtemp()
                self.ST.InsertSymbol(p3.temp, 0)
                self.ST.ModifySymbol(p3.temp, "type", p[3].totype)
                self.ST.ModifySymbol(p3.temp, "check", "TEMP")
                self.updateSizeInSymTab(p[3].totype, p3.temp)
                if self.ST.isGlobal(p3.temp):
                    self.ST.ModifySymbol(p3.temp, "varclass", "Global")
                else :
                    self.ST.ModifySymbol(p3.temp, "varclass", "Local")
                    found, entry = self.ST.ReturnSymTabEntry(p3.temp)
                    var_size = found['sizeAllocInBytes']
                    if found["varclass"] == "Local":
                        # self.TAC.emit('-_int', '%esp', '%esp', f'${var_size}')
                        if found["offset"] >0:
                            self.ST.ModifySymbol(p3.temp, 'temp', f'-{found["offset"] + found["sizeAllocInBytes"] }(%ebp)')
                        else:
                            self.ST.ModifySymbol(p3.temp, 'temp', f'{-found["offset"] - found["sizeAllocInBytes"] }(%ebp)')
                    p3.temp = found['temp']  

                fromtype = p[3].type
                currtype = []
                for single_type in fromtype:
                    if single_type != 'arr'  and single_type[0] != '[' and single_type[-1] != ']':
                        currtype.append(single_type)
                cstr =',' +  ' '.join(currtype).replace(' ','_')
                
                self.TAC.emit('cast',p3.temp,p[3].temp, ' '.join(p[3].totype).replace(' ','_') + cstr) 

            else:
                p3.temp = p[3].temp

            p[0].temp = self.TAC.newtemp()
            self.ST.InsertSymbol(p[0].temp, 0)
            self.ST.ModifySymbol(p[0].temp, "type", p[0].type)
            self.ST.ModifySymbol(p[0].temp, "check", "TEMP")
            self.updateSizeInSymTab(p[0].type, p[0].temp)
            if self.ST.isGlobal(p[0].temp):
                self.ST.ModifySymbol(p[0].temp, "varclass", "Global")
            else :
                self.ST.ModifySymbol(p[0].temp, "varclass", "Local")
                found, entry = self.ST.ReturnSymTabEntry(p[0].temp)
                var_size = found['sizeAllocInBytes']
                if found["varclass"] == "Local":
                    # self.TAC.emit('-_int', '%esp', '%esp', f'${var_size}')
                    if found["offset"] >0:
                        self.ST.ModifySymbol(p[0].temp, 'temp', f'-{found["offset"] + found["sizeAllocInBytes"] }(%ebp)')
                    else:
                        self.ST.ModifySymbol(p[0].temp, 'temp', f'{-found["offset"] - found["sizeAllocInBytes"] }(%ebp)')
                p[0].temp = found['temp']

            self.TAC.emit(p[0].label, p[0].temp, p1.temp, p3.temp)

            p[0].truelist.append(self.TAC.nextstat)
            p[0].falselist.append(self.TAC.nextstat+1)
            self.TAC.emit('ifnz goto','',p[0].temp,'')
            self.TAC.emit('goto','','','')
    
    def p_inclusive_or_expression(self, p):
        '''
        inclusive_or_expression : exclusive_or_expression
                                | inclusive_or_expression '|' exclusive_or_expression
        '''
        if self.isError :
            return
        if (len(p) == 2):
            p[0] = p[1]
        elif (len(p) == 4):
            if p[1] is None or p[3] is None or p[1].type is None or p[3].type is None or p[1].type == [] or p[3].type == []:
                self.ST.error = 1
                print(f'Cannot perform bitwise or between expressions on line {p.lineno(2)}')

            elif len(p[1].type)>0 and p[1].type[0] in iit and len(p[3].type)>0 and p[3].type[0] in iit:
                p0type = ['int']
                p0label = str(p[2])
                if max(iit.index(p[1].type[0]), iit.index(p[3].type[0])) == 4:
                    p0type = ['long int']
                    p0label += '_long_int'
                else:
                    p0label += '_int'

                if 'unsigned' in p[1].type or 'unsigned' in p[3].type:
                    p0type.append('unsigned')
                    p0label += '_unsigned'

                p0label = p0label.replace(" ", "_")
                
                isin = True
                for single_type in p0type:
                    if single_type not in p[1].type:
                        isin = False
                if isin == False:
                    p[1].totype = p0type
                    strp1 = 'to_'
                    for single_type in p[1].totype:
                        strp1 += single_type
                    p1 = Node(strp1 , [p[1]])
                else:
                    p1 = p[1]
                isin = True
                for single_type in p0type:
                    if single_type not in p[3].type:
                        isin = False
                if isin == False:
                    p[3].totype = p0type   
                    strp3 = 'to_'
                    for single_type in p[3].totype:
                        strp3 += single_type
                    p3 = Node(strp3 , [p[3]])

                else:
                    p3 = p[3]

                p[0] = Node(str(p[2]),[p1,p3])
                p[0].type = p0type
                p[0].label = p0label
                p[0].node.attr['label'] = p[0].label 

            else:
                self.ST.error = 1
                print(f'Bitwise or operation between incompatible types {p[1].type} and {p[3].type} on line {p.lineno(2)}')

            if self.ST.error:
                return
            if p[1].totype is not None and p[1].totype != p[1].type:
                p1.temp = self.TAC.newtemp()
                self.ST.InsertSymbol(p1.temp, 0)
                self.ST.ModifySymbol(p1.temp, "type", p[1].totype)
                self.ST.ModifySymbol(p1.temp, "check", "TEMP")
                self.updateSizeInSymTab(p[1].totype, p1.temp)
                if self.ST.isGlobal(p1.temp):
                    self.ST.ModifySymbol(p1.temp, "varclass", "Global")
                else :
                    self.ST.ModifySymbol(p1.temp, "varclass", "Local")
                    found, entry = self.ST.ReturnSymTabEntry(p1.temp)
                    var_size = found['sizeAllocInBytes']
                    if found["varclass"] == "Local":
                        # self.TAC.emit('-_int', '%esp', '%esp', f'${var_size}')
                        if found["offset"] >0:
                            self.ST.ModifySymbol(p1.temp, 'temp', f'-{found["offset"] + found["sizeAllocInBytes"] }(%ebp)')
                        else:
                            self.ST.ModifySymbol(p1.temp, 'temp', f'{-found["offset"] - found["sizeAllocInBytes"] }(%ebp)')
                    p1.temp = found['temp']  

                fromtype = p[1].type
                currtype = []
                for single_type in fromtype:
                    if single_type != 'arr'  and single_type[0] != '[' and single_type[-1] != ']':
                        currtype.append(single_type)
                cstr =',' +  ' '.join(currtype).replace(' ','_')

                self.TAC.emit('cast',p1.temp,p[1].temp, ' '.join(p[1].totype).replace(' ','_') + cstr) 

            else:
                p1.temp = p[1].temp

            if p[3].totype is not None and p[3].totype != p[3].type:
                p3.temp = self.TAC.newtemp()
                self.ST.InsertSymbol(p3.temp, 0)
                self.ST.ModifySymbol(p3.temp, "type", p[3].totype)
                self.ST.ModifySymbol(p3.temp, "check", "TEMP")
                self.updateSizeInSymTab(p[3].totype, p3.temp)
                if self.ST.isGlobal(p3.temp):
                    self.ST.ModifySymbol(p3.temp, "varclass", "Global")
                else :
                    self.ST.ModifySymbol(p3.temp, "varclass", "Local")
                    found, entry = self.ST.ReturnSymTabEntry(p3.temp)
                    var_size = found['sizeAllocInBytes']
                    if found["varclass"] == "Local":
                        # self.TAC.emit('-_int', '%esp', '%esp', f'${var_size}')
                        if found["offset"] >0:
                            self.ST.ModifySymbol(p3.temp, 'temp', f'-{found["offset"] + found["sizeAllocInBytes"] }(%ebp)')
                        else:
                            self.ST.ModifySymbol(p3.temp, 'temp', f'{-found["offset"] - found["sizeAllocInBytes"] }(%ebp)')
                    p3.temp = found['temp']  
                
                fromtype = p[3].type
                currtype = []
                for single_type in fromtype:
                    if single_type != 'arr'  and single_type[0] != '[' and single_type[-1] != ']':
                        currtype.append(single_type)
                cstr =',' +  ' '.join(currtype).replace(' ','_')

                self.TAC.emit('cast',p3.temp,p[3].temp, ' '.join(p[3].totype).replace(' ','_') + cstr) 

            else:
                p3.temp = p[3].temp

            p[0].temp = self.TAC.newtemp()
            self.ST.InsertSymbol(p[0].temp, 0)
            self.ST.ModifySymbol(p[0].temp, "type", p[0].type)
            self.ST.ModifySymbol(p[0].temp, "check", "TEMP")
            self.updateSizeInSymTab(p[0].type, p[0].temp)
            if self.ST.isGlobal(p[0].temp):
                self.ST.ModifySymbol(p[0].temp, "varclass", "Global")
            else :
                self.ST.ModifySymbol(p[0].temp, "varclass", "Local")
                found, entry = self.ST.ReturnSymTabEntry(p[0].temp)
                var_size = found['sizeAllocInBytes']
                if found["varclass"] == "Local":
                    # self.TAC.emit('-_int', '%esp', '%esp', f'${var_size}')
                    if found["offset"] >0:
                        self.ST.ModifySymbol(p[0].temp, 'temp', f'-{found["offset"] + found["sizeAllocInBytes"] }(%ebp)')
                    else:
                        self.ST.ModifySymbol(p[0].temp, 'temp', f'{-found["offset"] - found["sizeAllocInBytes"] }(%ebp)')
                p[0].temp = found['temp']

            self.TAC.emit(p[0].label, p[0].temp, p1.temp, p3.temp)

            p[0].truelist.append(self.TAC.nextstat)
            p[0].falselist.append(self.TAC.nextstat+1)
            self.TAC.emit('ifnz goto','',p[0].temp,'')
            self.TAC.emit('goto','','','')
            
    def p_logical_and_expression(self, p):
        '''
        logical_and_expression : inclusive_or_expression
                            | logical_and_expression AND_OP globalmarker1 inclusive_or_expression globalmarker1
        '''
        if self.isError :
            return
        if (len(p) == 2):
            p[0] = p[1]
        elif (len(p) == 6):
            if p[1] is None or p[4] is None or p[1].type is None or p[4].type is None or p[1].type == [] or p[4].type == []:
                self.ST.error = 1
                print(f'Cannot perform logical and between expressions on line {p.lineno(2)}')
            elif 'struct' in p[1].type or 'union' in p[1].type or 'struct' in p[1].type or 'union' in p[1].type:
                self.ST.error = 1
                print(f'Need scalars to perform logical operation at line {p.lineno(2)}')

            else:
                p[0] = Node(str(p[2]),[p[1],p[4]])
                p[0].type = ['int']
                if self.ST.error:
                    return
                self.TAC.backpatch(p[1].truelist,p[3].quad)
                self.TAC.backpatch(p[1].falselist,p[5].quad)
                p[0].falselist = p[1].falselist + p[4].falselist
                p[0].truelist = p[4].truelist

                p[0].temp = self.TAC.newtemp()
                self.ST.InsertSymbol(p[0].temp, 0)
                self.ST.ModifySymbol(p[0].temp, "type", p[0].type)
                self.ST.ModifySymbol(p[0].temp, "check", "TEMP")
                self.updateSizeInSymTab(p[0].type, p[0].temp)
                if self.ST.isGlobal(p[0].temp):
                    self.ST.ModifySymbol(p[0].temp, "varclass", "Global")
                else :
                    self.ST.ModifySymbol(p[0].temp, "varclass", "Local")
                    found, entry = self.ST.ReturnSymTabEntry(p[0].temp)
                    var_size = found['sizeAllocInBytes']
                    if found["varclass"] == "Local":
                        # self.TAC.emit('-_int', '%esp', '%esp', f'${var_size}')
                        if found["offset"] >0:
                            self.ST.ModifySymbol(p[0].temp, 'temp', f'-{found["offset"] + found["sizeAllocInBytes"] }(%ebp)')
                        else:
                            self.ST.ModifySymbol(p[0].temp, 'temp', f'{-found["offset"] - found["sizeAllocInBytes"] }(%ebp)')
                    p[0].temp = found['temp']

                # self.TAC.emit('ifnz goto', self.TAC.nextstat + 4, p[1].temp, '')
                # self.TAC.emit('=_int', p[0].temp, '$0', '')
                # self.TAC.emit('goto', self.TAC.nextstat + 3, '', '')
                # self.TAC.emit('&&', p[0].temp, p[1].temp, p[4].temp)

                self.TAC.emit('=_int', p[0].temp, "$0", '')
                self.TAC.emit('ifnz goto', self.TAC.nextstat + 3, p[1].temp, '')
                self.TAC.emit('goto', self.TAC.nextstat + 5, '', '')
                self.TAC.emit('ifnz goto', self.TAC.nextstat + 3, p[4].temp, '')
                self.TAC.emit('goto', self.TAC.nextstat + 3, '', '')
                self.TAC.emit('=_int', p[0].temp, '$1', '')

    def p_logical_or_expression(self, p):
        '''
        logical_or_expression : logical_and_expression
                            | logical_or_expression OR_OP globalmarker1 logical_and_expression globalmarker1
        '''
        if self.isError :
            return
        if (len(p) == 2):
            p[0] = p[1]
        elif (len(p) == 6):
            if p[1] is None or p[4] is None or p[1].type is None or p[4].type is None or p[1].type == [] or p[4].type == []:
                self.ST.error = 1
                print(f'Cannot perform logical or between expressions on line {p.lineno(2)}')
            elif 'struct' in p[1].type or 'union' in p[1].type or 'struct' in p[1].type or 'union' in p[1].type:
                self.ST.error = 1
                print(f'Need scalars to perform logical operation at line {p.lineno(2)}')
            else:
                p[0] = Node(str(p[2]),[p[1],p[4]])
                p[0].type = ['int']
                if self.ST.error:
                    return
                self.TAC.backpatch(p[1].falselist,p[3].quad)
                self.TAC.backpatch(p[1].truelist,p[5].quad)
                p[0].truelist = p[1].truelist + p[4].truelist
                p[0].falselist = p[4].falselist

                p[0].temp = self.TAC.newtemp()
                self.ST.InsertSymbol(p[0].temp, 0)
                self.ST.ModifySymbol(p[0].temp, "type", p[0].type)
                self.ST.ModifySymbol(p[0].temp, "check", "TEMP")
                self.updateSizeInSymTab(p[0].type, p[0].temp)
                if self.ST.isGlobal(p[0].temp):
                    self.ST.ModifySymbol(p[0].temp, "varclass", "Global")
                else :
                    self.ST.ModifySymbol(p[0].temp, "varclass", "Local")
                    found, entry = self.ST.ReturnSymTabEntry(p[0].temp)
                    var_size = found['sizeAllocInBytes']
                    if found["varclass"] == "Local":
                        # self.TAC.emit('-_int', '%esp', '%esp', f'${var_size}')
                        if found["offset"] >0:
                            self.ST.ModifySymbol(p[0].temp, 'temp', f'-{found["offset"] + found["sizeAllocInBytes"] }(%ebp)')
                        else:
                            self.ST.ModifySymbol(p[0].temp, 'temp', f'{-found["offset"] - found["sizeAllocInBytes"] }(%ebp)')
                    p[0].temp = found['temp']

                # self.TAC.emit('ifnz goto', self.TAC.nextstat + 3, p[1].temp, '')
                # self.TAC.emit('goto', self.TAC.nextstat + 4, '', '')
                # self.TAC.emit('=_int', p[0].temp, '$1', '')
                # self.TAC.emit('goto', self.TAC.nextstat + 3, '', '')
                # self.TAC.emit('ifnz goto', '' , p[4].temp, '')
                # self.TAC.emit('||', p[0].temp, p[1].temp, p[4].temp)

                self.TAC.emit('=_int', p[0].temp, "$1", '')
                self.TAC.emit('ifnz goto', self.TAC.nextstat + 4, p[1].temp, '')
                self.TAC.emit('ifnz goto', self.TAC.nextstat + 3, p[4].temp, '')
                self.TAC.emit('=_int', p[0].temp, '$0', '')

    def p_conditional_expression(self, p):
        '''
        conditional_expression : logical_or_expression
                            | logical_or_expression '?' globalmarker1 expression ':' globalmarker1 conditional_expression globalmarker1
        '''
        if self.isError :
            return
        # AST Done
        if (len(p) == 2):
            p[0] = p[1]
        elif (len(p) == 9):
            if p[1] is None or p[1].type is None or p[1].type == []:
                self.ST.error = 1
                print(f'Cannot perform conditional operation at line {p.lineno(2)}')
                return

            if 'struct' in p[1].type or 'union' in p[1].type:
                self.ST.error = 1
                print(f'Struct / Union type variable not allowed as first operand of ternary operator')
                return

            elif p[4] is None or p[7] is None:
                self.ST.error = 1
                print(f'Cannot perform conditional operation at line {p.lineno(2)}')
                return

            elif p[4].type in [None, []] or p[7].type in [None, []] :
                self.ST.error = 1
                print(f'Cannot perform conditional operation at line {p.lineno(2)}')
                return

            elif 'struct' in p[4].type and 'struct' not in p[7].type:
                self.ST.error = 1
                print(f'Type mismatch between {p[4].type} and {p[7].type} for conditional operation at line {p.lineno(2)}')
                return

            elif 'struct' in p[7].type and 'struct' not in p[4].type:
                self.ST.error = 1
                print(f'Type mismatch between {p[4].type} and {p[7].type} for conditional operation at line {p.lineno(2)}')
                return

            elif 'struct' in p[4].type and 'struct' in p[7].type and p[4].type[1] != p[7].type[1]:
                self.ST.error = 1
                print(f'Incompatible struct types to perform conditional operation at line {p.lineno(2)}')
                return

            elif 'union' in p[4].type and 'union' not in p[7].type:
                self.ST.error = 1
                print(f'Type mismatch between {p[4].type} and {p[7].type} for conditional operation at line {p.lineno(2)}')
                return

            elif 'union' in p[7].type and 'union' not in p[4].type:
                self.ST.error = 1
                print(f'Type mismatch between {p[4].type} and {p[7].type} for conditional operation at line {p.lineno(2)}')
                return

            elif 'union' in p[4].type and 'union' in p[7].type and p[4].type[1] != p[7].type[1]:
                self.ST.error = 1
                print(f'Incompatible union types to perform conditional operation at line {p.lineno(2)}')  
                return 
            elif p[4].type[0] not in aat and p[4].type[0][-1] != '*' and p[7].type[0] in aat:
                self.ST.error = 1
                print(f'Type mismatch while performing conditional operation at line {p.lineno(2)}')
                return
            
            elif p[4].type[0][-1] == '*' and p[7].type[0][-1] != '*' and p[7].type[0]  not in iit :    
                self.ST.error = 1
                print(f'Incompatible conditional operation between pointer and {p[7].type} at line {p.lineno(2)}')
                return

            elif p[7].type[0][-1] == '*' and p[4].type[0][-1] != '*' and p[4].type[0]  not in iit :    
                self.ST.error = 1
                print(f'Incompatible conditional operation between pointer and {p[4].type} at line {p.lineno(2)}')
                return

            isError = False
            if p[4].type == p[7].type:
                p0type = p[4].type
            
            elif len(p[4].type)>0 and p[4].type[0][-1] == '*' and len(p[7].type)>0 and p[7].type[0][-1] == '*':
                p0type = ['void *']

            elif len(p[4].type)>0 and p[4].type[0][-1] == '*' or len(p[7].type)>0 and p[7].type[0][-1] == '*':
                if p[4].type[0][-1] == '*':
                    p0type = p[4].type
                elif p[7].type[0][-1] == '*':
                    p0type = p[7].type   
                     
            elif 'str' in p[4].type:
                p0type = p[7].type

            elif 'str' in p[7].type:
                p0type = p[4].type
            
            elif len(p[4].type)>0 and p[4].type[0] in aat and len(p[7].type)>0 and p[7].type[0] in aat:
                p0type = []
                p0type.append(aat[max(aat.index(p[4].type[0]), aat.index(p[7].type[0]))])
                if 'unsigned' in p[4].type or 'unsigned' in p[7].type and p0type[0] in dit:
                    p0type.append('unsigned')
            else:
                isError = True

            if (isError == False):
                if p0type != p[4].type:
                    p4str = 'to'
                    for single_type in p0type:
                        p4str += '_' + single_type
                    p4str = p4str.replace(' ','_')
                    p4 = Node(p4str ,[p[4]])
                else:
                    p4 = p[4]

                if p0type != p[7].type:
                    p7str = 'to'
                    for single_type in p0type:
                        p7str += '_' + single_type
                    p7str = p7str.replace(' ','_')
                    p7 = Node(p7str,[p[7]])
                else:
                    p7 = p[7]

                p[0] = Node('TERNARY',[p[1],p4,p7])
                p[0].type = p0type

                if self.ST.error:
                    return

                p[4].totype = p0type

                p4type = []
                for single_type in p[4].type:
                    if single_type != 'arr' and single_type[0] != '[' and single_type[-1] != ']':
                        p4type.append(single_type)

                if p[4].totype is not None and p[4].totype != p4type:
                    p4.temp = self.TAC.newtemp()
                    self.ST.InsertSymbol(p4.temp, 0)
                    self.ST.ModifySymbol(p4.temp, "type", p[4].totype)
                    self.ST.ModifySymbol(p4.temp, "check", "TEMP")
                    self.updateSizeInSymTab(p[4].totype, p4.temp)
                    if self.ST.isGlobal(p4.temp):
                        self.ST.ModifySymbol(p4.temp, "varclass", "Global")
                    else :
                        self.ST.ModifySymbol(p4.temp, "varclass", "Local")
                        found, entry = self.ST.ReturnSymTabEntry(p4.temp)
                        var_size = found['sizeAllocInBytes']
                        if found["varclass"] == "Local":
                            # self.TAC.emit('-_int', '%esp', '%esp', f'${var_size}')
                            if found["offset"] >0:
                                self.ST.ModifySymbol(p4.temp, 'temp', f'-{found["offset"] + found["sizeAllocInBytes"] }(%ebp)')
                            else:
                                self.ST.ModifySymbol(p4.temp, 'temp', f'{-found["offset"] - found["sizeAllocInBytes"] }(%ebp)')
                        p4.temp = found['temp']  

                    fromtype = p[4].type
                    currtype = []
                    for single_type in fromtype:
                        if single_type != 'arr'  and single_type[0] != '[' and single_type[-1] != ']':
                            currtype.append(single_type)
                    cstr =',' +  ' '.join(currtype).replace(' ','_')

                    self.TAC.emit('cast',p4.temp,p[4].temp, ' '.join(p[4].totype).replace(' ','_') + cstr) 

                else:
                    p4.temp = p[4].temp

                p[7].totype = p0type

                p7type = []
                for single_type in p[7].type:
                    if single_type != 'arr' and single_type[0] != '[' and single_type[-1] != ']':
                        p7type.append(single_type)

                if p[7].totype is not None and p[7].totype != p7type:
                    p7.temp = self.TAC.newtemp()
                    self.ST.InsertSymbol(p7.temp, 0)
                    self.ST.ModifySymbol(p7.temp, "type", p[7].totype)
                    self.ST.ModifySymbol(p7.temp, "check", "TEMP")
                    self.updateSizeInSymTab(p[7].totype, p7.temp)
                    if self.ST.isGlobal(p7.temp):
                        self.ST.ModifySymbol(p7.temp, "varclass", "Global")
                    else :
                        self.ST.ModifySymbol(p7.temp, "varclass", "Local")
                        found, entry = self.ST.ReturnSymTabEntry(p7.temp)
                        var_size = found['sizeAllocInBytes']
                        if found["varclass"] == "Local":
                            # self.TAC.emit('-_int', '%esp', '%esp', f'${var_size}')
                            if found["offset"] >0:
                                self.ST.ModifySymbol(p7.temp, 'temp', f'-{found["offset"] + found["sizeAllocInBytes"] }(%ebp)')
                            else:
                                self.ST.ModifySymbol(p7.temp, 'temp', f'{-found["offset"] - found["sizeAllocInBytes"] }(%ebp)')
                        p7.temp = found['temp']  

                    fromtype = p[7].type
                    currtype = []
                    for single_type in fromtype:
                        if single_type != 'arr'  and single_type[0] != '[' and single_type[-1] != ']':
                            currtype.append(single_type)
                    cstr =',' +  ' '.join(currtype).replace(' ','_')

                    self.TAC.emit('cast',p7.temp,p[7].temp, ' '.join(p[7].totype).replace(' ','_') + cstr) 

                else:
                    p7.temp = p[7].temp

                p[0].temp = self.TAC.newtemp()
                self.ST.InsertSymbol(p[0].temp, 0)
                self.ST.ModifySymbol(p[0].temp, "type", p[0].type)
                self.ST.ModifySymbol(p[0].temp, "check", "TEMP")
                self.updateSizeInSymTab(p[0].type, p[0].temp)
                if self.ST.isGlobal(p[0].temp):
                    self.ST.ModifySymbol(p[0].temp, "varclass", "Global")
                else :
                    self.ST.ModifySymbol(p[0].temp, "varclass", "Local")
                    found, entry = self.ST.ReturnSymTabEntry(p[0].temp)
                    var_size = found['sizeAllocInBytes']
                    if found["varclass"] == "Local":
                        # self.TAC.emit('-_int', '%esp', '%esp', f'${var_size}')
                        if found["offset"] >0:
                            self.ST.ModifySymbol(p[0].temp, 'temp', f'-{found["offset"] + found["sizeAllocInBytes"] }(%ebp)')
                        else:
                            self.ST.ModifySymbol(p[0].temp, 'temp', f'{-found["offset"] - found["sizeAllocInBytes"] }(%ebp)')
                    p[0].temp = found['temp']
                
                self.TAC.emit('ifnz goto', self.TAC.nextstat + 3, p[1].temp, '')
                self.TAC.emit('goto', self.TAC.nextstat + 4, '', '')
                self.TAC.emit(f'=_{p[0].type[0]}', p[0].temp, p4.temp)
                self.TAC.emit('goto', self.TAC.nextstat + 3, '', '')
                self.TAC.emit(f'=_{p[0].type[0]}', p[0].temp, p7.temp)
                self.TAC.backpatch(p[1].truelist,p[3].quad)
                self.TAC.backpatch(p[1].falselist,p[6].quad)
                self.TAC.backpatch(p[4].truelist,p[8].quad)
                self.TAC.backpatch(p[4].falselist,p[8].quad)
                p[0].truelist = p[4].truelist + p[7].truelist
                p[0].falselist = p[4].falselist + p[7].falselist
                return

            self.ST.error = 1
            print(f'Cannot perform conditional operation at line {p.lineno(2)}')

    def p_assignment_expression(self, p):
        '''
        assignment_expression : conditional_expression
                            | unary_expression assignment_operator assignment_expression
        '''
        if self.isError :
            return
        # AST Done
        if (len(p) == 2):
            p[0] = p[1]
        elif (len(p) == 4):
            p[0] = p[2]
            if ((p[1] is not None) and (p[1].node is not None)):
                if ((p[3] is not None) and (p[3].node is not None)):
                    
                    if p[1].type in [None, []] or p[3].type in [None, []]:
                        self.ST.error = 1;
                        print(f'Cannot perform assignment at line {p[2].lineno}')

                    elif p[1].type[0][-1] == '*' and 'arr' in p[1].type and p[1].arrlvl > 0:
                        self.ST.error = 1
                        print(f'Cannot perform assignment to array type at line {p[2].lineno}')

                    elif p[1].isvar == 0 and 'struct' not  in p[1].type[0] and 'union' not in p[1].type[0]:
                        self.ST.error = 1
                        print(f'Left hand side has to be a variable at line {p[2].lineno}')

                    elif 'const' in p[1].type:
                        self.ST.error = 1
                        print(f'Cannot assign value to read only variable at line {p[2].lineno}')

                    elif 'struct' in p[1].type and 'struct' not in p[3].type:
                        self.ST.error = 1;
                        print(f'Cannot assign non-struct value {p[3].type} to struct type {p[1].type} at line {p[2].lineno}')

                    elif 'struct' in p[1].type and 'struct' in p[3].type and p[1].type[1] != p[3].type[1]:
                        self.ST.error = 1;
                        print(f'Incompatible struct types to perform assignment at line {p[2].lineno}')

                    elif 'union' in p[1].type and 'union' not in p[3].type:
                        self.ST.error = 1;
                        print(f'Cannot assign non-struct value {p[3].type} to struct type {p[1].type} at line {p[2].lineno}')

                    elif 'union' in p[1].type and 'union' in p[3].type and p[1].type[1] != p[3].type[1]:
                        self.ST.error = 1;
                        print(f'Incompatible union types to perform assignment at line {p[2].lineno}')

                    elif p[1].type in [None, []] or p[3].type in [None, []] :
                        self.ST.error = 1
                        print(f'Type mismatch while assigning value at line {p[2].lineno}') 

                    elif p[1].type[0] not in aat and p[1].type[0][-1] != '*' and p[3].type[0] in aat:
                        self.ST.error = 1
                        print(f'Type mismatch while assigning value at line {p[2].lineno}')

                    elif p[1].type[0][-1] == '*' and p[3].type[0][-1] != '*' and p[3].type[0]  not in iit and 'str' not in p[3].type:    
                        self.ST.error = 1
                        print(f'Incompatible assignment between pointer and {p[3].type} at line {p[2].lineno}')
                    
                    elif p[1].type[0][-1] == '*' and p[3].type[0] in iit and p[2].label[0] not in ['+', '-', '=']: # and p[2] is multiplicative
                        self.ST.error = 1
                        print(f'Incompatible operands to binary operator {p[2].label}, pointer and {p[3].type} at line {p[2].lineno}')

                    else:
                        p[0].type = p[1].type
                        isin = True
                        for single_type in p[0].type:
                            if single_type != 'arr' and (single_type[0] != '[' and single_type[-1] != ']'):
                                if single_type not in p[3].type:
                                    isin = False
                        if isin == False:
                            p[3].totype = []
                            for single_type in p[0].type:
                                if single_type != 'arr' and (single_type[0] != '[' and single_type[-1] != ']'):
                                    p[3].totype.append(single_type)
                            p3str = 'to'
                            for single_type in p[0].type:
                                p3str += '_' + single_type
                            p3str = p3str.replace(' ','_')
                            if '*' == p[0].type[0][-1]:
                                p3str = 'to_int_unsigned'
                            p3 = Node(p3str, [p[3]]) 
                        else:
                            p3 = p[3]   

                        p[0].onlyAddEdge([p[1],p3]) 

                        if 'struct' in p[0].type:
                            p[0].label += '_struct'
                        elif 'union' in p[0].type:
                            p[0].label += '_union'
                        elif p[0].type[0][-1] == '*':
                            p[0].label += '_int_unsigned'
                        else:
                            p[0].label += '_' + p[0].type[0]
                            if 'unsigned' in p[0].type:
                                p[0].label += '_unsigned'

                        p[0].label = p[0].label.replace(" ", "_")
                        p[0].node.attr['label'] = p[0].label

                else:
                    p[0].onlyAddEdge([p[1]])

            else:
                if ((p[3] is not None) and (p[3].node is not None)):
                    p[0].onlyAddEdge([p[3]])

            if self.ST.error:
                return

            if p[3].totype is not None and p[3].totype != p[3].type:
                p3.temp = self.TAC.newtemp()
                self.ST.InsertSymbol(p3.temp, 0)
                self.ST.ModifySymbol(p3.temp, "type", p[3].totype)
                self.ST.ModifySymbol(p3.temp, "check", "TEMP")
                self.updateSizeInSymTab(p[3].totype, p3.temp)
                if self.ST.isGlobal(p3.temp):
                    self.ST.ModifySymbol(p3.temp, "varclass", "Global")
                else :
                    self.ST.ModifySymbol(p3.temp, "varclass", "Local")
                    found, entry = self.ST.ReturnSymTabEntry(p3.temp)
                    var_size = found['sizeAllocInBytes']
                    if found["varclass"] == "Local":
                        # self.TAC.emit('-_int', '%esp', '%esp', f'${var_size}')
                        if found["offset"] >0:
                            self.ST.ModifySymbol(p3.temp, 'temp', f'-{found["offset"] + found["sizeAllocInBytes"] }(%ebp)')
                        else:
                            self.ST.ModifySymbol(p3.temp, 'temp', f'{-found["offset"] - found["sizeAllocInBytes"] }(%ebp)')
                    p3.temp = found['temp']  
                
                fromtype = p[3].type
                currtype = []
                for single_type in fromtype:
                    if single_type != 'arr'  and single_type[0] != '[' and single_type[-1] != ']':
                        currtype.append(single_type)
                cstr =',' +  ' '.join(currtype).replace(' ','_')

                self.TAC.emit('cast',p3.temp,p[3].temp,' '.join(p[3].totype).replace(' ','_') + cstr) 

            else:
                p3.temp = p[3].temp

            p[0].varname = p[1].varname
            p[0].temp = p[1].temp
            self.recursive_equate(p[1].type, p[0].label, p[1].temp, p3.temp)

            p[0].truelist.append(self.TAC.nextstat)
            p[0].falselist.append(self.TAC.nextstat+1)
            self.TAC.emit('ifnz goto','',p[0].temp,'')
            self.TAC.emit('goto','','','')

    def recursive_equate(self, p1type, p0label, p1temp, p3temp):
        if p0label == '=_struct' or p0label == '=_union':
                data_struc = self.ST.TT.ReturnTypeTabEntry(p1type[1], p1type[0])
                currOffset = 0
                left_offset = 0
                right_offset = 0
                left_new_temp = ''
                right_new_temp = ''

                for var in data_struc['vars'].keys():
                    
                    if p1temp[0] == '(':
                        left_new_temp = self.TAC.newtemp()
                        self.ST.InsertSymbol(left_new_temp, 0)
                        self.ST.ModifySymbol(left_new_temp, "type", ['int', 'unsigned'])
                        self.ST.ModifySymbol(left_new_temp, "check", "TEMP")
                        self.updateSizeInSymTab(['int', 'unsigned'], left_new_temp)
                        if self.ST.isGlobal(left_new_temp):
                            self.ST.ModifySymbol(left_new_temp, "varclass", "Global")
                        else :
                            self.ST.ModifySymbol(left_new_temp, "varclass", "Local")
                            found, entry = self.ST.ReturnSymTabEntry(left_new_temp)
                            var_size = found['sizeAllocInBytes']
                            if found["varclass"] == "Local":
                                # self.TAC.emit('-_int', '%esp', '%esp', f'${var_size}')
                                if found["offset"] >0:
                                    self.ST.ModifySymbol(left_new_temp, 'temp', f'-{found["offset"] + found["sizeAllocInBytes"] }(%ebp)')
                                else:
                                    self.ST.ModifySymbol(left_new_temp, 'temp', f'{-found["offset"] - found["sizeAllocInBytes"] }(%ebp)')
                            left_new_temp = found['temp'] 
                        self.TAC.emit('+_int', left_new_temp, p1temp[1:-1], f'${currOffset}')
                        left_new_temp = f'({left_new_temp})'
                    else:
                        left_offset = int(p1temp.split('(')[0])
                        left_new_temp = f'{left_offset+currOffset}(%ebp)'
                    
                    if p3temp[0] == '(':
                        right_new_temp = self.TAC.newtemp()
                        self.ST.InsertSymbol(right_new_temp, 0)
                        self.ST.ModifySymbol(right_new_temp, "type", ['int', 'unsigned'])
                        self.ST.ModifySymbol(right_new_temp, "check", "TEMP")
                        self.updateSizeInSymTab(['int', 'unsigned'], right_new_temp)
                        if self.ST.isGlobal(right_new_temp):
                            self.ST.ModifySymbol(right_new_temp, "varclass", "Global")
                        else :
                            self.ST.ModifySymbol(right_new_temp, "varclass", "Local")
                            found, entry = self.ST.ReturnSymTabEntry(right_new_temp)
                            var_size = found['sizeAllocInBytes']
                            if found["varclass"] == "Local":
                                # self.TAC.emit('-_int', '%esp', '%esp', f'${var_size}')
                                if found["offset"] >0:
                                    self.ST.ModifySymbol(right_new_temp, 'temp', f'-{found["offset"] + found["sizeAllocInBytes"] }(%ebp)')
                                else:
                                    self.ST.ModifySymbol(right_new_temp, 'temp', f'{-found["offset"] - found["sizeAllocInBytes"] }(%ebp)')
                            right_new_temp = found['temp'] 
                        self.TAC.emit('+_int', right_new_temp, p3temp[1:-1], f'${currOffset}')
                        right_new_temp = f'({right_new_temp})'
                    else:
                        right_offset = int(p3temp.split('(')[0])
                        right_new_temp = f'{right_offset+currOffset}(%ebp)'

                    if '*' in data_struc['vars'][var]['type']:
                        self.TAC.emit('=_unsigned_int', left_new_temp, right_new_temp)
                        if p0label=='=_struct': 
                            currOffset += 4
                    elif 'struct' in data_struc['vars'][var]['type']:
                        new_type = copy.deepcopy(data_struc['vars'][var]['type'])
                        new_type.reverse()
                        self.recursive_equate(new_type, '=_struct',left_new_temp, right_new_temp)
                        if p0label=='=_struct': 
                            currOffset += data_struc['vars'][var]['sizeAllocInBytes']
                    elif 'union' in data_struc['vars'][var]['type']:
                        new_type = copy.deepcopy(data_struc['vars'][var]['type'])
                        new_type.reverse()
                        self.recursive_equate(new_type, '=_union',left_new_temp, right_new_temp)
                        if p0label=='=_struct': 
                            currOffset += data_struc['vars'][var]['sizeAllocInBytes']
                    elif 'int' in data_struc['vars'][var]['type']:
                        self.TAC.emit('=_int', left_new_temp, right_new_temp)
                        if p0label=='=_struct': 
                            currOffset += 4
                    elif 'char' in data_struc['vars'][var]['type']:
                        self.TAC.emit('=_char', left_new_temp, right_new_temp)
                        if p0label=='=_struct': 
                            currOffset += 1
                    elif 'float' in data_struc['vars'][var]['type']:
                        self.TAC.emit('=_float', left_new_temp, right_new_temp)
                        if p0label=='=_struct': 
                            currOffset += 4
                    elif 'bool' in data_struc['vars'][var]['type']:
                        self.TAC.emit('=_bool', left_new_temp, right_new_temp)
                        if p0label=='=_struct': 
                            currOffset += 4
                    else:
                        print('Unknown data type for debugging') 
        else:
            self.TAC.emit(p0label, p1temp, p3temp, '')
            
    def p_assignment_operator(self, p):
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
        if self.isError :
            return
        p[0] = Node(str(p[1]))
        p[0].lineno = p.lineno(1)

    def p_expression(self, p):
        '''
        expression : assignment_expression
                | expression ',' assignment_expression
        '''
        if self.isError :
            return
        if (len(p) == 2):
            p[0] = p[1]
        elif (len(p) == 4):
            p[0] = Node(',',[p[1],p[3]])
            if self.ST.error:
                return
            p[0].temp = p[3].temp
            p[0].truelist = p[3].truelist
            p[0].falselist = p[3].falselist

    def p_constant_expression(self, p):
        '''
        constant_expression : conditional_expression
        '''
        if self.isError :
            return
        p[0] = p[1]

    def p_declaration(self, p):
        '''
        declaration : declaration_specifiers ';'
                    | declaration_specifiers init_declarator_list ';'
        '''
        if self.isError :
            return
        if (len(p) == 3):
             #  This rule is used when declaring structs and union
            p[0] = Node('TypeDecl',createAST=False)
        elif (len(p) == 4):
            p[0] = p[2]
        p[1].removeGraph()
        # Need to remove the nodes for declaration_specifiers

    def p_declaration_specifiers(self, p):
        '''
        declaration_specifiers : storage_class_specifier
                            | storage_class_specifier declaration_specifiers
                            | type_specifier
                            | type_specifier declaration_specifiers
        '''
        if self.isError :
            return
        if (len(p) == 2):
            p[0] = p[1]
        elif (len(p) == 3):
            p[0] = p[1]
            if ((p[2] is not None) and (p[2].node is not None)):
                p[0].onlyAddEdge([p[2]])
                if p[2].type and p[0].type:
                    p[0].type += p[2].type

            p[0].extraValues = p[2].extraValues + p[0].extraValues

        if len(p[0].type)>0 and 'struct' in p[0].type and len(p[0].type) >2:
            self.ST.error = 1
            print(f'Cannot have type specifiers for struct type at line {p[1].line}')
        elif len(p[0].type)>0 and 'union' in p[0].type and len(p[0].type) >2:
            self.ST.error = 1
            print(f'Cannot have type specifiers for union type at line {p[1].line}')

    def p_init_declarator_list(self, p):
        '''
        init_declarator_list : init_declarator
                            | init_declarator_list ',' InitM1 init_declarator
        '''
        if self.isError :
            return
        if (len(p) == 2):
            p[0] = p[1]
        elif (len(p) == 5):
            p[0] = Node(',',[p[1],p[4]])
        p[0].extraValues = p[-1].extraValues

    def p_InitM1(self, p):
        '''
        InitM1 : 
        '''
        if self.isError :
            return
        p[0] = Node('',createAST=False)
        p[0].extraValues = p[-2].extraValues

    def p_init_declarator(self, p):
        '''
        init_declarator : declarator
                        | declarator '=' initializer
        '''
        if self.isError :
            return
        if (len(p) == 2):
            p[1].removeGraph()
            p[0] = p[1]
        elif (len(p) == 4):
            p[0] = Node('=')
            p[0].variables = p[1].variables
            
        # Code to add types to variable
        p[0].extraValues = p[-1].extraValues
        for val in p[0].extraValues:
            p[0].addTypeInDict(val)

        # for key in p[0].variables.keys():
        #     print("The key is: " + key)
        #     print('  ', p[0].variables[key]) 
        for var_name in p[0].variables:
    
            #Updating type
            if p[0].variables[var_name] and p[0].variables[var_name][-1] in ['struct', 'union']:
                found = self.ST.TT.ReturnTypeTabEntry(p[0].variables[var_name][-2], p[0].variables[var_name][-1], p.lineno(1))
                if found:
                    self.ST.ModifySymbol(var_name, "vars", found['vars'], p.lineno(1))
                    self.ST.ModifySymbol(var_name, "check", found['check'], p.lineno(1))
                    self.ST.ModifySymbol(var_name, "type", p[0].variables[var_name],p.lineno(1))
            else:
                self.ST.ModifySymbol(var_name, "type", p[0].variables[var_name],p.lineno(1))
        
            #updating variable class
            if p[0].variables[var_name]:
                isGlobal = self.ST.isGlobal(var_name)
                isStatic = False
                if 'static' in p[0].variables[var_name]:
                    isStatic = True
                if isGlobal & isStatic:
                    self.ST.ModifySymbol(var_name, "varclass", "Global Static", p.lineno(1))
                elif isGlobal:
                    self.ST.ModifySymbol(var_name, "varclass", "Global", p.lineno(1))
                elif isStatic:
                    self.ST.ModifySymbol(var_name, "varclass", "Local Static", p.lineno(1))
                else:
                    self.ST.ModifySymbol(var_name, "varclass", "Local", p.lineno(1))

            # updating sizes
            if p[0].variables[var_name]:
                #handling arrays
                multiplier = 1
                for type_name in p[0].variables[var_name]:
                    if type_name[0]=='[' and type_name[-1]==']':
                        if type_name[1:-1] != '':
                            multiplier *= int(type_name[1:-1])
                    else:
                        break

                if '*' in p[0].variables[var_name]:
                    self.ST.ModifySymbol(var_name, "sizeAllocInBytes", multiplier*sizes["PTR"], p.lineno(1))
                elif 'struct' in p[0].variables[var_name] :
                    if not self.ST.isGlobal('dummy'):
                        struct_size = 0
                        found = self.ST.TT.ReturnTypeTabEntry(p[0].variables[var_name][-2], p[0].variables[var_name][-1], p.lineno(1))
                        if found:
                            struct_size = found['sizeAllocInBytes']
                        self.ST.ModifySymbol(var_name, "sizeAllocInBytes", multiplier*struct_size, p.lineno(1))
                    else:
                        print('Struct objects not allowed to be declared globally...')
                        self.ST.error = 1
                        return
                elif 'union' in p[0].variables[var_name]:
                    if not self.ST.isGlobal('dummy'):
                        struct_size = 0
                        found = self.ST.TT.ReturnTypeTabEntry(p[0].variables[var_name][-2], p[0].variables[var_name][-1], p.lineno(1))
                        if found:
                            struct_size = found['sizeAllocInBytes']
                            for var in found['vars']:
                                found['vars'][var]['offset'] = 0
                        self.ST.ModifySymbol(var_name, "sizeAllocInBytes", multiplier*struct_size, p.lineno(1))
                    else:
                        print('Union objects not allowed to be declared globally...')
                        self.ST.error = 1
                        return
                elif 'long' in p[0].variables[var_name]:
                    if 'int' in p[0].variables[var_name]:
                        self.ST.ModifySymbol(var_name, "sizeAllocInBytes", multiplier*sizes["long int"], p.lineno(1))
                    elif 'double' in p[0].variables[var_name]:
                        self.ST.ModifySymbol(var_name, "sizeAllocInBytes", multiplier*sizes["long double"], p.lineno(1))
                    else:
                        self.ST.ModifySymbol(var_name, "sizeAllocInBytes", multiplier*sizes["long"], p.lineno(1))
                elif 'float' in p[0].variables[var_name]:
                    self.ST.ModifySymbol(var_name, "sizeAllocInBytes", multiplier*sizes["float"], p.lineno(1))
                elif 'double' in p[0].variables[var_name]:
                    self.ST.ModifySymbol(var_name, "sizeAllocInBytes", multiplier*sizes["double"], p.lineno(1))
                elif 'short' in p[0].variables[var_name]:
                    self.ST.ModifySymbol(var_name, "sizeAllocInBytes", multiplier*sizes["short"], p.lineno(1))
                elif 'int' in p[0].variables[var_name]:
                    self.ST.ModifySymbol(var_name, "sizeAllocInBytes", multiplier*sizes["int"], p.lineno(1))
                elif 'char' in p[0].variables[var_name]:
                    self.ST.ModifySymbol(var_name, "sizeAllocInBytes", multiplier*sizes["char"], p.lineno(1))
                elif 'bool' in p[0].variables[var_name]:
                    self.ST.ModifySymbol(var_name, "sizeAllocInBytes", multiplier*sizes["bool"], p.lineno(1))
                else:
                    self.ST.ModifySymbol(var_name, "sizeAllocInBytes", multiplier*sizes["void"], p.lineno(1))

            # updating sizes to be allocated based on the type

            # ['void' , 'char', 'int', 'long', 'float', 'bool', 'double', 'signed', 'unsigned']

            found, entry = self.ST.ReturnSymTabEntry(var_name, p.lineno(1))
            temp_type_list = []
            temp2_type_list = []
            nums_arr = []

            for single_type in entry['type']:
                if single_type != '*':
                    temp_type_list.append(single_type)
                    if single_type[0] != '[' or single_type[-1] != ']':
                        temp2_type_list.append(single_type)

                if single_type[0] == '[' and single_type[-1] == ']':
                    if single_type[1:-1] == '':
                        self.ST.error = 1
                        print('Cannot have empty indices for array declarations at line', entry['line'])
                        return
                    elif int(single_type[1:-1]) <= 0:
                        self.ST.error = 1
                        print('Cannot have non-positive integers for array declarations at line', entry['line'])
                        return

            if len(temp2_type_list) != len(set(temp2_type_list)):
                self.ST.error = 1
                print('variables cannot have duplicating type of declarations at line', entry['line'])
                return

            if 'long' in entry['type'] and 'short' in entry['type']:
                self.ST.error = 1
                print('variable cannot be both long and short at line', entry['line'])
                return
            elif 'unsigned' in entry['type'] and 'signed' in entry['type']:
                self.ST.error = 1
                print('variable cannot be both signed and unsigned at line', entry['line'])
                return
            elif 'void' in entry['type'] and '*' not in entry['type']:
                self.ST.error = 1
                print('Cannot have a void type variable at line ', entry['line'])
                return
            else:
                data_type_count = 0
                if 'int' in entry['type'] or 'short' in entry['type']  or 'unsigned' in entry['type'] or 'signed' in entry['type']:
                    data_type_count += 1
                if 'char' in  entry['type']:
                    data_type_count += 1
                if 'bool' in  entry['type']:
                    data_type_count += 1
                if 'float' in entry['type']:
                    data_type_count += 1
                if 'double' in entry['type']:
                    data_type_count += 1
                if 'void' in entry['type']:
                    data_type_count += 1
                if 'struct' in entry['type']:
                    data_type_count += 1
                if 'union' in entry['type']:
                    data_type_count += 1
                if data_type_count > 1:    
                    self.ST.error = 1
                    print('Two or more conflicting data types specified for variable at line', entry['line']) 
                    return
                if 'long' in entry['type']:
                    if 'char' in entry['type'] or 'bool' in  entry['type'] or 'float' in  entry['type'] or 'void' in  entry['type']:
                        self.ST.error = 1
                        print('Two or more conflicting data types specified for variable at line', entry['line'])
                        return

            if (len(p) == 4):
                isarr = 0
                for i in range(len(entry['type'])):
                    if entry['type'][i][0]=='[' and entry['type'][i][-1] == ']':
                        isarr += 1
                
                type_list = entry['type']
                if entry['check'] == 'VAR':
                    p[1].isvar = 1

                if set(type_list) == {'*'}:
                    type_list = []
                                                 
                if 'unsigned' in type_list or 'signed' in type_list:
                    if 'bool' not in type_list and 'char' not in type_list and 'short' not in type_list:
                        type_list.append('int')
                p[1].type = []
                if 'long' in type_list and 'int' in type_list:
                    p[1].type.append('long int')
                    for single_type in type_list:
                        if single_type != 'long' and single_type != 'int':
                            p[1].type.append(single_type)
                
                elif 'long' in type_list and 'double' in type_list:
                    p[1].type.append('long double')
                    for single_type in type_list:
                        if single_type != 'long' and single_type != 'double':
                            p[1].type.append(single_type)
                
                elif 'long' in type_list:
                    p[1].type.append('long int')
                    for single_type in type_list:
                        if single_type != 'long':
                            p[1].type.append(single_type)

                elif 'int' in type_list:
                    p[1].type.append('int')
                    for single_type in type_list:
                        if single_type != 'int':
                            p[1].type.append(single_type)

                elif 'short' in type_list:
                    p[1].type.append('short')
                    for single_type in type_list:
                        if single_type != 'short':
                            p[1].type.append(single_type)
                
                elif 'char' in type_list:
                    p[1].type.append('char')
                    for single_type in type_list:
                        if single_type != 'char':
                            p[1].type.append(single_type)
                
                elif 'bool' in type_list:
                    p[1].type.append('bool')
                    for single_type in type_list:
                        if single_type != 'bool':
                            p[1].type.append(single_type)
                
                elif 'str' in type_list:
                    p[1].type.append('str')
                    for single_type in type_list:
                        if single_type != 'str':
                            p[1].type.append(single_type)
                
                elif 'float' in type_list:
                    p[1].type.append('float')
                    for single_type in type_list:
                        if single_type != 'float':
                            p[1].type.append(single_type)

                elif 'double' in type_list:
                    p[1].type.append('double')
                    for single_type in type_list:
                        if single_type != 'double':
                            p[1].type.append(single_type)

                if 'struct' in type_list:
                    p[1].type.append('struct')
                    for single_type in type_list:
                        if single_type != 'struct':
                                p[1].type.append(single_type)     

                if 'union' in type_list:
                    p[1].type.append('union')
                    for single_type in type_list:
                        if single_type != 'union':
                                p[1].type.append(single_type)     

                if isarr > 0:
                    temp_type = []
                    temp_type.append(p[1].type[0])
                    for i in range(isarr):
                        temp_type[0] += ' *'

                    for i in range(len(p[1].type)):
                        if i>isarr:
                            temp_type.append(p[1].type[i])
                    p[1].type = temp_type
                    p[1].type.append('arr')
                    for i in range(len(type_list)):
                        if type_list[len(type_list)-i-1][0] == '[' and type_list[len(type_list)-i-1][-1] == ']':  
                            p[1].type.append(type_list[len(type_list)-i-1])

                if 'void' in type_list:
                    p[1].type.append('void')
                    for single_type in type_list:
                        if single_type != 'void':
                                p[1].type.append(single_type)     

                if '*' in type_list:
                    temp_type = []
                    temp_type.append(p[1].type[0])
                    for i in range(1, len(p[1].type)):
                        if p[1].type[i] == '*':
                            temp_type[0] += ' *'
                        else:
                            temp_type.append(p[1].type[i])
                    p[1].type = temp_type
                if p[1] is None or p[3] is None or p[1].type is None or p[3].type is None or p[1].type == [] or p[3].type == []:
                    self.ST.error = 1
                    print(f'Cannot perform assignment at line {p.lineno(2)}')
                    return

                if 'struct' in p[1].type[0] or 'union' in p[1].type[0]:
                    p[1].vars = entry['vars']
                    if 'arr' in p[1].type and 'init_list' not in p[3].type:
                        self.ST.error = 1
                        print(f'Invalid array initialization at line {p.lineno(2)}')

                if 'struct' in p[1].type and 'struct' not in p[3].type:
                    self.ST.error = 1;
                    print(f'Cannot assign non-struct value {p[3].type} to struct type {p[1].type} at line {p.lineno(2)}')

                elif 'struct' in p[1].type and 'struct' in p[3].type and p[1].type[1] != p[3].type[1]:
                    self.ST.error = 1;
                    print(f'Incompatible struct types to perform assignment at line {p.lineno(2)}')
                
                elif 'union' in p[1].type and 'union' not in p[3].type:
                    self.ST.error = 1;
                    print(f'Cannot assign non-union value {p[3].type} to union type {p[1].type} at line {p.lineno(2)}')

                elif 'union' in p[1].type and 'union' in p[3].type and p[1].type[1] != p[3].type[1]:
                    self.ST.error = 1;
                    print(f'Incompatible union types to perform assignment at line {p.lineno(2)}')
                
                elif p[1].type[0] in aat and p[3].type[0] not in aat and p[3].type[0][-1] != '*':
                    self.ST.error = 1
                    print(f'Type mismatch while assigning value at line {p.lineno(2)}')

                elif p[1].type[0] not in aat and p[1].type[0][-1] != '*' and p[3].type[0] in aat:
                    self.ST.error = 1
                    print(f'Type mismatch while assigning value at line {p.lineno(2)}')

                elif 'arr' in p[1].type and 'init_list' not in p[3].type:
                    self.ST.error = 1
                    print(f'Invalid array initialization at line {p.lineno(2)}')
                
                elif 'arr' not in p[1].type and p[1].type[0][-1] == '*' and p[3].type[0] not in iit and p[3].type[0][-1] != '*' and 'str' not in p[3].type:    
                    self.ST.error = 1
                    print(f'Incompatible assignment between pointer and {p[3].type} at line {p.lineno(2)}')

                if self.ST.error:
                    return

                p[0].type = p[1].type
                if p[0].type is None:
                    p[0].type = []
                
                isin = True
                for single_type in p[0].type:
                    if single_type not in p[3].type:
                        isin = False
                if isin == False and 'arr' not in p[1].type and 'init_list' not in p[3].type:
                    p[3].totype = p[0].type
                    p3str = 'to'
                    for single_type in p[0].type:
                        p3str += '_' + single_type
                    p3str = p3str.replace(' ','_')
                    if '*' == p[0].type[0][-1]:
                        p3str = 'to_int_unsigned'
                    p3 = Node(p3str, [p[3]]) 
                else:
                    p3 = p[3]   

                p[0].onlyAddEdge([p[1],p3]) 

                if 'struct' in p[0].type:
                    p[0].label += '_struct'
                elif 'union' in p[0].type:
                    p[0].label += '_union'
                elif len(p[0].type)>0 and p[0].type[0][-1] == '*' and 'arr' not in p[0].type:
                    p[0].label += '_int_unsigned'
                else:
                    p[0].label += '_' + p[0].type[0]
                    if 'unsigned' in p[0].type:
                        p[0].label += '_unsigned'

                p[0].label = p[0].label.replace(" ", "_")
                p[0].node.attr['label'] = p[0].label

        if self.ST.error:
            return

        for var_name in p[0].variables:
            if 'struct' in p[0].variables[var_name] and '*' not in p[0].variables[var_name]:
                found, entry = self.ST.ReturnSymTabEntry(var_name, p.lineno(1))
                if found and found["varclass"] == "Local":
                    for var in found['vars']:
                       found['vars'][var]['temp'] = f'-{-found["vars"][var]["offset"] + self.ST.offset}(%ebp)'
            elif 'union' in p[0].variables[var_name] and '*' not in p[0].variables[var_name]:
                found, entry = self.ST.ReturnSymTabEntry(var_name, p.lineno(1))
                if found and found["varclass"] == "Local":
                    for var in found['vars']:
                        found['vars'][var]['temp'] = f'-{self.ST.offset}(%ebp)'
            found, entry = self.ST.ReturnSymTabEntry(var_name)
            # var_size = ((found['sizeAllocInBytes'] + 3) // 4) * 4
            if found["varclass"] == "Local":
                # self.TAC.emit('-_int', '%esp', '%esp', f'${var_size}')
                if found["offset"] >0:
                    self.ST.ModifySymbol(var_name, 'temp', f'-{found["offset"] + found["sizeAllocInBytes"] }(%ebp)')
                else:
                    self.ST.ModifySymbol(var_name, 'temp', f'{-found["offset"] - found["sizeAllocInBytes"] }(%ebp)')
            p[0].temp = found['temp']
        if len(p) == 4:
            if self.ST.error:
                return

            if p[3].totype is not None and p[3].totype != p[3].type and 'init_list' not in p[3].type and 'arr' not in p[3].totype:
                p3.temp = self.TAC.newtemp()
                self.ST.InsertSymbol(p3.temp, 0)
                self.ST.ModifySymbol(p3.temp, "type", p[3].totype)
                self.ST.ModifySymbol(p3.temp, "check", "TEMP")
                self.updateSizeInSymTab(p[3].totype, p3.temp)
                if self.ST.isGlobal(p3.temp):
                    self.ST.ModifySymbol(p3.temp, "varclass", "Global")
                else :
                    self.ST.ModifySymbol(p3.temp, "varclass", "Local")
                    found, entry = self.ST.ReturnSymTabEntry(p3.temp)
                    var_size = found['sizeAllocInBytes']
                    if found["varclass"] == "Local":
                        # self.TAC.emit('-_int', '%esp', '%esp', f'${var_size}')
                        if found["offset"] >0:
                            self.ST.ModifySymbol(p3.temp, 'temp', f'-{found["offset"] + found["sizeAllocInBytes"] }(%ebp)')
                        else:
                            self.ST.ModifySymbol(p3.temp, 'temp', f'{-found["offset"] - found["sizeAllocInBytes"] }(%ebp)')
                    p3.temp = found['temp']  

                fromtype = p[3].type
                currtype = []
                for single_type in fromtype:
                    if single_type != 'arr'  and single_type[0] != '[' and single_type[-1] != ']':
                        currtype.append(single_type)
                cstr =',' +  ' '.join(currtype).replace(' ','_')

                self.TAC.emit('cast',p3.temp,p[3].temp, ' '.join(p[3].totype).replace(' ','_') + cstr) 

            else:
                p3.temp = p[3].temp

            if self.ST.isGlobal():
                print("Cannot initialize global variables while declaring")
                self.ST.error = 1
                return
            elif "static" in p[1].type:
                if (len(p[1].type) != 2) or ("int" not in p[1].type):
                    print("Static Variables can only be of type int")
                    self.ST.error = 1
                    return
                elif p[3].label.isnumeric():
                    found, entry = self.ST.ReturnSymTabEntry(p[0].temp)
                    found["temp"] = found["temp"] + "." + str(self.TAC.staticCounter)
                    self.TAC.staticCounter += 1
                    self.TAC.staticSymbols.append([found["temp"],p[3].label,None])
                else:
                    print("Wrong Initialization of Static Variable")
                    self.ST.error = 1
                    return  
            else:
                self.recursive_equate(p[1].type, p[0].label, p[0].temp, p3.temp)

            # self.TAC.emit(p[0].label, p[0].temp, p3.temp,'')
        if (len(p) == 2):
            for var_name in p[0].variables:
                if self.ST.isGlobal():
                    found, entry = self.ST.ReturnSymTabEntry(var_name)
                    self.TAC.globalSymbols.append([p[1].temp,found["sizeAllocInBytes"]])
                elif "static" in p[0].variables[var_name]:
                    if (len(p[0].variables[var_name]) != 2) or ("int" not in p[0].variables[var_name]):
                        print("Static Variables can only be of type int")
                        self.ST.error = 1
                        return
                    found, entry = self.ST.ReturnSymTabEntry(var_name)
                    found["temp"] = found["temp"] + "." + str(self.TAC.staticCounter)
                    self.TAC.staticCounter += 1
                    self.TAC.staticSymbols.append([found["temp"],None,found["sizeAllocInBytes"]]) 

    def p_storage_class_specifier(self, p):
        '''
        storage_class_specifier : STATIC
        '''
        if self.isError :
            return
        p[0] = Node(str(p[1]))
        p[0].extraValues.append(str(p[1]))
        p[0].line = p.lineno(1)

    def p_type_specifier(self, p):
        '''
        type_specifier : VOID
                    | CHAR
                    | INT
                    | FLOAT
                    | UNSIGNED
                    | struct_or_union_specifier
        '''
        if self.isError :
            return
        if str(p[1]) in ['void' , 'char', 'short', 'int', 'long', 'float', 'bool', 'double', 'signed', 'unsigned']:
            p[0] = Node(str(p[1]))
            p[0].extraValues.append(str(p[1]))
            p[0].type = []
            p[0].type.append(str(p[1]))
            p[0].line = p.lineno(1)
        else:
            p[0] = p[1]
            p[0].line = p[1].line

    def p_struct_or_union_specifier(self, p):
        '''
        struct_or_union_specifier : struct_or_union ID '{' markerStructFlag2 struct_declaration_list '}' markerStructFlag0
                                | struct_or_union ID
        '''
        if self.isError :
            return
        p[0] = p[1]
        p[0].type += [p[2]['lexeme']]

        if (len(p) == 8):
            p2val = p[2]['lexeme']
            p[2] = Node(str(p2val))

            p[0].node.attr['label'] = p[0].node.attr['label'] + '{}'
            p[0].label = p[0].node.attr['label']

            if ((p[2] is not None) and (p[2].node is not None)):
                if ((p[5] is not None) and (p[5].node is not None)):
                    p[0].onlyAddEdge([p[2],p[5]])
                else:
                    p[0].onlyAddEdge([p[2]])
            else:
                if ((p[5] is not None) and (p[5].node is not None)):
                    p[0].onlyAddEdge([p[5]])
            
            data_struct_found = self.ST.TT.ReturnTypeTabEntry(p2val, p[1].type[0], p.lineno(1))
            struct_size = 0
            for var in data_struct_found['vars']:
                if 'sizeAllocInBytes' in data_struct_found['vars'][var].keys():
                    if p[1].type[0] =='struct':
                        struct_size += data_struct_found['vars'][var]['sizeAllocInBytes']
                    else:
                        struct_size = max(struct_size, data_struct_found['vars'][var]['sizeAllocInBytes'])
            
            self.ST.TT.ModifySymbol(p2val, "sizeAllocInBytes", struct_size,p.lineno(1), 1)
            sizes[f'{p[1].type[0]} {p2val}'] = struct_size

        elif (len(p) == 7): # not needed anymore
            p[0].node.attr['label'] = p[0].node.attr['label'] + '{}'
            p[0].label = p[0].node.attr['label']
        
            if ((p[3] is not None) and (p[3].node is not None)):
                p[0].onlyAddEdge([p[3]])

        elif (len(p) == 3):
            # This rule is used when declaring a struct type variable 
            # Eg:
            # struct tmp a;
            # While Type Checking check here whether 
            # the struct of given type exists or not
            p2val = p[2]['lexeme']
            p[2] = Node(str(p2val))
            if self.ST.TT.ReturnTypeTabEntry(p[2].label, p[0].label, p.lineno(2)) is None:
                self.ST.error = True
            else:
                p[0].extraValues.append(p2val)
                p[0].extraValues.append(p[1].label)
                
                p[0].onlyAddEdge([p[2]])

    def p_markerStructFlag2(self, p):
        '''
        markerStructFlag2 :
        '''
        if self.isError :
            return
        iden = p[-2]['lexeme']
        type_name = p[-3].label.upper()
        line_num = p[-2]['additional']['line']
        self.ST.flag = 1
        self.ST.InsertSymbol(iden, line_num, type_name)
        self.ST.flag = 2

    def p_markerStructFlag0(self, p):
        '''
        markerStructFlag0 :
        '''
        if self.isError :
            return
        self.ST.flag = 0

    def p_struct_or_union(self, p):
        '''
        struct_or_union : STRUCT
        '''
        if self.isError :
            return
        p[0] = Node(str(p[1]))
        p[0].type = [str(p[1]).lower()]
        p[0].line = p.lineno(1)
        
    def p_struct_declaration_list(self, p):
        '''
        struct_declaration_list : struct_declaration
                                | struct_declaration_list struct_declaration
        '''
        if self.isError :
            return

        if (len(p) == 2):
            p[0] = p[1]
        elif (len(p) == 3):
            p[0] = p[2]
            if ((p[1] is not None) and (p[1].node is not None)):
                p[0].onlyAddEdge([p[1]])

    def p_struct_declaration(self, p):
        '''
        struct_declaration : specifier_qualifier_list struct_declarator_list ';'
        '''
        if self.isError :
            return
        p[0] = Node('StructOrUnionDec',[p[1],p[2]])
        if p[1].type is None:
            p[1].type = []

        temp_type_list = []
        for single_type in p[1].type:
            if single_type != '*':
                temp_type_list.append(single_type)

        if len(temp_type_list) != len(set(temp_type_list)):
            self.ST.error = 1
            print('Structure/Union variable cannot have duplicating type of declarations at line', p.lineno(3))
            return

        if 'long' in p[1].type and 'short' in p[1].type:
            self.ST.error = 1
            print('Structure/Union variable cannot be both long and short at line', p.lineno(3))
        elif 'unsigned' in p[1].type and 'signed' in p[1].type:
            self.ST.error = 1
            print('Structure/Union variable cannot be both signed and unsigned at line', p.lineno(3))
        else:
            data_type_count = 0
            if 'int' in p[1].type or 'short' in p[1].type  or 'unsigned' in p[1].type or 'signed' in p[1].type:
                data_type_count += 1
            if 'char' in  p[1].type:
                data_type_count += 1
            if 'bool' in  p[1].type:
                data_type_count += 1
            if 'float' in p[1].type:
                data_type_count += 1
            if 'double' in p[1].type:
                data_type_count += 1
            if 'void' in p[1].type:
                data_type_count += 1
            if 'struct' in p[1].type:
                data_type_count += 1
            if 'union' in p[1].type:
                data_type_count += 1
            if data_type_count > 1:    
                self.ST.error = 1
                print('Two or more conflicting data types specified for variable at line', p.lineno(3)) 

            if 'long' in p[1].type:
                if 'char' in p[1].type or 'bool' in  p[1].type or 'float' in  p[1].type or 'void' in  p[1].type:
                    self.ST.error = 1
                    print('Two or more conflicting data types specified for variable at line', p.lineno(3))

    def p_specifier_qualifier_list(self, p):
        '''
        specifier_qualifier_list : type_specifier specifier_qualifier_list
                                | type_specifier
        '''
        if self.isError :
            return
        if (len(p) == 2):
            p[0] = p[1]
        elif (len(p) == 3):
            p[0] = p[1]

            for single_type in p[2].type:
                p[0].type.append(single_type)

            if ((p[2] is not None) and (p[2].node is not None)):
                p[0].onlyAddEdge([p[2]])
                p[0].extraValues += p[2].extraValues

    def p_struct_declarator_list(self, p):
        '''
        struct_declarator_list : struct_declarator
                            | struct_declarator_list ',' structDeclaratorMarker1 struct_declarator
        '''
        if self.isError :
            return
        if (len(p) == 2):
            p[0] = p[1]
        elif (len(p) == 5):
            p[0] = Node(',',[p[1],p[4]])
        p[0].extraValues = p[-1].extraValues
            
    def p_structDeclaratorMarker1(self, p):
        '''
        structDeclaratorMarker1 :
        '''
        if self.isError :
            return
        p[0] = Node('',createAST=False)
        p[0].extraValues = p[-2].extraValues

    def p_struct_declarator(self, p):
        '''
        struct_declarator : declarator
        '''
        if self.isError :
            return
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
        
        for var_name in p[0].variables.keys():
            self.ST.ModifySymbol(var_name, 'type', p[0].variables[var_name], p.lineno(0))
            self.ST.ModifySymbol(var_name, "varclass", "Local", p.lineno(0))
            # updating sizes
            if p[0].variables[var_name]:
                #handling arrays
                multiplier = 1
                for type_name in p[0].variables[var_name]:
                    if type_name[0]=='[' and type_name[-1]==']':
                        multiplier *= int(type_name[1:-1])
                    else:
                        break

                if '*' in p[0].variables[var_name]:
                    self.ST.ModifySymbol(var_name, "sizeAllocInBytes", multiplier*sizes["PTR"], p.lineno(0))
                elif 'struct' in p[0].variables[var_name] :
                    struct_size = 0
                    found = self.ST.TT.ReturnTypeTabEntry(p[0].variables[var_name][-2], p[0].variables[var_name][-1], p.lineno(1))
                    if found:
                        if 'sizeAllocInBytes' in found.keys():
                            struct_size = found['sizeAllocInBytes']
                        else:
                            struct_size = 0
                            print('Cannot define object of the same struct within itself.')
                            self.ST.error = 1
                        self.ST.ModifySymbol(var_name, "sizeAllocInBytes", multiplier*struct_size, p.lineno(1))
                    else:
                        self.ST.error = 1
                elif 'union' in p[0].variables[var_name]:
                    struct_size = 0
                    found = self.ST.TT.ReturnTypeTabEntry(p[0].variables[var_name][-2], p[0].variables[var_name][-1], p.lineno(1))
                    if found:
                        if found and 'sizeAllocInBytes' in found.keys():
                            struct_size = found['sizeAllocInBytes']
                            for var in found['vars']:
                                found['vars'][var]['offset'] = 0
                        else:
                            struct_size = 0
                            print('Cannot define object of the same union within itself.')
                            self.ST.error = 1
                        self.ST.ModifySymbol(var_name, "sizeAllocInBytes", multiplier*struct_size, p.lineno(1))
                    else:
                        self.ST.error = 1
                elif 'long' in p[0].variables[var_name]:
                    if 'int' in p[0].variables[var_name]:
                        self.ST.ModifySymbol(var_name, "sizeAllocInBytes", multiplier*sizes["long int"], p.lineno(0))
                    elif 'double' in p[0].variables[var_name]:
                        self.ST.ModifySymbol(var_name, "sizeAllocInBytes", multiplier*sizes["long double"], p.lineno(0))
                    else:
                        self.ST.ModifySymbol(var_name, "sizeAllocInBytes", multiplier*sizes["long"], p.lineno(0))
                elif 'float' in p[0].variables[var_name]:
                    self.ST.ModifySymbol(var_name, "sizeAllocInBytes", multiplier*sizes["float"], p.lineno(0))
                elif 'double' in p[0].variables[var_name]:
                    self.ST.ModifySymbol(var_name, "sizeAllocInBytes", multiplier*sizes["double"], p.lineno(0))
                elif 'short' in p[0].variables[var_name]:
                    self.ST.ModifySymbol(var_name, "sizeAllocInBytes", multiplier*sizes["short"], p.lineno(0))
                elif 'int' in p[0].variables[var_name]:
                    self.ST.ModifySymbol(var_name, "sizeAllocInBytes", multiplier*sizes["int"], p.lineno(0))
                elif 'char' in p[0].variables[var_name]:
                    self.ST.ModifySymbol(var_name, "sizeAllocInBytes", multiplier*sizes["char"], p.lineno(0))
                elif 'bool' in p[0].variables[var_name]:
                    self.ST.ModifySymbol(var_name, "sizeAllocInBytes", multiplier*sizes["bool"], p.lineno(0))
                else:
                    self.ST.ModifySymbol(var_name, "sizeAllocInBytes", multiplier*sizes["void"], p.lineno(0))
            else:
                self.ST.ModifySymbol(var_name, "sizeAllocInBytes", 0, p.lineno(0))

    def p_declarator(self, p):
        '''
        declarator : direct_declarator
                | pointer direct_declarator
        '''
        if self.isError :
            return
        if (len(p) == 2):
            p[0] = p[1]
        elif (len(p) == 3):
            p[0] = Node('Decl',[p[1],p[2]])
            p[0].variables = p[2].variables
            for val in p[1].extraValues:
                p[0].addTypeInDict(val)
            p[0].temp = p[2].temp

    def p_function_declarator(self, p):
        '''
        function_declarator : direct_declarator
                            | pointer direct_declarator
        '''
        if self.isError :
            return
        if (len(p) == 2):
            p[0] = p[1]
        elif (len(p) == 3):
            p[0] = Node('Decl',[p[1],p[2]])
            p[0].variables = p[2].variables
            p[0].extraValues += p[1].extraValues

    def p_direct_declarator(self, p):
        '''
        direct_declarator : identifier
                        | '(' declarator ')'
                        | direct_declarator '[' ']'
                        | direct_declarator '(' markerFuncPush ')'
                        | direct_declarator '[' IntegerConst ']'
                        | direct_declarator '(' markerFuncPush parameter_type_list ')'
        '''
        if self.isError :
            return
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
                try:
                    p[0].arrs = p[1].arrs
                except:
                    p[0].arrs = []
                
                p[0].arrs.append('empty')

        elif (len(p) == 5):
            if (p[2] == '('):
                if(p[3] is None):
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
                for variable in p[4].variables:
                    if variable == p[1].label:
                        self.ST.error = 1
                        self.isError = 1
                        print(f'Cannot have function parameter with same name as function at line {p.lineno(2)}')
                        return
                p[0].variables = p[4].variables
                p[0].variables[p[1].label] = ["Function Name"]

    def p_markerFuncPush(self, p):
        '''
        markerFuncPush :
        '''
        if self.isError :
            return
        p[0] = None
        self.ST.PushScope(self.TAC)
        self.ST.offset = 0

    def p_pointer(self, p):
        '''
        pointer : '*'
                | '*' pointer
        '''
        if self.isError :
            return
        if (len(p) == 2):
            p[0] = Node('PTR')
            p[0].extraValues.append("*")
            p[0].type = ['*']
        elif (len(p) == 3):
            p[0] = Node('PTR',[p[2]])
            p[0].extraValues = p[2].extraValues
            p[0].extraValues.append("*")
            p[0].type = ['*']
            if p[2].type:
                for single_type in p[2].type:
                    p[0].type.append(single_type)
        elif (len(p) == 4):
            p[0] = Node('PTR',[p[2],p[3]])
            p[0].extraValues = p[2].extraValues + p[3].extraValues
            p[0].extraValues.append("*")
            p[0].type = ['*']
            if p[1].type:
                for single_type in p[1].type:
                    p[0].type.append(single_type)
            if p[2].type:
                for single_type in p[2].type:
                    p[0].type.append(single_type)

    def p_parameter_type_list(self, p):
        '''
        parameter_type_list : parameter_list
        '''
        if self.isError :
            return
        if (len(p) == 2):
            p[0] = p[1]

    def p_parameter_list(self, p):
        '''
        parameter_list : parameter_declaration
                    | parameter_list ',' parameter_declaration
        '''
        if self.isError :
            return
        if (len(p) == 2):
            p[0] = p[1]
        else:
            p[0] = Node(',',[p[1],p[3]])
            p[0].variables = {**p[1].variables, **p[3].variables}

    def p_parameter_declaration_2(self, p):
        '''
        parameter_declaration : declaration_specifiers declarator
        '''
        if self.isError :
            return
        p[0] = Node('ParDecl',[p[1],p[2]], createAST=False)
        p[0].variables = p[2].variables
        p[1].removeGraph()
        p[2].removeGraph()
        for val in p[1].extraValues:
            p[0].addTypeInDict(val)
        
    def p_type_name(self, p):
        '''
        type_name : specifier_qualifier_list
                | specifier_qualifier_list abstract_declarator
        '''
        if self.isError :
            return

        if len(p) == 2:
            p[0] = Node('TypeName',[p[1]])
            p[0].type = p[1].type
        else:
            p[0] = Node('TypeName',[p[1],p[2]])
            p[0].type = p[1].type

            if not p[0].type:
                p[0].type = []

            if p[2].type:
                for single_type in p[2].type:
                    p[0].type.append(single_type)

    def p_abstract_declarator(self, p):
        '''
        abstract_declarator : pointer
                            | direct_abstract_declarator
                            | pointer direct_abstract_declarator
        '''
        if self.isError :
            return
        if len(p) == 2:
            p[0] = Node('AbsDecl',[p[1]])
            p[0].type = p[1].type

        else:
            p[0] = Node('AbsDecl',[p[1],p[2]])
            p[0].type = p[1].type
            if p[2].type:
                for single_type in p[2].type:
                    p[0].type.append(single_type)

    def p_direct_abstract_declarator(self, p):
        '''
        direct_abstract_declarator : '[' ']'
                                | '(' ')'
                                | '(' abstract_declarator ')'
                                | '(' parameter_type_list ')'
                                | '[' constant_expression ']'
                                | direct_abstract_declarator '[' ']'
                                | direct_abstract_declarator '(' ')'
                                | direct_abstract_declarator '[' IntegerConst ']'
                                | direct_abstract_declarator '(' parameter_type_list ')'
        '''
        if self.isError :
            return

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

    def p_initializer(self, p):
        '''
        initializer : assignment_expression
        '''
        if self.isError :
            return
        if len(p) == 2:
            p[0] = p[1]
        elif len(p) == 4:
            p[0] = Node('{}',[p[2]])
            p[0].type = ['init_list']

    def p_statement(self, p):
        '''
        statement : labeled_statement
                | compound_statement
                | expression_statement
                | selection_statement
                | iteration_statement
                | jump_statement
        '''
        if self.isError :
            return
        p[0] = p[1]

    def p_labeled_statement_2(self, p):
        '''
        labeled_statement : markerCase1 CASE constant_expression markerCase2 ':' statement
        '''
        if self.isError :
            return

        p[0] = Node('CASE:',[p[3],p[6]])
        p[0].numdef = p[6].numdef
        if self.ST.error:
            return
        p[0].breaklist = p[6].breaklist
        p[0].nextlist = p[6].nextlist
        p[0].testlist.append([p[3].temp, p[1].quad, p[4].quad])

    def p_labeled_statement_3(self, p):
        '''
        labeled_statement : markerCase1 DEFAULT ':' statement
        '''
        if self.isError :
            return

        p[0] = Node('DEFAULT:',[p[4]])
        p[0].numdef = 1 + p[4].numdef
        if self.ST.error:
            return
        p[0].breaklist = p[4].breaklist
        p[0].nextlist = p[4].nextlist
        p[0].testlist.append([None, p[1].quad, None])

    def p_markerCase1(self, p):
        '''
        markerCase1 : 
        '''

        if self.isError :
            return

        p[0] = Node('',createAST=False)
        if self.ST.error:
            return
        p[0].quad = self.TAC.nextstat

    def p_markerCase2(self, p):
        '''
        markerCase2 : 
        '''
        if self.isError :
            return

        p[0] = Node('',createAST=False)
        if self.ST.error:
            return
        p[0].quad = self.TAC.nextstat

    def p_compound_statement(self, p):
        '''
        compound_statement : '{' markerCompStatPush '}' markerCompStatPop
                        | '{' markerCompStatPush block_item_list '}' markerCompStatPop
        '''
        if self.isError :
            return
        if (len(p) == 5):
            p[0] = Node('EmptySCOPE',createAST = False)
        elif (len(p) == 6):
            p[0] = Node('SCOPE',[p[3]])
            if self.ST.error:
                return
            p[0].numdef = p[3].numdef
            p[0].truelist = p[3].truelist
            p[0].falselist = p[3].falselist
            p[0].breaklist = p[3].breaklist
            p[0].continuelist = p[3].continuelist
            p[0].nextlist = p[3].nextlist
            p[0].testlist = p[3].testlist

    def p_markerCompStatPush(self, p):
        '''
        markerCompStatPush :
        '''
        if self.isError :
            return
        self.ST.PushScope(self.TAC)

    def p_markerCompStatPop(self, p):
        '''
        markerCompStatPop :
        '''
        if self.isError :
            return
        # self.TAC.emit('add','esp', self.ST.offset - self.ST.offsetList[-1], '')
        # if self.ST.offset - self.ST.offsetList[-1] != 0:
            # self.TAC.emit('+_int', '%esp', '%esp', f'${self.ST.offset - self.ST.offsetList[-1]}')
        self.ST.PopScope(self.TAC)

    def p_block_item_list(self, p):
        '''
        block_item_list : block_item
                        | block_item_list globalmarker1 block_item
        '''
        if self.isError :
            return
        if (len(p) == 2):
            p[0] = Node(';',[p[1]])
            if p[1] is not None:
                p[0].numdef = p[1].numdef
            if p[1] != None and not self.ST.error:
                p[0].truelist = p[1].truelist
                p[0].falselist = p[1].falselist
                p[0].breaklist = p[1].breaklist
                p[0].continuelist = p[1].continuelist
                p[0].nextlist = p[1].nextlist
                p[0].testlist = p[1].testlist
        elif (len(p) == 4):
            p[0] = Node(';',[p[1],p[3]])
            if self.ST.error:
                return
            if p[1] is not None and p[3] is not None:
                p[0].numdef = p[1].numdef + p[3].numdef
            self.TAC.backpatch(p[1].nextlist,p[2].quad)
            if(p[3] != None):
                p[0].breaklist = p[1].breaklist + p[3].breaklist
                p[0].continuelist = p[1].continuelist + p[3].continuelist
                p[0].nextlist = p[3].nextlist
                p[0].testlist = p[1].testlist + p[3].testlist
            else:
                p[0].breaklist = p[1].breaklist
                p[0].continuelist = p[1].continuelist
                p[0].testlist = p[1].testlist

    def p_block_item(self, p):
        '''
        block_item : declaration
                    | statement
        '''
        if self.isError :
            return
        if (len(p) == 2):
            p[0] = p[1]

    def p_expression_statement(self, p):
        '''
        expression_statement : ';'
                            | expression ';'
        '''
        if self.isError :
            return
        if len(p) == 2:
            p[0] = Node('EmptyExprStmt')
        if (len(p) == 3):
            p[0] = p[1]

    def p_selection_statement(self, p):
        '''
        selection_statement : IF '(' expression ')' globalmarker1 statement globalN1
                            | IF '(' expression ')' globalmarker1 statement globalN1 ELSE globalmarker1 statement
                            | SWITCH '(' expression ')' markerSwitch statement
        '''
        if self.isError :
            return
        if(len(p) == 7):
            p[0] = Node(str(p[1]).upper(),[p[3],p[6]])
            if p[6].numdef > 1:
                self.ST.error = 1
                print(f'Cannot have multiple default labels in a single switch statement at line {p.lineno(1)}')


            if self.ST.error:
                return
            p[0].nextlist = p[6].breaklist + p[6].nextlist
            p[0].nextlist.append(self.TAC.nextstat)
            # To properly end switch case
            self.TAC.emit('goto', '', '', '')

            self.TAC.backpatch(p[5].nextlist, self.TAC.nextstat)

            # Appropriate jump for labels
            for item in p[6].testlist:
                if item[0] is not None:
                    for i in range(item[1], item[2]):
                        self.TAC.final_code.append(self.TAC.final_code[i])
                        self.TAC.nextstat += 1
                    temp = self.TAC.newtemp()

                    self.ST.InsertSymbol(temp, 0)
                    self.ST.ModifySymbol(temp, "type", ["int"])
                    self.ST.ModifySymbol(temp, "check", "TEMP")
                    self.updateSizeInSymTab(["int"], temp)
                    if self.ST.isGlobal(temp):
                        self.ST.ModifySymbol(temp, "varclass", "Global")
                    else :
                        self.ST.ModifySymbol(temp, "varclass", "Local")
                        found, entry = self.ST.ReturnSymTabEntry(temp)
                        var_size = found['sizeAllocInBytes']
                        if found["varclass"] == "Local":
                            # self.TAC.emit('-_int', '%esp', '%esp', f'${var_size}')
                            if found["offset"] >0:
                                self.ST.ModifySymbol(temp, 'temp', f'-{found["offset"] + found["sizeAllocInBytes"] }(%ebp)')
                            else:
                                self.ST.ModifySymbol(temp, 'temp', f'{-found["offset"] - found["sizeAllocInBytes"] }(%ebp)')
                        temp = found['temp']

                    self.TAC.emit('==', temp, p[3].temp, item[0])
                    tmplist = [self.TAC.nextstat]
                    self.TAC.emit('ifnz goto', '', temp, '')
                    self.TAC.backpatch(tmplist, item[1])
            
            # Appropriate jump for default label
            for item in p[6].testlist:
                if item[0] is None:
                    tmplist = [self.TAC.nextstat]
                    self.TAC.emit('goto', '', '', '')
                    self.TAC.backpatch(tmplist, item[1])

        elif (len(p) == 8):
            p[0] = Node('IF-ELSE',[p[3],p[6]])
            if self.ST.error:
                return
            self.TAC.backpatch(p[3].truelist,p[5].quad)
            p[0].nextlist = p[3].falselist + p[6].nextlist
            p[0].continuelist = p[6].continuelist
            p[0].breaklist = p[6].breaklist
            p[0].testlist = p[6].testlist
        else:
            p[0] = Node('IF-ELSE',[p[3],p[6],p[10]])
            if self.ST.error:
                return
            self.TAC.backpatch(p[3].truelist,p[5].quad)
            self.TAC.backpatch(p[3].falselist,p[9].quad)
            p[0].nextlist = p[6].nextlist + p[7].nextlist + p[10].nextlist
            p[0].continuelist = p[6].continuelist + p[10].continuelist
            p[0].breaklist = p[6].breaklist + p[10].breaklist
            p[0].testlist = p[6].testlist + p[10].testlist

    def p_markerSwitch(self, p):
        '''
        markerSwitch : 
        '''
        if self.isError :
            return

        p[0] = Node('',createAST=False)
        if self.ST.error:
            return
        p[0].nextlist.append(self.TAC.nextstat)
        self.TAC.emit('goto','','','')

    def p_globalN1(self, p):
        '''
        globalN1 : 
        '''
        if self.isError:
            return
        p[0] = Node('',createAST=False)
        if self.ST.error:
            return
        p[0].nextlist.append(self.TAC.nextstat)
        self.TAC.emit('goto','','','')

    def p_globalmarker1(self, p):
        '''
        globalmarker1 : 
        '''
        if self.isError:
            return
        p[0] = Node('',createAST=False)
        if self.ST.error:
            return
        p[0].quad = self.TAC.nextstat

    def p_iteration_statement_1(self, p):
        '''
        iteration_statement : WHILE globalmarker1 '(' expression ')' globalmarker1 statement
                            | DO globalmarker1 statement WHILE '(' globalmarker1 expression ')' ';'
                            | FOR '(' expression_statement globalmarker1 expression_statement ')' globalmarker1 statement
                            | FOR '(' expression_statement globalmarker1 expression_statement globalmarker1 expression ')' globalmarker1 statement
        '''
        # Grammar rules separated to differentiate between length 11 cases
        # In for loop we will have markers of the following form:
        # for ( E1; M1 E2; M2 E3){M3 Statement}
        # In the above grammar the third rule has only 2 expressions (no update statament)
        
        if self.isError :
            return
        if len(p) == 8:
            p[0] = Node('WHILE',[p[4],p[6]])
            if self.ST.error:
                return
            self.TAC.backpatch(p[7].nextlist, p[2].quad)
            self.TAC.backpatch(p[7].continuelist, p[2].quad)
            self.TAC.backpatch(p[4].truelist, p[6].quad)
            p[0].nextlist= p[4].falselist + p[7].breaklist
            self.TAC.emit('goto',int(p[2].quad) + 1,'','')
        elif len(p) == 9:
            p[0] = Node('FOR',[p[3],p[5],p[8]])
            # M1 is p[4]
            # S1 is p[8]
            # E1 is p[3]
            # E2 is p[5]
            # M2 is p[7]
            if self.ST.error:
                return
            self.TAC.backpatch(p[8].nextlist,p[4].quad)
            self.TAC.backpatch(p[8].continuelist,p[4].quad)
            self.TAC.backpatch(p[3].truelist,p[4].quad)
            self.TAC.backpatch(p[3].falselist,p[4].quad)
            self.TAC.backpatch(p[5].truelist,p[7].quad)
            p[0].nextlist = p[8].breaklist + p[5].falselist
            self.TAC.emit('goto',p[4].quad+1,'','')
        elif len(p) == 10:
            p[0] = Node('DO-WHILE',[p[3],p[7]])
            # Statement has continuelist, nextlist and breaklist
            # expression has truelist and falselist
            if self.ST.error:
                return
            self.TAC.backpatch(p[7].truelist,p[2].quad)
            p[0].nextlist = p[7].falselist + p[3].breaklist
            self.TAC.backpatch(p[3].nextlist,p[6].quad)
            self.TAC.backpatch(p[3].continuelist,p[6].quad)
            self.TAC.emit('goto',p[2].quad+1,'','')
        elif len(p) == 11:
            p[0] = Node('FOR',[p[3],p[5],p[7],p[10]])
            # M1 is p[4]
            # M2 is p[6]
            # M3 is p[9]
            # E1 is p[3]
            # E2 is p[5]
            # E3 is p[7]
            # S1 is p[10]
            if self.ST.error:
                return
            self.TAC.backpatch(p[3].truelist,p[4].quad)
            self.TAC.backpatch(p[3].falselist,p[4].quad)
            self.TAC.backpatch(p[10].nextlist,p[6].quad)
            self.TAC.backpatch(p[10].continuelist,p[6].quad)
            self.TAC.backpatch(p[5].truelist,p[9].quad)
            self.TAC.backpatch(p[7].truelist,p[4].quad)
            self.TAC.backpatch(p[7].falselist,p[4].quad)
            p[0].nextlist = p[10].breaklist + p[5].falselist
            self.TAC.emit('goto',p[6].quad+1,'','')

    def p_iteration_statement_2(self, p):
        '''
        iteration_statement : FOR '(' markerForPush declaration globalmarker1 expression_statement ')' globalmarker1 statement markerForPop
                            | FOR '(' markerForPush declaration globalmarker1 expression_statement globalmarker1 expression ')' globalmarker1 statement markerForPop
        '''
        # Grammar rule separated to differentiate between length 11 cases
        # In for loop we will have markers of the following form:
        # for ( E1; M1 E2; M2 E3){M3 Statement}
        # In the above grammar first rule has only 2 expressions (no update statament)
        
        if self.isError :
            return
        if len(p) == 11:
            p[0] = Node('FOR', [p[4], p[6], p[9]])
            # M1 is p[5]
            # M2 is p[8]
            # S1 is p[9]
            # E1 is p[6]
            if self.ST.error:
                return
            self.TAC.backpatch(p[6].truelist,p[8].quad)
            self.TAC.backpatch(p[9].continuelist,p[5].quad)
            self.TAC.backpatch(p[9].nextlist,p[5].quad)
            p[0].nextlist = p[9].breaklist + p[6].falselist
            self.TAC.emit('goto',p[5].quad+1,'','')
        else:
            p[0] = Node('FOR', [p[4], p[6], p[8], p[11]])
            # M1 is p[5]
            # M2 is p[7]
            # M3 is p[10]
            # S1 is p[11]
            # E1 is p[6]
            # E2 is p[8]
            if self.ST.error:
                return
            self.TAC.backpatch(p[11].nextlist,p[7].quad)
            self.TAC.backpatch(p[11].continuelist,p[7].quad)
            self.TAC.backpatch(p[6].truelist,p[10].quad)
            self.TAC.backpatch(p[8].truelist,p[5].quad)
            self.TAC.backpatch(p[8].falselist,p[5].quad)
            p[0].nextlist = p[11].breaklist + p[6].falselist
            self.TAC.emit('goto',p[7].quad+1,'','')

    # Markers for FOR loops
    def p_markerForPush(self, p):
        '''
        markerForPush :
        '''
        if self.isError :
            return
        self.ST.PushScope(self.TAC)

    def p_markerForPop(self, p):
        '''
        markerForPop :
        '''
        if self.isError :
            return
        # self.TAC.emit('add','esp', self.ST.offset - self.ST.offsetList[-1], '')
        if self.ST.error:
            return
#         if self.ST.offset - self.ST.offsetList[-1] != 0:
#             self.TAC.emit('+_int', '%esp', '%esp', f'${self.ST.offset - self.ST.offsetList[-1]}')
        self.ST.PopScope(self.TAC, 1)

    def p_jump_statement(self, p):
        '''
        jump_statement : CONTINUE ';'
                       | BREAK ';'
                       | RETURN ';'
                       | RETURN expression ';'
        '''
        if self.isError :
            return
        if (len(p) == 3):
            p[0] = Node(str(p[1]).upper())
            if self.ST.error:
                return
            if(p[1] == 'continue'):
                p[0].continuelist.append(self.TAC.nextstat)
                self.TAC.emit('goto','','','')
            elif(p[1] == 'break'):
                p[0].breaklist.append(self.TAC.nextstat)
                self.TAC.emit('goto','','','')
            elif p[1] == 'return':
                found = list(self.ST.Table[0])
                functype = self.ST.Table[0][found[-1]]['type']

                if functype != ['void']:
                    self.ST.error = 1
                    print(f'Need an argument to return of type {functype} at line {p.lineno(1)}')

                if self.ST.error:
                    return
                self.TAC.emit('retq','','','')
        else:
            if(p[1] == 'return'):
                p[0] = Node('RETURN')
                found = list(self.ST.Table[0])
                if not 'type' in self.ST.Table[0][found[-1]].keys():
                    self.ST.error = 1
                    return
                functype = self.ST.Table[0][found[-1]]['type']
                if functype is None:
                    functype = []

                if p[2] is None or p[2].type is None or p[2].type == []:
                    self.ST.error = 1
                    print(f'Cannot return expression at line {p.lineno(1)}')


                elif '*' in functype and len(p[2].type)>0 and '*' not in p[2].type[0] and p[2].type[0] not in iit :
                    self.ST.error = 1
                    print(f'Incompatible types while returning {p[2].type} where {functype} was expected at line {p.lineno(1)}')

                elif len(p[2].type)>0 and len(functype)>0 and functype[0] in aat and p[2].type[0] not in aat and p[2].type[0][-1] != '*':
                    self.ST.error = 1
                    print(f'Type mismatch while returning value at line {p.lineno(1)}')
                    
                elif functype == ['void'] and len(p[2].type)>0 and p[2].type[0] != 'void':
                    self.ST.error = 1
                    print(f'Cannot return non-void type at line {p.lineno(1)}')
                
                if self.ST.error:
                    return
                isarr = 0
                type_list = functype
                if type_list is None:
                    type_list = []
                for i in range(len(functype)):
                    if functype[i][0]=='[' and functype[i][-1] == ']':
                        isarr += 1

                if 'unsigned' in type_list or 'signed' in type_list:
                    if 'bool' not in type_list and 'char' not in type_list and 'short' not in type_list:
                        type_list.append('int')

                p[0].type = []
                if 'long' in type_list and 'int' in type_list:
                    p[0].type.append('long int')
                    for single_type in type_list:
                        if single_type != 'long' and single_type != 'int':
                            p[0].type.append(single_type)
                
                elif 'long' in type_list and 'double' in type_list:
                    p[0].type.append('long double')
                    for single_type in type_list:
                        if single_type != 'long' and single_type != 'double':
                            p[0].type.append(single_type)
                
                elif 'long' in type_list:
                    p[0].type.append('long int')
                    for single_type in type_list:
                        if single_type != 'long':
                            p[0].type.append(single_type)

                elif 'int' in type_list:
                    p[0].type.append('int')
                    for single_type in type_list:
                        if single_type != 'int':
                            p[0].type.append(single_type)

                elif 'short' in type_list:
                    p[0].type.append('short')
                    for single_type in type_list:
                        if single_type != 'short':
                            p[0].type.append(single_type)
                
                elif 'char' in type_list:
                    p[0].type.append('char')
                    for single_type in type_list:
                        if single_type != 'char':
                            p[0].type.append(single_type)
                
                elif 'bool' in type_list:
                    p[0].type.append('bool')
                    for single_type in type_list:
                        if single_type != 'bool':
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

                if 'struct' in type_list:
                    p[0].type.append('struct')
                    for single_type in type_list:
                        if single_type != 'struct':
                                p[0].type.append(single_type)     

                if 'union' in type_list:
                    p[0].type.append('union')
                    for single_type in type_list:
                        if single_type != 'union':
                                p[0].type.append(single_type)     

                if isarr > 0:
                    temp_type = []
                    temp_type.append(p[0].type[0])
                    for i in range(isarr):
                        temp_type[0] += ' *'

                    for i in range(len(p[0].type)):
                        if i>isarr:
                            temp_type.append(p[0].type[i])
                    p[0].type = temp_type
                    p[0].type.append('arr')
                    for i in range(len(type_list)):
                        if type_list[len(type_list)-i-1][0] == '[' and type_list[len(type_list)-i-1][-1] == ']':  
                            p[0].type.append(type_list[len(type_list)-i-1])

                if 'void' in type_list:
                    p[0].type.append('void')
                    for single_type in type_list:
                        if single_type != 'void':
                                p[0].type.append(single_type)     

                if '*' in type_list:
                    temp_type = []
                    temp_type.append(p[0].type[0])
                    for i in range(1, len(p[0].type)):
                            if p[0].type[i] == '*':
                                temp_type[0] += ' *'
                            else:
                                temp_type.append(p[0].type[i])
                    p[0].type = temp_type

                if ('struct' in p[0].type or 'union' in p[0].type) and p[0].type != p[2].type:
                    self.ST.error = 1
                    print(f'Need struct/union of type {p[0].type}, instead got return type {p[2].type} at line {p.lineno(1)}')
                    return

                isin = True
                for single_type in p[0].type:
                    if single_type not in p[2].type:
                        isin = False
                if isin == False:
                    p[2].totype = p[0].type
                    p2str = 'to'
                    for single_type in p[0].type:
                        p2str += '_' + single_type
                    p2str = p2str.replace(' ','_')
                    if '*' == p[0].type[0][-1]:
                        p2str = 'to_int_unsigned'
                    p2 = Node(p2str, [p[2]]) 
                else:
                    p2 = p[2]   

                p[0].onlyAddEdge([p2])       
                if self.ST.error:
                    return

                if p[2].totype is not None and p[2].totype != p[2].type:
                    p2.temp = self.TAC.newtemp()
                    self.ST.InsertSymbol(p2.temp, 0)
                    self.ST.ModifySymbol(p2.temp, "type", p[2].totype)
                    self.ST.ModifySymbol(p2.temp, "check", "TEMP")
                    self.updateSizeInSymTab(p[2].totype, p2.temp)
                    if self.ST.isGlobal(p2.temp):
                        self.ST.ModifySymbol(p2.temp, "varclass", "Global")
                    else :
                        self.ST.ModifySymbol(p2.temp, "varclass", "Local")
                        found, entry = self.ST.ReturnSymTabEntry(p2.temp)
                        var_size = found['sizeAllocInBytes']
                        if found["varclass"] == "Local":
                            # self.TAC.emit('-_int', '%esp', '%esp', f'${var_size}')
                            if found["offset"] >0:
                                self.ST.ModifySymbol(p2.temp, 'temp', f'-{found["offset"] + found["sizeAllocInBytes"] }(%ebp)')
                            else:
                                self.ST.ModifySymbol(p2.temp, 'temp', f'{-found["offset"] - found["sizeAllocInBytes"] }(%ebp)')
                        p2.temp = found['temp']  

                    fromtype = p[2].type
                    currtype = []
                    for single_type in fromtype:
                        if single_type != 'arr'  and single_type[0] != '[' and single_type[-1] != ']':
                            currtype.append(single_type)
                    cstr =',' +  ' '.join(currtype).replace(' ','_')

                    self.TAC.emit('cast',p2.temp,p[2].temp, ' '.join(p[2].totype).replace(' ','_') + cstr) 

                else:
                    p2.temp = p[2].temp
                if (('struct' in p[0].type) and ('*' not in p[0].type)):
                    temp_type = ' '.join(p[0].type)
                    self.TAC.emit('retq_struct', p[2].temp,sizes[temp_type],'')
                else:
                    self.TAC.emit('retq', p2.temp,'','')

    def p_start(self, p):
        '''
        start : pushObjectFuncs translation_unit
        '''
        if self.isError :
            return
        p[0] = self.AST_ROOT
        self.ST.StoreResults(self.TAC)
    
    def p_pushObjectFuncs(self, p):
        '''
        pushObjectFuncs :
        '''
        if self.isError:
            return
        # printf with 2 arguments (string, placeholder value)
        self.ST.InsertSymbol("printf", -1)
        self.ST.ModifySymbol("printf", "check", "FUNC")
        self.ST.ModifySymbol("printf", "type", ['void'])
        self.ST.ModifySymbol("printf", "PARAM_NUMS", 2)

        # scanf with 2 arguments (string, placeholder value)
        self.ST.InsertSymbol("scanf", -1)
        self.ST.ModifySymbol("scanf", "check", "FUNC")
        self.ST.ModifySymbol("scanf", "type", ['void'])
        self.ST.ModifySymbol("scanf", "PARAM_NUMS", 2)
        
        #abs with a single argument
        self.ST.InsertSymbol("abs", -1)
        self.ST.ModifySymbol("abs", "check", "FUNC")
        self.ST.ModifySymbol("abs", "type", ['int'])
        self.ST.ModifySymbol("abs", "PARAM_NUMS", 1)

        #sqrt with a single argument
        self.ST.InsertSymbol("sqrt", -1)
        self.ST.ModifySymbol("sqrt", "check", "FUNC")
        self.ST.ModifySymbol("sqrt", "type", ['float'])
        self.ST.ModifySymbol("sqrt", "PARAM_NUMS", 1)

        #ceil with a single argument
        self.ST.InsertSymbol("ceil", -1)
        self.ST.ModifySymbol("ceil", "check", "FUNC")
        self.ST.ModifySymbol("ceil", "type", ['float'])
        self.ST.ModifySymbol("ceil", "PARAM_NUMS", 1)

        #floor with a single argument
        self.ST.InsertSymbol("floor", -1)
        self.ST.ModifySymbol("floor", "check", "FUNC")
        self.ST.ModifySymbol("floor", "type", ['float'])
        self.ST.ModifySymbol("floor", "PARAM_NUMS", 1)

        #pow with  2 arguments
        self.ST.InsertSymbol("pow", -1)
        self.ST.ModifySymbol("pow", "check", "FUNC")
        self.ST.ModifySymbol("pow", "type", ['float'])
        self.ST.ModifySymbol("pow", "PARAM_NUMS", 2)

        #fabs with a single argument
        self.ST.InsertSymbol("fabs", -1)
        self.ST.ModifySymbol("fabs", "check", "FUNC")
        self.ST.ModifySymbol("fabs", "type", ['float'])
        self.ST.ModifySymbol("fabs", "PARAM_NUMS", 1)

        #log with a single argument
        self.ST.InsertSymbol("log", -1)
        self.ST.ModifySymbol("log", "check", "FUNC")
        self.ST.ModifySymbol("log", "type", ['float'])
        self.ST.ModifySymbol("log", "PARAM_NUMS", 1)

        #log10 with a single argument
        self.ST.InsertSymbol("log10", -1)
        self.ST.ModifySymbol("log10", "check", "FUNC")
        self.ST.ModifySymbol("log10", "type", ['float'])
        self.ST.ModifySymbol("log10", "PARAM_NUMS", 1)

        #fmod with  2 arguments
        self.ST.InsertSymbol("fmod", -1)
        self.ST.ModifySymbol("fmod", "check", "FUNC")
        self.ST.ModifySymbol("fmod", "type", ['float'])
        self.ST.ModifySymbol("fmod", "PARAM_NUMS", 2)

        #exp with a single argument
        self.ST.InsertSymbol("exp", -1)
        self.ST.ModifySymbol("exp", "check", "FUNC")
        self.ST.ModifySymbol("exp", "type", ['float'])
        self.ST.ModifySymbol("exp", "PARAM_NUMS", 1)

        # cos with a single argument
        self.ST.InsertSymbol("cos", -1)
        self.ST.ModifySymbol("cos", "check", "FUNC")
        self.ST.ModifySymbol("cos", "type", ['float'])
        self.ST.ModifySymbol("cos", "PARAM_NUMS", 1)

        # sin with a single argument
        self.ST.InsertSymbol("sin", -1)
        self.ST.ModifySymbol("sin", "check", "FUNC")
        self.ST.ModifySymbol("sin", "type", ['float'])
        self.ST.ModifySymbol("sin", "PARAM_NUMS", 1)

        # acos with a single argument
        self.ST.InsertSymbol("acos", -1)
        self.ST.ModifySymbol("acos", "check", "FUNC")
        self.ST.ModifySymbol("acos", "type", ['float'])
        self.ST.ModifySymbol("acos", "PARAM_NUMS", 1)

        # asin with a single argument
        self.ST.InsertSymbol("asin", -1)
        self.ST.ModifySymbol("asin", "check", "FUNC")
        self.ST.ModifySymbol("asin", "type", ['float'])
        self.ST.ModifySymbol("asin", "PARAM_NUMS", 1)

        # tan with a single argument
        self.ST.InsertSymbol("tan", -1)
        self.ST.ModifySymbol("tan", "check", "FUNC")
        self.ST.ModifySymbol("tan", "type", ['float'])
        self.ST.ModifySymbol("tan", "PARAM_NUMS", 1)
        
        # atan with a single argument
        self.ST.InsertSymbol("atan", -1)
        self.ST.ModifySymbol("atan", "check", "FUNC")
        self.ST.ModifySymbol("atan", "type", ['float'])
        self.ST.ModifySymbol("atan", "PARAM_NUMS", 1)

        # malloc with 1 arguments
        self.ST.InsertSymbol("malloc", -1)
        self.ST.ModifySymbol("malloc", "check", "FUNC")
        self.ST.ModifySymbol("malloc", "type", ['void', '*'])
        self.ST.ModifySymbol("malloc", "PARAM_NUMS", 1)

        # free with 1 arguments
        self.ST.InsertSymbol("free", -1)
        self.ST.ModifySymbol("free", "check", "FUNC")
        self.ST.ModifySymbol("free", "type", ['void'])
        self.ST.ModifySymbol("free", "PARAM_NUMS", 1)

        # strlen with a single argument
        self.ST.InsertSymbol("strlen", -1)
        self.ST.ModifySymbol("strlen", "check", "FUNC")
        self.ST.ModifySymbol("strlen", "type", ['int'])
        self.ST.ModifySymbol("strlen", "PARAM_NUMS", 1)
        
        #strlwr with a single argument
        self.ST.InsertSymbol("strlwr", -1)
        self.ST.ModifySymbol("strlwr", "check", "FUNC")
        self.ST.ModifySymbol("strlwr", "type", ['char', '*'])
        self.ST.ModifySymbol("strlwr", "PARAM_NUMS", 1)

        #strupr with a single argument
        self.ST.InsertSymbol("strupr", -1)
        self.ST.ModifySymbol("strupr", "check", "FUNC")
        self.ST.ModifySymbol("strupr", "type", ['char', '*'])
        self.ST.ModifySymbol("strupr", "PARAM_NUMS", 1)

        #strcpy with a single argument
        self.ST.InsertSymbol("strcpy", -1)
        self.ST.ModifySymbol("strcpy", "check", "FUNC")
        self.ST.ModifySymbol("strcpy", "type", ['void'])
        self.ST.ModifySymbol("strcpy", "PARAM_NUMS", 2)

        #strcat with a single argument
        self.ST.InsertSymbol("strcat", -1)
        self.ST.ModifySymbol("strcat", "check", "FUNC")
        self.ST.ModifySymbol("strcat", "type", ['char', '*'])
        self.ST.ModifySymbol("strcat", "PARAM_NUMS", 2)

        #strcmp with a single argument
        self.ST.InsertSymbol("strcmp", -1)
        self.ST.ModifySymbol("strcmp", "check", "FUNC")
        self.ST.ModifySymbol("strcmp", "type", ['int'])
        self.ST.ModifySymbol("strcmp", "PARAM_NUMS", 2)

    def p_translation_unit(self, p):
        '''
        translation_unit : external_declaration
                        | translation_unit external_declaration
        '''
        if self.isError :
            return
        p[0] = self.AST_ROOT

        if (len(p) == 2):
            if ((p[1] is not None) and (p[1].node is not None)):
                p[0].onlyAddEdge([p[1]])
        elif (len(p) == 3):
            if ((p[2] is not None) and (p[2].node is not None)):
                p[0].onlyAddEdge([p[2]])

    def p_external_declaration(self, p):
        '''
        external_declaration : function_definition
                            | declaration
        '''
        if self.isError :
            return
        p[0] = p[1]

    def p_function_definition(self, p):
        '''
        function_definition : declaration_specifiers function_declarator '{' markerFunc2 '}' markerFuncPop
                            | declaration_specifiers function_declarator '{' markerFunc2 block_item_list '}' markerFuncPop
        '''
        if self.isError :
            return
        line = 0
        if (len(p) == 7):
            p[0] = Node('FUNC',[p[2]])
            line = 3

        elif (len(p) == 8):
            if p[3] == '{':
                p[0] = Node('FUNC',[p[2],Node('SCOPE', [p[5]])])
                line = 3
                # Added here to provide goto to the last statement in the function
                if not self.ST.error:
                    self.TAC.backpatch(p[5].nextlist,p[7].quad)
                    self.TAC.backpatch(p[5].breaklist,p[7].quad)
            else:
                p[0] = Node('FUNC',[p[2],p[3]])
                line = 4

        p[1].removeGraph()

        temp_type_list = []
        if p[1].type is None:
            p[1].type = []
        
        for single_type in p[1].type:
            if single_type != '*':
                temp_type_list.append(single_type)

        if len(temp_type_list) != len(set(temp_type_list)):
            self.ST.error = 1
            print('Function type cannot have duplicating type of declarations at line', p.lineno(line))
            return

        if 'long' in p[1].type and 'short' in p[1].type:
            self.ST.error = 1
            print('Function type cannot be both long and short at line', p.lineno(line))
        elif 'unsigned' in p[1].type and 'signed' in p[1].type:
            self.ST.error = 1
            print('Function type cannot be both signed and unsigned at line', p.lineno(line))
        else:
            data_type_count = 0
            if 'int' in p[1].type or 'short' in p[1].type  or 'unsigned' in p[1].type or 'signed' in p[1].type:
                data_type_count += 1
            if 'char' in  p[1].type:
                data_type_count += 1
            if 'bool' in  p[1].type:
                data_type_count += 1
            if 'float' in p[1].type:
                data_type_count += 1
            if 'double' in p[1].type:
                data_type_count += 1
            if 'void' in p[1].type:
                data_type_count += 1
            if 'struct' in p[1].type:
                data_type_count += 1
            if 'union' in p[1].type:
                data_type_count += 1
            if data_type_count > 1:    
                self.ST.error = 1
                print('Two or more conflicting data types specified for function at line', p.lineno(line)) 

            if 'long' in p[1].type:
                if 'char' in p[1].type or 'bool' in  p[1].type or 'float' in  p[1].type or 'void' in  p[1].type:
                    self.ST.error = 1
                    print('Two or more conflicting data types specified for function at line', p.lineno(line))

        if self.ST.error:
            return

        for token in p[2].variables:
            temp_type = p[2].variables[token]
            if 'Function Name' not in temp_type:
                temp_type_arr = []
                for single_type in temp_type:
                    if single_type[0] == '[' and single_type[-1]==']':
                        temp_type_arr.append(single_type[1:-1])
                for i in range(len(temp_type_arr)):
                    if temp_type_arr[i] == '' and i != 0:
                        self.ST.error = 1
                        print('Multidimensional array must have bound for all dimensions except first at line ', p.lineno(line))
                    if temp_type_arr[i] != '' and int(temp_type_arr[i]) <= 0:
                        self.ST.error = 1
                        print('Array bound cannot be non-positive at line ', p.lineno(line))
        
        if self.ST.error:
            return

        function_name = str()
        for key in p[2].variables.keys():
            if(p[2].variables[key][0] == "Function Name"):
                function_name = key
                break
        if self.ST.error:
            return
        function, entry = self.ST.ReturnSymTabEntry(function_name)
        function = function['type']
        if 'void' in function and len(function) == 1:
            self.TAC.emit('retq','','','')
        else:
            self.TAC.emit('retq','$0','','')
        self.TAC.emit('','','','')

        for var in entry['#scope'][0]:
            if var == '#StructOrUnion':
                continue
            if var == '#scope':
                continue
            if var == '#scopeNum':
                continue
            if var[0] == '!':
                continue
            param = entry['#scope'][0][var]

            temp_type_list = []
            temp2_type_list = []
            nums_arr = []
            ctrpar = -1
            for single_type in param['type']:
                ctrpar += 1
                if single_type != '*':
                    temp_type_list.append(single_type)
                    if single_type[0] != '[' or single_type[-1] != ']':
                        temp2_type_list.append(single_type)

                if single_type[0] == '[' and single_type[-1] == ']':
                    if single_type[1:-1] == '' and ctrpar != 0:
                        self.ST.error = 1
                        print('Cannot have empty indices for array declarations at line', param['line'])
                        return
                    elif single_type[1:-1] != '' and int(single_type[1:-1]) <= 0:
                        self.ST.error = 1
                        print('Cannot have non-positive integers for array declarations at line', param['line'])
                        return

            if len(temp2_type_list) != len(set(temp2_type_list)):
                self.ST.error = 1
                print('variables cannot have duplicating type of declarations at line', param['line'])
                return

            if 'long' in param['type'] and 'short' in param['type']:
                self.ST.error = 1
                print('variable cannot be both long and short at line', param['line'])
                return
            elif 'unsigned' in param['type'] and 'signed' in param['type']:
                self.ST.error = 1
                print('variable cannot be both signed and unsigned at line', param['line'])
                return
            elif 'void' in param['type'] and '*' not in param['type']:
                self.ST.error = 1
                print('Cannot have a void type variable at line ', param['line'])
                return
            else:
                data_type_count = 0
                if 'int' in param['type'] or 'short' in param['type']  or 'unsigned' in param['type'] or 'signed' in param['type']:
                    data_type_count += 1
                if 'char' in param['type']:
                    data_type_count += 1
                if 'bool' in  param['type']:
                    data_type_count += 1
                if 'float' in param['type']:
                    data_type_count += 1
                if 'double' in param['type']:
                    data_type_count += 1
                if 'void' in param['type']:
                    data_type_count += 1
                if 'struct' in param['type']:
                    data_type_count += 1
                if 'union' in param['type']:
                    data_type_count += 1
                if data_type_count > 1:    
                    self.ST.error = 1
                    print('Two or more conflicting data types specified for variable at line', param['line']) 
                    return
                if 'long' in param['type']:
                    if 'char' in param['type'] or 'bool' in  param['type'] or 'float' in  param['type'] or 'void' in  param['type']:
                        self.ST.error = 1
                        print('Two or more conflicting data types specified for variable at line', param['line'])
                        return
                        
    def p_markerFunc2(self, p):
        '''
        markerFunc2 : 
        '''
        if self.isError :
            return
        p[0] = Node('',createAST=False)
        p[0].variables = p[-2].variables
        function_name = str()
        for key in p[0].variables.keys():
            if(p[0].variables[key][0] == "Function Name"):
                function_name = key
                break
        p[0].variables[key] += p[-3].extraValues + p[-2].extraValues

        self.ST.ModifySymbol(function_name, 'check', "FUNC", p.lineno(0)) # says that this entry is a function
        param_nums = 0 
        for var_name in p[0].variables.keys():
            if not var_name == function_name:
                if p[0].variables[var_name] and p[0].variables[var_name][-1] in ['struct', 'union']:
                    found = self.ST.TT.ReturnTypeTabEntry(p[0].variables[var_name][-2], p[0].variables[var_name][-1], p.lineno(0))
                    if found:
                        self.ST.ModifySymbol(var_name, "vars", found['vars'], p.lineno(0))
                        self.ST.ModifySymbol(var_name, "check", found['check'], p.lineno(0))
                        self.ST.ModifySymbol(var_name, "type", p[0].variables[var_name],p.lineno(0))
                else:
                    self.ST.ModifySymbol(var_name, "type", p[0].variables[var_name],p.lineno(0))
                self.ST.ModifySymbol(var_name, "check", "PARAM", p.lineno(0))
                param_nums += 1

                #updating variable class
                if p[0].variables[var_name]:
                    isGlobal = self.ST.isGlobal(var_name)
                    isStatic = False
                    if 'static' in p[0].variables[var_name]:
                        isStatic = True
                    if isGlobal & isStatic:
                        self.ST.ModifySymbol(var_name, "varclass", "Global Static", p.lineno(0))
                    elif isGlobal:
                        self.ST.ModifySymbol(var_name, "varclass", "Global", p.lineno(0))
                    elif isStatic:
                        self.ST.ModifySymbol(var_name, "varclass", "Local Static", p.lineno(0))
                    else:
                        self.ST.ModifySymbol(var_name, "varclass", "Local", p.lineno(0))

                # updating sizes
                if p[0].variables[var_name]:
                    #handling arrays
                    multiplier = 1
                    # for type_name in p[0].variables[var_name]:
                    #     if type_name[0]=='[' and type_name[-1]==']':
                    #         if type_name[1:-1] != '':
                    #             multiplier *= int(type_name[1:-1])
                    #     else:
                    #         break

                    if '*' in p[0].variables[var_name]:
                        self.ST.ModifySymbol(var_name, "sizeAllocInBytes", multiplier*sizes["PTR"], p.lineno(0))
                    elif 'struct' in p[0].variables[var_name] :
                        struct_size = 0
                        found = self.ST.TT.ReturnTypeTabEntry(p[0].variables[var_name][-2], p[0].variables[var_name][-1], p.lineno(0))
                        if found:
                            struct_size = found['sizeAllocInBytes']
                        self.ST.ModifySymbol(var_name, "sizeAllocInBytes", multiplier*struct_size, p.lineno(0))
                    elif 'union' in p[0].variables[var_name]:
                        struct_size = 0
                        found = self.ST.TT.ReturnTypeTabEntry(p[0].variables[var_name][-2], p[0].variables[var_name][-1], p.lineno(0))
                        if found:
                            struct_size = found['sizeAllocInBytes']
                            for var in found['vars']:
                                found['vars'][var]['offset'] = 0
                        self.ST.ModifySymbol(var_name, "sizeAllocInBytes", multiplier*struct_size, p.lineno(0))
                    elif 'long' in p[0].variables[var_name]:
                        if 'int' in p[0].variables[var_name]:
                            self.ST.ModifySymbol(var_name, "sizeAllocInBytes", multiplier*sizes["long int"], p.lineno(0))
                        elif 'double' in p[0].variables[var_name]:
                            self.ST.ModifySymbol(var_name, "sizeAllocInBytes", multiplier*sizes["long double"], p.lineno(0))
                        else:
                            self.ST.ModifySymbol(var_name, "sizeAllocInBytes", multiplier*sizes["long"], p.lineno(0))
                    elif 'float' in p[0].variables[var_name]:
                        self.ST.ModifySymbol(var_name, "sizeAllocInBytes", multiplier*sizes["float"], p.lineno(0))
                    elif 'double' in p[0].variables[var_name]:
                        self.ST.ModifySymbol(var_name, "sizeAllocInBytes", multiplier*sizes["double"], p.lineno(0))
                    elif 'short' in p[0].variables[var_name]:
                        self.ST.ModifySymbol(var_name, "sizeAllocInBytes", multiplier*sizes["short"], p.lineno(0))
                    elif 'int' in p[0].variables[var_name]:
                        self.ST.ModifySymbol(var_name, "sizeAllocInBytes", multiplier*sizes["int"], p.lineno(0))
                    elif 'char' in p[0].variables[var_name]:
                        self.ST.ModifySymbol(var_name, "sizeAllocInBytes", multiplier*sizes["char"], p.lineno(0))
                    elif 'bool' in p[0].variables[var_name]:
                        self.ST.ModifySymbol(var_name, "sizeAllocInBytes", multiplier*sizes["bool"], p.lineno(0))
                    else:
                        self.ST.ModifySymbol(var_name, "sizeAllocInBytes", multiplier*sizes["void"], p.lineno(0))
            else:
                self.ST.ModifySymbol(var_name, "type", p[0].variables[key][1:])

        found,entry = self.ST.ReturnSymTabEntry(function_name)
        self.ST.offset = 8
        if (("struct" in found["type"]) and ("*" not in found["type"])):
            self.ST.offset = 12
        for var_name in p[0].variables.keys():
            if not var_name == function_name:
                if 'struct' in p[0].variables[var_name] and '*' not in p[0].variables[var_name]:
                    found, entry = self.ST.ReturnSymTabEntry(var_name)
                    if found:
                        for var in found['vars']:
                            found['vars'][var]['temp'] = f'{found["vars"][var]["offset"] + self.ST.offset}(%ebp)'
                elif 'union' in p[0].variables[var_name] and '*' not in p[0].variables[var_name]:
                    found, entry = self.ST.ReturnSymTabEntry(var_name)
                    if found:
                        for var in found['vars']:
                            found['vars'][var]['temp'] = f'{self.ST.offset}(%ebp)'
                found, entry = self.ST.ReturnSymTabEntry(var_name)
                alignedSz = self.ST.TopScope[var_name]["sizeAllocInBytes"]
                self.ST.offset += alignedSz
                self.ST.ModifySymbol(var_name, "offset", -(self.ST.offset), p.lineno(0))
                if found["offset"] >0:
                    self.ST.ModifySymbol(var_name, 'temp', f'-{found["offset"] + alignedSz}(%ebp)')
                else:
                    self.ST.ModifySymbol(var_name, 'temp', f'{-found["offset"] - alignedSz}(%ebp)')

        self.ST.ModifySymbol(function_name, 'PARAM_NUMS', param_nums)
        self.ST.offset = 0
        if self.ST.error:
            return

        self.TAC.emit(str(function_name) + ":",'','','')

    def p_markerFuncPop(self, p):
        '''
        markerFuncPop :
        '''
        if self.isError :
            return
        self.ST.PopScope(self.TAC)
        p[0] = Node('',createAST = False)
        if self.ST.error:
            return
        p[0].quad = self.TAC.nextstat

    def p_error(self, p):
        if p is not None:
            print(f'Error found while parsing in line {p.lineno}!')
        else:
            print("Given file is empty")
        self.isError = 1
    
    def printTree(self):
        self.AST_ROOT.print_val()

# region Driver Code
####################### DRIVER CODE #############################################
if len(sys.argv) == 1:
    print('No file given as input')
    sys.exit(1)
file = open(sys.argv[1], 'r')
data = file.read()

def error_func():
    pass

def type_lookup_func():
    pass

# Lexer driver code
clex = CLexer( error_func=error_func, type_lookup_func=type_lookup_func)
clex.build()
clex.lexer.input(data)
tokens = clex.tokens
clex.lexer.lineno = 1

# driver code for parser

G = pgv.AGraph(strict=False, directed=True)
G.layout(prog='circo')
itr = 0 # Global var to give unique IDs to nodes of the graph
parser = CParser()
parser.build()
result = parser.parser.parse(data, lexer=clex.lexer)

fileNameCore = str(sys.argv[1]).split('/')[-1].split('.')[0]
outputFile = 'dot/' + fileNameCore + '.dot'

if parser.isError == 1:
    print(f'Error found. Aborting parsing of {sys.argv[1]}....')
    sys.exit(1)
elif parser.ST.error:
    print(f'Error in semantic analysis.')
    sys.exit(1) 
else:
    outputFileSymbolTable = open('ST/' + fileNameCore + '.txt',"w")
    print('Output AST file is: AST/' + fileNameCore + '.ps')
    print('Output Symbol Table file is: ST/' + fileNameCore + '.txt')
    print('Output TAC code is : TAC/' + fileNameCore + '.txt')
    parser.AST_ROOT.print_val()
    G.write(outputFile)
    orig_stdout = sys.stdout
    sys.stdout = outputFileSymbolTable
    parser.ST.PrintTable()
    sys.stdout = orig_stdout
    parser.TAC.add_strings()
    parser.TAC.clean_code()
    parser.TAC.print_code(fileNameCore) # remove later
    # parser.printTree()

# endregion
