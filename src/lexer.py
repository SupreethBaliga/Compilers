#------------------------------------------
# lexer.py
#
# scans the input and produces tokens
#
#--------------------------------------------
import ply.lex as lex
from ply.lex import TOKEN
import sys
import os
from tabulate import tabulate

# add reserved keywords to this list. Expand as needed
reserved_keywords = {
    'auto'      : 'AUTO',
    'bool'      : 'BOOL',
    'break'     : 'BREAK',
    'case'      : 'CASE',
    'char'      : 'CHAR',
    'const'     : 'CONST',
    'continue'  : 'CONTINUE',
    'default'   : 'DEFAULT',
    'do'        : 'DO',
    'double'    : 'DOUBLE',
    'else'      : 'ELSE',
    'enum'      : 'ENUM',
    'extern'    : 'EXTERN',
    'float'     : 'FLOAT',
    'for'       : 'FOR',
    'goto'      : 'GOTO',
    'if'        : 'IF',
    'int'       : 'INT',
    'long'      : 'LONG',
    'register'  : 'REGISTER',
    'return'    : 'RETURN',
    'short'     : 'SHORT',
    'signed'    : 'SIGNED',
    'sizeof'    : 'SIZEOF',
    'static'    : 'STATIC',
    'struct'    : 'STRUCT',
    'switch'    : 'SWITCH',
    'typedef'   : 'TYPEDEF',
    'union'     : 'UNION',
    'unsigned'  : 'UNSIGNED',
    'void'      : 'VOID',
    'volatile'  : 'VOLATILE',
    'while'     : 'WHILE'
}

tokens = list(reserved_keywords.values()) + [
    'ID',            # identifier
    #'WS',           # denotes whitespace // may have to modify this 
                     # to keep newline, space and tab separate to keep track of col no.
    'HEXA_CONSTANT',
    'OCTAL_CONSTANT',
    'CHAR_CONSTANT',
    'FLOAT_CONSTANT',
    'INT_CONSTANT',
    'STRING_LITERAL',
    'CONSTANT',
    'ERROR',         #to denote any kind of scanning error
    
    # Operators
    'ELLIPSIS',      # "..."
    'RIGHT_ASSIGN',  # ">>="
    'LEFT_ASSIGN',   # "<<="
    'ADD_ASSIGN',    # "+="
    'SUB_ASSIGN',    # "-="
    'MUL_ASSIGN',    # "*="
    'DIV_ASSIGN',    # "/="
    'MOD_ASSIGN',    # "%="
    'AND_ASSIGN',    # "&="
    'XOR_ASSIGN',    # "^="
    'OR_ASSIGN',     # "|="
    'RIGHT_OP',      # ">>"
    'LEFT_OP',       # "<<"
    'INC_OP',        # "++"
    'DEC_OP',        # "--"
    'PTR_OP',        # "->"
    'AND_OP',        # "&&"
    'OR_OP',         # "||"
    'LE_OP',         # "<="
    'GE_OP',         # ">="
    'EQ_OP',         # "=="
    'NE_OP'          # "!="
]

# Regular expression rules for simple tokens

# t_ELLIPSIS      = r'\.\.\.'
t_RIGHT_ASSIGN  = r'>>='
t_LEFT_ASSIGN   = r'<<='
t_ADD_ASSIGN    = r'\+='
t_SUB_ASSIGN    = r'\-='
t_MUL_ASSIGN    = r'\*='
t_DIV_ASSIGN    = r'/='
t_MOD_ASSIGN    = r'%='
t_AND_ASSIGN    = r'&='
t_XOR_ASSIGN    = r'\^='
t_OR_ASSIGN     = r'\|='
t_RIGHT_OP      = r'>>'
t_LEFT_OP       = r'<<'
t_INC_OP        = r'\+\+'
t_DEC_OP        = r'\-\-'
t_PTR_OP        = r'\->'
t_AND_OP        = r'&&'
t_OR_OP         = r'\|\|'
t_LE_OP         = r'<='
t_GE_OP         = r'>='
t_EQ_OP         = r'=='
t_NE_OP         = r'!='

literals = [';','{','}',',',':','=','(',')','[',']','.','&','!','~','-','+','*','/','%','<','>','^','|','?']

digit = r'([0-9])'
letter = r'([a-zA-Z_])'
hexa = r'([a-fA-F0-9])'
exponent = r'([Ee][+-]?' + digit + r'+)'

# Regular expression rules for complex tokens

# Character Constants 
char_const = r'(\'(\\.|[^\\\'])+\')'
@TOKEN(char_const)
def t_CHAR_CONSTANT(t):
    t.type = 'CONSTANT'
    return t

# Floating constants
exponent_const = r'(' + digit + r'+' + exponent + r')'
dec_constant = r'(' + digit + r'*[.]' + digit + r'+' + exponent + r'?)'
float_constant = r'(' + exponent_const + r'|' + dec_constant + r')'
@TOKEN(float_constant)
def t_FLOAT_CONSTANT(t):
    t.value = float(t.value) # converting the lexeme to float value
    t.type = 'CONSTANT'
    return t

# Hexadecimal Constants
hexa_const = r'(0[xX]' + hexa + '+' + r')'
@TOKEN(hexa_const)
def t_HEXA_CONSTANT(t):
    t.value = int(t.value, 16) # converting the lexeme to integer value
    t.type = 'CONSTANT'
    return t

# Octal Constants
octal_const = r'(0' + digit + '+' + r')'
@TOKEN(octal_const)
def t_OCTAL_CONSTANT(t):
    t.value = int(t.value, 8) # converting the lexeme to integer value
    t.type = 'CONSTANT'
    return t

# Decimal Constants
integer_const = r'(' + digit + '+' + r')'
@TOKEN(integer_const)
def t_INT_CONSTANT(t):
    t.value = int(t.value) # converting the lexeme to integer value
    t.type = 'CONSTANT'
    return t

# String Literals
string_literal = r'(\"(\\.|[^\\"])*\")'
@TOKEN(string_literal)
def t_STRING_LITERAL(t):
    return t

def t_INLINE_COMMENT(t):
    r'//.*'
    pass                #Ignore

def t_BLOCK_COMMENT(t):
    r'/\*(.|\n)*?\*/'
    t.lexer.lineno += t.value.count('\n')
    pass                #Ignore

# Identifiers
def t_ID(t):
    r'[a-zA-Z_][a-zA-Z_0-9]*'
    t.type = reserved_keywords.get(t.value,'ID')
    return t

# Track the line numbers
def t_newline(t):
    r'\n+'
    t.lexer.lineno += len(t.value)
    pass

# A string containing ignored characters (spaces and tabs)
t_ignore  = ' \t'

# Error handling: Ignore bad characters, as in ANSI specification
def t_error(t):
    print(f'Error found while scanning line number {t.lexer.lineno}')
    global isError
    isError = 1
    t.lexer.skip(1)
    

###############################################################
# END OF TOKENIZING RULES

isError = 0
# DRIVER CODE
if len(sys.argv) == 1:
    print("No file given as input")    
    sys.exit()

file = open(sys.argv[1],'r')
data = file.read()

lexers = lex.lex()
lexers.input(data)

def find_column(input, token):
    line_start = input.rfind('\n', 0, token.lexpos) + 1
    return (token.lexpos - line_start) + 1

table_list = []
for tok in lexers:
    row = [tok.type, tok.value, tok.lineno, find_column(data,tok)]
    table_list.append(row)

toPrint = os.environ['lex_env']

if isError == 1:
    print(f'Errors found. Aborting scanning of {sys.argv[1]}....')
    sys.exit(1)
else:
    if(toPrint != "0"):
        print(tabulate(table_list, headers=['Token', 'Lexeme', 'Line#', 'Column#']))
    # else:
    #     print("Not printing lexer table")
