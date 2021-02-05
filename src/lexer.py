#------------------------------------------
# lexer.py
#
# scans the input and produces tokens
#
#--------------------------------------------
import ply.lex as lex
import sys

# add reserved keywords to this list. Expand as needed
reserved_keywords = {
    #basic keywords
    'if' : 'IF',
    'then' : 'THEN',
    'else' : 'ELSE',
    'for' : 'FOR',
    'while' : 'WHILE',
    'do' : 'DO',
    'return' : 'RETURN',
    'include' : 'INCLUDE',

    #advanced keywords
    'switch': 'SWITCH',
    'case': 'CASE',
    'default' : 'DEFAULT',
    'break' : 'BREAK',
    'continue' : 'CONTINUE',
    'static' : 'STATIC',
    'auto' : 'AUTO',
    'enum' : 'ENUM',
    'extern' : 'EXTERN',
    'goto' : 'GOTO',

    #data types
    'int' : 'INT',
    'bool' : 'BOOL',
    'char' : 'CHAR',
    'void' : 'VOID',
    'struct' : 'STRUCT',
    'double' : 'DOUBLE',
    'float' : 'FLOAT',
    'const' : 'CONST',
    'long' : 'LONG'
}

tokens = list(reserved_keywords.values()) + [
    'ID',       # identifier
    # 'WS',     # denotes whitespace // may have to modify this 
                # to keep newline, space and tab separate to keep track of col no.
    'NUMBER',
    
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
    'NE_OP'         # "!="
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

# Integers
def t_NUMBER(t):
    r'\d+'
    t.value = int(t.value)    
    return t

# Identifiers
def t_ID(t):
    r'[a-zA-Z_][a-zA-Z_0-9]*'
    t.type = reserved_keywords.get(t.value,'ID')
    return t

# Track the line numbers
def t_newline(t):
    r'\n+'
    t.lexer.lineno += len(t.value)
    # return t

# A string containing ignored characters (spaces and tabs)
t_ignore  = ' \t'


###############################################################
# END OF TOKENS



if len(sys.argv) == 1:
    print("No file given as input")    
    exit()

file = open(sys.argv[1],'r')
data = file.read()

lexer = lex.lex()
lexer.input(data)

for tok in lexer:
    print(tok)