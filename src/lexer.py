#------------------------------------------
# lexer.py
#
# scans the input and produces tokens
#
#--------------------------------------------
import ply.lex as lex

# add reserved keywords to this list. Expand as needed
reserved_keywords = {
    'if' : 'IF',
    'then' : 'THEN',
    'else' : 'ELSE',
    'for' : 'FOR',
    'while' : 'WHILE',
    'do' : 'DO',
    'int' : 'INT'.
    'bool' : 'BOOL',
    'char' : 'CHAR',
    'struct' : 'STRUCT',
    'printf' : 'PRINTF',
    'scanf' : 'SCANF'
}

tokens = list(reserved_keywords.values()) + [
    'ID',       #identifier
    'PLUS',     #denotes + sign
    'MINUS',    #denotes -
    'MULT',     #denotes *
    'DIVIDE',   #denotes /
    'LPAREN',   #denotes (
    'RPAREN',   #denotes )
    'WS',       #denotes whitespace // may have to modify this to keep newline, space and tab separate to keep track of col no.
]

def t_ID(t):
    r'[a-zA-Z_][a-zA-Z_0-9]*'
    t.type = reserved_keywords.get(t.value,'ID')

# Track the line numbers
def t_newline(t):
    r'\n+'
    t.lexer.lineno += len(t.value)

lexer = lex.lex()