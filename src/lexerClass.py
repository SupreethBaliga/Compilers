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

class CLexer(object):
    """ A lexer for the C language. After building it, set the
        input text with input(), and call token() to get new
        tokens.

        The public attribute filename can be set to an initial
        filename, but the lexer will update it upon #line
        directives.
    """
    def __init__(self, error_func, type_lookup_func):
        self.error_func = error_func
        self.type_lookup_func = type_lookup_func

        # Keeps track of the last token returned from self.token()
        self.last_token = None

    def build(self, **kwargs):
        """ Builds the lexer from the specification. Must be
            called after the lexer object is created.

            This method exists separately, because the PLY
            manual warns against calling lex.lex inside
            __init__
        """
        self.lexer = lex.lex(object=self, **kwargs)

    def _error(self, msg, token):
        # location = self._make_tok_location(token)
        row = token.lineno
        line_start = self.lexer.lexdata.rfind('\n', 0, token.lexpos) + 1
        col = (token.lexpos - line_start) + 1
        self.error_func(msg, row, col)
        self.lexer.skip(1)


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
        'union'     : 'UNION',
        'unsigned'  : 'UNSIGNED',
        'void'      : 'VOID',
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
        # 'CONSTANT',
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

    # @TOKEN(r'\{')
    # def t_LBRACE(self, t):
    #     self.on_lbrace_func()
    #     t.type = '{'
    #     return t

    # @TOKEN(r'\}')
    # def t_RBRACE(self, t):
    #     self.on_rbrace_func()
    #     t.type = '}'
    #     return t

    # Character Constants 
    char_const = r'(\'(\\.|[^\\\'])+\')'
    @TOKEN(char_const)
    def t_CHAR_CONSTANT(self, t):
        t.type = 'CHAR_CONSTANT'
        return t

    # Floating constants
    exponent_const = r'(' + digit + r'+' + exponent + r')'
    dec_constant = r'(' + digit + r'*[.]' + digit + r'+' + exponent + r'?)'
    float_constant = r'(' + exponent_const + r'|' + dec_constant + r')'
    @TOKEN(float_constant)
    def t_FLOAT_CONSTANT(self, t):
        t.value = float(t.value) # converting the lexeme to float value
        t.type = 'FLOAT_CONSTANT'
        return t

    # Hexadecimal Constants
    hexa_const = r'(0[xX]' + hexa + '+' + r')'
    @TOKEN(hexa_const)
    def t_HEXA_CONSTANT(self, t):
        t.value = int(t.value, 16) # converting the lexeme to integer value
        t.type = 'INT_CONSTANT'
        return t

    # Octal Constants
    octal_const = r'(0' + r'[0-7]' + '+' + r')'
    wrong_octal_const = r'(0' + r'[0-7]' + '*' + r'[89]' + r')'

    @TOKEN(wrong_octal_const)
    def t_BAD_CONST_OCT(self, t):
        msg = "Invalid octal constant"
        self._error(msg, t)

    @TOKEN(octal_const)
    def t_OCTAL_CONSTANT(self, t):
        t.value = int(t.value, 8) # converting the lexeme to integer value
        t.type = 'INT_CONSTANT'
        return t

    # Decimal Constants
    integer_const = r'(' + digit + '+' + r')'
    @TOKEN(integer_const)
    def t_INT_CONSTANT(self, t):
        t.value = int(t.value) # converting the lexeme to integer value
        t.type = 'INT_CONSTANT'
        return t

    # String Literals
    string_literal = r'(\"(\\.|[^\\"])*\")'

    @TOKEN(string_literal)
    def t_STRING_LITERAL(self, t):
        return t

    def t_INLINE_COMMENT(self, t):
        r'//.*'
        pass                #Ignore

    def t_BLOCK_COMMENT(self, t):
        r'/\*(.|\n)*?\*/'
        t.lexer.lineno += t.value.count('\n')
        pass                #Ignore

    unending_block_comment = r'/\*(.|\n)*$'

    @TOKEN(unending_block_comment)
    def t_UNENDING_BLOCK_COMMENT(self, t):
        msg = "Block comment does not end"
        self._error(msg, t)

    # Identifiers
    def t_ID(self, t):
        r'[a-zA-Z_][a-zA-Z_0-9]*'
        t.type = self.reserved_keywords.get(t.value,'ID')
        if t.type == 'ID' :
            contents = {"line" : t.lineno}
            t.value = {"lexeme": t.value, "additional": contents}
        return t

    unmatched_single_quote = r'(\'(\\.|[^\\\'])+$)'
    @TOKEN(unmatched_single_quote)
    def t_UNMATCHED_SINGLE_QUOTE(self, t):
        msg = "Unmatched ' encountered"
        self._error(msg, t)


    unmatched_double_quote = r'(\"(\\.|[^\\"])*$)'
    @TOKEN(unmatched_double_quote)
    def t_UNMATCHED_DOUBLE_QUOTE(self, t):
        msg = "Unmatched \" encountered"
        self._error(msg, t)

    # Track the line numbers
    def t_newline(self, t):
        r'\n+'
        t.lexer.lineno += len(t.value)
        pass

    # A string containing ignored characters (spaces and tabs)
    t_ignore  = ' \t'

    # Error handling: Ignore bad characters, as in ANSI specification
    def t_error(self, t):

        msg = "Illegal token found"
        self._error(msg, t)

        

    ###############################################################
    # END OF TOKENIZING RULES




# clex = CLexer(self.error_func, on_lbrace, on_rbrace, self.type_lookup_func)
isError = 0

def error_func(msg, row, col):
    print(f'Error found while scanning line number {row}, column number {col}:')
    print(msg)
    global isError
    isError = 1


def type_lookup_func():
    return False

clex = CLexer(error_func, type_lookup_func)


# DRIVER CODE
if len(sys.argv) == 1:
    print("No file given as input")    
    sys.exit(1)

file = open(sys.argv[1],'r')
data = file.read()

clex.build()
clex.lexer.input(data)

def find_column(input, token):
    line_start = input.rfind('\n', 0, token.lexpos) + 1
    return (token.lexpos - line_start) + 1

table_list = []
for tok in clex.lexer:
    row = []
    if tok.type != 'ID':
        row = [tok.type, tok.value, tok.lineno, find_column(data,tok)]
    else:
        row = [tok.type, tok.value['lexeme'], tok.lineno, find_column(data,tok)]
    table_list.append(row)

toPrint = os.environ['lex_env']
clex.lexer.lineno = 1

if isError == 1:
    print(f'Errors found. Aborting scanning of {sys.argv[1]}....')
    sys.exit(1)
else:
    if(toPrint != "0"):
        print(tabulate(table_list, headers=['Token', 'Lexeme', 'Line#', 'Column#']))
    # else:
    #     print("Not printing lexer table")
