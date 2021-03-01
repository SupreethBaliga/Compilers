# import necessary libraries
import ply.yacc as yacc

# Get the token map from lexer
from lexer import tokens

precedence = (
    ('nonassoc', '<', '>'),
    ('left', '+', '-'),
    ('left', '/', '*'),
    ('right', 'UMINUS') # for the unary minus operator
)

def p_primary_expression(p):
    '''
    primary_expression : ID
                       | CONSTANT
                       | STRING_LITERAL
                       | '(' expression ')'
    '''

def p_postfix_expression(p):
    '''
    postfix_expression : primary_expression
                       | postfix_expression '[' expression ']'
                       | postfix_expression '(' ')'
                       | postfix_expression '(' argument_expression_list ')'
                       | postfix_expression '.' IDENTIFIER
                       | postfix_expression PTR_OP IDENTIFIER
                       | postfix_expression INC_OP
                       | postfix_expression DEC_OP
    '''

def p_argument_expression_list(p):
    '''
    argument_expression_list : assignment_expression
	                         | argument_expression_list ',' assignment_expression
    '''

def p_unary_expression(p):
    '''
    unary_expression : postfix_expression
                     | INC_OP unary_expression
                     | DEC_OP unary_expression
                     | unary_operator cast_expression
                     | SIZEOF unary_expression
                     | SIZEOF '(' type_name ')'
    '''

def p_unary_operator(p):
    '''
    unary_operator : '&'
                   | '*'
                   | '+'
                   | '-'
                   | '~'
                   | '!'
    '''

def p_cast_expression(p):
    '''
    cast_expression : unary_expression
	                | '(' type_name ')' cast_expression
    '''

def p_mulitplicative_expression(p):
    '''
    multiplicative_expression : cast_expression
	                          | multiplicative_expression '*' cast_expression
	                          | multiplicative_expression '/' cast_expression
	                          | multiplicative_expression '%' cast_expression
    '''

def p_additive_expression(p):
    '''
    additive_expression : multiplicative_expression
	                    | additive_expression '+' multiplicative_expression
	                    | additive_expression '-' multiplicative_expression
    '''

def p_shift_expression(p):
    '''
    shift_expression : additive_expression
	                 | shift_expression LEFT_OP additive_expression
	                 | shift_expression RIGHT_OP additive_expression
    '''

def p_relational_expression(p):
    '''
    relational_expression : shift_expression
	                      | relational_expression '<' shift_expression
	                      | relational_expression '>' shift_expression
	                      | relational_expression LE_OP shift_expression
	                      | relational_expression GE_OP shift_expression
    '''

# 10 rules done till here

def p_equality_expression(p):
    '''
    equality_expression : relational_expression
	                    | equality_expression EQ_OP relational_expression
	                    | equality_expression NE_OP relational_expression
    '''

def p_and_expression(p):
    '''
    and_expression : equality_expression
	               | and_expression '&' equality_expression
    '''

def p_exclusive_or_expression(p):
    '''
    exclusive_or_expression : and_expression
	                        | exclusive_or_expression '^' and_expression
    '''

def p_inclusive_or_expression(p):
    '''
    inclusive_or_expression : exclusive_or_expression
	                        | inclusive_or_expression '|' exclusive_or_expression
    '''

def p_logical_and_expression(p):
    '''
    logical_and_expression : inclusive_or_expression
	                       | logical_and_expression AND_OP inclusive_or_expression
    '''

def p_logical_or_expression(p):
    '''
    logical_or_expression : logical_and_expression
	                      | logical_or_expression OR_OP logical_and_expression
    '''

def p_conditional_expression(p):
    '''
    conditional_expression : logical_or_expression
	                       | logical_or_expression '?' expression ':' conditional_expression
    '''

def p_assignment_expression(p):
    '''
    assignment_expression : conditional_expression
	                      | unary_expression assignment_operator assignment_expression
    '''

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

def p_expression(p):
    '''
    expression : assignment_expression
	           | expression ',' assignment_expression
    '''

# 20 done here

def p_constant_expression(p):
    '''
    constant_expression : conditional_expression
    '''

# grammar for all expressions done

def p_empty(p):
    'empty :'
    pass