# import necessary libraries
import ply.yacc as yacc
import pygraphviz as pgv
import sys

# Get the token map from lexer
from lexer import tokens

# This has to be filled
precedence = (
    ('nonassoc', '<', '>'),
    ('left', '+', '-'),
    ('left', '/', '*'),
    ('right', 'UMINUS') # for the unary minus operator
)

############## Helper Functions #########################
def new_node():
    global itr
    G.add_node(itr)
    n = G.get_node(itr)
    itr += 1
    return n

######### Grammar Rules ################

def p_primary_expression(p):
    '''
    primary_expression : ID
                       | CONSTANT
                       | STRING_LITERAL
                       | '(' expression ')'
    '''
    # AST Done
    if (len(p) == 2):
        p[0] = new_node()
        p[0].attr['label'] = str(p[1])

    elif (len(p) == 4):
        p[0] = p[2]


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
        p[0] = new_node()
        p[0].attr['label'] = '#' + str(p[2])
        G.add_edge(p[0],p[1])
    elif (len(p) == 4):
        if p[2] == '.':
            p[0] = new_node()
            p[0].attr['label'] = '.'

            p3val = p[3]
            p[3] = new_node()
            p[3].attr['label'] = str(p3val)
            
            G.add_edge(p[0],p[1])
            G.add_edge(p[0],p[3])
            G.add_edge(p[1],p[3],style='invis')
            G.add_subgraph([p[1],p[3]],rank='same')

        elif p[2] == '(':
            p[0] = new_node()
            p[0].attr['label'] = '#()'

            G.add_edge(p[0],p[1])

        elif p[2] == '->':
            p[0] = new_node()
            p[0].attr['label'] = '->'

            p3val = p[3]
            p[3] = new_node()
            p[3].attr['label'] = str(p3val)
            
            G.add_edge(p[0],p[1])
            G.add_edge(p[0],p[3])
            G.add_edge(p[1],p[3],style='invis')
            G.add_subgraph([p[1],p[3]],rank='same')
            

    elif (len(p) == 5):
        if p[2] == '(':
            p[0] = new_node()
            p[0].attr['label'] = '#()'
        elif p[2] == '['
            p[0] = new_node()
            p[0].attr['label'] = '[]'
        
        G.add_edge(p[0],p[1])
        G.add_edge(p[0],p[3])
        G.add_edge(p[1],p[3],style='invis')
        G.add_subgraph([p[1],p[3]],rank='same')




def p_argument_expression_list(p):
    '''
    argument_expression_list : assignment_expression
	                         | argument_expression_list ',' assignment_expression
    '''
    # AST Done
    if (len(p) == 2):
        p[0] = p[1]
    elif (len(p) == 4):
        p[0] = new_node()
        p[0].attr['label'] = ','

        G.add_edge(p[0],p[1])
        G.add_edge(p[0],p[3])

        G.add_edge(p[1],p[3],style='invis')

        G.add_subgraph([p[1],p[3]], rank='same')

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
            p[0] = new_node()
            p[0].attr['label'] = str(p[1])
            G.add_edge(p[0],p[2])
        elif p[1] == 'sizeof':
            p[0] = new_node()
            p[0].attr['label'] = 'SIZEOF'
            G.add_edge(p[0],p[2])
        else:
            p[0] = p[1]
            G.add_edge(p[0],p[2])
    elif (len(p) == 5):
        p[0] = new_node()
        p[0].attr['label'] = 'SIZEOF'
        G.add_edge(p[0],p[3])


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
    p[0] = new_node()
    p[0].attr['label'] = str(p[1])

def p_cast_expression(p):
    '''
    cast_expression : unary_expression
	                | '(' type_name ')' cast_expression
    '''
    #AST DONE - rule for 2 in sheet
    if (len(p) == 2):
        p[0] = p[1]
    elif (len(p) == 5):
        p[0] = new_node()
        p[0].attr['label'] = 'CAST'

        G.add_edge(p[0],p[2])
        G.add_edge(p[0],p[4])

        G.add_edge(p[2],p[4],style='invis')
        G.add_subgraph([p[2],p[4]], rank='same')



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
        p[0]=new_node()
        p[0].attr['label'] = str(p[2])

        G.add_edge(p[0],p[1])
        G.add_edge(p[0],p[3])

        G.add_edge(p[1],p[3],style='invis')
        G.add_subgraph([p[1],p[3]], rank='same')



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
        p[0]=new_node()
        p[0].attr['label'] = str(p[2])

        G.add_edge(p[0],p[1])
        G.add_edge(p[0],p[3])

        G.add_edge(p[1],p[3],style='invis')
        G.add_subgraph([p[1],p[3]], rank='same')


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
        p[0]=new_node()
        p[0].attr['label'] = str(p[2])

        G.add_edge(p[0],p[1])
        G.add_edge(p[0],p[3])

        G.add_edge(p[1],p[3],style='invis')
        G.add_subgraph([p[1],p[3]], rank='same')

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
        p[0]=new_node()
        p[0].attr['label'] = str(p[2])

        G.add_edge(p[0],p[1])
        G.add_edge(p[0],p[3])

        G.add_edge(p[1],p[3],style='invis')
        G.add_subgraph([p[1],p[3]], rank='same')

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
        p[0]=new_node()
        p[0].attr['label'] = str(p[2])

        G.add_edge(p[0],p[1])
        G.add_edge(p[0],p[3])

        G.add_edge(p[1],p[3],style='invis')
        G.add_subgraph([p[1],p[3]], rank='same')

def p_and_expression(p):
    '''
    and_expression : equality_expression
	               | and_expression '&' equality_expression
    '''
    #AST done
    if (len(p) == 2):
        p[0] = p[1]
    elif (len(p) == 4):
        p[0]=new_node()
        p[0].attr['label'] = '&'

        G.add_edge(p[0],p[1])
        G.add_edge(p[0],p[3])

        G.add_edge(p[1],p[3],style='invis')
        G.add_subgraph([p[1],p[3]], rank='same')

def p_exclusive_or_expression(p):
    '''
    exclusive_or_expression : and_expression
	                        | exclusive_or_expression '^' and_expression
    '''
    #AST done
    if (len(p) == 2):
        p[0] = p[1]
    elif (len(p) == 4):
        p[0]=new_node()
        p[0].attr['label'] = '^'

        G.add_edge(p[0],p[1])
        G.add_edge(p[0],p[3])

        G.add_edge(p[1],p[3],style='invis')
        G.add_subgraph([p[1],p[3]], rank='same')

def p_inclusive_or_expression(p):
    '''
    inclusive_or_expression : exclusive_or_expression
	                        | inclusive_or_expression '|' exclusive_or_expression
    '''
    #AST done
    if (len(p) == 2):
        p[0] = p[1]
    elif (len(p) == 4):
        p[0]=new_node()
        p[0].attr['label'] = '|'

        G.add_edge(p[0],p[1])
        G.add_edge(p[0],p[3])

        G.add_edge(p[1],p[3],style='invis')
        G.add_subgraph([p[1],p[3]], rank='same')

def p_logical_and_expression(p):
    '''
    logical_and_expression : inclusive_or_expression
	                       | logical_and_expression AND_OP inclusive_or_expression
    '''
    #AST done
    if (len(p) == 2):
        p[0] = p[1]
    elif (len(p) == 4):
        p[0]=new_node()
        p[0].attr['label'] = '&&'

        G.add_edge(p[0],p[1])
        G.add_edge(p[0],p[3])

        G.add_edge(p[1],p[3],style='invis')
        G.add_subgraph([p[1],p[3]], rank='same')
    

def p_logical_or_expression(p):
    '''
    logical_or_expression : logical_and_expression
	                      | logical_or_expression OR_OP logical_and_expression
    '''
    #AST done
    if (len(p) == 2):
        p[0] = p[1]
    elif (len(p) == 4):
        p[0]=new_node()
        p[0].attr['label'] = '||'

        G.add_edge(p[0],p[1])
        G.add_edge(p[0],p[3])

        G.add_edge(p[1],p[3],style='invis')
        G.add_subgraph([p[1],p[3]], rank='same')


def p_conditional_expression(p):
    '''
    conditional_expression : logical_or_expression
	                       | logical_or_expression '?' expression ':' conditional_expression
    '''
    # AST Done
    if (len(p) == 2):
        p[0] = p[1]
    elif (len(p) == 6):
        p[0] = new_node()
        p[0].attr['label'] = '?:'

        G.add_edge(p[0],p[1])
        G.add_edge(p[0],p[3])
        G.add_edge(p[0],p[5])
        
        G.add_edge(p[1],p[3],style='invis')
        G.add_edge(p[3],p[5],style='invis')
        G.add_subgraph([p[1],p[3],p[5]], rank='same')

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
        G.add_edge(p[0],p[1])
        G.add_edge(p[0],p[3])

        G.add_edge(p[1],p[3],style='invis')
        G.add_subgraph([p[1],p[3]], rank='same')

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
    p[0] = new_node()
    p[0].attr['label'] = str(p[1])

def p_expression(p):
    '''
    expression : assignment_expression
	           | expression ',' assignment_expression
    '''
    # AST done
    if (len(p) == 2):
        p[0] = p[1]
    elif (len(p) == 4):
        p[0] = new_node()
        p[0].attr['label'] = ','

        G.add_edge(p[0],p[1])
        G.add_edge(p[0],p[3])

        G.add_edge(p[1],p[3],style='invis')
        G.add_subgraph([p[1],p[3]], rank='same')

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

def p_declaration_specifiers(p):
    '''
    declaration_specifiers : storage_class_specifier
	                       | storage_class_specifier declaration_specifiers
	                       | type_specifier
	                       | type_specifier declaration_specifiers
	                       | type_qualifier
	                       | type_qualifier declaration_specifiers
    '''

def p_init_declarator_list(p):
    '''
    init_declarator_list : init_declarator
	                     | init_declarator_list ',' init_declarator
    '''

def p_init_declarator(p):
    '''
    init_declarator : declarator
	                | declarator '=' initializer
    '''

def p_storage_class_specifier(p):
    '''
    storage_class_specifier : TYPEDEF
	                        | EXTERN
	                        | STATIC
	                        | AUTO
	                        | REGISTER
    '''

def p_type_specifier(p):
    '''
    type_specifier : VOID
	               | CHAR
	               | SHORT
	               | INT
	               | LONG
	               | FLOAT
	               | DOUBLE
	               | SIGNED
	               | UNSIGNED
	               | struct_or_union_specifier
	               | enum_specifier
	               | TYPE_NAME
    '''

def p_struct_or_union_specifier(p):
    '''
    struct_or_union_specifier : struct_or_union ID '{' struct_declaration_list '}'
	                          | struct_or_union '{' struct_declaration_list '}'
	                          | struct_or_union ID
    '''

def p_struct_or_union(p):
    '''
    struct_or_union : STRUCT
	                | UNION
    '''

def p_struct_declaration_list(p):
    '''
    struct_declaration_list : struct_declaration
	                        | struct_declaration_list struct_declaration
    '''

def p_struct_declaration(p):
    '''
    struct_declaration : specifier_qualifier_list struct_declarator_list ';'
    '''

def p_specifier_qualifier_list(p):
    '''
    specifier_qualifier_list : type_specifier specifier_qualifier_list
	                         | type_specifier
	                         | type_qualifier specifier_qualifier_list
	                         | type_qualifier
    '''

def p_struct_declarator_list(p):
    '''
    struct_declarator_list : struct_declarator
	                       | struct_declarator_list ',' struct_declarator
    '''

def p_struct_declarator(p):
    '''
    struct_declarator : declarator
	                  | ':' constant_expression
	                  | declarator ':' constant_expression
    '''
# correct till here

def p_enum_specifier(p):
    '''
    enum_specifier : ENUM '{' enumerator_list '}'
	               | ENUM ID '{' enumerator_list '}'
	               | ENUM ID
    '''

def p_enumerator_list(p):
    '''
    enumerator_list : enumerator
	                | enumerator_list ',' enumerator
    '''

def p_enumerator(p):
    '''
    enumerator : ID
	           | ID '=' constant_expression
    '''

def p_type_qualifier(p):
    '''
    type_qualifier : CONST
	               | VOLATILE
    '''

def p_declarator(p):
    '''
    declarator : pointer direct_declarator
	           | direct_declarator
    '''

def p_direct_declarator(p):
    '''
    direct_declarator : ID
	                  | '(' declarator ')'
	                  | direct_declarator '[' constant_expression ']'
	                  | direct_declarator '[' ']'
	                  | direct_declarator '(' parameter_type_list ')'
	                  | direct_declarator '(' identifier_list ')'
	                  | direct_declarator '(' ')'
    '''

# correct till here

def p_pointer(p):
    '''
    pointer : '*'
	        | '*' type_qualifier_list
	        | '*' pointer
	        | '*' type_qualifier_list pointer
    '''

def p_type_qualifier_list(p):
    '''
    type_qualifier_list : type_qualifier
	                    | type_qualifier_list type_qualifier
    '''

def p_parameter_type_list(p):
    '''
    parameter_type_list : parameter_list
	                    | parameter_list ',' ELLIPSIS
    '''

def p_parameter_list(p):
    '''
    parameter_list : parameter_declaration
	               | parameter_list ',' parameter_declaration
    '''

def p_parameter_declaration(p):
    '''
    parameter_declaration : declaration_specifiers declarator
	                      | declaration_specifiers abstract_declarator
	                      | declaration_specifiers
    '''

def p_identifier_list(p):
    '''
    identifier_list : ID
	                | identifier_list ',' ID
    '''

def p_type_name(p):
    '''
    type_name : specifier_qualifier_list
	          | specifier_qualifier_list abstract_declarator
    '''

def p_abstract_declarator(p):
    '''
    abstract_declarator : pointer
	                    | direct_abstract_declarator
	                    | pointer direct_abstract_declarator
    '''

def p_direct_abstract_declarator(p):
    '''
    direct_abstract_declarator : '(' abstract_declarator ')'
	                           | '[' ']'
	                           | '[' constant_expression ']'
	                           | direct_abstract_declarator '[' ']'
	                           | direct_abstract_declarator '[' constant_expression ']'
	                           | '(' ')'
	                           | '(' parameter_type_list ')'
	                           | direct_abstract_declarator '(' ')'
	                           | direct_abstract_declarator '(' parameter_type_list ')'
    '''

#correct till here

def p_initializer(p):
    '''
    initializer : assignment_expression
	            | '{' initializer_list '}'
	            | '{' initializer_list ',' '}'
    '''

def p_initializer_list(p):
    '''
    initializer_list : initializer
	                 | initializer_list ',' initializer
    '''

def p_statement(p):
    '''
    statement : labeled_statement
	          | compound_statement
	          | expression_statement
	          | selection_statement
	          | iteration_statement
	          | jump_statement
    '''

def p_labeled_statement(p):
    '''
    labeled_statement : ID ':' statement
	                  | CASE constant_expression ':' statement
	                  | DEFAULT ':' statement
    '''

def p_compound_statement(p):
    '''
    compound_statement : '{' '}'
	                   | '{' statement_list '}'
	                   | '{' declaration_list '}'
	                   | '{' declaration_list statement_list '}'
    '''

def p_declaration_list(p):
    '''
    declaration_list : declaration
	                 | declaration_list declaration
    '''

def p_statement_list(p):
    '''
    statement_list : statement
	               | statement_list statement
    '''

def p_expression_statement(p):
    '''
    expression_statement : ';'
	                     | expression ';'
    '''

def p_selection_statement(p):
    '''
    selection_statement : IF '(' expression ')' statement
	                    | IF '(' expression ')' statement ELSE statement
	                    | SWITCH '(' expression ')' statement
    '''

# Correct till here

def p_iteration_statement(p):
    '''
    iteration_statement : WHILE '(' expression ')' statement
	                    | DO statement WHILE '(' expression ')' ';'
	                    | FOR '(' expression_statement expression_statement ')' statement
	                    | FOR '(' expression_statement expression_statement expression ')' statement
    '''

def p_jump_statement(p):
    '''
    jump_statement : GOTO IDENTIFIER ';'
	               | CONTINUE ';'
	               | BREAK ';'
	               | RETURN ';'
	               | RETURN expression ';'
    '''

def p_translation_unit(p):
    '''
    translation_unit : external_declaration
	                 | translation_unit external_declaration
    '''

def p_external_declaration(p):
    '''
    external_declaration : function_definition
	                     | declaration
    '''

def p_function_definition(p):
    '''
    function_definition : declaration_specifiers declarator declaration_list compound_statement
	                    | declaration_specifiers declarator compound_statement
	                    | declarator declaration_list compound_statement
	                    | declarator compound_statement
    '''

def p_empty(p):
    'empty :'
    pass

def p_error(p):
    print("Error found while parsing!")


# add precedence and associativity of operators
parser = yacc.yacc(start='translation_unit')

# driver code
G = pgv.AGraph(strict=False, directed=True)
G.layout(prog="circo")

itr = 0 # Global var to give unique IDs to nodes of the graph

file = open(sys.argv[1], 'r')
data = file.read()
result = parser.parse(data)

G.write("dot/test1.dot") ## Change this later