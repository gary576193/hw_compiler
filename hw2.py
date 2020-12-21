import ply.lex as lex
import ply.yacc as yacc

reserved = {
    'if' : 'IF',
    'else' : 'ELSE',
    'for' : 'FOR',
}
tokens = [
    'NAME', 'NUMBER',
    'PLUS', 'MINUS', 'TIMES', 'DIVIDE', 'MODULO', 'EQUALS','POWER','ROOK',
    'LPAREN', 'RPAREN',
    'EQUAL', 'NOTEQ', 'LARGE', 'SMALL', 'LRGEQ', 'SMLEQ',
] + list(reserved.values())
t_PLUS    = r'\+'
t_MINUS   = r'-'
t_TIMES   = r'\*'
t_DIVIDE  = r'/'
t_MODULO  = r'%'
t_EQUALS  = r'='
t_POWER  = r'\^'
t_ROOK   = r'\*\*'
t_EQUAL   = r'\=\='
t_LPAREN  = r'\('
t_RPAREN  = r'\)'
t_NOTEQ   = r'\!\='
t_LARGE   = r'\>'
t_SMALL   = r'\<'
t_LRGEQ   = r'\>\='
t_SMLEQ   = r'\<\='


def t_NAME(t):

    r'[a-zA-Z_][a-zA-Z_0-9]*'

    t.type = reserved.get(t.value,'NAME')
    return t


def t_NUMBER(t):
    r'\d+'
    t.value = int(t.value)
    return t


t_ignore = " \t"

def t_newline(t):
    r'\n+'
    t.lexer.lineno += t.value.count("\n")

def t_error(t):
    print("Illegal character '%s'" % t.value[0])
    t.lexer.skip(1)

lexer = lex.lex()

precedence = (
    ('left', 'PLUS', 'MINUS'),
    ('left', 'TIMES', 'DIVIDE', 'MODULO'),
    ('right', 'UMINUS'),
)

names = {}


def p_for(p):
    '''statement    : FOR NAME NUMBER PLUS NUMBER
                    | FOR NAME NUMBER MINUS NUMBER
                    | FOR NAME NUMBER TIMES NUMBER'''
    i=p[2]
    num1=p[3]
    num2=p[5]
    total=num1
    if(p[4]=='+'):
        for i in range(num1+1,num2+1):
            total+=i
    elif(p[4]=='-'):
        for i in range(num1-1,num2-1,-1):
            total-=i
    elif(p[4]=='*'):
        for i in range(num1+1,num2+1):
            total=total*i      
    names[p[2]]=total

def p_if(p):
    '''statement    : IF compare NAME EQUALS expression
                    | IF compare NAME EQUALS expression ELSE NAME EQUALS expression '''

    if p[2]==True:
        names[p[3]] = p[5]
    elif p[2]==False:
        if p[7] is not None:
            names[p[7]]=p[9]

def p__assign(p):
    'statement : NAME EQUALS expression'
    names[p[1]] = p[3]
    
def p_expr(p):
    'statement : expression'
    print(p[1])
def p_comp(p):
    'statement : compare'
    print(p[1])
    

def p_compare(p):
    '''compare : expression EQUAL expression
                          | expression NOTEQ expression
                          | expression LARGE expression
                          | expression SMALL expression
                          | expression LRGEQ expression
                          | expression SMLEQ expression'''
    if p[2] == '==':
        p[0] = p[1] == p[3]
    elif p[2] == '!=':
        p[0] = p[1] != p[3]
    elif p[2] == '>':
        p[0] = p[1] > p[3]
    elif p[2] == '<':
        p[0] = p[1] < p[3]
    elif p[2] == '>=':
        p[0] = p[1] >= p[3]
    elif p[2] == '<=':
        p[0] = p[1] <= p[3]
    print(p[0])

def p_expression_binop(p):
    '''expression : expression PLUS expression
                          | expression MINUS expression
                          | expression TIMES expression
                          | expression DIVIDE expression
                          | expression POWER expression
                          | expression ROOK expression
                          | expression MODULO expression'''
    if p[2] == '+':
        p[0] = p[1] + p[3]
    elif p[2] == '-':
        p[0] = p[1] - p[3]
    elif p[2] == '*':
        p[0] = p[1] * p[3]
    elif p[2] == '/':
        p[0] = p[1] / p[3]
    elif p[2] == '^':
        p[0] = p[1] ** p[3]
    elif p[2] == '**':
         p[0] = round((p[1] ** (1/p[3])),3)
    elif p[2] == '%':
        p[0] = p[1] % p[3]

def p_expression_uminus(p):
    'expression : MINUS expression %prec UMINUS'
    p[0] = -p[2]


def p_expression_group(p):
    'expression : LPAREN expression RPAREN'
    p[0] = p[2]


def p_expression_number(p):
    'expression : NUMBER'
    p[0] = p[1]


def p_expression_name(p):
    'expression : NAME'
    try:
        p[0] = names[p[1]]
    except LookupError:
        print("Undefined name '%s'" % p[1])
        p[0] = 0


def p_error(p):
    print("Syntax error at '%s'" % p.value)

yacc.yacc()

while True:
    try:
        s = input('calc > ')
    except EOFError:
        break
    yacc.parse(s)
    print(names)
