from pyparsing import *

# Common literals declarations

LCBR, RCBR, QUOT, COLMN, SEMI, COMMA, EQL, LSB, RSB, \
DOT, LB, RB, QUES,  = map(Literal, '{}\':;,=[].()?')

OR, AND, OR_BITW, AND_BITW, XOR = map(Literal, ['||', '&&', '|', '&', '^'])

# assignmentOperator = map(Literal, ['=', '*=', '/=', '%=', '+=', '-=', '<<=', '>>=', '>>>=', '&=', '^=', '|='])

# common statement

Identifier = Word(alphas + "_$", alphanums + "_$") # Identifier in javascript_grammar.g

# --------------------------------------------------------------------------------------
# this guys is an essential to many expressions

assignmentExpression = Forward()

functionDeclaration = Forward()

statement = Forward()

sourceElement = Forward()

# --------------------------------------------------------------------------------------
# Destructing assignment

destructObject = Forward()

destructArray = Forward()

destructAssignmentKey = (LSB + assignmentExpression + RSB) | Identifier

destructAssignmentKeyWithDefault = (destructAssignmentKey + EQL + assignmentExpression) | \
                                   destructAssignmentKey

forwardingDestructEl = destructAssignmentKey + COLMN + \
                       ((destructObject | destructArray) | destructAssignmentKeyWithDefault)

destructAssignmentEl = forwardingDestructEl | destructAssignmentKeyWithDefault

destructArrayEl = destructObject | destructArray | destructAssignmentKey

destructArray <<= LSB + destructArrayEl + ZeroOrMore(COMMA + destructArrayEl) + RSB

destructObject <<= LCBR + \
                      destructAssignmentEl + ZeroOrMore(COMMA + destructAssignmentEl) + \
                    RCBR

destructAssignment = destructObject | destructArray

# --------------------------------------------------------------------------------------
sourceElements = sourceElement + ZeroOrMore(sourceElement)

sourceElement <<= functionDeclaration | statement

# --------------------------------------------------------------------------------------
# import statement

imported = (Identifier + COMMA + destructAssignment) | \
            Identifier

importStatement = Keyword('import') + imported + Keyword('from') + \
                    quotedString + Optional(SEMI)

# --------------------------------------------------------------------------------------
# Numeric literal

HexIntegerLiteral = Literal('0') + oneOf('x X') + Word(srange('[a-f0-9]'))

ExponentPart = oneOf('e E') + oneOf('+ -')*(0, 1) + Word(nums)

DecimalLiteral = (Word(nums) + '.' + Word(nums) + ExponentPart*(0, 1) | \
                 '.' + Word(nums) + ExponentPart*(0, 1) | \
                 Word(nums) + ExponentPart*(0, 1) | \
                 Word(nums))

NumericLiteral = HexIntegerLiteral | DecimalLiteral

# --------------------------------------------------------------------------------------
# Literals

ArrayLiteral = LSB + assignmentExpression*(0, 1) + ZeroOrMore(COMMA + assignmentExpression) + RSB

computedPropertyKeys = LSB + assignmentExpression + RSB # es6 computed property keys

propertyName = Identifier | \
               quotedString | \
               NumericLiteral | \
               computedPropertyKeys

propertyNameAndValue = propertyName + COLMN + assignmentExpression

# TODO: test here case for '{}'
ObjectLiteral = LCBR + propertyNameAndValue*(0, 1) + ZeroOrMore(COMMA + propertyNameAndValue) + RCBR

# --------------------------------------------------------------------------------------
# functions

functionBody = LCBR + sourceElements*(0, 1) + RCBR

# EQL + assignmentExpression part is for default values
parameterDeclaration = Identifier + (EQL + assignmentExpression)*(0, 1)

parameterListOptions = parameterDeclaration*(0, 1) + ZeroOrMore(COMMA + parameterDeclaration) | \
                destructAssignment | \
                Literal('...') + Identifier # later is for cases like `function (...params) {}`

formalParameterList = LB + parameterListOptions + RB

functionExpression = (Keyword('function') + Identifier*(0, 1) + formalParameterList + functionBody) | \
                    (formalParameterList + Literal('=>') + functionBody) | \
                    (Identifier + Literal('=>') + functionBody)

functionDeclaration <<= Keyword('function') + Identifier + formalParameterList + functionBody

funcArguments = LB + assignmentExpression + ZeroOrMore(COMMA + assignmentExpression) + RB

# --------------------------------------------------------------------------------------

expression = assignmentExpression + ZeroOrMore(COMMA + assignmentExpression)

indexSuffix = LSB + expression + RSB

propertyReferenceSuffix = DOT + Identifier

memberExpressionSuffix = indexSuffix | propertyReferenceSuffix

# in lieu of literal in javascript_grammar.g
jsLiteral = Keyword('null') | \
          Keyword('true') | \
          Keyword('false') | \
          quotedString | \
          NumericLiteral | \
          ArrayLiteral | \
          ObjectLiteral
          # TODO: add es6 template here

primaryExpression = Keyword('this') | \
                    Identifier | \
                    jsLiteral | \
                    LB + expression + RB

memberExpression = Forward()

memberExpression <<= ((Keyword('new') + memberExpression + funcArguments) | \
                    primaryExpression | functionExpression) \
                    + ZeroOrMore(memberExpressionSuffix)

callExpressionSuffix = funcArguments | indexSuffix | propertyReferenceSuffix

callExpression = memberExpression + funcArguments + ZeroOrMore(callExpressionSuffix)

newExpression = Forward()

newExpression <<= memberExpression | (Keyword('new') + newExpression)

leftHandSideExpression = callExpression | newExpression

# ------------------------------------------------------------------------------
# block of logical expressions

postfixExpression = leftHandSideExpression + oneOf('++ --')*(0, 1)

unaryExpression = Forward()

unaryExpression <<= postfixExpression | (oneOf('delete void typeof ++ -- + - ~ !') + unaryExpression)

multiplicativeExpression = unaryExpression + ZeroOrMore(oneOf('* / %') + unaryExpression)

additiveExpression = multiplicativeExpression + ZeroOrMore(oneOf('+ -') + multiplicativeExpression)

shiftExpression = additiveExpression + ZeroOrMore(oneOf('<< >> >>>') + additiveExpression)

relationalExpression = shiftExpression + ZeroOrMore(oneOf('< > <= >= instanceof in') + shiftExpression)

equalityExpression = relationalExpression + ZeroOrMore(oneOf('== != === !==') + relationalExpression)

bitwiseANDExpression = equalityExpression + ZeroOrMore(AND_BITW + equalityExpression)

bitwiseXORExpression = bitwiseANDExpression + ZeroOrMore(XOR + bitwiseANDExpression)

bitwiseORExpression = bitwiseXORExpression + ZeroOrMore(OR_BITW + bitwiseXORExpression)

logicalANDExpression = bitwiseORExpression + ZeroOrMore(AND + bitwiseORExpression)

logicalORExpression = logicalANDExpression + ZeroOrMore(OR + logicalANDExpression)

# ------------------------------------------------------------------------------

conditionalExpression = logicalORExpression + (QUES + assignmentExpression + COLMN + assignmentExpression)*(0, 1)

assignmentExpression <<= (leftHandSideExpression + oneOf('= *= /= %= += -= <<= >>= >>>= &= ^= |=') \
                        + assignmentExpression) | conditionalExpression

variableDeclaration = (destructAssignment | Identifier) + EQL + assignmentExpression

variableDeclarationList = variableDeclaration + ZeroOrMore(COMMA + variableDeclaration)


variableStatement = Keyword('var') + variableDeclarationList + Optional(SEMI)

constStatement = Keyword('const') + variableDeclarationList + Optional(SEMI)

letStatement = Keyword('let') + variableDeclarationList + Optional(SEMI)

# ------------------------------------------------------------------------------
# JS statements

expression = assignmentExpression + ZeroOrMore(COMMA + assignmentExpression)

doWhileStatement = Keyword('do') + statement + Keyword('while') + LB + expression + RB + SEMI*(0, 1)

whileStatement = Keyword('while') + LB + expression + RB + statement + SEMI*(0, 1)

forStatementInitialiserPart = (Keyword('var') | Keyword('let')) + variableDeclarationList

forStatement = Keyword('for') + \
               LB + forStatementInitialiserPart*(0, 1) + \
               SEMI + expression*(0, 1) + \
               SEMI + expression*(0, 1) + RB + \
               statement

forInStatementInitialiserPart = leftHandSideExpression | (Keyword('var') + variableDeclaration)

forInStatement = Keyword('for') + LB + forInStatementInitialiserPart + Keyword('in') + expression + RB + statement

iterationStatement = doWhileStatement \
                     | whileStatement \
                     | forInStatement \
                     | forStatement


ifStatement = Keyword('if') + LB + expression + RB + statement + \
                Keyword('else') + statement

expressionStatement = expression + SEMI*(0, 1)

emptyStatement = SEMI

statementList = statement + ZeroOrMore(statement)

statementBlock = LCBR + statementList*(0, 1) + RCBR

continueStatement = Keyword('continue') + Identifier*(0, 1) + SEMI*(0, 1)

breakStatement = Keyword('break') + Identifier*(0, 1) + SEMI*(0, 1)

returnStatement = Keyword('return') + expression*(0, 1) + SEMI*(0, 1)

withStatement = Keyword('with') + LB + expression + RB + statement

labelledStatement = Identifier + COLMN + statement

defaultClause = Keyword('default') + COLMN + statementList*(0, 1)

caseClause = Keyword('case') + expression + COLMN + statementList*(0, 1)

caseBlock = LCBR + ZeroOrMore(caseClause) + (defaultClause + ZeroOrMore(caseClause))*(0, 1) + RCBR

switchStatement = Keyword('switch') + LB + expression + RB + caseBlock

throwStatement = Keyword('throw') + expression + SEMI*(0, 1)

# ------------------------------------------------------------------------------
# Try statements

catchClause = Keyword('catch') + LB + Identifier + RB + statementBlock

finallyClause = Keyword('finally') + statementBlock

tryStatement = Keyword('try') + statementBlock + (finallyClause | (catchClause + finallyClause*(0, 1)))

# ------------------------------------------------------------------------------


statement <<= statementBlock | \
            constStatement | \
            letStatement | \
            importStatement | \
            variableStatement | \
            emptyStatement | \
            expressionStatement | \
            ifStatement | \
            iterationStatement | \
            continueStatement | \
            breakStatement | \
            returnStatement | \
            withStatement | \
            labelledStatement | \
            switchStatement | \
            throwStatement | \
            tryStatement
            # TODO: add class statement

# set name for every statement in the grammar

def name_grammar_rules(grammar_locals):
    import pyparsing
    exclude_list = dir(pyparsing)
    exclude_list += ['name_grammar_rules', '__annotations__']
    exclude_set = set(exclude_list)
    for grammar_rule_name, grammar_rule in grammar_locals.items():
        if not grammar_rule_name in exclude_set:
            grammar_rule.setName(grammar_rule_name)

name_grammar_rules(locals())
