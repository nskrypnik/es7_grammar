from pyparsing import *

# Common literals declarations

LBR, RBR, QUOT, COLMN, SEMI, COMMA, EQL, LSB, RSB, \
DOT, LB, RB, QUES,  = map(Literal, '{}\':;,=[].()?')

OR, AND, OR_BITW, AND_BITW, XOR = map(Literal, ['||', '&&', '|', '&', '^'])

# assignmentOperator = map(Literal, ['=', '*=', '/=', '%=', '+=', '-=', '<<=', '>>=', '>>>=', '&=', '^=', '|='])

# common statement

simpleVarName = Word(alphas + "_", alphanums + "_")

hashUnpack = Forward()

hashUnpackGroup = (simpleVarName + COLMN + hashUnpack)| simpleVarName

hashUnpack <<= LBR + hashUnpackGroup + ZeroOrMore(COMMA + hashUnpackGroup) + RBR

# --------------------------------------------------------------------------------------
# import statement

imported = simpleVarName + COMMA + hashUnpack| \
            simpleVarName

importStatement = Keyword('import') + imported + Keyword('from') + \
                    quotedString + Optional(SEMI)

# --------------------------------------------------------------------------------------

ExponentPart = oneOf('e E') + oneOf('+ -')*(0, 1) + Word(nums)

DecimalLiteral = (Word(nums) + '.' + Word(nums) + ExponentPart*(0, 1) | \
                 '.' + Word(nums) + ExponentPart*(0, 1) | \
                 Word(nums) + ExponentPart*(0, 1) | \
                 Word(nums))

NumericLiteral = DecimalLiteral # | HexIntegerLiteral

# --------------------------------------------------------------------------------------

functionExpression = Forward() # TODO: this is a dummy

memberExpressionSuffix = Forward() # TODO: this is a dummy

assignmentExpression = Forward()

funcArguments = LB + assignmentExpression + ZeroOrMore(COMMA + assignmentExpression) + RB

expression = assignmentExpression + ZeroOrMore(COMMA + assignmentExpression)

indexSuffix = LSB + expression + RSB

propertyReferenceSuffix = DOT + simpleVarName

jsLiteral = Keyword('null') | \
          Keyword('true') | \
          Keyword('false') | \
          quotedString | \
          NumericLiteral

primaryExpression = Keyword('this') | \
                    simpleVarName | \
                    jsLiteral # TODO: more goes here

memberExpression = Forward()

memberExpression <<= (primaryExpression | functionExpression | \
                    Keyword('new') + memberExpression + funcArguments) \
                    + memberExpressionSuffix

callExpressionSuffix = funcArguments | indexSuffix | propertyReferenceSuffix

callExpression = memberExpression + funcArguments + ZeroOrMore(callExpressionSuffix)

newExpression = Forward()

newExpression <<= memberExpression | (Keyword('new') + newExpression)

leftHandSideExpression = callExpression | newExpression

# ------------------------------------------------------------------------------
# block of logical expressions

postfixExpression = leftHandSideExpression + oneOf('++ --')*(0, 1)

unaryExpression = Forward()

unaryExpression = postfixExpression | (oneOf('delete void typeof ++ -- + - ~ !') + unaryExpression)

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

assignmentExpression <<= conditionalExpression| \
                        (leftHandSideExpression + oneOf('= *= /= %= += -= <<= >>= >>>= &= ^= |=') \
                            + assignmentExpression)

variableDeclaration = simpleVarName + EQL + assignmentExpression

variableDeclarationList = variableDeclaration + ZeroOrMore(COMMA + variableDeclaration)

variableStatement = variableDeclarationList + Optional(SEMI)

# assign statements

constStatement = Keyword('const') + variableStatement

letStatement = Keyword('let') + variableStatement
