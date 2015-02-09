/*
 * Elm Grammar
 * ============
 */
start = operatorPrecedenceDeclaration

/*
 * Rule : program
 *
 * Description :
 *    A list of `statement` separated by `space` representing
 *    a full Elm program.
 *
 * Definition : zero or one `spaceSeparatedStatements`
 *
 * Output : { type : "Program", value : Array `program` }
 */
program
  = program: spaceSeparatedStatements? {
     return {
       type : "Program",
       value : program ? program : []
     };
  }

/*
 * Rule : statement
 *
 * Description :
 *    A `program` is comprised of multiple `statement` and thus
 *    `statement` represents all the different kinds of statements
 *    supported in Elm.
 *    Note : Elm is an expression-based language and here, "statement"
 *    is used for convenience.
 *
 * Definition : Either
 *    1. `fullVaribleDefinition`
 *    2. `typeDefinition`
 *    3. `operatorPrecedenceDeclaration`
 *    4. `expression`
 *
 * Output : Either
 *    1. `fullVaribleDefinition`
 *    2. `typeDefinition`
 *    3. `operatorPrecedenceDeclaration`
 *    4. `expression`
 */
statement
  = operatorPrecedenceDeclaration
  / typeDefinition
  / fullVariableDefinition
  / expression


/*
 * =================================
 *  Operator Precedence Declaration
 * =================================
 */

/*
 * Rule : operatorPrecedenceDeclaration
 *
 * Description :
 *    This is a `statement` used to declate the associativity and
 *    precedence level of a user-defined infix operator
 *
 *    Examples :
 *        1. infixl 9 <<
 *        2. infixr 9 >>
 *        3. infixr 0 <|
 *        4. infixl 0 |>
 *
 * Definition :
 *    Either "infixr" or "infixl" followed by an `integer` followed by an
 *    `infixOperator`, all of which are interspersed with `whitespace`
 *
 * Output :
 *    { type : "OperatorPrecedenceDeclaration"
 *    , value :
 *        { level : `integer`
 *        , operator : `infixOperator`
 *        , associativity : Either "left" or "right"
 *        }
 *    }
 */
operatorPrecedenceDeclaration
  = "infixr" whitespace level: integer  whitespace operator:infixOperator {
      return {
        type : "OperatorPrecedenceDeclaration",
        value: {
          level : level,
          operator : operator,
          associativity : "right"
        }
      };
  }
  / "infixl" whitespace level: integer  whitespace operator: infixOperator {
      return {
        type : "OperatorPrecedenceDeclaration",
        value: {
          level : level,
          operator : operator,
          associativity : "left"
        }
      };
  }



/*
 * =============
 *  Definitions
 * =============
 */


/*
 * Rule : typeDefinition
 *
 * Description :
 *    In Elm, you can either create a new union type with the "type" keyword
 *    or you can alias an existing type with "type alias"
 *
 *    Examples :
 *        1. type Maybe a = Nothing | Just a
 *        2. type alias Point = { x : Float, y : Float }
 *        3. type alias Time = Float
 *
 * Definition : Either
 *    1. `unionTypeDefinition`
 *    2. `typeAliasDefinition`
 *
 * Output : Either
 *    1. `unionTypeDefinition`
 *    2. `typeAliasDefinition`
 */
typeDefinition
  = unionTypeDefinition
  / typeAliasDefinition

/*
 * Rule : unionTypeDefinition
 *
 * Description :
 *    `unionTypeDefinition` describes the creation of a new union type in Elm.
 *
 *    Examples :
 *        1. type Maybe a = Nothing | Just a
 *        2. type Tree a = Leaf a | Node (Tree a) (Tree a)
 *        3. type RGBColor = Red | Green | Blue
 *        4. type Phantom = Phantom
 *
 * Definition :
 *    "type" keyword followed by a `validTypeName` followed "=" followed by
 *    `barSeparatedUnionTypes`, all of which interspersed by either some `whitespace`
 *    or some `space` accordingly
 *
 * Output :
 *    { type : "TypeUnionDefinition"
 *    , value :
 *        { type : `validTypeName`
 *        , arguments : Array `validVariableName`
 *        , definition : Array `validUnionType`
 *        }
 *    }
 *
 */
unionTypeDefinition
  = "type" whitespace+ type: validTypeName whitespace* args: spaceSeparatedVariables? whitespace* "=" space* definition: barSeparatedUnionTypes {
      return {
        type : "TypeUnionDefinition",
        value : {
          type : type,
          arguments: args ? args : [],
          definition : definition
        }
      };
  }

/*
 * Rule : typeAliasDefinition
 *
 * Description :
 *    `typeAliasDefinition` describes the aliasing of an existing type in Elm.
 *
 *    Examples :
 *        1. type alias Point = { x : Float, y : Float }
 *        2. type alias Time = Float
 *        3. type alias Transform = { position : { x : Float, y : Float}, rotation : Float, scale : { x : Float, y : Float }}
 *        4. type alias Mapper a = a -> a
 *        5. type alias Positioned a = { a | position : Point }
 *        6. type alias Described a = Named { a | description : String }
 *
 * Definition :
 *    "type alias" keyword followed by a `validTypeName` followed by
 *    zero or one `spaceSeparatedVariables` followed by "=" followed by a
 *    `validType`, all of which interspersed by either some `whitespace` or
 *    some `space` accordingly
 *
 * Output :
 *    { type : "TypeAliasDefinition"
 *    , value :
 *        { type : `validTypeName`
 *        , arguments : Array `validVariableName`
 *        , definition : `validType`}}
 */
typeAliasDefinition
  = "type alias" whitespace* type: validTypeName whitespace* args: spaceSeparatedVariables? whitespace* "=" space* definition: validType {
      return {
        type : "TypeAliasDefinition",
        value : {
          type : type,
          arguments : args ? args : [],
          definition : definition
        }
      };
  }





/* Letters */
lowerCaseLetter
  = [a-z]
upperCaseLetter
  = [A-Z]
letter
  = lowerCaseLetter
  / upperCaseLetter

/* Numbers */
digit
  = [0-9]

symbolIncludingReserved
  = [^0-9a-zA-Z,.-:= \n]

symbol
  = [^0-9a-zA-Z,. \n]

validInfixOperator
  = first: symbol rest:symbol+ {
      return [first].concat(rest).join("");
  }
  / first: symbolIncludingReserved rest:symbol* {
      return rest ? [first].concat(rest).join("") : first;
  }

integer
  = first: [1-9] rest: digit* {
     return parseInt((rest ? [first].concat(rest) : [first]).join(""), 10);
  }

float
  = digit "." digit+
  / integer "." digit+

/* STRINGS AND CHARS */
singleQuote
  = "'"

escapedSingleQuote
  = "\\'"{return "\'";}

notSingleQuote
  = escapedSingleQuote
  / [^']

singleQuotedChar
  = singleQuote char: notSingleQuote singleQuote {
      return char;
  }

doubleQuote
  = '"'

escapedDoubleQuote
  = '\\"'{return '\"';}

notDoubleQuote
  = escapedDoubleQuote
  / [^"]

doubleQuotedString
  = doubleQuote string:notDoubleQuote* doubleQuote {
      return string ? string.join("") : "";
  }

/* LISTS */
classicalList
  = "[" whitespace* list: commaSeparatedExpressions? whitespace* "]" {
      return {
        type : "ListLiteral",
        value : list ? list : []
      };
  }

rangeList
  = "[" whitespace* start: expression whitespace* ".." whitespace* end: expression whitespace* "]" {
      return {
        type : "RangeListLiteral",
        value : {
          start : start,
          end : end
        }
      };
  }

list
  = rangeList
  / classicalList


/* TUPLES */
tuple
  = "(" whitespace* tuple: commaSeparatedExpressions? whitespace* ")" {
      return {
        type : "TupleLiteral",
        value : tuple ? tuple : []
      };
  }


/* BINARY OPERATIONS */
infixOperator
  = operator: validInfixOperator {
      return {
        type : "InfixOperator",
        value : operator
      };
  }

guardedInfixOperator
  = "(" operator: validInfixOperator ")" { return operator; }

infixFunction
  = "`" func: validVariableName "`" {
      return {
        type : "InfixFunction",
        value : func
      };
  }

binaryOperation
  = left: literal whitespace* operator: (infixOperator / infixFunction) whitespace* right: binaryOperation {
      return {
        type : "BinaryOperation",
        value : {
          left : left,
          operator : operator,
          right : right
        }
      };
  }
  / literal




/* CONDITIONALS */
ifThenElseExpression
  = "if" whitespace* predicate: expression whitespace* "then" whitespace* thenBranch: expression whitespace* "else" whitespace* elseBranch: expression {
      return {
        type : "IfThenElseExpression",
        value : {
          predicate : predicate,
          thenBranch : thenBranch,
          elseBranch : elseBranch
        }
      };
  }

multiWayIfExpression
  = "if" whitespace* conditions: barSeparatedConditionalExpressions {
      return {
        type : "MultiWayIfExpression",
        value : conditions
      };
  }

caseExpression
  = "case" whitespace* test: expression whitespace* "of" whitespace* patterns: newLineSeparatedPatterns {
      return {
        type : "CaseExpression",
        value : {
          test : test,
          patterns : patterns
        }
      };
  }

/* LET IN EXPRESSION */
letInExpression
  = "let" space* variables: spaceSeparatedVariableDefinitions space* "in" space* result: expression {
      return {
        type : "LetInExpression",
        value : {
          variables: variables,
          result : result
        }
      };
  }

/* IDENTIFIERS */
validTypeName
  = first: upperCaseLetter rest: (lowerCaseLetter / upperCaseLetter / digit)*{
      return rest ? [first].concat(rest).join("") : first;
  }

validVariableName
  = first: lowerCaseLetter rest: (lowerCaseLetter / upperCaseLetter / digit)*{
      return rest ? [first].concat(rest).join("") : first;
  }

validFunctionName
  = validVariableName
  / validTypeName
  / guardedInfixOperator

variableIdentifier
  = variable: validVariableName {
      return {
        type : "VariableIdentifier",
        value : variable
      };
  }

typeConstructorIdentifier
  = typeConstructor: validTypeName {
      return {
        type : "TypeConstructorIdentifier",
        value : typeConstructor
      };
  }

/* RECORDS */
recordOperation
  = key: validVariableName space* "=" space* value: expression {
      return {
        type : "AssignRecordOperation",
        value : {
          key : key,
          value : value
        }
      };
  }
  / key: validVariableName space* "<-" space* value: expression {
      return {
        type : "UpdateRecordOperation",
        value : {
          key : key,
          value : value
        }
      };
  }


record
  = "{" space* base: (validVariableName whitespace* "|")? space* operations: commaSeparatedRecordOperations? space* "}" {
      return {
        type : "RecordLiteral",
        value : {
          base : base ? base[0] : {},
          operations : operations ? operations : []
        }
      };
  }


/* VALUE LEVEL EXPRESSIONS */
number
  = float: float {
      return {
        type : "FloatLiteral",
        value : parseFloat(float)
      };
  }
  / integer: integer {
      return {
        type : "IntegerLiteral",
        value : parseInt(integer, 10)
      };
  }

string
  = string: doubleQuotedString {
      return {
        type : "StringLiteral",
        value : string
      };
  }

char
  = char: singleQuotedChar {
      return {
        type : "CharLiteral",
        value : char
      };
  }


lambda
  = "(" space* "\\" args: spaceSeparatedVariables? space* "->" space* definition: expression {
      return {
        type : "LambdaExpression",
        value : {
          arguments : args ? args : [],
          definition : definition
        }
      };
  }


functionCall
  = func: (validFunctionName / guardedExpression) space* args: spaceSeparatedLiterals {
      return {
        type : "FunctionCall",
        value : {
          func : func,
          arguments : args
        }
      };
  }

guardedFunctionCall
 = "(" space* func: functionCall space* ")" {return func;}

guardedLiteral
  = "(" whitespace* literal: literal whitespace* ")" { return literal; }


literal
  = guardedLiteral
  / record
  / list
  / number
  / string
  / char
  / variableIdentifier
  / typeConstructorIdentifier
  / tuple

guardedExpression
  = "(" space* expression: expression space* ")" { return expression; }

expression
  = letInExpression
  / functionCall
  / guardedExpression
  / binaryOperation
  / caseExpression
  / multiWayIfExpression
  / ifThenElseExpression
  / literal


variableDefinition
  = variable: validVariableName space* args: spaceSeparatedVariables? space* "=" space* definition: expression {
      return {
        type : "VariableDefinition",
        value : {
          variable : variable,
          arguments : args ? args : [],
          definition : definition
        }
      };
  }

fullVariableDefinition
  = annotation: typeAnnotationDeclaration space* definition: variableDefinition {
      return {
        type : "FullVariableDefinition",
        value : {
          annotation : annotation,
          definition : definition
        }
      };
  }
  / variableDefinition


/* TYPES */
boundedType
  = type: validTypeName {
      return {
        type : "BoundedType",
        value : type
      };
  }

unboundedType
  = type: validVariableName {
      return {
        type : "UnboundedType",
        value : type
      };
  }

guardedType
  = "(" whitespace* type: validType whitespace* ")" { return type; }

composedType
  = typeConstructor: boundedType whitespace* args: spaceSeparatedTypeArguments {
      return {
        type : "ComposedType",
        value : {
          typeConstructor : typeConstructor,
          arguments : args
        }
      };
  }


recordTypeProperty
  = key: validVariableName whitespace* ":" whitespace* value: validType {
      return {
        type : "RecordTypeProperty",
        value : {
          key : key,
          value : value
        }
      };
  }

recordType
  = "{" space* properties: commaSeparatedRecordTypeProperties? space* "}" {
      return {
        type : "RecordType",
        value : properties ? properties : []
      };
  }

extensibleRecordType
  = "{" space* extension: validVariableName space* "|" space* properties: commaSeparatedRecordTypeProperties? whitespace* "}" {
      return {
        type : "ExtensibleRecordType",
        value : {
          properties : properties ? properties : [],
          extension : extension
        }
      };
  }

validType
  = guardedType
  / extensibleRecordType
  / recordType
  / composedType
  / functionType


functionType
  = arrowSeparatedTypes


/* TYPE LEVEL EXPRESSIONS */

validTypeArgument
  = "(" whitespace* type: validUnionType whitespace* ")" { return type;}
  / validType

validUnionType
  = typeConstructor: validTypeName whitespace* args: spaceSeparatedTypeArguments? {
      return {
        type : "UnionType",
        value : {
          typeConstructor : typeConstructor,
          arguments : args ? args : []
        }
      };
  };



typeAnnotationDeclaration
  = variable: validVariableName whitespace+ ":" whitespace+ annotation: validType {
      return {
        type : "TypeAnnotationDeclaration",
        value : {
          variable : variable,
          annotation : annotation
        }
      };
  }


/* SEPARATED STUFF */
commaSeparatedExpressions
  = first: expression whitespace* "," whitespace* rest: commaSeparatedExpressions {
      return rest ? [first].concat(rest) : [first];
  }
  / rest: expression {return [rest];}


newLineSeparatedPatterns
  = newLine whitespace* "_" whitespace* "->" whitespace* result: expression {
      return [{
        pattern : "CatchAll",
        result : result
      }];
  }
  / newLine whitespace* pattern: expression whitespace* "->" whitespace* result: expression whitespace* rest: newLineSeparatedPatterns {
      var condition = {
        pattern: pattern,
        result : result
      };
      return [condition].concat(rest);
  }
  / newLine whitespace* pattern: expression whitespace* "->" whitespace* result: expression {
      return [{
        pattern : pattern,
        result : result
      }];
  }

barSeparatedConditionalExpressions
  = "|" whitespace* test: expression whitespace* "->" whitespace* result: expression whitespace* newLine whitespace* rest: barSeparatedConditionalExpressions {
      var condition = {
        test : test,
        result : result
      };

      return [condition].concat(rest);
  }
  / "|" whitespace* "otherwise" whitespace* "->" whitespace* result: expression {
      return [{
        test : "otherwise",
        result : result
      }];
  }
  / "|" whitespace* test: expression whitespace* "->" whitespace* result: expression {
      return [{
        test : test,
        result : result
      }];
  }

spaceSeparatedVariables
  = whitespace* first: validVariableName rest: spaceSeparatedVariables {
      return [first].concat(rest);
  }
  / whitespace* rest: validVariableName {
      return [rest];
  }

spaceSeparatedTypeArguments
  = whitespace* first: validTypeArgument rest: spaceSeparatedTypeArguments {
      return [first].concat(rest);
  }
  / whitespace* rest: validTypeArgument {
      return [rest];
  }

spaceSeparatedVariableDefinitions
  = first: variableDefinition space* rest: spaceSeparatedVariableDefinitions {
      return [first].concat(rest);
  }
  / rest: variableDefinition {
      return [rest];
  }

spaceSeparatedLiterals
  = first: literal space* rest: spaceSeparatedLiterals {
      return [first].concat(rest);
  }
  / rest: literal {
      return [rest];
  }

spaceSeparatedStatements
  = first: statement space* rest: spaceSeparatedStatements {
      return [first].concat(rest);
  }
  / rest: statement {
      return [rest];
  }


barSeparatedUnionTypes
  = first: validUnionType whitespace* "|" space* rest: barSeparatedUnionTypes {
      return [first].concat(rest);
  }
  / rest: validUnionType {
      return [rest];
  }

arrowSeparatedTypes
  = input: (unboundedType / boundedType) space* "->" space* output: arrowSeparatedTypes {
      return {
        input : input,
        output : output
      };
  }
  / (unboundedType / boundedType)


commaSeparatedRecordTypeProperties
  = first: recordTypeProperty space* "," space* rest: commaSeparatedRecordTypeProperties {
      return [first].concat(rest);
  }
  / rest: recordTypeProperty {
      return [rest];
  }

commaSeparatedRecordOperations
  = first: recordOperation space* ',' space* rest: commaSeparatedRecordOperations {
      return [first].concat(rest);
  }
  / rest: recordOperation {
      return [rest];
  }

/* SPECIAL TOKENS / SYMBOLS */

whitespace
  = " "

newLine
  = "\n"

space
  = whitespace / newLine
