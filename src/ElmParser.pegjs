/*
 * Elm Grammar
 * ============
 */
start = program

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
  = space* program: spaceSeparatedStatements? space* {
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
  = singleLineComment
  / moduleStatement
  / importStatement
  / portAnnotation
  / portDefinition
  / operatorPrecedenceDeclaration
  / typeDefinition
  / fullVariableDefinition
  / expression


singleLineComment
  = "--" commentContent: [^\n]* {
      return {
        type : "SingleLineComment",
        value : commentContent ? commentContent.join("") : ""
      };
  }


moduleStatement
  = "module" whitespace moduleName: validModuleName space* exportList: (exportList space*)? "where" {
      return {
        type : "ModuleStatement",
        value : {
          name : moduleName,
          exports : exportList ? exportList[0] : []
        }
      };
  }

exportList
  = "(" space* exportList: commaSeparatedExports space* ")" {
      return exportList;
  }

importStatement
  = "import" whitespace moduleName: validModuleName whitespace "as" whitespace qualification: validModuleName {
      return {
        type : "QualifiedImportStatement",
        value : {
          name : moduleName,
          qualification : qualification
        }
      };
  }
  / "import" whitespace moduleName: validModuleName whitespace "(..)" {
      return {
        type : "AllOpenImportStatement",
        value : moduleName
      };
  }
  / "import" whitespace moduleName: validModuleName whitespace space* "(" space* importList: commaSeparatedImports space* ")" {
      return {
        type : "SelectiveOpenImportStatement",
        value : {
          name : moduleName,
          imports : importList
        }
      };
  }
  / "import" whitespace moduleName: validModuleName {
      return {
        type : "SelfQualifiedImportStatement",
        value : moduleName
      };
  }

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


/*
 * Rule : fullVariableDefinition
 *
 * Description :
 *   A variable of function definition, optionally accompanied by a type annotation
 *
 *    Examples :
 *        1. x : Int
 *           x = 2
 *
 *        2. square : Float -> Float
 *           square x = x * x
 *
 *        3. (<<) : (b -> c) -> (a -> b) -> a -> c
 *           (<<) f g x = f (g x)
 *
 *        4. helloworld = "HelloWorld"
 *
 * Definition : Either
 *    1. `typeAnnotationDeclaration` followed by some `space` followed by a `variableDefinition`
 *    2. `variableDefinition`
 *
 * Output : Either
 *    1. { type : "FullVariableDefinition", value : { annotation : `typeAnnotationDeclaration`, definition : `variableDefinition`}}
 *    2. `variableDefinition`
 */
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


portDefinition
 = "port" whitespace value: variableDefinition {
      return {
        type : "PortDefinition",
        value : value
      };
 }

portAnnotation
 = "port" whitespace value: typeAnnotationDeclaration {
      return {
        type : "PortAnnotation",
        value : value
      };
 }

// TODO: Modify this to also take into account pattern-matching in arguments
/*
 * Rule : variableDefinition
 *
 * Description :
 *   A variable or function definition. Optionally may have arguments.
 *
 *   Examples :
 *      1. x = 1
 *      2. square x = x * x
 *      3. (<<) f g x = f (g x)
 *
 * Definition :
 *   A `validVariableName` followed by zero or one `spaceSeparatedVariables`
 *   followed by "=" followed by an `expression`, all of which interspersed
 *   with some `space`
 *
 * Output :
 *   { type : "VariableDefinition"
 *   , value :
 *       { variable : `validVariableName`
 *       , arguments : Array `validVariableName`
 *       , definition : `expression`
 *       }
 *   }
 */
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







expression
  = letInExpression
  / functionCall
  / guardedExpression
  / binaryOperation
  / caseExpression
  / multiWayIfExpression
  / ifThenElseExpression
  / literal

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

guardedExpression
  = "(" space* expression: expression space* ")" { return expression; }

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

multiWayIfExpression
  = "if" whitespace* conditions: barSeparatedConditionalExpressions {
      return {
        type : "MultiWayIfExpression",
        value : conditions
      };
  }

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



/*
 * ==========
 *  Literals
 * ==========
 */

literal
  = lambda
  / guardedLiteral
  / guardedExpression
  / record
  / list
  / number
  / string
  / char
  / variableIdentifier
  / typeConstructorIdentifier
  / tuple

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

guardedLiteral
  = "(" whitespace* literal: literal whitespace* ")" { return literal; }

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

list
  = rangeList
  / classicalList

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

classicalList
  = "[" whitespace* list: commaSeparatedExpressions? whitespace* "]" {
      return {
        type : "ListLiteral",
        value : list ? list : []
      };
  }

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
        value : integer
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

tuple
  = "(" whitespace* tuple: commaSeparatedExpressions? whitespace* ")" {
      return {
        type : "TupleLiteral",
        value : tuple ? tuple : []
      };
  }


/*
 * ===============
 *  Simple Tokens
 * ===============
 */


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

integer
  = first: [1-9] rest: digit* {
     return parseInt((rest ? [first].concat(rest) : [first]).join(""), 10);
  }

float
  = digit "." digit+
  / integer "." digit+

/* Symbols */
symbolIncludingReserved
  = [^0-9a-zA-Z,.-:= \n]

symbol
  = [^0-9a-zA-Z,. \n]

/* Infix Operators and Functions */
validInfixOperator
  = first: symbol rest:symbol+ {
      return [first].concat(rest).join("");
  }
  / first: symbolIncludingReserved rest:symbol* {
      return rest ? [first].concat(rest).join("") : first;
  }

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


/* Quotation for strings and chars */
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




/* Identifiers */
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

validModuleName
  = names: dotSeparatedValidTypeNames {
      return names.join(".");
  }

validExport
  = mainExport: validTypeName space* "(" space* selectedExports: commaSeparatedTypeNames space* ")" {
      return {
        type : "SelectiveExport",
        value : {
          mainExport : mainExport,
          selectedExports : selectedExports
        }
      };
  }
  / exportName: validTypeName space* "(" space* ".." space* ")" {
      return {
        type : "AllExport",
        value : exportName
      };
  }
  / exportName: validFunctionName {
      return {
        type : "SingleExport",
        value : exportName
      };
  }

validImport
  = mainImport: validTypeName space* "(" space* selectedImports: commaSeparatedTypeNames space* ")" {
      return {
        type : "SelectiveImport",
        value : {
          mainImport : mainImport,
          selectedImports : selectedImports
        }
      };
  }
  / importName: validTypeName space* "(" space* ".." space* ")" {
      return {
        type : "AllImport",
        value : importName
      };
  }
  / importName: validFunctionName {
      return {
        type : "SingleImport",
        value : importName
      };
  }


/* Types */
validType
  = guardedType
  / extensibleRecordType
  / recordType
  / composedType
  / functionType

guardedType
  = "(" whitespace* type: validType whitespace* ")" { return type; }

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

recordType
  = "{" space* properties: commaSeparatedRecordTypeProperties? space* "}" {
      return {
        type : "RecordType",
        value : properties ? properties : []
      };
  }

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

functionType
  = arrowSeparatedTypes

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

validTypeArgument
  = "(" whitespace* type: validUnionType whitespace* ")" { return type;}
  / validType


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



/*
 * ===================
 *  Recursion Helpers
 * ===================
 */


/* Separated stuff (helpers for recursive rules) */
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

commaSeparatedExports
  = first: validExport space* "," space* rest: commaSeparatedExports {
      return [first].concat(rest);
  }
  / rest: validExport {
      return [rest];
  }

commaSeparatedImports
  = first: validImport space* "," space* rest: commaSeparatedImports {
      return [first].concat(rest);
  }
  / rest: validImport {
      return [rest];
  }

commaSeparatedTypeNames
  = first: validTypeName space* "," space* rest: commaSeparatedTypeNames {
      return [first].concat(rest);
  }
  / rest: validTypeName {
      return [rest];
  }

dotSeparatedValidTypeNames
  = first: validTypeName "." rest: dotSeparatedValidTypeNames {
      return [first].concat(rest);
  }
  / rest: validTypeName {
      return [rest];
  }

/*
 * ================
 *  Ignore / Space
 * ================
 */

whitespace
  = " "

newLine
  = "\n"

space
  = whitespace / newLine
