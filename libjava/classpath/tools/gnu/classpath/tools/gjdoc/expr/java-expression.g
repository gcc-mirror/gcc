/*
 * This grammar is derived from the Java 1.3 Recognizer
 * (http://www.antlr.org/grammar/java/java.g) by Mitchell, Parr, Lilley,
 * Stanchfield, Mohnen, Williams, Jacobs, Messick and Pybus, Version 
 * 1.21.
 *
 * This grammar recognizes simple Java expressions. The following 
 * language elements are NOT supported:
 *
 * - type casts to non-primitive types
 * - method calls
 * - constructor calls
 * - array access
 * - comma expressions
 * - increment and decrement operators (both prefix/postfix)
 * - expressions involving constant classes (Abc.class)
 */

header { 
   package gnu.classpath.tools.gjdoc.expr; 
}

class JavaRecognizer extends Parser;
options {
	k = 2;                           // two token lookahead
	exportVocab=Java;                // Call its vocabulary "Java"
	codeGenMakeSwitchThreshold = 2;  // Some optimizations
	codeGenBitsetTestThreshold = 3;
	defaultErrorHandler = false;     // Don't generate parser error handlers
	buildAST = true;
}

tokens {
	BLOCK; MODIFIERS; OBJBLOCK; SLIST; CTOR_DEF; METHOD_DEF; VARIABLE_DEF;
	INSTANCE_INIT; STATIC_INIT; TYPE; CLASS_DEF; INTERFACE_DEF;
	PACKAGE_DEF; ARRAY_DECLARATOR; EXTENDS_CLAUSE; IMPLEMENTS_CLAUSE;
	PARAMETERS; PARAMETER_DEF; LABELED_STAT; TYPECAST; INDEX_OP;
	POST_INC; POST_DEC; METHOD_CALL; EXPR; ARRAY_INIT;
	IMPORT; UNARY_MINUS; UNARY_PLUS; CASE_GROUP; ELIST; FOR_INIT; FOR_CONDITION;
	FOR_ITERATOR; EMPTY_STAT; FINAL="final"; ABSTRACT="abstract";
	STRICTFP="strictfp"; SUPER_CTOR_CALL; CTOR_CALL;
}

// A builtin type specification is a builtin type with possible brackets
// afterwards (which would make it an array type).
builtInTypeSpec[boolean addImagNode] returns [Type t = null]
	:	t=builtInType (lb:LBRACK^ {#lb.setType(ARRAY_DECLARATOR);} RBRACK!)*
		{
			if ( addImagNode ) {
				#builtInTypeSpec = #(#[TYPE,"TYPE"], #builtInTypeSpec);
			}
		}
	;

// A type name. which is either a (possibly qualified) class name or
//   a primitive (builtin) type
type returns [Type t]
	:	t=builtInType
	;

// The primitive types.
builtInType returns [Type t = null]
	:	"void" {t=Type.VOID;}
	|	"boolean" {t=Type.BOOLEAN;}
	|	"byte" {t=Type.BYTE;}
	|	"char" {t=Type.CHAR;}
	|	"short" {t=Type.SHORT;}
	|	"int" {t=Type.INTEGER;}
	|	"float"{t=Type.FLOAT;}
	|	"long" {t=Type.LONG;}
	|	"double" {t=Type.DOUBLE;}
	|	"String" {t=Type.STRING;}
	;

// A (possibly-qualified) java identifier.  We start with the first IDENT
//   and expand its name by adding dots and following IDENTS
identifier returns [String s = null;]
	:	i:IDENT {s=i.getText();}  ( DOT^ i2:IDENT {s+="."+i2.getText();} )*
	;

expression returns [Expression e = null]
    :   e=conditionalExpression EOF!
    ;

// conditional test (level 12)
conditionalExpression returns [Expression e = null] { Expression a,b,c; }
	:	e=logicalOrExpression
		( QUESTION^ b=conditionalExpression COLON! c=conditionalExpression {e=new ConditionalExpression(e,b,c);} )?
	;


// logical or (||)  (level 11)
logicalOrExpression returns [Expression e = null] { Expression a,b; }
	:	e=logicalAndExpression (LOR^ b=logicalAndExpression {e=new LogicalOrExpression(e,b);})*
	;


// logical and (&&)  (level 10)
logicalAndExpression returns [Expression e = null] { Expression a,b; }
	:	e=inclusiveOrExpression (LAND^ b=inclusiveOrExpression {e=new LogicalAndExpression(e,b);})*
	;


// bitwise or non-short-circuiting or (|)  (level 9)
inclusiveOrExpression returns [Expression e = null] { Expression a,b; }
	:	e=exclusiveOrExpression (BOR^ b=exclusiveOrExpression {e=new InclusiveOrExpression(e,b);})*
	;


// exclusive or (^)  (level 8)
exclusiveOrExpression returns [Expression e = null] { Expression a,b; }
	:	e=andExpression (BXOR^ b=andExpression {e=new ExclusiveOrExpression(e,b);})*
	;


// bitwise or non-short-circuiting and (&)  (level 7)
andExpression returns [Expression e = null] { Expression a,b; }
	:	e=equalityExpression (BAND^ b=equalityExpression {e=new AndExpression(e,b);})*
	;


// equality/inequality (==/!=) (level 6)
equalityExpression returns [Expression e = null] { Expression a,b; }
	:	e=relationalExpression ((NOT_EQUAL^ a=relationalExpression {e=new NotEqualExpression(e,a);} | EQUAL^ a=relationalExpression {e=new EqualExpression(e,a);}))*
	;


// boolean relational expressions (level 5)
relationalExpression returns [Expression e = null] { Expression a,b; }
	:	e=shiftExpression
		(	(	(	LT^ a=shiftExpression {e=new LessThanExpression(e,a);}
				|	GT^ a=shiftExpression {e=new GreaterThanExpression(e,a);}
				|	LE^ a=shiftExpression {e=new LessThanOrEqualExpression(e,a);}
				|	GE^ a=shiftExpression {e=new GreaterThanOrEqualExpression(e,a);}
				)
				
			)*
		)
	;


// bit shift expressions (level 4)
shiftExpression returns [Expression e = null] { Expression a,b; }
	:	e=additiveExpression ((SL^ a=additiveExpression {e=new ShiftLeftExpression(e,a);} | SR^ a=additiveExpression {e=new ShiftRightExpression(e,a);} | BSR^ a=additiveExpression {e=new BitShiftRightExpression(e,a);}))*
	;


// binary addition/subtraction (level 3)
additiveExpression returns [Expression e = null] { Expression a,b; }
   :	e=multiplicativeExpression ((PLUS^ a=multiplicativeExpression {e=new AdditionExpression(e,a);} | MINUS^ a=multiplicativeExpression {e=new SubtractionExpression(e,a);}))*
	;


// multiplication/division/modulo (level 2)
multiplicativeExpression returns [Expression e = null] { Expression a,b; }
	:	e=unaryExpression ((STAR^ a=unaryExpression {e=new MultiplicationExpression(e,a);} | DIV^ a=unaryExpression {e=new DivisionExpression(e,a);} | MOD^ a=unaryExpression {e=new ModuloExpression(e,a);} ))*
	;


unaryExpression returns [Expression e = null] { Expression a,b; }
	:	MINUS^ {#MINUS.setType(UNARY_MINUS);} a=unaryExpression {e=new NegateExpression(a);}
	|	PLUS^  {#PLUS.setType(UNARY_PLUS);} e=unaryExpression
	|	e=unaryExpressionNotPlusMinus
	;

unaryExpressionNotPlusMinus returns [Expression e = null] { Expression a; Type t; }
	:	BNOT^ a=unaryExpression {e=new NotExpression(a);}
	|	LNOT^ a=unaryExpression {e=new LogicalNotExpression(a);}

		// use predicate to skip cases like: (int.class)
    |   (LPAREN builtInTypeSpec[true] RPAREN) =>
        lpb:LPAREN^ {#lpb.setType(TYPECAST);} t=builtInTypeSpec[true] RPAREN!
        a=unaryExpression {e=new TypeCastExpression(t,a);}

    |	e=primaryExpression
	;

// the basic element of an expression
primaryExpression returns [Expression e = null; String i = null;]
	:	e=constant
	|	i=identifier {e=new IdentifierExpression(i);}
	|	"true" { e=new ConstantBoolean(true); }
	|	"false" { e=new ConstantBoolean(false); }
	|	"null" { e=new ConstantNull(); }
    |	LPAREN! e=conditionalExpression RPAREN!
	;

/** Match a, a.b.c refs
 */
identPrimary returns [Expression e = null]
	:	IDENT
		(
            options {
				// .ident could match here or in postfixExpression.
				// We do want to match here.  Turn off warning.
				greedy=true;
			}
		:	DOT^ IDENT
		)*
    ;

constant returns [Expression e = null]
	:	l1:NUM_INT {e=new ConstantInteger(l1.getText());}
	|	l2:CHAR_LITERAL {e=new ConstantChar(l2.getText());}
	|	l3:STRING_LITERAL {e=new ConstantString(l3.getText().substring(1, l3.getText().length()-1)); }
	|	l4:NUM_FLOAT {e=new ConstantFloat(l4.getText());}
	|	l5:NUM_LONG {e=new ConstantLong(l5.getText());}
	|	l6:NUM_DOUBLE {e=new ConstantDouble(l6.getText());}
	;


//----------------------------------------------------------------------------
// The Java scanner
//----------------------------------------------------------------------------
class JavaLexer extends Lexer;

options {
	exportVocab=Java;      // call the vocabulary "Java"
	testLiterals=false;    // don't automatically test for literals
	k=4;                   // four characters of lookahead
	charVocabulary='\u0003'..'\uFFFF';
	// without inlining some bitset tests, couldn't do unicode;
	// I need to make ANTLR generate smaller bitsets; see
	// bottom of JavaLexer.java
	codeGenBitsetTestThreshold=20;
}



// OPERATORS
QUESTION		:	'?'		;
LPAREN			:	'('		;
RPAREN			:	')'		;
LBRACK			:	'['		;
RBRACK			:	']'		;
LCURLY			:	'{'		;
RCURLY			:	'}'		;
COLON			:	':'		;
COMMA			:	','		;
//DOT 			:	'.'		;
ASSIGN			:	'='		;
EQUAL			:	"=="	;
LNOT			:	'!'		;
BNOT			:	'~'		;
NOT_EQUAL		:	"!="	;
DIV				:	'/'		;
DIV_ASSIGN		:	"/="	;
PLUS			:	'+'		;
PLUS_ASSIGN		:	"+="	;
INC				:	"++"	;
MINUS			:	'-'		;
MINUS_ASSIGN	:	"-="	;
DEC				:	"--"	;
STAR			:	'*'		;
STAR_ASSIGN		:	"*="	;
MOD				:	'%'		;
MOD_ASSIGN		:	"%="	;
SR				:	">>"	;
SR_ASSIGN		:	">>="	;
BSR				:	">>>"	;
BSR_ASSIGN		:	">>>="	;
GE				:	">="	;
GT				:	">"		;
SL				:	"<<"	;
SL_ASSIGN		:	"<<="	;
LE				:	"<="	;
LT				:	'<'		;
BXOR			:	'^'		;
BXOR_ASSIGN		:	"^="	;
BOR				:	'|'		;
BOR_ASSIGN		:	"|="	;
LOR				:	"||"	;
BAND			:	'&'		;
BAND_ASSIGN		:	"&="	;
LAND			:	"&&"	;
SEMI			:	';'		;


// Whitespace -- ignored
WS	:	(	' '
		|	'\t'
		|	'\f'
			// handle newlines
		|	(	options {generateAmbigWarnings=false;}
			:	"\r\n"  // Evil DOS
			|	'\r'    // Macintosh
			|	'\n'    // Unix (the right way)
			)
			{ newline(); }
		)+
		{ _ttype = Token.SKIP; }
	;

// Single-line comments
SL_COMMIT
	:	"//"
		(~('\n'|'\r'))* ('\n'|'\r'('\n')?)
		{$setType(Token.SKIP); newline();}
	;

// multiple-line comments
ML_COMMENT
	:	"/*"
		(	/*	'\r' '\n' can be matched in one alternative or by matching
				'\r' in one iteration and '\n' in another.  I am trying to
				handle any flavor of newline that comes in, but the language
				that allows both "\r\n" and "\r" and "\n" to all be valid
				newline is ambiguous.  Consequently, the resulting grammar
				must be ambiguous.  I'm shutting this warning off.
			 */
			options {
				generateAmbigWarnings=false;
			}
		:
			{ LA(2)!='/' }? '*'
		|	'\r' '\n'		{newline();}
		|	'\r'			{newline();}
		|	'\n'			{newline();}
		|	~('*'|'\n'|'\r')
		)*
		"*/"
		{$setType(Token.SKIP);}
	;


// character literals
CHAR_LITERAL
	:	'\'' ( ESC | ~('\''|'\n'|'\r'|'\\') ) '\''
	;

// string literals
STRING_LITERAL
	:	'"' (ESC|~('"'|'\\'|'\n'|'\r'))* '"'
	;


// escape sequence -- note that this is protected; it can only be called
//   from another lexer rule -- it will not ever directly return a token to
//   the parser
// There are various ambiguities hushed in this rule.  The optional
// '0'...'9' digit matches should be matched here rather than letting
// them go back to STRING_LITERAL to be matched.  ANTLR does the
// right thing by matching immediately; hence, it's ok to shut off
// the FOLLOW ambig warnings.
protected
ESC
	:	'\\'
		(	'n'
		|	'r'
		|	't'
		|	'b'
		|	'f'
		|	'"'
		|	'\''
		|	'\\'
		|	('u')+ HEX_DIGIT HEX_DIGIT HEX_DIGIT HEX_DIGIT
		|	'0'..'3'
			(
				options {
					warnWhenFollowAmbig = false;
				}
			:	'0'..'7'
				(
					options {
						warnWhenFollowAmbig = false;
					}
				:	'0'..'7'
				)?
			)?
		|	'4'..'7'
			(
				options {
					warnWhenFollowAmbig = false;
				}
			:	'0'..'7'
			)?
		)
	;


// hexadecimal digit (again, note it's protected!)
protected
HEX_DIGIT
	:	('0'..'9'|'A'..'F'|'a'..'f')
	;


// a dummy rule to force vocabulary to be all characters (except special
//   ones that ANTLR uses internally (0 to 2)
protected
VOCAB
	:	'\3'..'\377'
	;


// an identifier.  Note that testLiterals is set to true!  This means
// that after we match the rule, we look in the literals table to see
// if it's a literal or really an identifer
IDENT
	options {testLiterals=true;}
	:	('a'..'z'|'A'..'Z'|'_'|'$') ('a'..'z'|'A'..'Z'|'_'|'0'..'9'|'$')*
	;


// a numeric literal
NUM_INT
	{boolean isDecimal=false; Token t=null;}
    :   '.' {_ttype = DOT;}
            (	('0'..'9')+ (EXPONENT)? (f1:FLOAT_SUFFIX {t=f1;})?
                {
				if (t != null && t.getText().toUpperCase().indexOf('F')>=0) {
                	_ttype = NUM_FLOAT;
				}
				else {
                	_ttype = NUM_DOUBLE; // assume double
				}
				}
            )?

	|	(	'0' {isDecimal = true;} // special case for just '0'
			(	('x'|'X')
				(											// hex
					// the 'e'|'E' and float suffix stuff look
					// like hex digits, hence the (...)+ doesn't
					// know when to stop: ambig.  ANTLR resolves
					// it correctly by matching immediately.  It
					// is therefor ok to hush warning.
					options {
						warnWhenFollowAmbig=false;
					}
				:	HEX_DIGIT
				)+

			|	//float or double with leading zero
				(('0'..'9')+ ('.'|EXPONENT|FLOAT_SUFFIX)) => ('0'..'9')+

			|	('0'..'7')+									// octal
			)?
		|	('1'..'9') ('0'..'9')*  {isDecimal=true;}		// non-zero decimal
		)
		(	('l'|'L') { _ttype = NUM_LONG; }

		// only check to see if it's a float if looks like decimal so far
		|	{isDecimal}?
            (   '.' ('0'..'9')* (EXPONENT)? (f2:FLOAT_SUFFIX {t=f2;})?
            |   EXPONENT (f3:FLOAT_SUFFIX {t=f3;})?
            |   f4:FLOAT_SUFFIX {t=f4;}
            )
            {
			if (t != null && t.getText().toUpperCase() .indexOf('F') >= 0) {
                _ttype = NUM_FLOAT;
			}
            else {
	           	_ttype = NUM_DOUBLE; // assume double
			}
			}
        )?
	;


// a couple protected methods to assist in matching floating point numbers
protected
EXPONENT
	:	('e'|'E') ('+'|'-')? ('0'..'9')+
	;


protected
FLOAT_SUFFIX
	:	'f'|'F'|'d'|'D'
	;
