// $ANTLR 2.7.7 (20080530): "java-expression.g" -> "JavaRecognizer.java"$
 
   package gnu.classpath.tools.gjdoc.expr; 

import antlr.TokenBuffer;
import antlr.TokenStreamException;
import antlr.TokenStreamIOException;
import antlr.ANTLRException;
import antlr.LLkParser;
import antlr.Token;
import antlr.TokenStream;
import antlr.RecognitionException;
import antlr.NoViableAltException;
import antlr.MismatchedTokenException;
import antlr.SemanticException;
import antlr.ParserSharedInputState;
import antlr.collections.impl.BitSet;
import antlr.collections.AST;
import java.util.Hashtable;
import antlr.ASTFactory;
import antlr.ASTPair;
import antlr.collections.impl.ASTArray;

public class JavaRecognizer extends antlr.LLkParser       implements JavaTokenTypes
 {

protected JavaRecognizer(TokenBuffer tokenBuf, int k) {
  super(tokenBuf,k);
  tokenNames = _tokenNames;
  buildTokenTypeASTClassMap();
  astFactory = new ASTFactory(getTokenTypeToASTClassMap());
}

public JavaRecognizer(TokenBuffer tokenBuf) {
  this(tokenBuf,2);
}

protected JavaRecognizer(TokenStream lexer, int k) {
  super(lexer,k);
  tokenNames = _tokenNames;
  buildTokenTypeASTClassMap();
  astFactory = new ASTFactory(getTokenTypeToASTClassMap());
}

public JavaRecognizer(TokenStream lexer) {
  this(lexer,2);
}

public JavaRecognizer(ParserSharedInputState state) {
  super(state,2);
  tokenNames = _tokenNames;
  buildTokenTypeASTClassMap();
  astFactory = new ASTFactory(getTokenTypeToASTClassMap());
}

	public final Type  builtInTypeSpec(
		boolean addImagNode
	) throws RecognitionException, TokenStreamException {
		Type t = null;
		
		returnAST = null;
		ASTPair currentAST = new ASTPair();
		AST builtInTypeSpec_AST = null;
		Token  lb = null;
		AST lb_AST = null;
		
		t=builtInType();
		astFactory.addASTChild(currentAST, returnAST);
		{
		_loop3:
		do {
			if ((LA(1)==LBRACK)) {
				lb = LT(1);
				lb_AST = astFactory.create(lb);
				astFactory.makeASTRoot(currentAST, lb_AST);
				match(LBRACK);
				if ( inputState.guessing==0 ) {
					lb_AST.setType(ARRAY_DECLARATOR);
				}
				match(RBRACK);
			}
			else {
				break _loop3;
			}
			
		} while (true);
		}
		if ( inputState.guessing==0 ) {
			builtInTypeSpec_AST = (AST)currentAST.root;
			
						if ( addImagNode ) {
							builtInTypeSpec_AST = (AST)astFactory.make( (new ASTArray(2)).add(astFactory.create(TYPE,"TYPE")).add(builtInTypeSpec_AST));
						}
					
			currentAST.root = builtInTypeSpec_AST;
			currentAST.child = builtInTypeSpec_AST!=null &&builtInTypeSpec_AST.getFirstChild()!=null ?
				builtInTypeSpec_AST.getFirstChild() : builtInTypeSpec_AST;
			currentAST.advanceChildToEnd();
		}
		builtInTypeSpec_AST = (AST)currentAST.root;
		returnAST = builtInTypeSpec_AST;
		return t;
	}
	
	public final Type  builtInType() throws RecognitionException, TokenStreamException {
		Type t = null;
		
		returnAST = null;
		ASTPair currentAST = new ASTPair();
		AST builtInType_AST = null;
		
		switch ( LA(1)) {
		case LITERAL_void:
		{
			AST tmp2_AST = null;
			tmp2_AST = astFactory.create(LT(1));
			astFactory.addASTChild(currentAST, tmp2_AST);
			match(LITERAL_void);
			if ( inputState.guessing==0 ) {
				t=Type.VOID;
			}
			builtInType_AST = (AST)currentAST.root;
			break;
		}
		case LITERAL_boolean:
		{
			AST tmp3_AST = null;
			tmp3_AST = astFactory.create(LT(1));
			astFactory.addASTChild(currentAST, tmp3_AST);
			match(LITERAL_boolean);
			if ( inputState.guessing==0 ) {
				t=Type.BOOLEAN;
			}
			builtInType_AST = (AST)currentAST.root;
			break;
		}
		case LITERAL_byte:
		{
			AST tmp4_AST = null;
			tmp4_AST = astFactory.create(LT(1));
			astFactory.addASTChild(currentAST, tmp4_AST);
			match(LITERAL_byte);
			if ( inputState.guessing==0 ) {
				t=Type.BYTE;
			}
			builtInType_AST = (AST)currentAST.root;
			break;
		}
		case LITERAL_char:
		{
			AST tmp5_AST = null;
			tmp5_AST = astFactory.create(LT(1));
			astFactory.addASTChild(currentAST, tmp5_AST);
			match(LITERAL_char);
			if ( inputState.guessing==0 ) {
				t=Type.CHAR;
			}
			builtInType_AST = (AST)currentAST.root;
			break;
		}
		case LITERAL_short:
		{
			AST tmp6_AST = null;
			tmp6_AST = astFactory.create(LT(1));
			astFactory.addASTChild(currentAST, tmp6_AST);
			match(LITERAL_short);
			if ( inputState.guessing==0 ) {
				t=Type.SHORT;
			}
			builtInType_AST = (AST)currentAST.root;
			break;
		}
		case LITERAL_int:
		{
			AST tmp7_AST = null;
			tmp7_AST = astFactory.create(LT(1));
			astFactory.addASTChild(currentAST, tmp7_AST);
			match(LITERAL_int);
			if ( inputState.guessing==0 ) {
				t=Type.INTEGER;
			}
			builtInType_AST = (AST)currentAST.root;
			break;
		}
		case LITERAL_float:
		{
			AST tmp8_AST = null;
			tmp8_AST = astFactory.create(LT(1));
			astFactory.addASTChild(currentAST, tmp8_AST);
			match(LITERAL_float);
			if ( inputState.guessing==0 ) {
				t=Type.FLOAT;
			}
			builtInType_AST = (AST)currentAST.root;
			break;
		}
		case LITERAL_long:
		{
			AST tmp9_AST = null;
			tmp9_AST = astFactory.create(LT(1));
			astFactory.addASTChild(currentAST, tmp9_AST);
			match(LITERAL_long);
			if ( inputState.guessing==0 ) {
				t=Type.LONG;
			}
			builtInType_AST = (AST)currentAST.root;
			break;
		}
		case LITERAL_double:
		{
			AST tmp10_AST = null;
			tmp10_AST = astFactory.create(LT(1));
			astFactory.addASTChild(currentAST, tmp10_AST);
			match(LITERAL_double);
			if ( inputState.guessing==0 ) {
				t=Type.DOUBLE;
			}
			builtInType_AST = (AST)currentAST.root;
			break;
		}
		case LITERAL_String:
		{
			AST tmp11_AST = null;
			tmp11_AST = astFactory.create(LT(1));
			astFactory.addASTChild(currentAST, tmp11_AST);
			match(LITERAL_String);
			if ( inputState.guessing==0 ) {
				t=Type.STRING;
			}
			builtInType_AST = (AST)currentAST.root;
			break;
		}
		default:
		{
			throw new NoViableAltException(LT(1), getFilename());
		}
		}
		returnAST = builtInType_AST;
		return t;
	}
	
	public final Type  type() throws RecognitionException, TokenStreamException {
		Type t;
		
		returnAST = null;
		ASTPair currentAST = new ASTPair();
		AST type_AST = null;
		
		t=builtInType();
		astFactory.addASTChild(currentAST, returnAST);
		type_AST = (AST)currentAST.root;
		returnAST = type_AST;
		return t;
	}
	
	public final String  identifier() throws RecognitionException, TokenStreamException {
		String s = null;;
		
		returnAST = null;
		ASTPair currentAST = new ASTPair();
		AST identifier_AST = null;
		Token  i = null;
		AST i_AST = null;
		Token  i2 = null;
		AST i2_AST = null;
		
		i = LT(1);
		i_AST = astFactory.create(i);
		astFactory.addASTChild(currentAST, i_AST);
		match(IDENT);
		if ( inputState.guessing==0 ) {
			s=i.getText();
		}
		{
		_loop8:
		do {
			if ((LA(1)==DOT)) {
				AST tmp12_AST = null;
				tmp12_AST = astFactory.create(LT(1));
				astFactory.makeASTRoot(currentAST, tmp12_AST);
				match(DOT);
				i2 = LT(1);
				i2_AST = astFactory.create(i2);
				astFactory.addASTChild(currentAST, i2_AST);
				match(IDENT);
				if ( inputState.guessing==0 ) {
					s+="."+i2.getText();
				}
			}
			else {
				break _loop8;
			}
			
		} while (true);
		}
		identifier_AST = (AST)currentAST.root;
		returnAST = identifier_AST;
		return s;
	}
	
	public final Expression  expression() throws RecognitionException, TokenStreamException {
		Expression e = null;
		
		returnAST = null;
		ASTPair currentAST = new ASTPair();
		AST expression_AST = null;
		
		e=conditionalExpression();
		astFactory.addASTChild(currentAST, returnAST);
		match(Token.EOF_TYPE);
		expression_AST = (AST)currentAST.root;
		returnAST = expression_AST;
		return e;
	}
	
	public final Expression  conditionalExpression() throws RecognitionException, TokenStreamException {
		Expression e = null;
		
		returnAST = null;
		ASTPair currentAST = new ASTPair();
		AST conditionalExpression_AST = null;
		Expression a,b,c;
		
		e=logicalOrExpression();
		astFactory.addASTChild(currentAST, returnAST);
		{
		switch ( LA(1)) {
		case QUESTION:
		{
			AST tmp14_AST = null;
			tmp14_AST = astFactory.create(LT(1));
			astFactory.makeASTRoot(currentAST, tmp14_AST);
			match(QUESTION);
			b=conditionalExpression();
			astFactory.addASTChild(currentAST, returnAST);
			match(COLON);
			c=conditionalExpression();
			astFactory.addASTChild(currentAST, returnAST);
			if ( inputState.guessing==0 ) {
				e=new ConditionalExpression(e,b,c);
			}
			break;
		}
		case EOF:
		case COLON:
		case RPAREN:
		{
			break;
		}
		default:
		{
			throw new NoViableAltException(LT(1), getFilename());
		}
		}
		}
		conditionalExpression_AST = (AST)currentAST.root;
		returnAST = conditionalExpression_AST;
		return e;
	}
	
	public final Expression  logicalOrExpression() throws RecognitionException, TokenStreamException {
		Expression e = null;
		
		returnAST = null;
		ASTPair currentAST = new ASTPair();
		AST logicalOrExpression_AST = null;
		Expression a,b;
		
		e=logicalAndExpression();
		astFactory.addASTChild(currentAST, returnAST);
		{
		_loop14:
		do {
			if ((LA(1)==LOR)) {
				AST tmp16_AST = null;
				tmp16_AST = astFactory.create(LT(1));
				astFactory.makeASTRoot(currentAST, tmp16_AST);
				match(LOR);
				b=logicalAndExpression();
				astFactory.addASTChild(currentAST, returnAST);
				if ( inputState.guessing==0 ) {
					e=new LogicalOrExpression(e,b);
				}
			}
			else {
				break _loop14;
			}
			
		} while (true);
		}
		logicalOrExpression_AST = (AST)currentAST.root;
		returnAST = logicalOrExpression_AST;
		return e;
	}
	
	public final Expression  logicalAndExpression() throws RecognitionException, TokenStreamException {
		Expression e = null;
		
		returnAST = null;
		ASTPair currentAST = new ASTPair();
		AST logicalAndExpression_AST = null;
		Expression a,b;
		
		e=inclusiveOrExpression();
		astFactory.addASTChild(currentAST, returnAST);
		{
		_loop17:
		do {
			if ((LA(1)==LAND)) {
				AST tmp17_AST = null;
				tmp17_AST = astFactory.create(LT(1));
				astFactory.makeASTRoot(currentAST, tmp17_AST);
				match(LAND);
				b=inclusiveOrExpression();
				astFactory.addASTChild(currentAST, returnAST);
				if ( inputState.guessing==0 ) {
					e=new LogicalAndExpression(e,b);
				}
			}
			else {
				break _loop17;
			}
			
		} while (true);
		}
		logicalAndExpression_AST = (AST)currentAST.root;
		returnAST = logicalAndExpression_AST;
		return e;
	}
	
	public final Expression  inclusiveOrExpression() throws RecognitionException, TokenStreamException {
		Expression e = null;
		
		returnAST = null;
		ASTPair currentAST = new ASTPair();
		AST inclusiveOrExpression_AST = null;
		Expression a,b;
		
		e=exclusiveOrExpression();
		astFactory.addASTChild(currentAST, returnAST);
		{
		_loop20:
		do {
			if ((LA(1)==BOR)) {
				AST tmp18_AST = null;
				tmp18_AST = astFactory.create(LT(1));
				astFactory.makeASTRoot(currentAST, tmp18_AST);
				match(BOR);
				b=exclusiveOrExpression();
				astFactory.addASTChild(currentAST, returnAST);
				if ( inputState.guessing==0 ) {
					e=new InclusiveOrExpression(e,b);
				}
			}
			else {
				break _loop20;
			}
			
		} while (true);
		}
		inclusiveOrExpression_AST = (AST)currentAST.root;
		returnAST = inclusiveOrExpression_AST;
		return e;
	}
	
	public final Expression  exclusiveOrExpression() throws RecognitionException, TokenStreamException {
		Expression e = null;
		
		returnAST = null;
		ASTPair currentAST = new ASTPair();
		AST exclusiveOrExpression_AST = null;
		Expression a,b;
		
		e=andExpression();
		astFactory.addASTChild(currentAST, returnAST);
		{
		_loop23:
		do {
			if ((LA(1)==BXOR)) {
				AST tmp19_AST = null;
				tmp19_AST = astFactory.create(LT(1));
				astFactory.makeASTRoot(currentAST, tmp19_AST);
				match(BXOR);
				b=andExpression();
				astFactory.addASTChild(currentAST, returnAST);
				if ( inputState.guessing==0 ) {
					e=new ExclusiveOrExpression(e,b);
				}
			}
			else {
				break _loop23;
			}
			
		} while (true);
		}
		exclusiveOrExpression_AST = (AST)currentAST.root;
		returnAST = exclusiveOrExpression_AST;
		return e;
	}
	
	public final Expression  andExpression() throws RecognitionException, TokenStreamException {
		Expression e = null;
		
		returnAST = null;
		ASTPair currentAST = new ASTPair();
		AST andExpression_AST = null;
		Expression a,b;
		
		e=equalityExpression();
		astFactory.addASTChild(currentAST, returnAST);
		{
		_loop26:
		do {
			if ((LA(1)==BAND)) {
				AST tmp20_AST = null;
				tmp20_AST = astFactory.create(LT(1));
				astFactory.makeASTRoot(currentAST, tmp20_AST);
				match(BAND);
				b=equalityExpression();
				astFactory.addASTChild(currentAST, returnAST);
				if ( inputState.guessing==0 ) {
					e=new AndExpression(e,b);
				}
			}
			else {
				break _loop26;
			}
			
		} while (true);
		}
		andExpression_AST = (AST)currentAST.root;
		returnAST = andExpression_AST;
		return e;
	}
	
	public final Expression  equalityExpression() throws RecognitionException, TokenStreamException {
		Expression e = null;
		
		returnAST = null;
		ASTPair currentAST = new ASTPair();
		AST equalityExpression_AST = null;
		Expression a,b;
		
		e=relationalExpression();
		astFactory.addASTChild(currentAST, returnAST);
		{
		_loop30:
		do {
			if ((LA(1)==NOT_EQUAL||LA(1)==EQUAL)) {
				{
				switch ( LA(1)) {
				case NOT_EQUAL:
				{
					AST tmp21_AST = null;
					tmp21_AST = astFactory.create(LT(1));
					astFactory.makeASTRoot(currentAST, tmp21_AST);
					match(NOT_EQUAL);
					a=relationalExpression();
					astFactory.addASTChild(currentAST, returnAST);
					if ( inputState.guessing==0 ) {
						e=new NotEqualExpression(e,a);
					}
					break;
				}
				case EQUAL:
				{
					AST tmp22_AST = null;
					tmp22_AST = astFactory.create(LT(1));
					astFactory.makeASTRoot(currentAST, tmp22_AST);
					match(EQUAL);
					a=relationalExpression();
					astFactory.addASTChild(currentAST, returnAST);
					if ( inputState.guessing==0 ) {
						e=new EqualExpression(e,a);
					}
					break;
				}
				default:
				{
					throw new NoViableAltException(LT(1), getFilename());
				}
				}
				}
			}
			else {
				break _loop30;
			}
			
		} while (true);
		}
		equalityExpression_AST = (AST)currentAST.root;
		returnAST = equalityExpression_AST;
		return e;
	}
	
	public final Expression  relationalExpression() throws RecognitionException, TokenStreamException {
		Expression e = null;
		
		returnAST = null;
		ASTPair currentAST = new ASTPair();
		AST relationalExpression_AST = null;
		Expression a,b;
		
		e=shiftExpression();
		astFactory.addASTChild(currentAST, returnAST);
		{
		{
		_loop35:
		do {
			if (((LA(1) >= LT && LA(1) <= GE))) {
				{
				switch ( LA(1)) {
				case LT:
				{
					AST tmp23_AST = null;
					tmp23_AST = astFactory.create(LT(1));
					astFactory.makeASTRoot(currentAST, tmp23_AST);
					match(LT);
					a=shiftExpression();
					astFactory.addASTChild(currentAST, returnAST);
					if ( inputState.guessing==0 ) {
						e=new LessThanExpression(e,a);
					}
					break;
				}
				case GT:
				{
					AST tmp24_AST = null;
					tmp24_AST = astFactory.create(LT(1));
					astFactory.makeASTRoot(currentAST, tmp24_AST);
					match(GT);
					a=shiftExpression();
					astFactory.addASTChild(currentAST, returnAST);
					if ( inputState.guessing==0 ) {
						e=new GreaterThanExpression(e,a);
					}
					break;
				}
				case LE:
				{
					AST tmp25_AST = null;
					tmp25_AST = astFactory.create(LT(1));
					astFactory.makeASTRoot(currentAST, tmp25_AST);
					match(LE);
					a=shiftExpression();
					astFactory.addASTChild(currentAST, returnAST);
					if ( inputState.guessing==0 ) {
						e=new LessThanOrEqualExpression(e,a);
					}
					break;
				}
				case GE:
				{
					AST tmp26_AST = null;
					tmp26_AST = astFactory.create(LT(1));
					astFactory.makeASTRoot(currentAST, tmp26_AST);
					match(GE);
					a=shiftExpression();
					astFactory.addASTChild(currentAST, returnAST);
					if ( inputState.guessing==0 ) {
						e=new GreaterThanOrEqualExpression(e,a);
					}
					break;
				}
				default:
				{
					throw new NoViableAltException(LT(1), getFilename());
				}
				}
				}
			}
			else {
				break _loop35;
			}
			
		} while (true);
		}
		}
		relationalExpression_AST = (AST)currentAST.root;
		returnAST = relationalExpression_AST;
		return e;
	}
	
	public final Expression  shiftExpression() throws RecognitionException, TokenStreamException {
		Expression e = null;
		
		returnAST = null;
		ASTPair currentAST = new ASTPair();
		AST shiftExpression_AST = null;
		Expression a,b;
		
		e=additiveExpression();
		astFactory.addASTChild(currentAST, returnAST);
		{
		_loop39:
		do {
			if (((LA(1) >= SL && LA(1) <= BSR))) {
				{
				switch ( LA(1)) {
				case SL:
				{
					AST tmp27_AST = null;
					tmp27_AST = astFactory.create(LT(1));
					astFactory.makeASTRoot(currentAST, tmp27_AST);
					match(SL);
					a=additiveExpression();
					astFactory.addASTChild(currentAST, returnAST);
					if ( inputState.guessing==0 ) {
						e=new ShiftLeftExpression(e,a);
					}
					break;
				}
				case SR:
				{
					AST tmp28_AST = null;
					tmp28_AST = astFactory.create(LT(1));
					astFactory.makeASTRoot(currentAST, tmp28_AST);
					match(SR);
					a=additiveExpression();
					astFactory.addASTChild(currentAST, returnAST);
					if ( inputState.guessing==0 ) {
						e=new ShiftRightExpression(e,a);
					}
					break;
				}
				case BSR:
				{
					AST tmp29_AST = null;
					tmp29_AST = astFactory.create(LT(1));
					astFactory.makeASTRoot(currentAST, tmp29_AST);
					match(BSR);
					a=additiveExpression();
					astFactory.addASTChild(currentAST, returnAST);
					if ( inputState.guessing==0 ) {
						e=new BitShiftRightExpression(e,a);
					}
					break;
				}
				default:
				{
					throw new NoViableAltException(LT(1), getFilename());
				}
				}
				}
			}
			else {
				break _loop39;
			}
			
		} while (true);
		}
		shiftExpression_AST = (AST)currentAST.root;
		returnAST = shiftExpression_AST;
		return e;
	}
	
	public final Expression  additiveExpression() throws RecognitionException, TokenStreamException {
		Expression e = null;
		
		returnAST = null;
		ASTPair currentAST = new ASTPair();
		AST additiveExpression_AST = null;
		Expression a,b;
		
		e=multiplicativeExpression();
		astFactory.addASTChild(currentAST, returnAST);
		{
		_loop43:
		do {
			if ((LA(1)==PLUS||LA(1)==MINUS)) {
				{
				switch ( LA(1)) {
				case PLUS:
				{
					AST tmp30_AST = null;
					tmp30_AST = astFactory.create(LT(1));
					astFactory.makeASTRoot(currentAST, tmp30_AST);
					match(PLUS);
					a=multiplicativeExpression();
					astFactory.addASTChild(currentAST, returnAST);
					if ( inputState.guessing==0 ) {
						e=new AdditionExpression(e,a);
					}
					break;
				}
				case MINUS:
				{
					AST tmp31_AST = null;
					tmp31_AST = astFactory.create(LT(1));
					astFactory.makeASTRoot(currentAST, tmp31_AST);
					match(MINUS);
					a=multiplicativeExpression();
					astFactory.addASTChild(currentAST, returnAST);
					if ( inputState.guessing==0 ) {
						e=new SubtractionExpression(e,a);
					}
					break;
				}
				default:
				{
					throw new NoViableAltException(LT(1), getFilename());
				}
				}
				}
			}
			else {
				break _loop43;
			}
			
		} while (true);
		}
		additiveExpression_AST = (AST)currentAST.root;
		returnAST = additiveExpression_AST;
		return e;
	}
	
	public final Expression  multiplicativeExpression() throws RecognitionException, TokenStreamException {
		Expression e = null;
		
		returnAST = null;
		ASTPair currentAST = new ASTPair();
		AST multiplicativeExpression_AST = null;
		Expression a,b;
		
		e=unaryExpression();
		astFactory.addASTChild(currentAST, returnAST);
		{
		_loop47:
		do {
			if (((LA(1) >= STAR && LA(1) <= MOD))) {
				{
				switch ( LA(1)) {
				case STAR:
				{
					AST tmp32_AST = null;
					tmp32_AST = astFactory.create(LT(1));
					astFactory.makeASTRoot(currentAST, tmp32_AST);
					match(STAR);
					a=unaryExpression();
					astFactory.addASTChild(currentAST, returnAST);
					if ( inputState.guessing==0 ) {
						e=new MultiplicationExpression(e,a);
					}
					break;
				}
				case DIV:
				{
					AST tmp33_AST = null;
					tmp33_AST = astFactory.create(LT(1));
					astFactory.makeASTRoot(currentAST, tmp33_AST);
					match(DIV);
					a=unaryExpression();
					astFactory.addASTChild(currentAST, returnAST);
					if ( inputState.guessing==0 ) {
						e=new DivisionExpression(e,a);
					}
					break;
				}
				case MOD:
				{
					AST tmp34_AST = null;
					tmp34_AST = astFactory.create(LT(1));
					astFactory.makeASTRoot(currentAST, tmp34_AST);
					match(MOD);
					a=unaryExpression();
					astFactory.addASTChild(currentAST, returnAST);
					if ( inputState.guessing==0 ) {
						e=new ModuloExpression(e,a);
					}
					break;
				}
				default:
				{
					throw new NoViableAltException(LT(1), getFilename());
				}
				}
				}
			}
			else {
				break _loop47;
			}
			
		} while (true);
		}
		multiplicativeExpression_AST = (AST)currentAST.root;
		returnAST = multiplicativeExpression_AST;
		return e;
	}
	
	public final Expression  unaryExpression() throws RecognitionException, TokenStreamException {
		Expression e = null;
		
		returnAST = null;
		ASTPair currentAST = new ASTPair();
		AST unaryExpression_AST = null;
		Expression a,b;
		
		switch ( LA(1)) {
		case MINUS:
		{
			AST tmp35_AST = null;
			tmp35_AST = astFactory.create(LT(1));
			astFactory.makeASTRoot(currentAST, tmp35_AST);
			match(MINUS);
			if ( inputState.guessing==0 ) {
				tmp35_AST.setType(UNARY_MINUS);
			}
			a=unaryExpression();
			astFactory.addASTChild(currentAST, returnAST);
			if ( inputState.guessing==0 ) {
				e=new NegateExpression(a);
			}
			unaryExpression_AST = (AST)currentAST.root;
			break;
		}
		case PLUS:
		{
			AST tmp36_AST = null;
			tmp36_AST = astFactory.create(LT(1));
			astFactory.makeASTRoot(currentAST, tmp36_AST);
			match(PLUS);
			if ( inputState.guessing==0 ) {
				tmp36_AST.setType(UNARY_PLUS);
			}
			e=unaryExpression();
			astFactory.addASTChild(currentAST, returnAST);
			unaryExpression_AST = (AST)currentAST.root;
			break;
		}
		case IDENT:
		case BNOT:
		case LNOT:
		case LPAREN:
		case LITERAL_true:
		case LITERAL_false:
		case LITERAL_null:
		case NUM_INT:
		case CHAR_LITERAL:
		case STRING_LITERAL:
		case NUM_FLOAT:
		case NUM_LONG:
		case NUM_DOUBLE:
		{
			e=unaryExpressionNotPlusMinus();
			astFactory.addASTChild(currentAST, returnAST);
			unaryExpression_AST = (AST)currentAST.root;
			break;
		}
		default:
		{
			throw new NoViableAltException(LT(1), getFilename());
		}
		}
		returnAST = unaryExpression_AST;
		return e;
	}
	
	public final Expression  unaryExpressionNotPlusMinus() throws RecognitionException, TokenStreamException {
		Expression e = null;
		
		returnAST = null;
		ASTPair currentAST = new ASTPair();
		AST unaryExpressionNotPlusMinus_AST = null;
		Token  lpb = null;
		AST lpb_AST = null;
		Expression a; Type t;
		
		switch ( LA(1)) {
		case BNOT:
		{
			AST tmp37_AST = null;
			tmp37_AST = astFactory.create(LT(1));
			astFactory.makeASTRoot(currentAST, tmp37_AST);
			match(BNOT);
			a=unaryExpression();
			astFactory.addASTChild(currentAST, returnAST);
			if ( inputState.guessing==0 ) {
				e=new NotExpression(a);
			}
			unaryExpressionNotPlusMinus_AST = (AST)currentAST.root;
			break;
		}
		case LNOT:
		{
			AST tmp38_AST = null;
			tmp38_AST = astFactory.create(LT(1));
			astFactory.makeASTRoot(currentAST, tmp38_AST);
			match(LNOT);
			a=unaryExpression();
			astFactory.addASTChild(currentAST, returnAST);
			if ( inputState.guessing==0 ) {
				e=new LogicalNotExpression(a);
			}
			unaryExpressionNotPlusMinus_AST = (AST)currentAST.root;
			break;
		}
		default:
			boolean synPredMatched51 = false;
			if (((LA(1)==LPAREN) && ((LA(2) >= LITERAL_void && LA(2) <= LITERAL_String)))) {
				int _m51 = mark();
				synPredMatched51 = true;
				inputState.guessing++;
				try {
					{
					match(LPAREN);
					builtInTypeSpec(true);
					match(RPAREN);
					}
				}
				catch (RecognitionException pe) {
					synPredMatched51 = false;
				}
				rewind(_m51);
inputState.guessing--;
			}
			if ( synPredMatched51 ) {
				lpb = LT(1);
				lpb_AST = astFactory.create(lpb);
				astFactory.makeASTRoot(currentAST, lpb_AST);
				match(LPAREN);
				if ( inputState.guessing==0 ) {
					lpb_AST.setType(TYPECAST);
				}
				t=builtInTypeSpec(true);
				astFactory.addASTChild(currentAST, returnAST);
				match(RPAREN);
				a=unaryExpression();
				astFactory.addASTChild(currentAST, returnAST);
				if ( inputState.guessing==0 ) {
					e=new TypeCastExpression(t,a);
				}
				unaryExpressionNotPlusMinus_AST = (AST)currentAST.root;
			}
			else if ((_tokenSet_0.member(LA(1))) && (_tokenSet_1.member(LA(2)))) {
				e=primaryExpression();
				astFactory.addASTChild(currentAST, returnAST);
				unaryExpressionNotPlusMinus_AST = (AST)currentAST.root;
			}
		else {
			throw new NoViableAltException(LT(1), getFilename());
		}
		}
		returnAST = unaryExpressionNotPlusMinus_AST;
		return e;
	}
	
	public final Expression  primaryExpression() throws RecognitionException, TokenStreamException {
		Expression e = null; String i = null;;
		
		returnAST = null;
		ASTPair currentAST = new ASTPair();
		AST primaryExpression_AST = null;
		
		switch ( LA(1)) {
		case NUM_INT:
		case CHAR_LITERAL:
		case STRING_LITERAL:
		case NUM_FLOAT:
		case NUM_LONG:
		case NUM_DOUBLE:
		{
			e=constant();
			astFactory.addASTChild(currentAST, returnAST);
			primaryExpression_AST = (AST)currentAST.root;
			break;
		}
		case IDENT:
		{
			i=identifier();
			astFactory.addASTChild(currentAST, returnAST);
			if ( inputState.guessing==0 ) {
				e=new IdentifierExpression(i);
			}
			primaryExpression_AST = (AST)currentAST.root;
			break;
		}
		case LITERAL_true:
		{
			AST tmp40_AST = null;
			tmp40_AST = astFactory.create(LT(1));
			astFactory.addASTChild(currentAST, tmp40_AST);
			match(LITERAL_true);
			if ( inputState.guessing==0 ) {
				e=new ConstantBoolean(true);
			}
			primaryExpression_AST = (AST)currentAST.root;
			break;
		}
		case LITERAL_false:
		{
			AST tmp41_AST = null;
			tmp41_AST = astFactory.create(LT(1));
			astFactory.addASTChild(currentAST, tmp41_AST);
			match(LITERAL_false);
			if ( inputState.guessing==0 ) {
				e=new ConstantBoolean(false);
			}
			primaryExpression_AST = (AST)currentAST.root;
			break;
		}
		case LITERAL_null:
		{
			AST tmp42_AST = null;
			tmp42_AST = astFactory.create(LT(1));
			astFactory.addASTChild(currentAST, tmp42_AST);
			match(LITERAL_null);
			if ( inputState.guessing==0 ) {
				e=new ConstantNull();
			}
			primaryExpression_AST = (AST)currentAST.root;
			break;
		}
		case LPAREN:
		{
			match(LPAREN);
			e=conditionalExpression();
			astFactory.addASTChild(currentAST, returnAST);
			match(RPAREN);
			primaryExpression_AST = (AST)currentAST.root;
			break;
		}
		default:
		{
			throw new NoViableAltException(LT(1), getFilename());
		}
		}
		returnAST = primaryExpression_AST;
		return e;
	}
	
	public final Expression  constant() throws RecognitionException, TokenStreamException {
		Expression e = null;
		
		returnAST = null;
		ASTPair currentAST = new ASTPair();
		AST constant_AST = null;
		Token  l1 = null;
		AST l1_AST = null;
		Token  l2 = null;
		AST l2_AST = null;
		Token  l3 = null;
		AST l3_AST = null;
		Token  l4 = null;
		AST l4_AST = null;
		Token  l5 = null;
		AST l5_AST = null;
		Token  l6 = null;
		AST l6_AST = null;
		
		switch ( LA(1)) {
		case NUM_INT:
		{
			l1 = LT(1);
			l1_AST = astFactory.create(l1);
			astFactory.addASTChild(currentAST, l1_AST);
			match(NUM_INT);
			if ( inputState.guessing==0 ) {
				e=new ConstantInteger(l1.getText());
			}
			constant_AST = (AST)currentAST.root;
			break;
		}
		case CHAR_LITERAL:
		{
			l2 = LT(1);
			l2_AST = astFactory.create(l2);
			astFactory.addASTChild(currentAST, l2_AST);
			match(CHAR_LITERAL);
			if ( inputState.guessing==0 ) {
				e=new ConstantChar(l2.getText());
			}
			constant_AST = (AST)currentAST.root;
			break;
		}
		case STRING_LITERAL:
		{
			l3 = LT(1);
			l3_AST = astFactory.create(l3);
			astFactory.addASTChild(currentAST, l3_AST);
			match(STRING_LITERAL);
			if ( inputState.guessing==0 ) {
				e=new ConstantString(l3.getText().substring(1, l3.getText().length()-1));
			}
			constant_AST = (AST)currentAST.root;
			break;
		}
		case NUM_FLOAT:
		{
			l4 = LT(1);
			l4_AST = astFactory.create(l4);
			astFactory.addASTChild(currentAST, l4_AST);
			match(NUM_FLOAT);
			if ( inputState.guessing==0 ) {
				e=new ConstantFloat(l4.getText());
			}
			constant_AST = (AST)currentAST.root;
			break;
		}
		case NUM_LONG:
		{
			l5 = LT(1);
			l5_AST = astFactory.create(l5);
			astFactory.addASTChild(currentAST, l5_AST);
			match(NUM_LONG);
			if ( inputState.guessing==0 ) {
				e=new ConstantLong(l5.getText());
			}
			constant_AST = (AST)currentAST.root;
			break;
		}
		case NUM_DOUBLE:
		{
			l6 = LT(1);
			l6_AST = astFactory.create(l6);
			astFactory.addASTChild(currentAST, l6_AST);
			match(NUM_DOUBLE);
			if ( inputState.guessing==0 ) {
				e=new ConstantDouble(l6.getText());
			}
			constant_AST = (AST)currentAST.root;
			break;
		}
		default:
		{
			throw new NoViableAltException(LT(1), getFilename());
		}
		}
		returnAST = constant_AST;
		return e;
	}
	
/** Match a, a.b.c refs
 */
	public final Expression  identPrimary() throws RecognitionException, TokenStreamException {
		Expression e = null;
		
		returnAST = null;
		ASTPair currentAST = new ASTPair();
		AST identPrimary_AST = null;
		
		AST tmp45_AST = null;
		tmp45_AST = astFactory.create(LT(1));
		astFactory.addASTChild(currentAST, tmp45_AST);
		match(IDENT);
		{
		_loop55:
		do {
			if ((LA(1)==DOT)) {
				AST tmp46_AST = null;
				tmp46_AST = astFactory.create(LT(1));
				astFactory.makeASTRoot(currentAST, tmp46_AST);
				match(DOT);
				AST tmp47_AST = null;
				tmp47_AST = astFactory.create(LT(1));
				astFactory.addASTChild(currentAST, tmp47_AST);
				match(IDENT);
			}
			else {
				break _loop55;
			}
			
		} while (true);
		}
		identPrimary_AST = (AST)currentAST.root;
		returnAST = identPrimary_AST;
		return e;
	}
	
	
	public static final String[] _tokenNames = {
		"<0>",
		"EOF",
		"<2>",
		"NULL_TREE_LOOKAHEAD",
		"BLOCK",
		"MODIFIERS",
		"OBJBLOCK",
		"SLIST",
		"CTOR_DEF",
		"METHOD_DEF",
		"VARIABLE_DEF",
		"INSTANCE_INIT",
		"STATIC_INIT",
		"TYPE",
		"CLASS_DEF",
		"INTERFACE_DEF",
		"PACKAGE_DEF",
		"ARRAY_DECLARATOR",
		"EXTENDS_CLAUSE",
		"IMPLEMENTS_CLAUSE",
		"PARAMETERS",
		"PARAMETER_DEF",
		"LABELED_STAT",
		"TYPECAST",
		"INDEX_OP",
		"POST_INC",
		"POST_DEC",
		"METHOD_CALL",
		"EXPR",
		"ARRAY_INIT",
		"IMPORT",
		"UNARY_MINUS",
		"UNARY_PLUS",
		"CASE_GROUP",
		"ELIST",
		"FOR_INIT",
		"FOR_CONDITION",
		"FOR_ITERATOR",
		"EMPTY_STAT",
		"\"final\"",
		"\"abstract\"",
		"\"strictfp\"",
		"SUPER_CTOR_CALL",
		"CTOR_CALL",
		"LBRACK",
		"RBRACK",
		"\"void\"",
		"\"boolean\"",
		"\"byte\"",
		"\"char\"",
		"\"short\"",
		"\"int\"",
		"\"float\"",
		"\"long\"",
		"\"double\"",
		"\"String\"",
		"IDENT",
		"DOT",
		"QUESTION",
		"COLON",
		"LOR",
		"LAND",
		"BOR",
		"BXOR",
		"BAND",
		"NOT_EQUAL",
		"EQUAL",
		"LT",
		"GT",
		"LE",
		"GE",
		"SL",
		"SR",
		"BSR",
		"PLUS",
		"MINUS",
		"STAR",
		"DIV",
		"MOD",
		"BNOT",
		"LNOT",
		"LPAREN",
		"RPAREN",
		"\"true\"",
		"\"false\"",
		"\"null\"",
		"NUM_INT",
		"CHAR_LITERAL",
		"STRING_LITERAL",
		"NUM_FLOAT",
		"NUM_LONG",
		"NUM_DOUBLE",
		"LCURLY",
		"RCURLY",
		"COMMA",
		"ASSIGN",
		"DIV_ASSIGN",
		"PLUS_ASSIGN",
		"INC",
		"MINUS_ASSIGN",
		"DEC",
		"STAR_ASSIGN",
		"MOD_ASSIGN",
		"SR_ASSIGN",
		"BSR_ASSIGN",
		"SL_ASSIGN",
		"BXOR_ASSIGN",
		"BOR_ASSIGN",
		"BAND_ASSIGN",
		"SEMI",
		"WS",
		"SL_COMMIT",
		"ML_COMMENT",
		"ESC",
		"HEX_DIGIT",
		"VOCAB",
		"EXPONENT",
		"FLOAT_SUFFIX"
	};
	
	protected void buildTokenTypeASTClassMap() {
		tokenTypeToASTClassMap=null;
	};
	
	private static final long[] mk_tokenSet_0() {
		long[] data = { 72057594037927936L, 268042240L, 0L, 0L};
		return data;
	}
	public static final BitSet _tokenSet_0 = new BitSet(mk_tokenSet_0());
	private static final long[] mk_tokenSet_1() {
		long[] data = { -72057594037927934L, 268435455L, 0L, 0L};
		return data;
	}
	public static final BitSet _tokenSet_1 = new BitSet(mk_tokenSet_1());
	
	}
