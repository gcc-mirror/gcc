// created by jay 0.8 (c) 1998 Axel.Schreiner@informatik.uni-osnabrueck.de

					// line 2 "XPathParser.y"
/* XPathParser.java -- An XPath 1.0 parser.
   Copyright (C) 2004 Free Software Foundation, Inc.

This file is part of GNU Classpath.

GNU Classpath is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2, or (at your option)
any later version.

GNU Classpath is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
General Public License for more details.

You should have received a copy of the GNU General Public License
along with GNU Classpath; see the file COPYING.  If not, write to the
Free Software Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA
02111-1307 USA.

Linking this library statically or dynamically with other modules is
making a combined work based on this library.  Thus, the terms and
conditions of the GNU General Public License cover the whole
combination.

As a special exception, the copyright holders of this library give you
permission to link this library with independent modules to produce an
executable, regardless of the license terms of these independent
modules, and to copy and distribute the resulting executable under
terms of your choice, provided that you also meet, for each linked
independent module, the terms and conditions of the license of that
module.  An independent module is a module which is not derived from
or based on this library.  If you modify this library, you may extend
this exception to your version of the library, but you are not
obligated to do so.  If you do not wish to do so, delete this
exception statement from your version. */

package gnu.xml.xpath;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import javax.xml.namespace.NamespaceContext;
import javax.xml.namespace.QName;
import javax.xml.xpath.XPathFunctionResolver;
import javax.xml.xpath.XPathVariableResolver;
import org.w3c.dom.Node;

/**
 * An XPath 1.0 parser.
 *
 * @author <a href='mailto:dog@gnu.org'>Chris Burdess</a>
 */
public class XPathParser
{

  NamespaceContext namespaceContext;
  XPathVariableResolver variableResolver;
  XPathFunctionResolver functionResolver;

  QName getQName(String name)
  {
    QName qName = QName.valueOf(name);
    if (namespaceContext != null)
      {
        String prefix = qName.getPrefix();
        String uri = qName.getNamespaceURI();
        if (prefix != null && (uri == null || uri.length() == 0))
          {
            uri = namespaceContext.getNamespaceURI(prefix);
            String localName = qName.getLocalPart();
            qName = new QName(uri, localName, prefix);
          }
      }
    return qName;
  }

  Expr lookupFunction(String name, List args)
  {
    int arity = args.size();
    if ("position".equals(name) && arity == 0)
      {
        return new PositionFunction();
      }
    else if ("last".equals(name) && arity == 0)
      {
        return new LastFunction();
      }
    else if ("string".equals(name) && (arity == 1 || arity == 0))
      {
        return new StringFunction(args);
      }
    else if ("number".equals(name) && (arity == 1 || arity == 0))
      {
        return new NumberFunction(args);
      }
    else if ("boolean".equals(name) && arity == 1)
      {
        return new BooleanFunction(args);
      }
    else if ("count".equals(name) && arity == 1)
      {
        return new CountFunction(args);
      }
    else if ("not".equals(name) && arity == 1)
      {
        return new NotFunction(args);
      }
    else if ("id".equals(name) && arity == 1)
      {
        return new IdFunction(args);
      }
    else if ("concat".equals(name) && arity > 1)
      {
        return new ConcatFunction(args);
      }
    else if ("true".equals(name) && arity == 0)
      {
        return new TrueFunction();
      }
    else if ("false".equals(name) && arity == 0)
      {
        return new FalseFunction();
      }
    else if ("name".equals(name) && (arity == 1 || arity == 0))
      {
        return new NameFunction(args);
      }
    else if ("local-name".equals(name) && (arity == 1 || arity == 0))
      {
        return new LocalNameFunction(args);
      }
    else if ("namespace-uri".equals(name) && (arity == 1 || arity == 0))
      {
        return new NamespaceUriFunction(args);
      }
    else if ("starts-with".equals(name) && arity == 2)
      {
        return new StartsWithFunction(args);
      }
    else if ("contains".equals(name) && arity == 2)
      {
        return new ContainsFunction(args);
      }
    else if ("string-length".equals(name) && (arity == 1 || arity == 0))
      {
        return new StringLengthFunction(args);
      }
    else if ("translate".equals(name) && arity == 3)
      {
        return new TranslateFunction(args);
      }
    else if ("normalize-space".equals(name) && (arity == 1 || arity == 0))
      {
        return new NormalizeSpaceFunction(args);
      }
    else if ("substring".equals(name) && (arity == 2 || arity == 3))
      {
        return new SubstringFunction(args);
      }
    else if ("substring-before".equals(name) && arity == 2)
      {
        return new SubstringBeforeFunction(args);
      }
    else if ("substring-after".equals(name) && arity == 2)
      {
        return new SubstringAfterFunction(args);
      }
    else if ("lang".equals(name) && arity == 1)
      {
        return new LangFunction(args);
      }
    else if ("sum".equals(name) && arity == 1)
      {
        return new SumFunction(args);
      }
    else if ("floor".equals(name) && arity == 1)
      {
        return new FloorFunction(args);
      }
    else if ("ceiling".equals(name) && arity == 1)
      {
        return new CeilingFunction(args);
      }
    else if ("round".equals(name) && arity == 1)
      {
        return new RoundFunction(args);
      }
    else if (functionResolver != null)
      {
        QName qName = QName.valueOf(name);
        Object function = functionResolver.resolveFunction(qName, arity);
        if (function != null &&
            function instanceof Function &&
            function instanceof Expr)
          {
            Function f = (Function) function;
            f.setArguments(args);
            return (Expr) function;
          }
      }
    return new FunctionCall(functionResolver, name, args);
  }

					// line 210 "-"
// %token constants

  public static final int LITERAL = 257;
  public static final int DIGITS = 258;
  public static final int NAME = 259;
  public static final int LP = 260;
  public static final int RP = 261;
  public static final int LB = 262;
  public static final int RB = 263;
  public static final int COMMA = 264;
  public static final int PIPE = 265;
  public static final int SLASH = 266;
  public static final int DOUBLE_SLASH = 267;
  public static final int EQ = 268;
  public static final int NE = 269;
  public static final int GT = 270;
  public static final int LT = 271;
  public static final int GTE = 272;
  public static final int LTE = 273;
  public static final int PLUS = 274;
  public static final int MINUS = 275;
  public static final int AT = 276;
  public static final int STAR = 277;
  public static final int DOLLAR = 278;
  public static final int COLON = 279;
  public static final int DOUBLE_COLON = 280;
  public static final int DOT = 281;
  public static final int DOUBLE_DOT = 282;
  public static final int ANCESTOR = 283;
  public static final int ANCESTOR_OR_SELF = 284;
  public static final int ATTRIBUTE = 285;
  public static final int CHILD = 286;
  public static final int DESCENDANT = 287;
  public static final int DESCENDANT_OR_SELF = 288;
  public static final int FOLLOWING = 289;
  public static final int FOLLOWING_SIBLING = 290;
  public static final int NAMESPACE = 291;
  public static final int PARENT = 292;
  public static final int PRECEDING = 293;
  public static final int PRECEDING_SIBLING = 294;
  public static final int SELF = 295;
  public static final int DIV = 296;
  public static final int MOD = 297;
  public static final int OR = 298;
  public static final int AND = 299;
  public static final int COMMENT = 300;
  public static final int PROCESSING_INSTRUCTION = 301;
  public static final int TEXT = 302;
  public static final int NODE = 303;
  public static final int UNARY = 304;
  public static final int yyErrorCode = 256;

  /** thrown for irrecoverable syntax errors and stack overflow.
    */
  public static class yyException extends java.lang.Exception {
    public yyException (String message) {
      super(message);
    }
  }

  /** must be implemented by a scanner object to supply input to the parser.
    */
  public interface yyInput {
    /** move on to next token.
        @return false if positioned beyond tokens.
        @throws IOException on input error.
      */
    boolean advance () throws java.io.IOException;
    /** classifies current token.
        Should not be called if advance() returned false.
        @return current %token or single character.
      */
    int token ();
    /** associated with current token.
        Should not be called if advance() returned false.
        @return value for token().
      */
    Object value ();
  }

  /** simplified error message.
      @see <a href="#yyerror(java.lang.String, java.lang.String[])">yyerror</a>
    */
  public void yyerror (String message) {
    yyerror(message, null);
  }

  /** (syntax) error message.
      Can be overwritten to control message format.
      @param message text to be displayed.
      @param expected vector of acceptable tokens, if available.
    */
  public void yyerror (String message, String[] expected) {
    if (expected != null && expected.length > 0) {
      System.err.print(message+", expecting");
      for (int n = 0; n < expected.length; ++ n)
        System.err.print(" "+expected[n]);
      System.err.println();
    } else
      System.err.println(message);
  }

  /** debugging support, requires the package jay.yydebug.
      Set to null to suppress debugging messages.
    */
//t  protected jay.yydebug.yyDebug yydebug;

  protected static final int yyFinal = 30;

  /** index-checked interface to yyName[].
      @param token single character or %token value.
      @return token name or [illegal] or [unknown].
    */
//t  public static final String yyname (int token) {
//t    if (token < 0 || token > YyNameClass.yyName.length) return "[illegal]";
//t    String name;
//t    if ((name = YyNameClass.yyName[token]) != null) return name;
//t    return "[unknown]";
//t  }

  /** computes list of expected tokens on error by tracing the tables.
      @param state for which to compute the list.
      @return list of token names.
    */
  protected String[] yyExpecting (int state) {
    int token, n, len = 0;
    boolean[] ok = new boolean[YyNameClass.yyName.length];

    if ((n = YySindexClass.yySindex[state]) != 0)
      for (token = n < 0 ? -n : 0;
           token < YyNameClass.yyName.length && n+token < YyTableClass.yyTable.length; ++ token)
        if (YyCheckClass.yyCheck[n+token] == token && !ok[token] && YyNameClass.yyName[token] != null) {
          ++ len;
          ok[token] = true;
        }
    if ((n = YyRindexClass.yyRindex[state]) != 0)
      for (token = n < 0 ? -n : 0;
           token < YyNameClass.yyName.length && n+token < YyTableClass.yyTable.length; ++ token)
        if (YyCheckClass.yyCheck[n+token] == token && !ok[token] && YyNameClass.yyName[token] != null) {
          ++ len;
          ok[token] = true;
        }

    String result[] = new String[len];
    for (n = token = 0; n < len;  ++ token)
      if (ok[token]) result[n++] = YyNameClass.yyName[token];
    return result;
  }

  /** the generated parser, with debugging messages.
      Maintains a state and a value stack, currently with fixed maximum size.
      @param yyLex scanner.
      @param yydebug debug message writer implementing yyDebug, or null.
      @return result of the last reduction, if any.
      @throws yyException on irrecoverable parse error.
    */
  public Object yyparse (yyInput yyLex, Object yydebug)
				throws java.io.IOException, yyException {
//t    this.yydebug = (jay.yydebug.yyDebug)yydebug;
    return yyparse(yyLex);
  }

  /** initial size and increment of the state/value stack [default 256].
      This is not final so that it can be overwritten outside of invocations
      of yyparse().
    */
  protected int yyMax;

  /** executed at the beginning of a reduce action.
      Used as $$ = yyDefault($1), prior to the user-specified action, if any.
      Can be overwritten to provide deep copy, etc.
      @param first value for $1, or null.
      @return first.
    */
  protected Object yyDefault (Object first) {
    return first;
  }

  /** the generated parser.
      Maintains a state and a value stack, currently with fixed maximum size.
      @param yyLex scanner.
      @return result of the last reduction, if any.
      @throws yyException on irrecoverable parse error.
    */
  public Object yyparse (yyInput yyLex)
				throws java.io.IOException, yyException {
    if (yyMax <= 0) yyMax = 256;			// initial size
    int yyState = 0, yyStates[] = new int[yyMax];	// state stack
    Object yyVal = null, yyVals[] = new Object[yyMax];	// value stack
    int yyToken = -1;					// current input
    int yyErrorFlag = 0;				// #tks to shift

    yyLoop: for (int yyTop = 0;; ++ yyTop) {
      if (yyTop >= yyStates.length) {			// dynamically increase
        int[] i = new int[yyStates.length+yyMax];
        System.arraycopy(yyStates, 0, i, 0, yyStates.length);
        yyStates = i;
        Object[] o = new Object[yyVals.length+yyMax];
        System.arraycopy(yyVals, 0, o, 0, yyVals.length);
        yyVals = o;
      }
      yyStates[yyTop] = yyState;
      yyVals[yyTop] = yyVal;
//t      if (yydebug != null) yydebug.push(yyState, yyVal);

      yyDiscarded: for (;;) {	// discarding a token does not change stack
        int yyN;
        if ((yyN = YyDefRedClass.yyDefRed[yyState]) == 0) {	// else [default] reduce (yyN)
          if (yyToken < 0) {
            yyToken = yyLex.advance() ? yyLex.token() : 0;
//t            if (yydebug != null)
//t              yydebug.lex(yyState, yyToken, yyname(yyToken), yyLex.value());
          }
          if ((yyN = YySindexClass.yySindex[yyState]) != 0 && (yyN += yyToken) >= 0
              && yyN < YyTableClass.yyTable.length && YyCheckClass.yyCheck[yyN] == yyToken) {
//t            if (yydebug != null)
//t              yydebug.shift(yyState, YyTableClass.yyTable[yyN], yyErrorFlag-1);
            yyState = YyTableClass.yyTable[yyN];		// shift to yyN
            yyVal = yyLex.value();
            yyToken = -1;
            if (yyErrorFlag > 0) -- yyErrorFlag;
            continue yyLoop;
          }
          if ((yyN = YyRindexClass.yyRindex[yyState]) != 0 && (yyN += yyToken) >= 0
              && yyN < YyTableClass.yyTable.length && YyCheckClass.yyCheck[yyN] == yyToken)
            yyN = YyTableClass.yyTable[yyN];			// reduce (yyN)
          else
            switch (yyErrorFlag) {
  
            case 0:
              yyerror("syntax error", yyExpecting(yyState));
//t              if (yydebug != null) yydebug.error("syntax error");
  
            case 1: case 2:
              yyErrorFlag = 3;
              do {
                if ((yyN = YySindexClass.yySindex[yyStates[yyTop]]) != 0
                    && (yyN += yyErrorCode) >= 0 && yyN < YyTableClass.yyTable.length
                    && YyCheckClass.yyCheck[yyN] == yyErrorCode) {
//t                  if (yydebug != null)
//t                    yydebug.shift(yyStates[yyTop], YyTableClass.yyTable[yyN], 3);
                  yyState = YyTableClass.yyTable[yyN];
                  yyVal = yyLex.value();
                  continue yyLoop;
                }
//t                if (yydebug != null) yydebug.pop(yyStates[yyTop]);
              } while (-- yyTop >= 0);
//t              if (yydebug != null) yydebug.reject();
              throw new yyException("irrecoverable syntax error");
  
            case 3:
              if (yyToken == 0) {
//t                if (yydebug != null) yydebug.reject();
                throw new yyException("irrecoverable syntax error at end-of-file");
              }
//t              if (yydebug != null)
//t                yydebug.discard(yyState, yyToken, yyname(yyToken),
//t  							yyLex.value());
              yyToken = -1;
              continue yyDiscarded;		// leave stack alone
            }
        }
        int yyV = yyTop + 1-YyLenClass.yyLen[yyN];
//t        if (yydebug != null)
//t          yydebug.reduce(yyState, yyStates[yyV-1], yyN, YyRuleClass.yyRule[yyN], YyLenClass.yyLen[yyN]);
        yyVal = yyDefault(yyV > yyTop ? null : yyVals[yyV]);
        switch (yyN) {
case 4:
					// line 276 "XPathParser.y"
  {
      yyVal = new Root();
    }
  break;
case 5:
					// line 280 "XPathParser.y"
  {
      Steps steps;
      if (yyVals[0+yyTop] instanceof Steps)
        {
          steps = (Steps) yyVals[0+yyTop];
        }
      else
        {
          steps = new Steps();
          steps.path.addFirst(yyVals[0+yyTop]);
        }
      steps.path.addFirst(new Root());
      yyVal = steps;
      /*$$ = new Step(new Root(), (Path) $2);*/
    }
  break;
case 6:
					// line 296 "XPathParser.y"
  {
      Test nt = new NodeTypeTest((short) 0);
      Selector s = new Selector(Selector.DESCENDANT_OR_SELF,
                                Collections.singletonList (nt));
      Steps steps;
      if (yyVals[0+yyTop] instanceof Steps)
        {
          steps = (Steps) yyVals[0+yyTop];
        }
      else
        {
          steps = new Steps();
          steps.path.addFirst(yyVals[0+yyTop]);
        }
      steps.path.addFirst(s);
      steps.path.addFirst(new Root());
      yyVal = steps;
      /*Step step = new Step(s, (Path) $2);*/
      /*$$ = new Step(new Root(), step);*/
    }
  break;
case 8:
					// line 321 "XPathParser.y"
  {
      Steps steps;
      if (yyVals[-2+yyTop] instanceof Steps)
        {
          steps = (Steps) yyVals[-2+yyTop];
        }
      else
        {
          steps = new Steps();
          steps.path.addFirst(yyVals[-2+yyTop]);
        }
      steps.path.addLast(yyVals[0+yyTop]);
      yyVal = steps;
      /*$$ = new Step((Expr) $1, (Path) $3);*/
    }
  break;
case 9:
					// line 337 "XPathParser.y"
  {
      Test nt = new NodeTypeTest((short) 0);
      Selector s = new Selector(Selector.DESCENDANT_OR_SELF,
                                Collections.singletonList (nt));
      Steps steps;
      if (yyVals[-2+yyTop] instanceof Steps)
        {
          steps = (Steps) yyVals[-2+yyTop];
        }
      else
        {
          steps = new Steps();
          steps.path.addFirst(yyVals[-2+yyTop]);
        }
      steps.path.addLast(s);
      steps.path.addLast(yyVals[0+yyTop]);
      yyVal = steps;
      /*Step step = new Step(s, (Path) $3);*/
      /*$$ = new Step((Expr) $1, step);*/
    }
  break;
case 10:
					// line 361 "XPathParser.y"
  {
      yyVal = new Selector (Selector.CHILD, (List) yyVals[0+yyTop]);
    }
  break;
case 11:
					// line 365 "XPathParser.y"
  {
      yyVal = new Selector (Selector.ATTRIBUTE, (List) yyVals[0+yyTop]);
    }
  break;
case 12:
					// line 369 "XPathParser.y"
  {
      yyVal = new Selector (((Integer) yyVals[-2+yyTop]).intValue (), (List) yyVals[0+yyTop]);
    }
  break;
case 13:
					// line 373 "XPathParser.y"
  {
      yyVal = new Selector (Selector.SELF, Collections.EMPTY_LIST);
    }
  break;
case 14:
					// line 377 "XPathParser.y"
  {
      yyVal = new Selector (Selector.PARENT, Collections.EMPTY_LIST);
    }
  break;
case 15:
					// line 384 "XPathParser.y"
  {
      List list = new ArrayList();
      list.add(yyVals[0+yyTop]);
      yyVal = list;
    }
  break;
case 16:
					// line 390 "XPathParser.y"
  {
      List list = (List)yyVals[-1+yyTop];
      list.add(yyVals[0+yyTop]);
      yyVal = list;
    }
  break;
case 17:
					// line 414 "XPathParser.y"
  {
      yyVal = new Integer(Selector.ANCESTOR);
    }
  break;
case 18:
					// line 418 "XPathParser.y"
  {
      yyVal = new Integer(Selector.ANCESTOR_OR_SELF);
    }
  break;
case 19:
					// line 422 "XPathParser.y"
  {
      yyVal = new Integer(Selector.ATTRIBUTE);
    }
  break;
case 20:
					// line 426 "XPathParser.y"
  {
      yyVal = new Integer(Selector.CHILD);
    }
  break;
case 21:
					// line 430 "XPathParser.y"
  {
      yyVal = new Integer(Selector.DESCENDANT);
    }
  break;
case 22:
					// line 434 "XPathParser.y"
  {
      yyVal = new Integer(Selector.DESCENDANT_OR_SELF);
    }
  break;
case 23:
					// line 438 "XPathParser.y"
  {
      yyVal = new Integer(Selector.FOLLOWING);
    }
  break;
case 24:
					// line 442 "XPathParser.y"
  {
      yyVal = new Integer(Selector.FOLLOWING_SIBLING);
    }
  break;
case 25:
					// line 446 "XPathParser.y"
  {
      yyVal = new Integer(Selector.NAMESPACE);
    }
  break;
case 26:
					// line 450 "XPathParser.y"
  {
      yyVal = new Integer(Selector.PARENT);
    }
  break;
case 27:
					// line 454 "XPathParser.y"
  {
      yyVal = new Integer(Selector.PRECEDING);
    }
  break;
case 28:
					// line 458 "XPathParser.y"
  {
      yyVal = new Integer(Selector.PRECEDING_SIBLING);
    }
  break;
case 29:
					// line 462 "XPathParser.y"
  {
      yyVal = new Integer(Selector.SELF);
    }
  break;
case 31:
					// line 471 "XPathParser.y"
  {
      yyVal = new NodeTypeTest(Node.PROCESSING_INSTRUCTION_NODE, (String) yyVals[-1+yyTop]);
    }
  break;
case 32:
					// line 476 "XPathParser.y"
  {
      yyVal = new NodeTypeTest(((Short) yyVals[-1+yyTop]).shortValue());
    }
  break;
case 33:
					// line 483 "XPathParser.y"
  {
      yyVal = new Predicate((Expr) yyVals[-1+yyTop]);
    }
  break;
case 35:
					// line 491 "XPathParser.y"
  {
      yyVal = new ParenthesizedExpr((Expr) yyVals[-1+yyTop]);
    }
  break;
case 36:
					// line 495 "XPathParser.y"
  {
      yyVal = new Constant(yyVals[0+yyTop]);
    }
  break;
case 37:
					// line 499 "XPathParser.y"
  {
      yyVal = new Constant(yyVals[0+yyTop]);
    }
  break;
case 39:
					// line 507 "XPathParser.y"
  {
      yyVal = lookupFunction((String) yyVals[-2+yyTop], Collections.EMPTY_LIST);
    }
  break;
case 40:
					// line 511 "XPathParser.y"
  {
      yyVal = lookupFunction((String) yyVals[-3+yyTop], (List) yyVals[-1+yyTop]);
    }
  break;
case 41:
					// line 518 "XPathParser.y"
  {
      List list = new ArrayList();
      list.add(yyVals[0+yyTop]);
      yyVal = list;
    }
  break;
case 42:
					// line 524 "XPathParser.y"
  {
      List list = (List) yyVals[0+yyTop];
      list.add(0, yyVals[-2+yyTop]);
      yyVal = list;
    }
  break;
case 44:
					// line 534 "XPathParser.y"
  {
      yyVal = new UnionExpr((Expr) yyVals[-2+yyTop], (Expr) yyVals[0+yyTop]);
    }
  break;
case 47:
					// line 543 "XPathParser.y"
  {
      Steps steps;
      if (yyVals[0+yyTop] instanceof Steps)
        {
          steps = (Steps) yyVals[0+yyTop];
        }
      else
        {
          steps = new Steps();
          steps.path.addFirst(yyVals[0+yyTop]);
        }
      steps.path.addFirst(yyVals[-2+yyTop]);
      yyVal = steps;
      /*$$ = new Step ((Expr) $1, (Path) $3);*/
    }
  break;
case 48:
					// line 559 "XPathParser.y"
  {
      Test nt = new NodeTypeTest((short) 0);
      Selector s = new Selector(Selector.DESCENDANT_OR_SELF,
                                Collections.singletonList(nt));
      Steps steps;
      if (yyVals[0+yyTop] instanceof Steps)
        {
          steps = (Steps) yyVals[0+yyTop];
        }
      else
        {
          steps = new Steps();
          steps.path.addFirst(yyVals[0+yyTop]);
        }
      steps.path.addFirst(s);
      steps.path.addFirst(yyVals[-2+yyTop]);
      yyVal = steps;
      /*Step step = new Step (s, (Path) $3);*/
      /*$$ = new Step ((Expr) $1, step);*/
    }
  break;
case 50:
					// line 584 "XPathParser.y"
  {
      Predicate filter = (Predicate) yyVals[0+yyTop];
      Selector s = new Selector(Selector.SELF,
                                Collections.singletonList(filter));
      Steps steps;
      if (yyVals[-1+yyTop] instanceof Steps)
        {
          steps = (Steps) yyVals[-1+yyTop];
        }
      else
        {
          steps = new Steps();
          steps.path.addFirst(yyVals[-1+yyTop]);
        }
      steps.path.addLast(s);
      yyVal = steps;
      /*$$ = new Step ((Expr) $1, s);*/
    }
  break;
case 52:
					// line 607 "XPathParser.y"
  {
      yyVal = new OrExpr((Expr) yyVals[-2+yyTop], (Expr) yyVals[0+yyTop]);
    }
  break;
case 54:
					// line 615 "XPathParser.y"
  {
      yyVal = new AndExpr((Expr) yyVals[-2+yyTop], (Expr) yyVals[0+yyTop]);
    }
  break;
case 56:
					// line 623 "XPathParser.y"
  {
      yyVal = new EqualityExpr((Expr) yyVals[-2+yyTop], (Expr) yyVals[0+yyTop], false);
    }
  break;
case 57:
					// line 627 "XPathParser.y"
  {
      yyVal = new EqualityExpr((Expr) yyVals[-2+yyTop], (Expr) yyVals[0+yyTop], true);
    }
  break;
case 59:
					// line 635 "XPathParser.y"
  {
      yyVal = new RelationalExpr((Expr) yyVals[-2+yyTop], (Expr) yyVals[0+yyTop], true, false);
    }
  break;
case 60:
					// line 639 "XPathParser.y"
  {
      yyVal = new RelationalExpr((Expr) yyVals[-2+yyTop], (Expr) yyVals[0+yyTop], false, false);
    }
  break;
case 61:
					// line 643 "XPathParser.y"
  {
      yyVal = new RelationalExpr((Expr) yyVals[-2+yyTop], (Expr) yyVals[0+yyTop], true, true);
    }
  break;
case 62:
					// line 647 "XPathParser.y"
  {
      yyVal = new RelationalExpr((Expr) yyVals[-2+yyTop], (Expr) yyVals[0+yyTop], false, true);
    }
  break;
case 64:
					// line 655 "XPathParser.y"
  {
      yyVal = new ArithmeticExpr((Expr) yyVals[-2+yyTop], (Expr) yyVals[0+yyTop], ArithmeticExpr.ADD);
    }
  break;
case 65:
					// line 659 "XPathParser.y"
  {
      yyVal = new ArithmeticExpr((Expr) yyVals[-2+yyTop], (Expr) yyVals[0+yyTop], ArithmeticExpr.SUBTRACT);
    }
  break;
case 67:
					// line 667 "XPathParser.y"
  {
      yyVal = new ArithmeticExpr((Expr) yyVals[-2+yyTop], (Expr) yyVals[0+yyTop], ArithmeticExpr.MULTIPLY);
    }
  break;
case 68:
					// line 671 "XPathParser.y"
  {
      yyVal = new ArithmeticExpr((Expr) yyVals[-2+yyTop], (Expr) yyVals[0+yyTop], ArithmeticExpr.DIVIDE);
    }
  break;
case 69:
					// line 675 "XPathParser.y"
  {
      yyVal = new ArithmeticExpr((Expr) yyVals[-2+yyTop], (Expr) yyVals[0+yyTop], ArithmeticExpr.MODULO);
    }
  break;
case 71:
					// line 683 "XPathParser.y"
  {
      yyVal = new NegativeExpr((Expr) yyVals[0+yyTop]);
    }
  break;
case 72:
					// line 690 "XPathParser.y"
  {
      yyVal = new Double((String) yyVals[0+yyTop] + ".0");
    }
  break;
case 73:
					// line 694 "XPathParser.y"
  {
      yyVal = new Double((String) yyVals[-1+yyTop] + ".0");
    }
  break;
case 74:
					// line 698 "XPathParser.y"
  {
      yyVal = new Double((String) yyVals[-2+yyTop] + "." + (String) yyVals[0+yyTop]);
    }
  break;
case 75:
					// line 702 "XPathParser.y"
  {
      yyVal = new Double("0." + (String) yyVals[0+yyTop]);
    }
  break;
case 77:
					// line 731 "XPathParser.y"
  {
      yyVal = new VariableReference(variableResolver, (String) yyVals[0+yyTop]);
    }
  break;
case 78:
					// line 738 "XPathParser.y"
  {
      yyVal = new NameTest(null, true, true);
    }
  break;
case 79:
					// line 742 "XPathParser.y"
  {
      QName qName = getQName((String) yyVals[-2+yyTop]);
      yyVal = new NameTest(qName, true, false);
    }
  break;
case 80:
					// line 747 "XPathParser.y"
  {
      QName qName = getQName((String) yyVals[0+yyTop]);
      yyVal = new NameTest(qName, false, false);
    }
  break;
case 82:
					// line 756 "XPathParser.y"
  {
      yyVal = (String) yyVals[-2+yyTop] + ':' + (String) yyVals[0+yyTop];
    }
  break;
case 83:
					// line 763 "XPathParser.y"
  {
      yyVal = new Short(Node.COMMENT_NODE);
    }
  break;
case 84:
					// line 767 "XPathParser.y"
  {
      yyVal = new Short(Node.TEXT_NODE);
    }
  break;
case 85:
					// line 771 "XPathParser.y"
  {
      yyVal = new Short(Node.PROCESSING_INSTRUCTION_NODE);
    }
  break;
case 86:
					// line 775 "XPathParser.y"
  {
      yyVal = new Short((short) 0);
    }
  break;
					// line 986 "-"
        }
        yyTop -= YyLenClass.yyLen[yyN];
        yyState = yyStates[yyTop];
        int yyM = YyLhsClass.yyLhs[yyN];
        if (yyState == 0 && yyM == 0) {
//t          if (yydebug != null) yydebug.shift(0, yyFinal);
          yyState = yyFinal;
          if (yyToken < 0) {
            yyToken = yyLex.advance() ? yyLex.token() : 0;
//t            if (yydebug != null)
//t               yydebug.lex(yyState, yyToken,yyname(yyToken), yyLex.value());
          }
          if (yyToken == 0) {
//t            if (yydebug != null) yydebug.accept(yyVal);
            return yyVal;
          }
          continue yyLoop;
        }
        if ((yyN = YyGindexClass.yyGindex[yyM]) != 0 && (yyN += yyState) >= 0
            && yyN < YyTableClass.yyTable.length && YyCheckClass.yyCheck[yyN] == yyState)
          yyState = YyTableClass.yyTable[yyN];
        else
          yyState = YyDgotoClass.yyDgoto[yyM];
//t        if (yydebug != null) yydebug.shift(yyStates[yyTop], yyState);
	 continue yyLoop;
      }
    }
  }

  protected static final class YyLhsClass {

    public static final short yyLhs [] = {              -1,
          0,    2,    2,    4,    4,    4,    3,    3,    3,    5,
          5,    5,    5,    5,    6,    6,    7,    7,    7,    7,
          7,    7,    7,    7,    7,    7,    7,    7,    7,    8,
          8,    8,    9,   12,   12,   12,   12,   12,   15,   15,
         17,   17,   18,   18,   19,   19,   19,   19,   20,   20,
          1,    1,   21,   21,   22,   22,   22,   23,   23,   23,
         23,   23,   24,   24,   24,   25,   25,   25,   25,   26,
         26,   14,   14,   14,   14,   16,   13,   10,   10,   10,
         27,   27,   11,   11,   11,   11,
    };
  } /* End of class YyLhsClass */

  protected static final class YyLenClass {

    public static final short yyLen [] = {           2,
          1,    1,    1,    1,    2,    2,    1,    3,    3,    1,
          2,    3,    1,    1,    1,    2,    1,    1,    1,    1,
          1,    1,    1,    1,    1,    1,    1,    1,    1,    1,
          3,    2,    3,    1,    3,    1,    1,    1,    3,    4,
          1,    3,    1,    3,    1,    1,    3,    3,    1,    2,
          1,    3,    1,    3,    1,    3,    3,    1,    3,    3,
          3,    3,    1,    3,    3,    1,    3,    3,    3,    1,
          2,    1,    2,    3,    2,    1,    2,    1,    3,    1,
          1,    3,    1,    1,    1,    1,
    };
  } /* End class YyLenClass */

  protected static final class YyDefRedClass {

    public static final short yyDefRed [] = {            0,
         36,    0,    0,    0,    0,    0,    0,    0,   78,    0,
          0,   14,   17,   18,   19,   20,   21,   22,   23,   24,
         25,   26,   27,   28,   29,   83,    0,   84,   86,    0,
          0,   45,    0,    3,    7,    0,    0,   15,   30,    0,
         49,   34,   37,   38,    0,    0,   43,    0,    0,    0,
          0,    0,    0,   66,    0,    0,    0,    0,   13,    0,
         80,    0,   71,    0,    0,   77,   75,    0,    0,    0,
          0,    0,   16,    0,   32,    0,    0,    0,    0,   50,
          0,    0,    0,    0,    0,    0,    0,    0,    0,    0,
          0,    0,   74,   82,   79,   35,    0,   31,    0,    8,
          9,    0,    0,   39,    0,    0,   44,    0,    0,    0,
          0,    0,    0,    0,    0,    0,    0,    0,   67,   68,
         69,   33,    0,   40,   42,
    };
  } /* End of class YyDefRedClass */

  protected static final class YyDgotoClass {

    public static final short yyDgoto [] = {           105,
         31,   32,   33,   34,   35,   36,   37,   38,   73,   39,
         40,   41,   42,   43,   44,   45,  106,   46,   47,   48,
         49,   50,   51,   52,   53,   54,   55,
    };
  } /* End of class YyDgotoClass */

  protected static final class YySindexClass {

    public static final short yySindex [] = {          -97,
          0, -271, -267,  -97, -239, -239,  -97, -199,    0, -236,
       -222,    0,    0,    0,    0,    0,    0,    0,    0,    0,
          0,    0,    0,    0,    0,    0, -218,    0,    0,    0,
       -257,    0, -241,    0,    0, -205, -221,    0,    0, -194,
          0,    0,    0,    0, -190, -185,    0, -238, -211, -234,
       -255, -209, -275,    0,    0, -169, -250, -168,    0, -241,
          0, -241,    0, -205, -187,    0,    0, -167,  -97, -239,
       -239,  -97,    0, -199,    0, -151,  -43, -239, -239,    0,
        -97,  -97,  -97,  -97,  -97,  -97,  -97,  -97,  -97,  -97,
        -97,  -97,    0,    0,    0,    0, -164,    0, -211,    0,
          0, -166, -205,    0, -165, -163,    0, -241, -241, -234,
       -255, -255, -209, -209, -209, -209, -275, -275,    0,    0,
          0,    0,  -97,    0,    0,
    };
  } /* End of class YySindexClass */

  protected static final class YyRindexClass {

    public static final short yyRindex [] = {            0,
          0,   58,    1,    0,  420,    0,    0,    0,    0,    0,
        129,    0,    0,    0,    0,    0,    0,    0,    0,    0,
          0,    0,    0,    0,    0,    0, -161,    0,    0,    0,
         40,    0,  237,    0,    0,  168,    0,    0,    0,    0,
          0,    0,    0,    0,    0,  459,    0,  277,  557,  544,
        656,  561,  474,    0,   19,   75,    0,    0,    0,  295,
          0,  334,    0,  183,  114,    0,    0,    0,    0,    0,
          0,    0,    0,    0,    0,    0,    0,    0,    0,    0,
          0,    0,    0,    0,    0,    0,    0,    0,    0,    0,
          0,    0,    0,    0,    0,    0,    0,    0,  686,    0,
          0,    0,  222,    0, -156,    0,    0,  351,  405,  553,
        665,  697,  577,  600,  617,  639,  513,  528,    0,    0,
          0,    0,    0,    0,    0,
    };
  } /* End of class YyRindexClass */

  protected static final class YyGindexClass {

    public static final short yyGindex [] = {            7,
          0,    0,    8,    0,    3,   -3,    0,    0,   48,    0,
          0,    0,    0,    0,    0,    0,  -12,    0,   35,    0,
         44,   36,   -1,  -54,    2,   -7,   -2,
    };
  } /* End of class YyGindexClass */

  protected static final class YyTableClass {

    public static final short yyTable [] = {            63,
         81,   90,   61,   61,   64,   61,   30,   66,   94,   56,
         58,   57,   60,   62,   84,   85,   86,   87,   80,    3,
         91,   92,   65,   72,   70,   71,   95,   78,   79,  113,
        114,  115,  116,   82,   83,   67,    8,    9,   68,    1,
         69,   59,   12,   13,   14,   15,   16,   17,   18,   19,
         20,   21,   22,   23,   24,   25,   72,   72,   74,    3,
         26,   27,   28,   29,   88,   89,   75,   61,   61,   76,
        103,   61,  100,  101,   73,   61,   61,    9,  102,   77,
        111,  112,  119,  120,  121,  108,  109,   81,   93,  117,
        118,   97,   96,   98,   94,   80,  122,  124,  123,   85,
         26,   27,   28,   29,   41,    1,    2,    3,    4,  104,
        125,  107,   99,   81,    5,    6,  110,    0,    0,    0,
          0,    0,    0,    7,    8,    9,   10,    0,   13,   11,
         12,   13,   14,   15,   16,   17,   18,   19,   20,   21,
         22,   23,   24,   25,    0,    0,    0,    0,   26,   27,
         28,   29,    0,    0,    0,    0,    0,    0,    0,    1,
          2,    3,    4,    0,    0,    0,    0,   10,    5,    6,
          0,    0,    0,    0,    0,    0,    0,    7,    8,    9,
         10,    0,   11,   11,   12,   13,   14,   15,   16,   17,
         18,   19,   20,   21,   22,   23,   24,   25,    0,    0,
          0,    0,   26,   27,   28,   29,    0,    0,    0,    0,
          0,    0,    0,    1,    2,    3,    4,    0,    0,    0,
          0,   12,    5,    6,    0,    0,    0,    0,    0,    0,
          0,    0,    8,    9,   10,    0,    2,   11,   12,   13,
         14,   15,   16,   17,   18,   19,   20,   21,   22,   23,
         24,   25,    0,    0,    0,    0,   26,   27,   28,   29,
         81,   81,   81,   81,   81,   81,   81,   81,   81,   81,
         81,   81,   81,   81,   81,   81,   46,   81,   76,   80,
         80,   80,   80,   80,   80,   80,   80,   80,   80,   80,
         80,   80,   80,   80,    5,   80,   81,   81,   81,   81,
          1,    0,    1,    1,    0,    0,    0,    0,    0,    0,
          0,    0,    0,    0,   80,   80,   80,   80,   72,   72,
         72,   72,   72,   72,   72,   72,   72,   72,   72,   72,
         72,   72,   72,    6,   72,   73,   73,   73,   73,   73,
         73,   73,   73,   73,   73,   73,   73,   73,   73,   73,
         47,   73,    0,   72,   72,   72,   72,    0,    0,    0,
          0,    0,    0,    0,    0,    0,    0,    0,    0,    0,
         73,   73,   73,   73,   81,   81,   81,   81,   81,   81,
         81,   81,   81,   81,   81,   81,   81,   81,   81,   13,
         81,   13,   13,   13,   13,   13,   13,   13,   13,   13,
         13,   13,   13,   13,   48,   13,    0,    0,    0,   81,
         81,   81,   81,    0,    0,    0,    0,    0,    0,    4,
          0,    0,    0,    0,   13,   13,   13,   13,   10,    0,
         10,   10,   10,   10,   10,   10,   10,   10,   10,   10,
         10,   10,   10,   11,   10,   11,   11,   11,   11,   11,
         11,   11,   11,   11,   11,   11,   11,   11,   70,   11,
          0,    0,    0,   10,   10,   10,   10,    0,    0,    0,
          0,    0,    0,   63,    0,    0,    0,    0,   11,   11,
         11,   11,   12,    0,   12,   12,   12,   12,   12,   12,
         12,   12,   12,   12,   12,   12,   12,    2,   12,    2,
          2,    2,    0,    0,    2,    2,    2,    2,    2,    2,
          2,    2,   64,    2,    0,    0,    0,   12,   12,   12,
         12,    0,    0,    0,    0,    0,    0,   65,    0,    0,
          0,    0,    2,    2,    2,    2,    0,   46,    0,   46,
         46,   46,    0,   53,   46,   46,   46,   46,   46,   46,
         46,   46,   54,   46,    0,    5,   51,    5,    5,    5,
         58,    0,    5,    5,    5,    5,    5,    5,    5,    5,
          0,    5,   46,   46,   46,   46,   60,    0,    0,    0,
          0,    0,    0,    0,    0,    0,    0,    0,    0,    0,
          5,    5,    5,    5,    6,    0,    6,    6,    6,   59,
          0,    6,    6,    6,    6,    6,    6,    6,    6,    0,
          6,   47,    0,   47,   47,   47,   62,    0,   47,   47,
         47,   47,   47,   47,   47,   47,    0,   47,    0,    6,
          6,    6,    6,    0,    0,    0,    0,    0,   61,    0,
          0,    0,    0,    0,    0,    0,   47,   47,   47,   47,
          0,    0,    0,    0,    0,   55,    0,    0,    0,    0,
          0,    0,    0,    0,   56,   48,    0,   48,   48,   48,
          0,    0,   48,   48,   48,   48,   48,   48,   48,   48,
          4,   48,    4,    4,    4,   52,    0,    4,    4,    4,
          4,    4,    4,    4,    4,    0,   57,    0,    0,    0,
         48,   48,   48,   48,    0,    0,    0,    0,    0,    0,
          0,    0,    0,    0,    0,    4,    4,    4,    4,   70,
          0,   70,   70,    0,    0,    0,   70,   70,   70,   70,
         70,   70,   70,   70,   63,   70,   63,   63,    0,    0,
          0,   63,   63,   63,   63,   63,   63,   63,   63,    0,
          0,    0,    0,    0,   70,   70,   70,   70,    0,    0,
          0,    0,    0,    0,    0,    0,    0,    0,    0,    0,
          0,   63,   63,   64,    0,   64,   64,    0,    0,    0,
         64,   64,   64,   64,   64,   64,   64,   64,   65,    0,
         65,   65,    0,    0,    0,   65,   65,   65,   65,   65,
         65,   65,   65,    0,   53,    0,   53,   53,    0,    0,
         64,   64,    0,   54,    0,   54,   54,   51,    0,   51,
         51,   58,    0,   58,   58,   65,   65,    0,   58,   58,
         58,   58,   58,   58,    0,    0,    0,   60,    0,   60,
         60,   53,   53,    0,   60,   60,   60,   60,   60,   60,
         54,   54,    0,    0,   51,    0,    0,    0,   58,   58,
         59,    0,   59,   59,    0,    0,    0,   59,   59,   59,
         59,   59,   59,    0,   60,   60,    0,   62,    0,   62,
         62,    0,    0,    0,   62,   62,   62,   62,   62,   62,
          0,    0,    0,    0,    0,    0,    0,   59,   59,   61,
          0,   61,   61,    0,    0,    0,   61,   61,   61,   61,
         61,   61,    0,    0,   62,   62,   55,    0,   55,   55,
          0,    0,    0,   55,   55,   56,    0,   56,   56,    0,
          0,    0,   56,   56,    0,    0,   61,   61,    0,    0,
          0,    0,    0,    0,    0,    0,   52,    0,   52,   52,
          0,    0,    0,   55,   55,    0,    0,   57,    0,   57,
         57,    0,   56,   56,   57,   57,    0,    0,    0,    0,
          0,    0,    0,    0,    0,    0,    0,    0,    0,    0,
          0,    0,    0,   52,    0,    0,    0,    0,    0,    0,
          0,    0,    0,    0,   57,   57,
    };
  } /* End of class YyTableClass */

  protected static final class YyCheckClass {

    public static final short yyCheck [] = {             7,
          0,  277,    5,    6,    8,    8,    0,   10,  259,  281,
          4,  279,    5,    6,  270,  271,  272,  273,    0,  259,
        296,  297,  259,  262,  266,  267,  277,  266,  267,   84,
         85,   86,   87,  268,  269,  258,  276,  277,  257,    0,
        298,  281,  282,  283,  284,  285,  286,  287,  288,  289,
        290,  291,  292,  293,  294,  295,  262,    0,  280,  259,
        300,  301,  302,  303,  274,  275,  261,   70,   71,  260,
         74,   74,   70,   71,    0,   78,   79,  277,   72,  265,
         82,   83,   90,   91,   92,   78,   79,  299,  258,   88,
         89,  279,  261,  261,  259,   48,  263,  261,  264,  261,
        300,  301,  302,  303,  261,  257,  258,  259,  260,  261,
        123,   77,   69,    0,  266,  267,   81,   -1,   -1,   -1,
         -1,   -1,   -1,  275,  276,  277,  278,   -1,    0,  281,
        282,  283,  284,  285,  286,  287,  288,  289,  290,  291,
        292,  293,  294,  295,   -1,   -1,   -1,   -1,  300,  301,
        302,  303,   -1,   -1,   -1,   -1,   -1,   -1,   -1,  257,
        258,  259,  260,   -1,   -1,   -1,   -1,    0,  266,  267,
         -1,   -1,   -1,   -1,   -1,   -1,   -1,  275,  276,  277,
        278,   -1,    0,  281,  282,  283,  284,  285,  286,  287,
        288,  289,  290,  291,  292,  293,  294,  295,   -1,   -1,
         -1,   -1,  300,  301,  302,  303,   -1,   -1,   -1,   -1,
         -1,   -1,   -1,  257,  258,  259,  260,   -1,   -1,   -1,
         -1,    0,  266,  267,   -1,   -1,   -1,   -1,   -1,   -1,
         -1,   -1,  276,  277,  278,   -1,    0,  281,  282,  283,
        284,  285,  286,  287,  288,  289,  290,  291,  292,  293,
        294,  295,   -1,   -1,   -1,   -1,  300,  301,  302,  303,
        260,  261,  262,  263,  264,  265,  266,  267,  268,  269,
        270,  271,  272,  273,  274,  275,    0,  277,  260,  261,
        262,  263,  264,  265,  266,  267,  268,  269,  270,  271,
        272,  273,  274,  275,    0,  277,  296,  297,  298,  299,
        261,   -1,  263,  264,   -1,   -1,   -1,   -1,   -1,   -1,
         -1,   -1,   -1,   -1,  296,  297,  298,  299,  261,  262,
        263,  264,  265,  266,  267,  268,  269,  270,  271,  272,
        273,  274,  275,    0,  277,  261,  262,  263,  264,  265,
        266,  267,  268,  269,  270,  271,  272,  273,  274,  275,
          0,  277,   -1,  296,  297,  298,  299,   -1,   -1,   -1,
         -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,
        296,  297,  298,  299,  261,  262,  263,  264,  265,  266,
        267,  268,  269,  270,  271,  272,  273,  274,  275,  261,
        277,  263,  264,  265,  266,  267,  268,  269,  270,  271,
        272,  273,  274,  275,    0,  277,   -1,   -1,   -1,  296,
        297,  298,  299,   -1,   -1,   -1,   -1,   -1,   -1,    0,
         -1,   -1,   -1,   -1,  296,  297,  298,  299,  261,   -1,
        263,  264,  265,  266,  267,  268,  269,  270,  271,  272,
        273,  274,  275,  261,  277,  263,  264,  265,  266,  267,
        268,  269,  270,  271,  272,  273,  274,  275,    0,  277,
         -1,   -1,   -1,  296,  297,  298,  299,   -1,   -1,   -1,
         -1,   -1,   -1,    0,   -1,   -1,   -1,   -1,  296,  297,
        298,  299,  261,   -1,  263,  264,  265,  266,  267,  268,
        269,  270,  271,  272,  273,  274,  275,  261,  277,  263,
        264,  265,   -1,   -1,  268,  269,  270,  271,  272,  273,
        274,  275,    0,  277,   -1,   -1,   -1,  296,  297,  298,
        299,   -1,   -1,   -1,   -1,   -1,   -1,    0,   -1,   -1,
         -1,   -1,  296,  297,  298,  299,   -1,  261,   -1,  263,
        264,  265,   -1,    0,  268,  269,  270,  271,  272,  273,
        274,  275,    0,  277,   -1,  261,    0,  263,  264,  265,
          0,   -1,  268,  269,  270,  271,  272,  273,  274,  275,
         -1,  277,  296,  297,  298,  299,    0,   -1,   -1,   -1,
         -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,
        296,  297,  298,  299,  261,   -1,  263,  264,  265,    0,
         -1,  268,  269,  270,  271,  272,  273,  274,  275,   -1,
        277,  261,   -1,  263,  264,  265,    0,   -1,  268,  269,
        270,  271,  272,  273,  274,  275,   -1,  277,   -1,  296,
        297,  298,  299,   -1,   -1,   -1,   -1,   -1,    0,   -1,
         -1,   -1,   -1,   -1,   -1,   -1,  296,  297,  298,  299,
         -1,   -1,   -1,   -1,   -1,    0,   -1,   -1,   -1,   -1,
         -1,   -1,   -1,   -1,    0,  261,   -1,  263,  264,  265,
         -1,   -1,  268,  269,  270,  271,  272,  273,  274,  275,
        261,  277,  263,  264,  265,    0,   -1,  268,  269,  270,
        271,  272,  273,  274,  275,   -1,    0,   -1,   -1,   -1,
        296,  297,  298,  299,   -1,   -1,   -1,   -1,   -1,   -1,
         -1,   -1,   -1,   -1,   -1,  296,  297,  298,  299,  261,
         -1,  263,  264,   -1,   -1,   -1,  268,  269,  270,  271,
        272,  273,  274,  275,  261,  277,  263,  264,   -1,   -1,
         -1,  268,  269,  270,  271,  272,  273,  274,  275,   -1,
         -1,   -1,   -1,   -1,  296,  297,  298,  299,   -1,   -1,
         -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,
         -1,  298,  299,  261,   -1,  263,  264,   -1,   -1,   -1,
        268,  269,  270,  271,  272,  273,  274,  275,  261,   -1,
        263,  264,   -1,   -1,   -1,  268,  269,  270,  271,  272,
        273,  274,  275,   -1,  261,   -1,  263,  264,   -1,   -1,
        298,  299,   -1,  261,   -1,  263,  264,  261,   -1,  263,
        264,  261,   -1,  263,  264,  298,  299,   -1,  268,  269,
        270,  271,  272,  273,   -1,   -1,   -1,  261,   -1,  263,
        264,  298,  299,   -1,  268,  269,  270,  271,  272,  273,
        298,  299,   -1,   -1,  298,   -1,   -1,   -1,  298,  299,
        261,   -1,  263,  264,   -1,   -1,   -1,  268,  269,  270,
        271,  272,  273,   -1,  298,  299,   -1,  261,   -1,  263,
        264,   -1,   -1,   -1,  268,  269,  270,  271,  272,  273,
         -1,   -1,   -1,   -1,   -1,   -1,   -1,  298,  299,  261,
         -1,  263,  264,   -1,   -1,   -1,  268,  269,  270,  271,
        272,  273,   -1,   -1,  298,  299,  261,   -1,  263,  264,
         -1,   -1,   -1,  268,  269,  261,   -1,  263,  264,   -1,
         -1,   -1,  268,  269,   -1,   -1,  298,  299,   -1,   -1,
         -1,   -1,   -1,   -1,   -1,   -1,  261,   -1,  263,  264,
         -1,   -1,   -1,  298,  299,   -1,   -1,  261,   -1,  263,
        264,   -1,  298,  299,  268,  269,   -1,   -1,   -1,   -1,
         -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,
         -1,   -1,   -1,  298,   -1,   -1,   -1,   -1,   -1,   -1,
         -1,   -1,   -1,   -1,  298,  299,
    };
  } /* End of class YyCheckClass */


//t  protected static final class YyRuleClass {

//t    public static final String yyRule [] = {
//t    "$accept : expr",
//t    "expr : or_expr",
//t    "location_path : relative_location_path",
//t    "location_path : absolute_location_path",
//t    "absolute_location_path : SLASH",
//t    "absolute_location_path : SLASH relative_location_path",
//t    "absolute_location_path : DOUBLE_SLASH relative_location_path",
//t    "relative_location_path : step",
//t    "relative_location_path : relative_location_path SLASH step",
//t    "relative_location_path : relative_location_path DOUBLE_SLASH step",
//t    "step : step_node_test",
//t    "step : AT step_node_test",
//t    "step : axis_name DOUBLE_COLON step_node_test",
//t    "step : DOT",
//t    "step : DOUBLE_DOT",
//t    "step_node_test : node_test",
//t    "step_node_test : step_node_test predicate",
//t    "axis_name : ANCESTOR",
//t    "axis_name : ANCESTOR_OR_SELF",
//t    "axis_name : ATTRIBUTE",
//t    "axis_name : CHILD",
//t    "axis_name : DESCENDANT",
//t    "axis_name : DESCENDANT_OR_SELF",
//t    "axis_name : FOLLOWING",
//t    "axis_name : FOLLOWING_SIBLING",
//t    "axis_name : NAMESPACE",
//t    "axis_name : PARENT",
//t    "axis_name : PRECEDING",
//t    "axis_name : PRECEDING_SIBLING",
//t    "axis_name : SELF",
//t    "node_test : name_test",
//t    "node_test : PROCESSING_INSTRUCTION LITERAL RP",
//t    "node_test : node_type RP",
//t    "predicate : LB expr RB",
//t    "primary_expr : variable_reference",
//t    "primary_expr : LP expr RP",
//t    "primary_expr : LITERAL",
//t    "primary_expr : number",
//t    "primary_expr : function_call",
//t    "function_call : function_name LP RP",
//t    "function_call : function_name LP argument_list RP",
//t    "argument_list : expr",
//t    "argument_list : expr COMMA argument_list",
//t    "union_expr : path_expr",
//t    "union_expr : union_expr PIPE path_expr",
//t    "path_expr : location_path",
//t    "path_expr : filter_expr",
//t    "path_expr : filter_expr SLASH relative_location_path",
//t    "path_expr : filter_expr DOUBLE_SLASH relative_location_path",
//t    "filter_expr : primary_expr",
//t    "filter_expr : filter_expr predicate",
//t    "or_expr : and_expr",
//t    "or_expr : or_expr OR and_expr",
//t    "and_expr : equality_expr",
//t    "and_expr : and_expr AND equality_expr",
//t    "equality_expr : relational_expr",
//t    "equality_expr : equality_expr EQ relational_expr",
//t    "equality_expr : equality_expr NE relational_expr",
//t    "relational_expr : additive_expr",
//t    "relational_expr : relational_expr LT additive_expr",
//t    "relational_expr : relational_expr GT additive_expr",
//t    "relational_expr : relational_expr LTE additive_expr",
//t    "relational_expr : relational_expr GTE additive_expr",
//t    "additive_expr : multiplicative_expr",
//t    "additive_expr : additive_expr PLUS multiplicative_expr",
//t    "additive_expr : additive_expr MINUS multiplicative_expr",
//t    "multiplicative_expr : unary_expr",
//t    "multiplicative_expr : multiplicative_expr STAR unary_expr",
//t    "multiplicative_expr : multiplicative_expr DIV unary_expr",
//t    "multiplicative_expr : multiplicative_expr MOD unary_expr",
//t    "unary_expr : union_expr",
//t    "unary_expr : MINUS unary_expr",
//t    "number : DIGITS",
//t    "number : DIGITS DOT",
//t    "number : DIGITS DOT DIGITS",
//t    "number : DOT DIGITS",
//t    "function_name : qname",
//t    "variable_reference : DOLLAR qname",
//t    "name_test : STAR",
//t    "name_test : NAME COLON STAR",
//t    "name_test : qname",
//t    "qname : NAME",
//t    "qname : NAME COLON NAME",
//t    "node_type : COMMENT",
//t    "node_type : TEXT",
//t    "node_type : PROCESSING_INSTRUCTION",
//t    "node_type : NODE",
//t    };
//t  } /* End of class YyRuleClass */

  protected static final class YyNameClass {

    public static final String yyName [] = {    
    "end-of-file",null,null,null,null,null,null,null,null,null,null,null,
    null,null,null,null,null,null,null,null,null,null,null,null,null,null,
    null,null,null,null,null,null,null,null,null,null,null,null,null,null,
    null,null,null,null,null,null,null,null,null,null,null,null,null,null,
    null,null,null,null,null,null,null,null,null,null,null,null,null,null,
    null,null,null,null,null,null,null,null,null,null,null,null,null,null,
    null,null,null,null,null,null,null,null,null,null,null,null,null,null,
    null,null,null,null,null,null,null,null,null,null,null,null,null,null,
    null,null,null,null,null,null,null,null,null,null,null,null,null,null,
    null,null,null,null,null,null,null,null,null,null,null,null,null,null,
    null,null,null,null,null,null,null,null,null,null,null,null,null,null,
    null,null,null,null,null,null,null,null,null,null,null,null,null,null,
    null,null,null,null,null,null,null,null,null,null,null,null,null,null,
    null,null,null,null,null,null,null,null,null,null,null,null,null,null,
    null,null,null,null,null,null,null,null,null,null,null,null,null,null,
    null,null,null,null,null,null,null,null,null,null,null,null,null,null,
    null,null,null,null,null,null,null,null,null,null,null,null,null,null,
    null,null,null,null,null,null,null,null,null,null,null,null,null,null,
    null,null,null,null,null,null,null,"LITERAL","DIGITS","NAME","LP",
    "RP","LB","RB","COMMA","PIPE","SLASH","DOUBLE_SLASH","EQ","NE","GT",
    "LT","GTE","LTE","PLUS","MINUS","AT","STAR","DOLLAR","COLON",
    "DOUBLE_COLON","DOT","DOUBLE_DOT","ANCESTOR","ANCESTOR_OR_SELF",
    "ATTRIBUTE","CHILD","DESCENDANT","DESCENDANT_OR_SELF","FOLLOWING",
    "FOLLOWING_SIBLING","NAMESPACE","PARENT","PRECEDING",
    "PRECEDING_SIBLING","SELF","DIV","MOD","OR","AND","COMMENT",
    "PROCESSING_INSTRUCTION","TEXT","NODE","UNARY",
    };
  } /* End of class YyNameClass */


					// line 781 "XPathParser.y"

}
					// line 1461 "-"
