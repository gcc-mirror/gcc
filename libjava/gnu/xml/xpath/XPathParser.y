%{
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
import java.util.Map;
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

%}

%token LITERAL
%token DIGITS
%token NAME

%token LP // '('
%token RP // ')'
%token LB // '['
%token RB // ']'
%token COMMA // ','
%token PIPE // '|'
%token SLASH // '/'
%token DOUBLE_SLASH // '//'
%token EQ // '='
%token NE // '!='
%token GT // '>'
%token LT // '<'
%token GTE // '>='
%token LTE // '<='
%token PLUS // '+'
%token MINUS // '-'
%token AT // '@'
%token STAR // '*'
%token DOLLAR // '$'
%token COLON // ':'
%token DOUBLE_COLON // '::'
%token DOT // '.'
%token DOUBLE_DOT // '..'

%token ANCESTOR
%token ANCESTOR_OR_SELF
%token ATTRIBUTE
%token CHILD
%token DESCENDANT
%token DESCENDANT_OR_SELF
%token FOLLOWING
%token FOLLOWING_SIBLING
%token NAMESPACE
%token PARENT
%token PRECEDING
%token PRECEDING_SIBLING
%token SELF
%token DIV
%token MOD
%token OR
%token AND
%token COMMENT
%token PROCESSING_INSTRUCTION
%token TEXT
%token NODE

%right UNARY

%start expr

%%

expr:
  or_expr
  ;

location_path:
  relative_location_path
  | absolute_location_path
  ;

absolute_location_path:
  SLASH
    {
      $$ = new Root();
    }
  | SLASH relative_location_path
    {
      Steps steps;
      if ($2 instanceof Steps)
        {
          steps = (Steps) $2;
        }
      else
        {
          steps = new Steps();
          steps.path.addFirst($2);
        }
      steps.path.addFirst(new Root());
      $$ = steps;
      //$$ = new Step(new Root(), (Path) $2);
    }
  | DOUBLE_SLASH relative_location_path
    {
      Test nt = new NodeTypeTest((short) 0);
      Selector s = new Selector(Selector.DESCENDANT_OR_SELF,
                                Collections.singletonList (nt));
      Steps steps;
      if ($2 instanceof Steps)
        {
          steps = (Steps) $2;
        }
      else
        {
          steps = new Steps();
          steps.path.addFirst($2);
        }
      steps.path.addFirst(s);
      steps.path.addFirst(new Root());
      $$ = steps;
      //Step step = new Step(s, (Path) $2);
      //$$ = new Step(new Root(), step);
    }
  ;

relative_location_path:
  step
  | relative_location_path SLASH step
    {
      Steps steps;
      if ($1 instanceof Steps)
        {
          steps = (Steps) $1;
        }
      else
        {
          steps = new Steps();
          steps.path.addFirst($1);
        }
      steps.path.addLast($3);
      $$ = steps;
      //$$ = new Step((Expr) $1, (Path) $3);
    }
  | relative_location_path DOUBLE_SLASH step
    {
      Test nt = new NodeTypeTest((short) 0);
      Selector s = new Selector(Selector.DESCENDANT_OR_SELF,
                                Collections.singletonList (nt));
      Steps steps;
      if ($1 instanceof Steps)
        {
          steps = (Steps) $1;
        }
      else
        {
          steps = new Steps();
          steps.path.addFirst($1);
        }
      steps.path.addLast(s);
      steps.path.addLast($3);
      $$ = steps;
      //Step step = new Step(s, (Path) $3);
      //$$ = new Step((Expr) $1, step);
    }
  ;

step:
  step_node_test
    {
      $$ = new Selector (Selector.CHILD, (List) $1);
    }
  | AT step_node_test
    {
      $$ = new Selector (Selector.ATTRIBUTE, (List) $2);
    }
  | axis_name DOUBLE_COLON step_node_test
    {
      $$ = new Selector (((Integer) $1).intValue (), (List) $3);
    }
  | DOT
    {
      $$ = new Selector (Selector.SELF, Collections.EMPTY_LIST);
    }
  | DOUBLE_DOT
    {
      $$ = new Selector (Selector.PARENT, Collections.EMPTY_LIST);
    }
  ;

step_node_test:
  node_test
    {
      List list = new ArrayList();
      list.add($1);
      $$ = list;
    }
  | step_node_test predicate
    {
      List list = (List)$1;
      list.add($2);
      $$ = list;
    }
  ;

/*predicate_list:
  predicate
    {
      List list = new ArrayList ();
      list.add ($1);
      $$ = list;
    }
  | predicate predicate_list
    {
      List list = (List) $3;
      list.add (0, $1);
      $$ = list;
    }
  ;*/

axis_name:
  ANCESTOR
    {
      $$ = new Integer(Selector.ANCESTOR);
    }
  | ANCESTOR_OR_SELF
    {
      $$ = new Integer(Selector.ANCESTOR_OR_SELF);
    }
  | ATTRIBUTE
    {
      $$ = new Integer(Selector.ATTRIBUTE);
    }
  | CHILD
    {
      $$ = new Integer(Selector.CHILD);
    }
  | DESCENDANT
    {
      $$ = new Integer(Selector.DESCENDANT);
    }
  | DESCENDANT_OR_SELF
    {
      $$ = new Integer(Selector.DESCENDANT_OR_SELF);
    }
  | FOLLOWING
    {
      $$ = new Integer(Selector.FOLLOWING);
    }
  | FOLLOWING_SIBLING
    {
      $$ = new Integer(Selector.FOLLOWING_SIBLING);
    }
  | NAMESPACE
    {
      $$ = new Integer(Selector.NAMESPACE);
    }
  | PARENT
    {
      $$ = new Integer(Selector.PARENT);
    }
  | PRECEDING
    {
      $$ = new Integer(Selector.PRECEDING);
    }
  | PRECEDING_SIBLING
    {
      $$ = new Integer(Selector.PRECEDING_SIBLING);
    }
  | SELF
    {
      $$ = new Integer(Selector.SELF);
    }
  ;

node_test:
  name_test
  /*| PROCESSING_INSTRUCTION LP LITERAL RP*/
  | PROCESSING_INSTRUCTION LITERAL RP
    {
      $$ = new NodeTypeTest(Node.PROCESSING_INSTRUCTION_NODE, (String) $2);
    }
  /*| node_type LP RP*/
  | node_type RP
    {
      $$ = new NodeTypeTest(((Short) $1).shortValue());
    }
  ;

predicate:
  LB expr RB
    {
      $$ = new Predicate((Expr) $2);
    }
  ;

primary_expr:
  variable_reference
  | LP expr RP
    {
      $$ = new ParenthesizedExpr((Expr) $2);
    }
  | LITERAL
    {
      $$ = new Constant($1);
    }
  | number
    {
      $$ = new Constant($1);
    }
  | function_call
  ;

function_call:
  function_name LP RP
    {
      $$ = lookupFunction((String) $1, Collections.EMPTY_LIST);
    }
  | function_name LP argument_list RP
    {
      $$ = lookupFunction((String) $1, (List) $3);
    }
  ;

argument_list:
  expr
    {
      List list = new ArrayList();
      list.add($1);
      $$ = list;
    }
  | expr COMMA argument_list
    {
      List list = (List) $3;
      list.add(0, $1);
      $$ = list;
    }
  ;

union_expr:
  path_expr
  | union_expr PIPE path_expr
    {
      $$ = new UnionExpr((Expr) $1, (Expr) $3);
    }
  ;

path_expr:
  location_path
  | filter_expr
  | filter_expr SLASH relative_location_path
    {
      Steps steps;
      if ($3 instanceof Steps)
        {
          steps = (Steps) $3;
        }
      else
        {
          steps = new Steps();
          steps.path.addFirst($3);
        }
      steps.path.addFirst($1);
      $$ = steps;
      //$$ = new Step ((Expr) $1, (Path) $3);
    }
  | filter_expr DOUBLE_SLASH relative_location_path
    {
      Test nt = new NodeTypeTest((short) 0);
      Selector s = new Selector(Selector.DESCENDANT_OR_SELF,
                                Collections.singletonList(nt));
      Steps steps;
      if ($3 instanceof Steps)
        {
          steps = (Steps) $3;
        }
      else
        {
          steps = new Steps();
          steps.path.addFirst($3);
        }
      steps.path.addFirst(s);
      steps.path.addFirst($1);
      $$ = steps;
      //Step step = new Step (s, (Path) $3);
      //$$ = new Step ((Expr) $1, step);
    }
  ;

filter_expr:
  primary_expr
  | filter_expr predicate
    {
      Predicate filter = (Predicate) $2;
      Selector s = new Selector(Selector.SELF,
                                Collections.singletonList(filter));
      Steps steps;
      if ($1 instanceof Steps)
        {
          steps = (Steps) $1;
        }
      else
        {
          steps = new Steps();
          steps.path.addFirst($1);
        }
      steps.path.addLast(s);
      $$ = steps;
      //$$ = new Step ((Expr) $1, s);
    }
  ;

or_expr:
  and_expr
  | or_expr OR and_expr
    {
      $$ = new OrExpr((Expr) $1, (Expr) $3);
    }
  ;

and_expr:
  equality_expr
  | and_expr AND equality_expr
    {
      $$ = new AndExpr((Expr) $1, (Expr) $3);
    }
  ;

equality_expr:
  relational_expr
  | equality_expr EQ relational_expr
    {
      $$ = new EqualityExpr((Expr) $1, (Expr) $3, false);
    }
  | equality_expr NE relational_expr
    {
      $$ = new EqualityExpr((Expr) $1, (Expr) $3, true);
    }
  ;

relational_expr:
  additive_expr
  | relational_expr LT additive_expr
    {
      $$ = new RelationalExpr((Expr) $1, (Expr) $3, true, false);
    }
  | relational_expr GT additive_expr
    {
      $$ = new RelationalExpr((Expr) $1, (Expr) $3, false, false);
    }
  | relational_expr LTE additive_expr
    {
      $$ = new RelationalExpr((Expr) $1, (Expr) $3, true, true);
    }
  | relational_expr GTE additive_expr
    {
      $$ = new RelationalExpr((Expr) $1, (Expr) $3, false, true);
    }
  ;

additive_expr:
  multiplicative_expr
  | additive_expr PLUS multiplicative_expr
    {
      $$ = new ArithmeticExpr((Expr) $1, (Expr) $3, ArithmeticExpr.ADD);
    }
  | additive_expr MINUS multiplicative_expr
    {
      $$ = new ArithmeticExpr((Expr) $1, (Expr) $3, ArithmeticExpr.SUBTRACT);
    }
  ;

multiplicative_expr:
  unary_expr
  | multiplicative_expr STAR unary_expr
    {
      $$ = new ArithmeticExpr((Expr) $1, (Expr) $3, ArithmeticExpr.MULTIPLY);
    }
  | multiplicative_expr DIV unary_expr
    {
      $$ = new ArithmeticExpr((Expr) $1, (Expr) $3, ArithmeticExpr.DIVIDE);
    }
  | multiplicative_expr MOD unary_expr
    {
      $$ = new ArithmeticExpr((Expr) $1, (Expr) $3, ArithmeticExpr.MODULO);
    }
  ;

unary_expr:
  union_expr
  | MINUS unary_expr %prec UNARY
    {
      $$ = new NegativeExpr((Expr) $2);
    }
  ;

number:
  DIGITS
    {
      $$ = new Double((String) $1 + ".0");
    }
  | DIGITS DOT
    {
      $$ = new Double((String) $1 + ".0");
    }
  | DIGITS DOT DIGITS
    {
      $$ = new Double((String) $1 + "." + (String) $3);
    }
  | DOT DIGITS
    {
      $$ = new Double("0." + (String) $2);
    }
  ;

function_name:
  qname
/*  | node_type
    {
      switch (((Short) $1).shortValue ())
        {
        case Node.COMMENT_NODE:
          $$ = "comment";
          break;
        case Node.TEXT_NODE:
          $$ = "text";
          break;
        case Node.PROCESSING_INSTRUCTION_NODE:
          $$ = "processing-instruction";
          break;
        default:
          $$ = "node";
          break;
        }
    }*/
  ;

variable_reference:
  DOLLAR qname
    {
      $$ = new VariableReference(variableResolver, (String) $2);
    }
  ;

name_test:
  STAR
    {
      $$ = new NameTest(null, true, true);
    }
  | NAME COLON STAR
    {
      QName qName = getQName((String) $1);
      $$ = new NameTest(qName, true, false);
    }
  | qname
    {
      QName qName = getQName((String) $1);
      $$ = new NameTest(qName, false, false);
    }
  ;

qname:
  NAME
  | NAME COLON NAME
    {
      $$ = (String) $1 + ':' + (String) $3;
    }
  ;

node_type:
  COMMENT
    {
      $$ = new Short(Node.COMMENT_NODE);
    }
  | TEXT
    {
      $$ = new Short(Node.TEXT_NODE);
    }
  | PROCESSING_INSTRUCTION
    {
      $$ = new Short(Node.PROCESSING_INSTRUCTION_NODE);
    }
  | NODE
    {
      $$ = new Short((short) 0);
    }
  ;

%%

}
