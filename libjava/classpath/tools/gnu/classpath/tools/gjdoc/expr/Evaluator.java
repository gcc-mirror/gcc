/* gnu.classpath.tools.gjdoc.expr.Evaluator
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
02111-1307 USA. */

package gnu.classpath.tools.gjdoc.expr;

import java.io.StringReader;
import java.math.BigInteger;
import antlr.RecognitionException;
import antlr.TokenStreamException;
import java.util.Set;

public class Evaluator
{
   /**
    *  Try to evaluate the given Java expression in the context of the
    *  given environment.
    *
    *  @param expression the Java expression to evaluate. The
    *  expression string must not include a terminating semicolon.
    *
    *  @param source the FieldDoc (part of) whose constant field value
    *  expression is being evaluated.  Used to prevent circular
    *  references.
    *
    *  @param environment callback hook used by the Evaluator to query
    *  the value of static fields referenced in the expression.
    *
    *  @return a Java object representing the value of the expression,
    *  or <code>null</code> if the expression evaluates to
    *  <code>null</code>.
    *
    *  @throws IllegalExpressionException if the expression is
    *  invalid, uses unsupported syntax constructs (e.g. method calls,
    *  array access) or references unknown static fields.
    */
   public static Object evaluate(String expression, 
                                 Set visitedFields,
                                 EvaluatorEnvironment environment)
      throws IllegalExpressionException
   {
      try {
         JavaLexer lexer = new JavaLexer(new StringReader(expression));
         JavaRecognizer recognizer = new JavaRecognizer(lexer);
         Expression e = recognizer.expression();
         ConstantExpression value = e.evaluate(new Context(environment, visitedFields));
         return value.asObject();
      }
      catch (RecognitionException e) {
         throw new IllegalExpressionException(e);
      }
      catch (TokenStreamException e) {
         throw new IllegalExpressionException(e);
      }
   }

   static int parseInt(String stringValue)
   {
      int base = 10;

      if (stringValue.startsWith("0x")) {
         base = 16;
         stringValue = stringValue.substring(2);
      }
      else if (stringValue.length() > 1 && stringValue.startsWith("0")) {
         base = 8;
         stringValue = stringValue.substring(1);
      }
      while (stringValue.length() > 1 && stringValue.startsWith("0")) {
         stringValue = stringValue.substring(1);
      }

      if (10 == base) {
         return Integer.parseInt(stringValue);
      }
      else {
         long result = Long.parseLong(stringValue, base);

         if (result > Integer.MAX_VALUE) {
            result -= 0x100000000L;
         }

         if (result > Integer.MAX_VALUE) {
            throw new NumberFormatException(result + " > " + Integer.MAX_VALUE);
         }
         else if (result < Integer.MIN_VALUE) {
            throw new NumberFormatException(result + " < " + Integer.MIN_VALUE);
         }
         else {
            return (int)result;
         }
      }
   }

   static long parseLong(String stringValue)
   {
      int base = 10;

      if (stringValue.startsWith("0x")) {
         base = 16;
         stringValue = stringValue.substring(2);
      }
      else if (stringValue.length() > 1 && stringValue.startsWith("0")) {
         base = 8;
         stringValue = stringValue.substring(1);
      }
      while (stringValue.length() > 1 && stringValue.startsWith("0")) {
         stringValue = stringValue.substring(1);
      }

      BigInteger bigInt = new BigInteger(stringValue, base);
      long result = bigInt.longValue();
      return result;
   }
}
