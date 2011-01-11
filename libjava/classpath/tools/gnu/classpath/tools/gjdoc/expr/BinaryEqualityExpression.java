/* gnu.classpath.tools.gjdoc.expr.BinaryEqualityExpression
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

package gnu.classpath.tools.gjdoc.expr;

abstract class BinaryEqualityExpression
   extends BinaryExpression
{
   protected BinaryEqualityExpression(Expression left, Expression right)
   {
      super(left, right);
   }

   public ConstantExpression evaluate(Context context)
      throws IllegalExpressionException
   {
      ConstantExpression leftValue = left.evaluate(context);
      ConstantExpression rightValue = right.evaluate(context);

      if (Type.DOUBLE == leftValue.getType()
          || Type.DOUBLE == rightValue.getType()) {

         return new ConstantBoolean(compute(leftValue.asNumber().doubleValue(),
                                            rightValue.asNumber().doubleValue()));
      }
      else if (Type.FLOAT == leftValue.getType()
          || Type.FLOAT == rightValue.getType()) {

         return new ConstantBoolean(compute(leftValue.asNumber().floatValue(),
                                            rightValue.asNumber().floatValue()));
      }
      else if (Type.LONG == leftValue.getType()
          || Type.LONG == rightValue.getType()) {

         return new ConstantBoolean(compute(leftValue.asNumber().longValue(),
                                            rightValue.asNumber().longValue()));
      }
      else if (Type.BOOLEAN == leftValue.getType()
               && Type.BOOLEAN == rightValue.getType()) {

         return new ConstantBoolean(compute(((ConstantBoolean)leftValue).booleanValue(),
                                            ((ConstantBoolean)rightValue).booleanValue()));
      }
      else {
         return new ConstantBoolean(compute(leftValue.asNumber().intValue(),
                                            rightValue.asNumber().intValue()));
      }
   }

   protected abstract boolean compute(double leftValue, double rightValue);
   protected abstract boolean compute(float leftValue, float rightValue);
   protected abstract boolean compute(long leftValue, long rightValue);
   protected abstract boolean compute(int leftValue, int rightValue);
   protected abstract boolean compute(boolean leftValue, boolean rightValue);
}
