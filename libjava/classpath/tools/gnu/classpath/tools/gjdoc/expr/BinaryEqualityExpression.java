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
02111-1307 USA. */

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
