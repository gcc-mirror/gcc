/* gnu.classpath.tools.gjdoc.expr.TypeCastExpression
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

class TypeCastExpression
   extends UnaryExpression
{
   private Type type;

   public TypeCastExpression(Type type, Expression expr)
   {
      super(expr);
      this.type = type;
   }

   public ConstantExpression evaluate(Context context)
      throws IllegalExpressionException
   {
      ConstantExpression value = expr.evaluate(context);

      if (Type.BYTE == type) {
         return new ConstantByte(value.asNumber().byteValue());
      }
      else if (Type.SHORT == type) {
         return new ConstantShort(value.asNumber().shortValue());
      }
      else if (Type.INTEGER == type) {
         return new ConstantInteger(value.asNumber().intValue());
      }
      else if (Type.LONG == type) {
         return new ConstantLong(value.asNumber().longValue());
      }
      else if (Type.CHAR == type) {
         return new ConstantChar((char)value.asNumber().intValue());
      }
      else if (Type.FLOAT == type) {
         return new ConstantFloat((float)value.asNumber().intValue());
      }
      else if (Type.DOUBLE == type) {
         return new ConstantDouble((float)value.asNumber().intValue());
      }
      else if (Type.BOOLEAN == type && Type.BOOLEAN == value.getType()) {
         return value;
      }
      else if (Type.STRING == type && Type.STRING == value.getType()) {
         return value;
      }
      else {
         throw new IllegalExpressionException("Cannot cast " + value.getType() + " to " + type);
      }
   }
}
