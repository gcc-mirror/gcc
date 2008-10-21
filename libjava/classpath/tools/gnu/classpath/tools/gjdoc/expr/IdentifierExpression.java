/* gnu.classpath.tools.gjdoc.expr.IdentifierExpression
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

class IdentifierExpression
   implements Expression
{
   private String identifier;

   public IdentifierExpression(String identifier)
   {
      this.identifier = identifier;
   }

   public ConstantExpression evaluate(Context context)
      throws IllegalExpressionException
   {
      Object value = context.getEvaluatorEnvironment().getValue(identifier, context.getVisitedFields());

      if (value instanceof Byte) {
         return new ConstantByte(((Byte)value).byteValue());
      }
      else if (value instanceof Short) {
         return new ConstantShort(((Short)value).shortValue());
      }
      else if (value instanceof Integer) {
         return new ConstantInteger(((Integer)value).intValue());
      }
      else if (value instanceof Long) {
         return new ConstantLong(((Long)value).longValue());
      }
      else if (value instanceof Float) {
         return new ConstantFloat(((Float)value).floatValue());
      }
      else if (value instanceof Double) {
         return new ConstantDouble(((Double)value).doubleValue());
      }
      else if (value instanceof Boolean) {
         return new ConstantBoolean(((Boolean)value).booleanValue());
      }
      else if (value instanceof Character) {
         return new ConstantChar(((Character)value).charValue());
      }
      else if (value instanceof String) {
         return new ConstantString((String)value);
      }
      else if (null != value) {
         throw new IllegalExpressionException("Unsupported type " + value.getClass().getName() + " for identifier " + identifier);
      }
      else {
         throw new IllegalExpressionException("Cannot resolve identifier " + identifier);
      }
   }
}
