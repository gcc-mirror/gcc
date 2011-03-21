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
