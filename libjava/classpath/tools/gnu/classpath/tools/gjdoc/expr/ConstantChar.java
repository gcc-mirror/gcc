/* gnu.classpath.tools.gjdoc.expr.ConstantChar
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

class ConstantChar
   extends ConstantExpression
{
   private char value;

   public ConstantChar(String stringValue)
   {
      this.value = stringValue.charAt(1); // FIXME
      if (value == '\\') {
         switch (stringValue.charAt(2)) {
         case 'n': value = '\n'; break;
         case 't': value = '\t'; break;
         case 'f': value = '\f'; break;
         case 'r': value = '\r'; break;
         case 'b': value = '\b'; break;
         case 'u': 
            {
               String stringVal = stringValue.substring(3, stringValue.length() - 1);
               /*
               while (stringVal.length() > 1 && stringVal.charAt(0) == '0') {
                  stringVal = stringVal.substring(1);
               }
               */
               value = (char)Integer.parseInt(stringVal, 16); break;
            }
         }
      }
   }

   public ConstantChar(char charValue)
   {
      this.value = charValue;
   }

   public Type getType()
   {
      return Type.CHAR;
   }

   public Number asNumber()
   {
      return new Integer((int)value);
   }

   public Object asObject()
   {
      return new Character(value);
   }

   public boolean isNumber()
   {
      return true;
   }

   public String toString()
   {
      return Character.toString(value);
   }
}
