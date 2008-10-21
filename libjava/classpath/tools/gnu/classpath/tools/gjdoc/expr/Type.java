/* gnu.classpath.tools.gjdoc.expr.Type
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

class Type
{
   public static final Type LONG = new Type(Long.TYPE);
   public static final Type INTEGER = new Type(Integer.TYPE);
   public static final Type BOOLEAN = new Type(Boolean.TYPE);
   public static final Type DOUBLE = new Type(Double.TYPE);
   public static final Type FLOAT = new Type(Float.TYPE);
   public static final Type CHAR = new Type(Character.TYPE);
   public static final Type BYTE = new Type(Byte.TYPE);
   public static final Type SHORT = new Type(Short.TYPE);
   public static final Type VOID = new Type(Void.TYPE);
   public static final Type STRING = new Type(String.class);
   public static final Type NULL = new Type(null);

   private Class clazz;

   private Type(Class clazz)
   {
      this.clazz = clazz;
   }
}
