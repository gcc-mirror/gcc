/* gnu.classpath.tools.gjdoc.TypeImpl
   Copyright (C) 2001 Free Software Foundation, Inc.

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

package gnu.classpath.tools.gjdoc;

import com.sun.javadoc.*;
import java.util.Collections;
import java.util.Set;
import java.util.HashSet;

public class TypeImpl implements Type, WritableType {

   private String packageName;
   private String typeName;
   private String dimension;

   TypeImpl(String packageName, String typeName, String dimension) {
      this.packageName=packageName;
      this.typeName=typeName;
      this.dimension=dimension;

      if (typeName.indexOf('[') >= 0 || typeName.indexOf(']') >= 0) {
         throw new RuntimeException("Typename must not contain dimension information.");
      }
   }

   public ClassDoc asClassDoc() {

      if (this instanceof ClassDoc)
	 return ((ClassDocImpl)(ClassDoc)this).getBaseClassDoc();
      else
	 return null;
   }

   public String typeName() { return typeName; }
   
   public String qualifiedTypeName() { return (packageName!=null)?(packageName+"."+typeName):(typeName); }

   public String dimension() { return dimension; }
   public void setDimension(String dimension) { this.dimension = dimension; }

   public String toString() { return "Type{"+qualifiedTypeName()+dimension()+"}"; }

   public Object clone() throws CloneNotSupportedException {
      return super.clone();
   }

   public boolean isPrimitive()
   {
      return null == packageName && primitiveNames.contains(typeName);
   }

   private static final Set primitiveNames;
   static {
      Set _primitiveNames = new HashSet();
      _primitiveNames.add("boolean");
      _primitiveNames.add("char");
      _primitiveNames.add("byte");
      _primitiveNames.add("short");
      _primitiveNames.add("int");
      _primitiveNames.add("long");
      _primitiveNames.add("float");
      _primitiveNames.add("double");
      primitiveNames = Collections.unmodifiableSet(_primitiveNames);
   }

  public TypeVariable asTypeVariable() 
  {
    if (this instanceof TypeVariable)
      return (TypeVariable) this;
    else
      return null;
  }

}
