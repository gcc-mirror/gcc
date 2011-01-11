/* gnu.classpath.tools.gjdoc.ProgramElementDocImpl
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

package gnu.classpath.tools.gjdoc;

import com.sun.javadoc.*;
import java.lang.reflect.Modifier;

public abstract class ProgramElementDocImpl extends DocImpl implements ProgramElementDoc {

   protected ClassDoc containingClass;
   protected PackageDoc containingPackage;

   protected boolean isFinal;
   protected boolean isStatic;
   protected int     accessLevel=ProgramElementDocImpl.ACCESS_PACKAGEPRIVATE;

   public static final int ACCESS_PUBLIC          = 0;
   public static final int ACCESS_PROTECTED       = 1;
   public static final int ACCESS_PACKAGEPRIVATE  = 2;
   public static final int ACCESS_PRIVATE         = 3;

   private static final String[] accessModifiers = { "public ", "protected ", "", "private "};

   public ProgramElementDocImpl(ClassDoc containingClass,
                                PackageDoc containingPackage,
                                SourcePosition position) {
      super(position);
      this.containingClass=containingClass;
      this.containingPackage=containingPackage;
   }
   public ProgramElementDocImpl(ClassDoc containingClass, SourcePosition position) {
      super(position);
      this.containingClass=containingClass;
      this.containingPackage=containingClass.containingPackage();
   }
   public ProgramElementDocImpl(ClassDoc containingClass,
                                PackageDoc containingPackage,
                                int accessLevel,
                                boolean isFinal,
                                boolean isStatic,
                                SourcePosition position) {
      super(position);
      this.containingClass=containingClass;
      this.containingPackage=containingPackage;
      this.accessLevel=accessLevel;
      this.isFinal=isFinal;
      this.isStatic=isStatic;
   }

   //Get the containing class of this program element.
   public ClassDoc containingClass() {
      return containingClass;
   }

   // Get the package that this program element is contained in.
   public PackageDoc containingPackage() {
      return containingPackage;
   }

   // Return true if this program element is final
   public boolean isFinal() {
      return isFinal;
   }

   // Return true if this program element is package private
   public boolean isPackagePrivate() {
      return accessLevel==ACCESS_PACKAGEPRIVATE;
   }

   // Return true if this program element is private
   public boolean isPrivate() {
      return accessLevel==ACCESS_PRIVATE;
   }

   // Return true if this program element is protected
   public boolean isProtected() {
      return accessLevel==ACCESS_PROTECTED;
   }

   // Return true if this program element is public
   public boolean isPublic() {
      return accessLevel==ACCESS_PUBLIC;
   }

   // Return true if this program element is static
   public boolean isStatic() {
      return isStatic;
   }

   // Get modifiers string.
   public String modifiers() {
      return
         (accessModifiers[accessLevel]+
          (isStatic()?"static ":"")+
          (isFinal()?"final ":"")).trim();
   }

   // Get the modifier specifier integer.
   public int modifierSpecifier() {
      return (isStatic()?Modifier.STATIC:0)
         | (isFinal()?Modifier.FINAL:0)
         | (isPublic()?Modifier.PUBLIC:0)
         | (isProtected()?Modifier.PROTECTED:0)
         | (isPrivate()?Modifier.PRIVATE:0)
//       | (isAbstract()?Modifier.ABSTRACT:0)
         ;
   }

   // Get the fully qualified name.
   public abstract String qualifiedName();

   protected boolean processModifier(String word) {
      if (word.equals("public")) {
         accessLevel=ACCESS_PUBLIC;
         return true;
      }
      else if (word.equals("protected")) {
         accessLevel=ACCESS_PROTECTED;
         return true;
      }
      else if (word.equals("private")) {
         accessLevel=ACCESS_PRIVATE;
         return true;
      }
      else if (word.equals("static")) {
         isStatic=true;
         return true;
      }
      else if (word.equals("final")) {
         isFinal=true;
         return true;
      }
      else {
         return false;
      }
   }

   void setIsStatic(boolean b) {
      this.isStatic=b;
   }

}
