/* gnu.classpath.tools.gjdoc.ClassDocProxy
   Copyright (C) 2001, 2012 Free Software Foundation, Inc.

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

import java.io.*;

public class ClassDocProxy implements ClassDoc, WritableType {

   private String name;
   private String qualifiedName;
   private ClassDoc classContext;
   private String dimension = "";

   public ClassDocProxy(String qualifiedName, ClassDoc classContext)
   {
      this.qualifiedName
         = Main.getRootDoc().resolveClassName(qualifiedName,
                                              (ClassDocImpl)classContext);
      this.classContext=classContext;
      int pndx=qualifiedName.lastIndexOf('.');
      if (pndx>=0) {
         this.name=qualifiedName.substring(pndx+1);
      }
      else {
         this.name=qualifiedName;
      }
   }

   private final String errorText()
   {
      return "CLASS "+qualifiedName+" NOT LOADED.";
   }

   public ConstructorDoc[] constructors() { return new ConstructorDoc[0]; }
   public ConstructorDoc[] constructors(boolean filtered) { return new ConstructorDoc[0]; }
   public boolean definesSerializableFields() { return false; }
   public FieldDoc[] fields() { return new FieldDoc[0]; }
   public FieldDoc[] fields(boolean filtered) { return new FieldDoc[0]; }
   public ClassDoc findClass(java.lang.String className) { return null; }
   public ClassDoc[] importedClasses() { return new ClassDoc[0]; }
   public PackageDoc[] importedPackages() { return new PackageDoc[0]; }
   public ClassDoc[] innerClasses() { return new ClassDoc[0]; }
   public ClassDoc[] innerClasses(boolean filtered) { return new ClassDoc[0]; }
   public ClassDoc[] interfaces() { return new ClassDoc[0]; }
   public boolean isAbstract() { return false; }
   public boolean isExternalizable() { return false; }
   public boolean isSerializable() { return false; }
   public MethodDoc[] methods() { return new MethodDoc[0]; }
   public MethodDoc[] methods(boolean filtered) { return new MethodDoc[0]; }
   public FieldDoc[] serializableFields() { return new FieldDoc[0]; }
   public MethodDoc[] serializationMethods() { return new MethodDoc[0]; }
   public boolean subclassOf(ClassDoc cd) { return false; }
   public ClassDoc superclass() { return null; }
   public ClassDoc containingClass() { return null; }
   public PackageDoc containingPackage() {
      /*
      try {
         File file=Main.getRootDoc().findScheduledClass(qualifiedName, classContext);
         if (file!=null) {
            //String clsName=file.getCanonicalFile().getAbsolutePath().substring(new File(Main.getRootDoc().getSourcePath()).getCanonicalFile().getAbsolutePath().length()+1);
            String clsName=file.getAbsolutePath().substring(new File(Main.getRootDoc().getSourcePath()).getAbsolutePath().length()+1);
            clsName=clsName.substring(0,clsName.length()-5).replace(File.separatorChar,'.');
            Debug.log(9,"ClassDocProxy '"+qualifiedName+"': found class "+clsName);
            qualifiedName=clsName;
         }
         return new PackageDocImpl("test.");
      }
      catch (Exception e) {
         return PackageDocImpl.DEFAULT_PACKAGE;
      }
      */
      return PackageDocImpl.DEFAULT_PACKAGE;
   }

   public boolean isFinal() { return false; }
   public boolean isPackagePrivate() { return false; }
   public boolean isPrivate() { return false; }
   public boolean isProtected() { return false; }
   public boolean isPublic() { return false; }
   public boolean isStatic() { return false; }
   public String modifiers() { return ""; }
   public int modifierSpecifier() { return 0; }
   public String qualifiedName() { return qualifiedName; }
   public String commentText() { return null; }
   public Tag[] firstSentenceTags() { return new Tag[0]; }
   public String getRawCommentText() { return null; }
   public Tag[] inlineTags() { return new Tag[0]; }
   public boolean isClass() { return false; }
   public boolean isConstructor() { return false; }
   public boolean isError() { return false; }
   public boolean isException() { return false; }
   public boolean isField() { return false; }
   public boolean isIncluded() { return false; }
   public boolean isInterface() { return false; }
   public boolean isMethod() { return false; }
   public boolean isOrdinaryClass() { return false; }
   public String name() { return name; }
   public SourcePosition position() { return null; }
   public SeeTag[] seeTags() { return new SeeTag[0]; }
   public void setRawCommentText(java.lang.String rawDocumentation) {}
   public Tag[] tags() { return new Tag[0]; }
   public Tag[] tags(java.lang.String tagname) { return new Tag[0]; }
   public String typeName() { return name; }
   public String qualifiedTypeName() { return qualifiedName; }
   public String dimension() { return dimension; }
   public ClassDoc asClassDoc() { return this; }
   public TypeVariable asTypeVariable() { return null; }
   public boolean isPrimitive() { return false; }

   public String toString() { return "ClassDocProxy{"+qualifiedName+", context="+classContext+"}"; }

   public void setDimension(String dimension) {
      this.dimension = dimension;
   }

   public Object clone() throws CloneNotSupportedException {
      return super.clone();
   }

   // Compares this Object with the specified Object for order.
   public int compareTo(Doc d) {
     return Main.getInstance().getCollator().compare(name(), d.name());
   }

   public TypeVariable[] typeParameters() { return new TypeVariable[0]; }

}
