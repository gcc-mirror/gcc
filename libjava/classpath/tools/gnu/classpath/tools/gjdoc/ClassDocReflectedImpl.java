/* gnu.classpath.tools.gjdoc.ClassDocReflectedImpl
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
import java.util.Map;
import java.util.HashMap;

public class ClassDocReflectedImpl
   implements ClassDoc, WritableType
{
   private Class clazz;
   private String name;
   private ClassDoc superclassDoc;
   private ClassDoc[] unfilteredInnerClasses;
   private String dimension = "";

   private static Map reflectionCache = new HashMap();

   public static ClassDocReflectedImpl newInstance(Class clazz)
   {
      ClassDocReflectedImpl result 
         = (ClassDocReflectedImpl)reflectionCache.get(clazz);
      if (null != result) {
         return result;
      }
      else {
         return new ClassDocReflectedImpl(clazz);
      }
   }

   public ClassDocReflectedImpl(Class clazz)
   {
      reflectionCache.put(clazz, this);

      //System.err.println("ClassDocReflectedImpl: " + clazz);
      
      this.clazz = clazz;
      String className = clazz.getName();
      int ndx = className.lastIndexOf('.');
      if (ndx >= 0) {
         this.name = className.substring(ndx + 1);
      }
      else {
         this.name = className;
      }

      Class superclass = clazz.getSuperclass();
      if (null != superclass && !clazz.getName().equals("java.lang.Object")) {
         this.superclassDoc = (ClassDocReflectedImpl)reflectionCache.get(superclass);
         if (null == this.superclassDoc) {
            this.superclassDoc = new ClassDocReflectedImpl(superclass);
         }
      }

      Class[] innerclasses = clazz.getDeclaredClasses();
      this.unfilteredInnerClasses = new ClassDoc[innerclasses.length];
      for (int i=0; i<innerclasses.length; ++i) {
         this.unfilteredInnerClasses[i] = (ClassDocReflectedImpl)reflectionCache.get(innerclasses[i]);
         if (null == this.unfilteredInnerClasses[i]) {
            this.unfilteredInnerClasses[i] = new ClassDocReflectedImpl(innerclasses[i]);
            //System.err.println("adding " + this.unfilteredInnerClasses[i] + " [" + innerclasses[i] + "] as inner class of " + this + " [" + clazz + "]");
         }
      }
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
   public ClassDoc[] innerClasses(boolean filtered) 
   { 
      if (filtered) {
         return new ClassDoc[0];
      }
      else {
         return unfilteredInnerClasses;
      }
   }
   
   public ClassDoc[] interfaces() { return new ClassDoc[0]; } 
   public boolean isAbstract() { return false; } 
   public boolean isExternalizable() { return false; } 
   public boolean isSerializable() { return false; } 
   public MethodDoc[] methods() { return new MethodDoc[0]; } 
   public MethodDoc[] methods(boolean filtered) { return new MethodDoc[0]; } 
   public FieldDoc[] serializableFields() { return new FieldDoc[0]; } 
   public MethodDoc[] serializationMethods() { return new MethodDoc[0]; } 
   public boolean subclassOf(ClassDoc cd) { return false; } 
   public ClassDoc superclass() { 
      return superclassDoc;
   } 
   public ClassDoc containingClass() 
   {
      Class declaringClass = clazz.getDeclaringClass();
      if (null != declaringClass) {
         return new ClassDocReflectedImpl(declaringClass);
      }
      else {
         return null;
      }
   } 
   public PackageDoc containingPackage() 
   {
      Class outerClass = clazz;
      while (null != outerClass.getDeclaringClass()) {
         outerClass = outerClass.getDeclaringClass();
      }

      String packageName = outerClass.getName();
      int ndx = packageName.lastIndexOf('.');
      if (ndx > 0) {
         packageName = packageName.substring(0, ndx);
      }
      else {
         packageName = "";
      }
      PackageDoc result =  Main.getRootDoc().findOrCreatePackageDoc(packageName);
      return result;
   }

   public boolean isFinal() { return false; } 
   public boolean isPackagePrivate() { return false; } 
   public boolean isPrivate() { return false; } 
   public boolean isProtected() { return false; } 
   public boolean isPublic() { return false; } 
   public boolean isStatic() { return false; } 
   public String modifiers() { return ""; } 
   public int modifierSpecifier() { return 0; } 
   public String qualifiedName() { return clazz.getName().replace('$', '.'); } 
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
   public String qualifiedTypeName() { return qualifiedName(); }
   public ClassDoc asClassDoc() { return this; }
   public TypeVariable asTypeVariable() { return null; }
   public boolean isPrimitive() { return false; }

   public String toString() { return "ClassDocReflectedImpl{"+qualifiedName()+"}"; }

   public int compareTo(java.lang.Object o) {
      if (o instanceof Doc) {
         return Main.getInstance().getCollator().compare(name(), ((Doc)o).name());
      }
      else {
         return 0;
      }
   }

   public String dimension() { return dimension; }

   public void setDimension(String dimension) {
      this.dimension = dimension;
   }

   public Object clone() throws CloneNotSupportedException {
      return super.clone();
   }

   public TypeVariable[] typeParameters() { return new TypeVariable[0]; } 

}
