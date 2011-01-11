/* gnu.classpath.tools.gjdoc.ClassDocImpl
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
import java.util.*;
import java.io.*;
import gnu.classpath.tools.gjdoc.expr.EvaluatorEnvironment;
import gnu.classpath.tools.gjdoc.expr.CircularExpressionException;
import gnu.classpath.tools.gjdoc.expr.IllegalExpressionException;
import gnu.classpath.tools.gjdoc.expr.UnknownIdentifierException;

public class ClassDocImpl
   extends ProgramElementDocImpl
   implements ClassDoc, WritableType, EvaluatorEnvironment {

   private ClassDoc baseClassDoc;
   private ClassDoc[] importedClasses;
   private PackageDoc[] importedPackages;
   private boolean definesSerializableFields;
   private FieldDoc[] serialPersistentField;
   private MethodDoc[] serializationMethods;
   private String dimension = "";

   public ClassDocImpl(ClassDoc containingClass,
                       PackageDoc containingPackage,
                       int accessLevel,
                       boolean isFinal,
                       boolean isStatic,
                       SourcePosition position) {
      super(containingClass, containingPackage, accessLevel, isFinal, isStatic,
            position);
      this.baseClassDoc = this;
   }

   public ClassDocImpl(ClassDoc containingClass,
                       PackageDoc containingPackage,
                       ClassDoc[] importedClasses,
                       PackageDoc[] importedPackages,
                       SourcePosition position) {
      super(containingClass, containingPackage,
            position);
      this.importedClasses=importedClasses;
      this.importedPackages=importedPackages;
      this.baseClassDoc = this;
   }

   // Return constructors in class.
   public ConstructorDoc[] constructors() {
      return constructors(true);
   }

   public ConstructorDoc[] constructors(boolean filter) {
      return filter ? filteredConstructors : unfilteredConstructors;
   }

   // Return true if Serializable fields are explicitly defined with the special class member serialPersistentFields.
   public boolean definesSerializableFields() {
      return definesSerializableFields;
   }

   // Return fields in class.
   public FieldDoc[] fields() {
      return fields(true);
   }

   public FieldDoc[] fields(boolean filter) {
      return filter ? filteredFields : unfilteredFields;
   }

   private static Set primitiveNames;
   static {
      primitiveNames = new HashSet();
      primitiveNames.add("int");
      primitiveNames.add("long");
      primitiveNames.add("char");
      primitiveNames.add("short");
      primitiveNames.add("byte");
      primitiveNames.add("float");
      primitiveNames.add("double");
      primitiveNames.add("boolean");
   }

   private Map findClassCache = new HashMap();

   public ClassDoc findClass(String className, String dimension)
   {
      ClassDoc cached = (ClassDoc)findClassCache.get(className + dimension);
      if (null != cached) {
         return cached;
      }
      else {
         ClassDoc classDoc = findClass(className);

         if (null!=classDoc) {
            try {
               if (classDoc.dimension().equals(dimension)) {
                  return classDoc;
               }
               else {
                  ClassDoc rc = (ClassDoc) ((WritableType)classDoc).clone();
                  ((WritableType)rc).setDimension(dimension);
                  findClassCache.put(className + dimension, rc);
                  return rc;
               }
            }
            catch (CloneNotSupportedException e) {
               throw new RuntimeException(e);
            }
         }
         else {
            return null;
         }
      }
   }

   public ClassDoc findClass(String className)
   {
      String qualifiedName = Main.getRootDoc().resolveClassName(className, this);
      ClassDoc rc=Main.getRootDoc().classNamed(qualifiedName);

      if (null == rc) {
         for (ClassDoc cdi=this; cdi!=null; cdi=cdi.containingClass()) {
            for (ClassDoc sdi=cdi; sdi!=null; sdi=sdi.superclass()) {
               if (sdi instanceof ClassDocProxy) {
                  ClassDoc realClass = Main.getRootDoc().classNamed(sdi.qualifiedName());
                  if (null != realClass) {
                     sdi = realClass;
                  }
               }
               rc=Main.getRootDoc().classNamed(sdi.qualifiedName()+"."+className);
               if (rc!=null) return rc;
            }
         }
      }

      return rc;
   }

   // Get the list of classes declared as imported.
   public ClassDoc[] importedClasses() {
      return importedClasses;
   }

   // Get the list of packages declared as imported.
   public PackageDoc[] importedPackages() {
      return importedPackages;
   }

   // Return inner classes within this class.
   public ClassDoc[] innerClasses() {
      return innerClasses(true);
   }

   public ClassDoc[] innerClasses(boolean filtered) {
      return filtered ? filteredInnerClasses : unfilteredInnerClasses;
   }

   void setFilteredInnerClasses(ClassDoc[] filteredInnerClasses) {
      this.filteredInnerClasses=filteredInnerClasses;
   }

   void setInnerClasses(ClassDoc[] unfilteredInnerClasses) {
      this.unfilteredInnerClasses=unfilteredInnerClasses;
   }

   // Return interfaces implemented by this class or interfaces extended by this interface.
   public ClassDoc[] interfaces() {
      return interfaces;
   }

   public void setInterfaces(ClassDoc[] interfaces) {
      this.interfaces=interfaces;
   }

   // Return true if this class is abstract
   public boolean isAbstract() {
      return isAbstract || isInterface();
   }

   public boolean isInterface() {
      return isInterface;
   }

   public boolean isAnnotation() {
      return isAnnotation;
   }

  public boolean isEnum()
  {
    return isEnum;
  }

   // Return true if this class is abstract
   public void setIsAbstract(boolean b) {
      this.isAbstract=b;
   }

   // Return true if this class implements java.io.Externalizable.
   public boolean isExternalizable() {
      return implementsInterface("java.io.Externalizable");
   }

   // Return true if this class implements java.io.Serializable.
   public boolean isSerializable() {
      return implementsInterface("java.io.Serializable");
   }

   public boolean implementsInterface(String name) {
      for (ClassDoc cdi=this; cdi!=null; cdi=(ClassDoc)cdi.superclass()) {
         if (cdi instanceof ClassDocImpl) {
            ClassDoc[] cdiInterfaces=(ClassDoc[])cdi.interfaces();
            if (null != cdiInterfaces) {
               for (int i=0; i<cdiInterfaces.length; ++i) {
                  if (cdiInterfaces[i].qualifiedName().equals(name))
                     return true;
               }
            }
         }
         else {
            //throw new RuntimeException("implementsInterface(\""+name+"\") failed: Not a ClassDocImpl:"+cdi);
         }
      }
      return false;
   }

   // Return methods in class.
   public MethodDoc[] methods() {
      return methods(true);
   }

   // Return methods in class.
   public MethodDoc[] methods(boolean filter) {
      return filter ? filteredMethods : unfilteredMethods;
   }

   // Return the Serializable fields of class. Return either a list of default fields documented by serial tag or return a single FieldDoc for serialPersistentField member.
   public FieldDoc[] serializableFields() {
      if (serialPersistentField!=null) {
         return serialPersistentField;
      }
      else{
         return serializableFields;
      }
   }

   // Return the serialization methods for this class.
   public MethodDoc[] serializationMethods() {
      return serializationMethods;
   }

   // Test whether this class is a subclass of the specified class.
   public boolean subclassOf(ClassDoc cd) {
      for (ClassDocImpl cdi=(ClassDocImpl)superclass(); cdi!=null; cdi=(ClassDocImpl)cdi.superclass()) {
         if (cdi.equals(cd))
            return true;
      }
      return false;
   }

   // Return the superclass of this class
   public ClassDoc superclass() {
      return superclass;
   }

   // Implementation of Interface Type

   public ClassDoc asClassDoc() {

      return (ClassDoc)this;
   }

   public String typeName() { return name(); }

   public String qualifiedTypeName() {
      return (containingPackage()!=null && !containingPackage().equals(PackageDocImpl.DEFAULT_PACKAGE))?(containingPackage().name()+"."+name()):(name());
   }

   public String qualifiedName() { return qualifiedTypeName(); }

   public String dimension() { return dimension; }

   public String toString() { return "ClassDoc{"+qualifiedTypeName()+"}"; }

   public TypeVariable asTypeVariable() { return null; }

   public static ClassDocImpl createInstance(ClassDoc containingClass,
                                             PackageDoc containingPackage,
                                             ClassDoc[] importedClasses,
                                             PackageDoc[] importedPackages,
                                             char[] source, int startIndex, int endIndex,
                                             List importStatementList) throws ParseException, IOException {

      String superclassName = "java.lang.Object";

      ClassDocImpl rc=new ClassDocImpl(containingClass,
                                       containingPackage,
                                       importedClasses,
                                       importedPackages,
                                       null);
      rc.setImportStatementList(importStatementList);
      List implementedInterfaces = new ArrayList();

      String word="";
      int item=0;

      final int STATE_NORMAL = 1;
      final int STATE_SLASHC = 2;
      final int STATE_STARC  = 3;
      final int STATE_ANNO   = 4;

      int state=STATE_NORMAL;
      int varLevel=0;
      int parLevel=0;
      char prev=0;
      for (int ndx=startIndex; ndx<=endIndex; ++ndx) {
         char c=(ndx==endIndex)?10:source[ndx];
         boolean processWord=false;
         if (state==STATE_SLASHC) {
            if (c=='\n') {
               state=STATE_NORMAL;
               c=0;
            }
         }
         else if (state==STATE_STARC) {
            if (c=='/' && prev=='*') {
               state=STATE_NORMAL;
               c=0;
            }
         }
         else {
            if (c=='/' && prev=='/') {
               state=STATE_SLASHC;
               c=0;
               word=word.substring(0,word.length()-1);
               processWord=true;
            }
            else if (c=='*' && prev=='/') {
               state=STATE_STARC;
               c=0;
               word=word.substring(0,word.length()-1);
               processWord=true;
            }
            else if (c=='@') {
               state=STATE_ANNO;
               word += c;
            }
            else if (c=='(' && state==STATE_ANNO) {
               ++parLevel;
               word += c;
            }
            else if (c==')' && state==STATE_ANNO) {
               --parLevel;
               word += c;
               if (parLevel == 0)
                   state=STATE_NORMAL;
            }
            else if (c=='<')
              {
                ++varLevel;
                word += c;
              }
            else if (c=='>')
              {
                --varLevel;
                word += c;
              }
            else if (c=='{' && parLevel == 0 ||
                     c==',' && varLevel == 0 && parLevel == 0 ||
                     Parser.WHITESPACE.indexOf(c)>=0 && parLevel == 0 && varLevel == 0) {
               processWord=true;
               state=STATE_NORMAL;
            }
            else {
               word+=c;
            }

            if (processWord && word.length()>0) {
               if (item==0) {
                  if (rc.processModifier(word)) {
                  }
                  else if (word.equals("abstract")) {
                     rc.setIsAbstract(true);
                  }
                  else if (word.equals("class")) {
                     rc.setIsInterface(false);
                     item=1;
                  }
                  else if (word.equals("enum"))
                    {
                      rc.setIsEnum(true);
                      item = 1;
                    }
                  else if (word.equals("interface")) {
                     rc.setIsInterface(true);
                     item=1;
                  }
                  else if (word.equals("@interface")) {
                     rc.setIsInterface(true);
                     rc.setIsAnnotation(true);
                     item=1;
                  }
                  else if (word.equals("strictfp")) {
                  }
                  else {
                     Main.getRootDoc().printWarning("unknown modifier '"+word+"'");
                  }
               }
               else if (word.equals("extends") && !rc.isAnnotation()) {
                  if (rc.isInterface()) {
                     item=3;
                  }
                  else {
                     item=2;
                  }
               }
               else if (word.equals("implements") && !rc.isAnnotation()) {
                  item=3;
               }
               else if (item==1) {
                 int parameterIndex = word.indexOf("<");
                 if (parameterIndex == -1)
                   rc.setClass(word);
                 else
                   {
                     rc.setClass(word.substring(0, parameterIndex));
                     parseTypeVariables(rc,word.substring(parameterIndex,
                                                          word.length()));
                   }
               }
               else if (item==2) {
                  //Debug.log(9,"setting baseclass of "+rc+" to "+word);
                 int parameterIndex = word.indexOf("<");
                 if (parameterIndex == -1)
                   superclassName=word;
                 else
                   {
                     /* FIXME: Parse type parameters */
                     superclassName=word.substring(0,parameterIndex);
                   }
               }
               else if (item==3) {
                 int parameterIndex = word.indexOf("<");
                 if (parameterIndex == -1)
                   implementedInterfaces.add(word);
                 else
                   {
                     /* FIXME: Parse type parameters */
                     implementedInterfaces.add(word.substring(0,parameterIndex));
                   }
               }
               word="";
            }

            if (c=='{' && state==STATE_NORMAL) break;
         }
         prev=c;
      }

      if (null != containingClass
          && containingClass.isInterface()) {
         rc.accessLevel = ACCESS_PUBLIC;
      }

      if (rc.name()==null) {
         throw new ParseException("No classdef found in expression \""+new String(source,startIndex,endIndex-startIndex)+"\"");
      }

      rc.setPosition(ClassDocImpl.getPosition(rc, source, startIndex));

      ClassDoc superclassProxy=new ClassDocProxy(superclassName, rc);

      if (!rc.qualifiedName().equals("java.lang.Object")) {
         rc.setSuperclass(superclassProxy);
      }

      ClassDoc[] interfaces=new ClassDoc[implementedInterfaces.size()];
      for (int i=0; i<interfaces.length; ++i) {
         interfaces[i]=new ClassDocProxy((String)implementedInterfaces.get(i), rc);
      }
      rc.setInterfaces(interfaces);

      if (rc.isInterface() && rc.containingClass()!=null) {
         rc.setIsStatic(true);
      }
      return rc;
   }

   public void setFields(FieldDoc[] fields) {
      this.unfilteredFields=fields;
   }

   public void setFilteredFields(FieldDoc[] fields) {
      this.filteredFields=fields;
   }

   public void setSerializableFields(FieldDoc[] sfields) {
      this.serializableFields=sfields;
   }

   public void setMethods(MethodDoc[] methods) {
      this.unfilteredMethods=methods;
   }

   public void setFilteredMethods(MethodDoc[] methods) {
      this.filteredMethods=methods;
   }

   public void setConstructors(ConstructorDoc[] constructors) {
      this.unfilteredConstructors=constructors;
   }

   public void setFilteredConstructors(ConstructorDoc[] constructors) {
      this.filteredConstructors=constructors;
   }

   // Returns the name of this Doc item.
   public String name() {
      if (containingClass==null) {
         return className;
      }
      else {
         return containingClass.name()+"."+className;
      }
   }

   public String getClassName() {
      return className;
   }

   public void setClass(String className) {
      this.className=className;
   }

   void setSuperclass(ClassDoc superclass) {
      this.superclass=superclass;
   }

   public void resolve() throws ParseException {
      if (!resolved) {
         resolved=true;

         if (containingClass!=null)
            ((ClassDocImpl)containingClass).resolve();

         //Debug.log(9,"resolving class '"+qualifiedName()+"'");
         /*
         for (int i=0; i<importedPackages.length; ++i) {
               Debug.log(9,"class "+qualifiedName()+" imports "+importedPackages[i].name());
         }
         */

         if (superclass instanceof ClassDocProxy) {

            ClassDoc realClassDoc=findClass(superclass.qualifiedName());

            if (realClassDoc==null) {
               /*
               if (true) { // Main.recursiveClasses) {
                  throw new ParseException("In class '"+qualifiedName()+"': class '"+className+"' not found.");
               }
               */
            }
            else {
               superclass=realClassDoc;
            }
         }

         if (null != interfaces) {
            for (int i=0; i<interfaces.length; ++i) {
               if (interfaces[i] instanceof ClassDocProxy) {
                  //Debug.log(9,"class "+qualifiedName()+" implements "+interfaces[i].qualifiedName());
                  ClassDoc realClassDoc=findClass(interfaces[i].qualifiedName());
                  if (realClassDoc==null) {
                     /*
                       if (Main.recursiveClasses) {
                       throw new ParseException("In class '"+qualifiedName()+"': class '"+className+"' not found.");
                       }
                     */
                  }
                  else {
                     //Debug.log(9,"found class '"+className+"': "+interfaces[i]);
                     interfaces[i]=realClassDoc;
                  }
               }
            }
         }

         if (unfilteredFields!=null) {
            for (int i=0; i<unfilteredFields.length; ++i) {
               ((FieldDocImpl)unfilteredFields[i]).resolve();
               if (unfilteredFields[i].name().equals("serialPersistentField")) {
                  serialPersistentField=new FieldDoc[]{unfilteredFields[i]};
                  definesSerializableFields=true;
               }
            }
         }

         if (unfilteredMethods!=null) {
            for (int i=0; i<unfilteredMethods.length; ++i) {
               ((MethodDocImpl)unfilteredMethods[i]).resolve();
            }
         }

         if (unfilteredConstructors!=null) {
            for (int i=0; i<unfilteredConstructors.length; ++i) {
               ((ConstructorDocImpl)unfilteredConstructors[i]).resolve();
            }
         }

         List isSerMethodList=new ArrayList();

         if (null != maybeSerMethodList) {
            for (Iterator it=maybeSerMethodList.iterator(); it.hasNext(); ) {
               MethodDocImpl method=(MethodDocImpl)it.next();
               method.resolve();

               if (((method.name().equals("readObject")
                     && method.signature().equals("(java.io.ObjectInputStream)"))
                    || (method.name().equals("writeObject")
                        && method.signature().equals("(java.io.ObjectOutputStream)"))
                    || (method.name().equals("readExternal")
                        && method.signature().equals("(java.io.ObjectInput)"))
                    || (method.name().equals("writeExternal")
                        && method.signature().equals("(java.io.ObjectOutput)"))
                    || (method.name().equals("readResolve")
                        && method.signature().equals("()")))) {

                  isSerMethodList.add(method);
               }
            }
            this.serializationMethods=(MethodDoc[])isSerMethodList.toArray(new MethodDoc[0]);
            maybeSerMethodList=null;
         }
      }
   }

   public FieldDoc findFieldRec(String name) {
      return findFieldRec(this, name);
   }

   private static FieldDoc findFieldRec(ClassDoc classDoc, String name)
   {
      FieldDoc field = findField(classDoc, name);
      if (null!=field) {
         return field;
      }
      else {
         ClassDoc[] interfaces = classDoc.interfaces();
         for (int i=0; i<interfaces.length; ++i) {
            field = findFieldRec(interfaces[i], name);
            if (null != field) {
               return field;
            }
         }
         if (null != classDoc.superclass()) {
            return findFieldRec(classDoc.superclass(), name);
         }
         else {
            return null;
         }
      }
   }

   private static FieldDoc findField(ClassDoc classDoc, String name)
   {
      FieldDoc[] fields = classDoc.fields(false);
      for (int i=0; i<fields.length; ++i) {
         if (fields[i].name().equals(name)) {
            return fields[i];
         }
      }
      return null;
   }

   public FieldDoc findField(String fieldName) {
      for (int i=0; i<filteredFields.length; ++i) {
         if (filteredFields[i].name().equals(fieldName)) {
            return filteredFields[i];
         }
      }
      return null;
   }

   public void resolveComments() {

      super.resolveComments();

      if (null != unfilteredFields) {
         for (int i=0; i<unfilteredFields.length; ++i) {
            ((FieldDocImpl)unfilteredFields[i]).resolveComments();
         }
      }

      if (null != serializableFields) {
         for (int i=0; i<serializableFields.length; ++i) {
            ((FieldDocImpl)serializableFields[i]).resolveComments();
         }
      }
      if (null != unfilteredMethods) {
         for (int i=0; i<unfilteredMethods.length; ++i) {
            ((MethodDocImpl)unfilteredMethods[i]).resolveComments();
         }
      }
      if (null != unfilteredConstructors) {
         for (int i=0; i<unfilteredConstructors.length; ++i) {
            ((ConstructorDocImpl)unfilteredConstructors[i]).resolveComments();
         }
      }

      resolveTags();
   }


   private String className=null;

   private boolean isAbstract;
   private boolean isInterface;
   private boolean isAnnotation;
   private boolean isEnum;
   private ClassDoc[] interfaces;
   private ClassDoc[] filteredInnerClasses;
   private ClassDoc[] unfilteredInnerClasses;
   private FieldDoc[] filteredFields;
   private FieldDoc[] unfilteredFields;
   private FieldDoc[] serializableFields;
   private MethodDoc[] filteredMethods;
   private MethodDoc[] unfilteredMethods;
   private ConstructorDoc[] filteredConstructors;
   private ConstructorDoc[] unfilteredConstructors;
   private TypeVariable[] typeParameters;

   private boolean resolved=false;

   private ClassDoc superclass;

   // Is this Doc item a class.
   public boolean isClass() {
      return !isInterface;
   }

   // return true if this Doc is include in the active set.
   public boolean isIncluded() {
      if (this == baseClassDoc) {
         return isIncluded
            || (null != containingClass && Main.getInstance().includeAccessLevel(accessLevel));
      }
      else {
         return baseClassDoc.isIncluded();
      }
   }

   void setIsIncluded(boolean b) {
      this.isIncluded=b;
   }

   private boolean isIncluded=false;

   void setImportedClasses(ClassDoc[] importedClasses) {
      this.importedClasses=importedClasses;
   }

   private static Map typeMap = new HashMap();

   Type typeForString(String typeName) throws ParseException {
      String orgTypename=typeName;
      int ndx=typeName.indexOf('[');
      String dim="";
      if (ndx>=0) {
         for (int i=ndx; i<typeName.length(); ++i) {
            if ("[]".indexOf(typeName.charAt(i))>=0) {
               dim+=typeName.charAt(i);
            }
         }
         typeName=typeName.substring(0,ndx).trim();
      }

      ClassDoc classDoc = findClass(typeName, dim);
      if (null != classDoc) {
         return classDoc;
      }

      Type type = (Type)typeMap.get(typeName+dim);
      if (null!=type) {
         try {
            if (type.dimension().equals(dim)) {
               return type;
            }
            else {
               Type rc = (Type) ((WritableType)type).clone();
               ((WritableType)rc).setDimension(dim);
               return rc;
            }
         }
         catch (CloneNotSupportedException e) {
            throw new ParseException(e.toString());
         }
      }

      if ("boolean".equals(typeName)
          || "char".equals(typeName)
          || "byte".equals(typeName)
          || "short".equals(typeName)
          || "int".equals(typeName)
          || "long".equals(typeName)
          || "void".equals(typeName)
          || "float".equals(typeName)
          || "double".equals(typeName)) {
         Type rc=new TypeImpl(null, typeName, dim);
         typeMap.put(typeName+dim, rc);
         return rc;
      }

      if (Main.getInstance().isDocletRunning()) {
         //System.err.println(findClass("java.lang.String"));
         //throw new ParseException("Doclet running, class not found: "+typeName+" ("+orgTypename+")");
      }
      Type rc=new ClassDocProxy(typeName, this);
      ((WritableType)rc).setDimension(dim);
      return rc;
   }

   public boolean isException() {
      for (ClassDoc cdi=this;
           cdi!=null;
           cdi=cdi.superclass()) {

         if ("java.lang.Exception".equals(cdi.qualifiedName()))
            return true;
      }
      return false;
   }

   public boolean isError() {
      for (ClassDoc cdi=this; cdi!=null; cdi=cdi.superclass()) {
         if ("java.lang.Error".equals(cdi.qualifiedName()))
            return true;
      }
      return false;
   }

   public boolean isOrdinaryClass() {
      return !isException() && !isError() && !isInterface();
   }

   public void setIsInterface(boolean b) {
      this.isInterface=b;
   }

   public void setIsAnnotation(boolean b) {
      this.isAnnotation=b;
   }

   public void setIsEnum(boolean b)
   {
     isEnum = b;
   }

   public ExecutableMemberDoc findExecutableRec(String nameAndSignature) {

      ExecutableMemberDoc rc;
      for (ClassDoc cdi=this; cdi!=null; ) {
         rc=findMethod(cdi, nameAndSignature);
         if (rc!=null) return rc;
         rc=findConstructor(cdi, nameAndSignature);
         if (rc!=null) return rc;

         ClassDoc _superclass = cdi.superclass();
         if (null == _superclass) {
            break;
         }
         else {
            cdi = _superclass;
         }
      }
      return null;
  }

   public static ConstructorDoc findConstructor(ClassDoc classDoc, String nameAndSignature) {
      int ndx=nameAndSignature.indexOf('(');
      if (ndx<=0)
         return null;
      else {
         String fullSignature = resolveSignature(classDoc, nameAndSignature.substring(ndx));
         return findConstructor(classDoc,
                                nameAndSignature.substring(0,ndx),
                                fullSignature);
      }
   }

   public static ConstructorDoc findConstructor(ClassDoc classDoc, String name, String signature) {
      ConstructorDoc[] filteredConstructors = classDoc.constructors(true);
      if (null != filteredConstructors) {
         for (int i=0; i<filteredConstructors.length; ++i) {
            ConstructorDoc constructor = filteredConstructors[i];
            if (constructor.name().equals(name) && constructor.signature().equals(signature))
               return constructor;
         }
      }
      return null;
   }

   public static MethodDoc findMethod(ClassDoc classDoc, String nameAndSignature) {
      int ndx=nameAndSignature.indexOf('(');
      if (ndx<=0) {
         return null;
      }
      else {
         String name = nameAndSignature.substring(0,ndx);
         String fullSignature = resolveSignature(classDoc, nameAndSignature.substring(ndx));
         return findMethod(classDoc, name, fullSignature);
      }
   }

   private static String resolveSignature(ClassDoc classDoc, String signature)
   {
      signature = signature.substring(1, signature.length() - 1).trim();
      if (0 == signature.length()) {
         return "()";
      }
      StringTokenizer st = new StringTokenizer(signature, ",");
      StringBuffer fullSignature = new StringBuffer("(");
      while (st.hasMoreTokens()) {
         String type = st.nextToken().trim();
         int ndx = type.length();
         while (ndx > 0 && type.charAt(ndx - 1) == '[' || type.charAt(ndx - 1) == ']') {
            -- ndx;
         }
         String dim = type.substring(ndx);
         type = type.substring(0, ndx);
         ClassDoc typeClass = classDoc.findClass(type);
         if (fullSignature.length() > 1) {
            fullSignature.append(",");
         }
         if (null != typeClass) {
            fullSignature.append(typeClass.qualifiedName());
         }
         else {
            fullSignature.append(type);
         }
         fullSignature.append(dim);
      }
      fullSignature.append(')');
      return fullSignature.toString();
   }

   public static MethodDoc findMethod(ClassDoc classDoc, String name, String signature) {
      MethodDoc[] filteredMethods = classDoc.methods(true);
      if (null != filteredMethods) {
         for (int i=0; i<filteredMethods.length; ++i) {
            MethodDoc method = filteredMethods[i];
            if (method.name().equals(name) && method.signature().equals(signature))
               return method;
         }
      }
      return null;
   }

   public boolean equals(Object o) {
      return (o!=null) && (o instanceof ClassDoc) && ((ClassDoc)o).qualifiedName().equals(qualifiedName());
   }

   private List maybeSerMethodList;

   void setMaybeSerMethodList(List maybeSerMethodList) {
      this.maybeSerMethodList=maybeSerMethodList;
   }

   public void setDimension(String dimension) {
      this.dimension = dimension;
   }

   public Object clone() throws CloneNotSupportedException {
      ClassDocImpl result = (ClassDocImpl)super.clone();
      result.baseClassDoc = baseClassDoc;
      return result;
   }

   public int superHashCode()
   {
      return super.hashCode();
   }

   public int hashCode()
   {
      return qualifiedTypeName().hashCode();
   }

   public ClassDoc getBaseClassDoc()
   {
      return baseClassDoc;
   }

   public FieldDoc getFieldDoc(String name)
   {
      for (int i=0; i<unfilteredFields.length; ++i) {
         if (name.equals(unfilteredFields[i].name())) {
            return unfilteredFields[i];
         }
      }
      return null;
   }

   public MethodDoc getMethodDoc(String name, String signature)
   {
      for (int i=0; i<unfilteredMethods.length; ++i) {
         if (name.equals(unfilteredMethods[i].name())
             && signature.equals(unfilteredMethods[i].signature())) {
            return unfilteredMethods[i];
         }
      }
      return null;
   }


   public ConstructorDoc getConstructorDoc(String signature)
   {
      for (int i=0; i<unfilteredConstructors.length; ++i) {
         if (signature.equals(unfilteredConstructors[i].signature())) {
            return unfilteredConstructors[i];
         }
      }
      return null;
   }

   private Object findFieldValue(String identifier,
                                 ClassDoc classDoc,
                                 String fieldName,
                                 Set visitedFields)
      throws UnknownIdentifierException, IllegalExpressionException
   {
      while (classDoc != null) {
         if (classDoc instanceof ClassDocImpl) {
            FieldDocImpl fieldDoc
               = (FieldDocImpl)((ClassDocImpl)classDoc).getFieldDoc(fieldName);
            if (visitedFields.contains(fieldDoc)) {
               throw new CircularExpressionException("Circular reference detected");
            }
            else if (null != fieldDoc) {
               return fieldDoc.constantValue(visitedFields);
            }
         }
         else {
            ClassDoc[] _interfaces = classDoc.interfaces();
            if (null != _interfaces) {
               for (int i=0; i<_interfaces.length; ++i) {
                  if (_interfaces[i] instanceof ClassDocImpl) {
                     FieldDocImpl fieldDoc
                        = (FieldDocImpl)((ClassDocImpl)_interfaces[i]).getFieldDoc(fieldName);
                     if (visitedFields.contains(fieldDoc)) {
                        throw new CircularExpressionException("Circular reference detected");
                     }
                     else if (null != fieldDoc) {
                        return fieldDoc.constantValue(visitedFields);
                     }
                  }
               }
            }
         }
         classDoc = classDoc.superclass();
      }
      throw new UnknownIdentifierException(identifier);
   }

   public Object getValue(String identifier, Set visitedFields)
      throws UnknownIdentifierException, IllegalExpressionException
   {
      int ndx = identifier.lastIndexOf('.');
      if (ndx >= 0) {
         String _className = identifier.substring(0, ndx);
         String _fieldName = identifier.substring(ndx + 1);

         ClassDoc _classDoc = findClass(_className);
         if (null != _classDoc) {
            return findFieldValue(identifier, _classDoc, _fieldName, visitedFields);
         }
         else {
            throw new UnknownIdentifierException(identifier);
         }
      }
      else {
         return findFieldValue(identifier, this, identifier, visitedFields);
      }
   }

   public boolean isPrimitive()
   {
      return false;
   }

   // Compares this Object with the specified Object for order.
   public int compareTo(java.lang.Object o) {
      int rc;

      if (o instanceof ClassDocImpl) {

         ClassDocImpl c1 = this;
         ClassDocImpl c2 = (ClassDocImpl)o;

         if (null != c1.containingClass() && null == c2.containingClass()) {
            rc = c1.containingClass().compareTo(c2);
            if (0 == rc) {
               rc = 1;
            }
            return rc;
         }
         else if (null == c1.containingClass() && null != c2.containingClass()) {
            rc = c1.compareTo(c2.containingClass());
            if (0 == rc) {
               rc = -1;
            }
            return rc;
         }
         else if (null != c1.containingClass() && null != c2.containingClass()) {
            rc = c1.containingClass().compareTo(c2.containingClass());
            if (0 != rc) {
               return rc;
            }
         }

         rc = super.compareTo(o);
         if (0 == rc) {
            return Main.getInstance().getCollator().compare(containingPackage().name(),
                                                            ((ClassDocImpl)o).containingPackage().name());
         }
         else {
            return rc;
         }
      }
      else {
         return 1;
      }
   }

   private List importStatementList;

   public void setImportStatementList(List importStatementList)
   {
      this.importStatementList = new LinkedList();
      this.importStatementList.addAll(importStatementList);
   }

   public List getImportSpecifierList()
   {
      return importStatementList;
   }

  public TypeVariable[] typeParameters()
  {
    return typeParameters;
  }

  /**
   * <p>
   * Parses the type variables declared in the class definition.
   * The syntax is:
   * </p>
   * <p>
   * <dl>
   * <dt>TypeParameters:</dt>
   * <dd><code>&lt; <em>TypeParameter</em> { <em>, TypeParameter }</code></dd>
   * <dt>TypeParameter:</dt>
   * <dd><code><em>Identifier</em> { <strong>extends</strong> <em>Bound</em>
   *     }</dd>
   * <dt>Bound:</dt>
   * <dd><code><em>Type</em>{<strong>&</strong> <em>Type</em> } </dd>
   * </dl>
   *
   * @param rc the owning class.
   * @param typeVariables the string to be parsed.
   * @throws ParseException if parsing fails.
   */
  public static void parseTypeVariables(ClassDocImpl rc,
                                        String typeVariables)
    throws ParseException
  {
    List parsedBounds = null;
    StringTokenizer parameters = new StringTokenizer(typeVariables,
                                                     Parser.WHITESPACE +
                                                     "<>,");
    List variables = new ArrayList();
    while (parameters.hasMoreTokens())
      {
        String parameter = parameters.nextToken();
        StringTokenizer parts = new StringTokenizer(parameter,
                                                    Parser.WHITESPACE);
        TypeVariableImpl variable = new TypeVariableImpl(rc.qualifiedName(),
                                                         parts.nextToken(),"",
                                                         rc);
        if (parts.hasMoreTokens())
          {
            if (!parts.nextToken().equals("extends"))
              throw new ParseException("Invalid type parameter: " + parameter);
            StringTokenizer bounds = new StringTokenizer(parts.nextToken(),
                                                         Parser.WHITESPACE
                                                         + "&");
            parsedBounds = new ArrayList();
            while (bounds.hasMoreTokens())
              {
                String bound = bounds.nextToken();
                int nameSep = bound.lastIndexOf(".");
                String packageName = bound.substring(0, nameSep);
                String boundName = bound.substring(nameSep, bound.length());
                parsedBounds.add(new TypeImpl(packageName,boundName,""));
              }
          }
        if (parsedBounds != null)
          variable.setBounds(parsedBounds);
        variables.add(variable);
      }
    rc.setTypeParameters(variables);
  }

  /**
   * Set the type parameters to the contents of the supplied list.
   *
   * @param variables a list of type parameters.
   */
  void setTypeParameters(List variables)
  {
    typeParameters =
      (TypeVariable[]) variables.toArray(new TypeVariable[variables.size()]);
  }

}
