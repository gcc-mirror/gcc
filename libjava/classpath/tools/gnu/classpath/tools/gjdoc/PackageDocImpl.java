/* gnu.classpath.tools.gjdoc.PackageDocImpl
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
import java.util.*;
import java.io.File;

class PackageDocImpl extends DocImpl implements GjdocPackageDoc {

   private String packageName;
   private File   packageDirectory;

   private Set    allClassesSet       = new TreeSet();
   private List   ordinaryClassesList = new ArrayList();
   private List   exceptionsList      = new ArrayList();
   private List   interfacesList      = new ArrayList();
   private List   errorsList          = new ArrayList();   

   private ClassDoc[] allClasses;
   private ClassDoc[] ordinaryClasses;
   private ClassDoc[] exceptions;
   private ClassDoc[] interfaces;
   private ClassDoc[] errors;

   PackageDocImpl(String packageName) {
      super(null);
      this.packageName=packageName;
   }

   public void addClass(ClassDoc classDoc) {
      if (Main.getInstance().includeAccessLevel(((ClassDocImpl)classDoc).accessLevel)) {
	 allClassesSet.add(classDoc);
      }
   }

   public void resolve() {
      for (Iterator it=allClassesSet.iterator(); it.hasNext(); ) {
	 ClassDocImpl classDoc=(ClassDocImpl)it.next();
	 try {
	     classDoc.resolve();
	 } catch (ParseException e) {
	     System.err.println("FIXME: add try-catch to force compilation"
				+ e);
	 }

	 if (classDoc.isInterface()) {
	    interfacesList.add(classDoc);
	 }
	 else if (classDoc.isException()) {
	    exceptionsList.add(classDoc);
	 }
	 else if (classDoc.isError()) {
	    errorsList.add(classDoc);
	 }
	 else {
	    ordinaryClassesList.add(classDoc);
	 }
      }
   }

   public void resolveComments() {
      if (rawDocumentation!=null) {
	 this.tagMap=parseCommentTags(rawDocumentation.toCharArray(),
				      0,
				      rawDocumentation.length(),
				      null,
                                      null,
                                      null,
                                      null);
      }

      resolveTags();
   }

   public String name() { 
      return packageName; 
   }

   public ClassDoc[] allClasses() 
   { 
      if (null == this.allClasses) {
         this.allClasses = toClassDocArray(allClassesSet);
      }
      return this.allClasses;
   }

   public ClassDoc[] ordinaryClasses() 
   { 
      if (null == this.ordinaryClasses) {
         this.ordinaryClasses = toClassDocArray(ordinaryClassesList);
      }
      return this.ordinaryClasses;
   }


   public ClassDoc[] exceptions() 
   { 
      if (null == this.exceptions) {
         this.exceptions = toClassDocArray(exceptionsList);
      }
      return this.exceptions;
   }

   public ClassDoc[] interfaces() 
   { 
      if (null == this.interfaces) {
         this.interfaces = toClassDocArray(interfacesList);
      }
      return this.interfaces;
   }

   public ClassDoc[] errors() 
   { 
      if (null == this.errors) {
         this.errors = toClassDocArray(errorsList);
      }
      return this.errors;
   }

   private ClassDoc[] toClassDocArray(Collection classDocList)
   {
      ClassDoc[] result = (ClassDoc[])classDocList.toArray(new ClassDoc[classDocList.size()]);
      Arrays.sort(result);
      return result;
   }

   public ClassDoc findClass(String name) { 
      return Main.getRootDoc().classNamed(packageName+"."+name);
   }

   public void dump(int level) {
      Debug.log(level, "All classes:");
      Debug.dumpArray(level, allClasses());

      Debug.log(level, "Ordinary classes:");
      Debug.dumpArray(level, ordinaryClasses());
      
   }

   public static final PackageDocImpl DEFAULT_PACKAGE = new PackageDocImpl("");

   public boolean isPackage() {
      return true;
   }

   public boolean isIncluded() {
      return isIncluded;
   }

   void setIsIncluded(boolean b) {
      this.isIncluded=b;
   }

   private boolean isIncluded = false;

   public String toString() {
      return packageName;
   }

   public int compareTo(Object o) {
      if (o!=null && o instanceof PackageDocImpl)
	 return name().compareTo(((PackageDocImpl)o).name());
      else
	 return 0;
   }

   public boolean equals(Object o) {
      if (o!=null && o instanceof PackageDocImpl)
	 return name().equals(((PackageDocImpl)o).name());
      else
	 return false;
   }

   /**
    *  Sets the directory containing this package's source files.
    */
   public void setPackageDirectory(File packageDirectory) {
      this.packageDirectory = packageDirectory;
   }

   /**
    *  Gets the directory containing this package's source files.
    */
   public File packageDirectory() {
      return this.packageDirectory;
   }
}
