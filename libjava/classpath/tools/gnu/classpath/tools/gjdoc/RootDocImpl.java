/* gnu.classpath.tools.gjdoc.RootDocImpl
   Copyright (C) 2001, 2007 Free Software Foundation, Inc.

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
import java.lang.reflect.*;

public class RootDocImpl
   extends DocImpl
   implements GjdocRootDoc {

   private ErrorReporter reporter = new ErrorReporter();

   private RandomAccessFile rawCommentCache;

   /**
    *  All options and their corresponding values which are not recognized
    *  by Gjdoc. These are passed to the Doclet as "custom options".
    *  Each element in this array is again a String array, with the
    *  option name as first element (including prefix dash) and possible
    *  option values as following elements.
    */
   private String[][] customOptionArr;

   /**
    *  All source files explicitly specified on the command line.
    *
    *  @contains File
    */
   private List specifiedSourceFiles = new LinkedList();

   /**
    *  The names of all packages explicitly specified on the
    *  command line.
    *
    *  @contains String
    */
   private Set specifiedPackageNames = new LinkedHashSet();

   /**
    *  Stores all classes specified by the user: those given by
    *  individual class names on the command line, and those
    *  contained in the packages given on the command line.
    *
    *  @contains ClassDocImpl
    */
   private List classesList = new LinkedList(); //new LinkedList();

   /**
    *  Stores all classes loaded in the course of preparing
    *  the documentation data. Maps the fully qualified name
    *  of a class to its ClassDocImpl representation.
    *
    *  @contains String->ClassDocImpl
    */
   private Map classDocMap = new HashMap();

   /**
    *  Stores all packages loaded in the course of preparing
    *  the documentation data. Maps the package name
    *  to its PackageDocImpl representation.
    *
    *  @contains String->PackageDocImpl
    */
   private Map packageDocMap = new HashMap();

   /**
    *  All classes specified by the user, both those explicitly
    *  individually specified on the command line and those contained
    *  in packages specified on the command line (as Array for quick
    *  retrieval by Doclet).  This is created from classesList after
    *  all classes have been loaded.
    */
   private ClassDocImpl[] classes;

   /**
    *  All classes which were individually specified on the command
    *  line (as Array for quick retrieval by Doclet). This is created
    *  from specifiedClassNames after all classes have been loaded.
    */
   private List specifiedClasses;

   /**
    *  All packages which were specified on the command line (as Array
    *  for quick retrieval by Doclet). This is created from
    *  specifiedPackageNames after all classes have been loaded.
    */
   private Set specifiedPackages;


   /**
    *  Temporarily stores a list of classes which are referenced
    *  by classes already loaded and which still have to be
    *  resolved.
    */
   private List scheduledClasses=new LinkedList();

   private List sourcePath;

   private String sourceEncoding;

   private Parser parser = new Parser();

   private Set unlocatableReportedSet = new HashSet();

   private Set inaccessibleReportedSet = new HashSet();

   //--------------------------------------------------------------------------
   //
   // Implementation of RootDoc interface
   //
   //--------------------------------------------------------------------------

   /**
    *  Return classes and interfaces to be documented.
    */
   public ClassDoc[] classes() { return classes; }

   /**
    *  Return a ClassDoc object for the specified class/interface
    *  name.
    *
    *  @return a ClassDoc object describing the given class, or
    *  <code>null</code> if no corresponding ClassDoc object
    *  has been constructed.
    */
   public ClassDoc classNamed(String qualifiedName) {
      return (ClassDoc)classDocMap.get(qualifiedName);
   }

   /**
    *  Return an xxx
    */
   public String[][] options() { return customOptionArr; }

   // Return a PackageDoc for the specified package name
   public PackageDoc packageNamed(String name) {
      return (PackageDoc)packageDocMap.get(name);
   }


  // classes and interfaces specified on the command line.
  public ClassDoc[] specifiedClasses()
  {
    return (ClassDocImpl[]) specifiedClasses.toArray(new ClassDocImpl[0]);
  }

   // packages specified on the command line.
  public PackageDoc[] specifiedPackages()
  {
    return (PackageDocImpl[])specifiedPackages.toArray(new PackageDocImpl[0]);
  }

   // Print error message, increment error count.
   public void printError(java.lang.String msg) {
      reporter.printError(msg);
   }

   // Print error message, increment error count.
   public void printFatal(java.lang.String msg) {
      reporter.printFatal(msg);
   }

   // Print a message.
   public void printNotice(java.lang.String msg) {
      reporter.printNotice(msg);
   }

   // Print warning message, increment warning count.
   public void printWarning(java.lang.String msg) {
      reporter.printWarning(msg);
   }

   public String name() {
      return "RootDoc";
   }

   public ErrorReporter getReporter() {
      return reporter;
   }

   public void build() throws ParseException, IOException {

      //--- Create a temporary random access file for caching comment text.

      //File rawCommentCacheFile=File.createTempFile("gjdoc_rawcomment",".cache");
      File rawCommentCacheFile = new File("gjdoc_rawcomment.cache");
      rawCommentCacheFile.deleteOnExit();
      rawCommentCache = new RandomAccessFile(rawCommentCacheFile, "rw");

      //--- Parse all files in "java.lang".

      List javaLangSourceDirs = findSourceFiles("java/lang");
      if (!javaLangSourceDirs.isEmpty()) {
         Iterator it = javaLangSourceDirs.iterator();
         while (it.hasNext()) {
            File javaLangSourceDir = (File)it.next();
            parser.processSourceDir(javaLangSourceDir,
                                    sourceEncoding, "java.lang");
         }
      }
      else {

         Debug.log(1, "Sourcepath is "+sourcePath);

         // Core docs not included in source-path:
         // we need to gather the information about java.lang
         // classes via reflection...

      }

      //--- Parse all files in explicitly specified package directories.

      for (Iterator it=specifiedPackageNames.iterator(); it.hasNext(); ) {

         String specifiedPackageName = (String)it.next();
         String displayPackageName = specifiedPackageName;
         if (null == displayPackageName || 0 == displayPackageName.length()) {
            displayPackageName = "<unnamed>";
         }
         printNotice("Loading classes for package "+displayPackageName+"...");
         String relPath;
         if (null != specifiedPackageName) {
            relPath = specifiedPackageName.replace('.',File.separatorChar);
         }
         else {
            relPath = "";
         }
         List sourceDirs = findSourceFiles(relPath);
         if (!sourceDirs.isEmpty()) {
            Iterator sourceDirIt = sourceDirs.iterator();
            while (sourceDirIt.hasNext()) {
               File sourceDir = (File)sourceDirIt.next();
               parser.processSourceDir(sourceDir, sourceEncoding, specifiedPackageName);
            }
         }
         else {
            printError("Package '"+specifiedPackageName+"' not found.");
         }
      }

      specifiedClasses = new LinkedList();

      //--- Parse all explicitly specified source files.

      for (Iterator it=specifiedSourceFiles.iterator(); it.hasNext(); ) {

         File specifiedSourceFile = (File)it.next();
         printNotice("Loading source file "+specifiedSourceFile+" ...");
         ClassDocImpl classDoc = parser.processSourceFile(specifiedSourceFile, true, sourceEncoding, null);
         if (null != classDoc) {
           specifiedClasses.add(classDoc);
           classesList.add(classDoc);
           classDoc.setIsIncluded(true);
           addPackageDoc(classDoc.containingPackage());
         }
      }


      //--- Let the user know that all specified classes are loaded.

      printNotice("Constructing Javadoc information...");

      //--- Load all classes implicitly referenced by explicitly specified classes.

      loadScheduledClasses(parser);

      printNotice("Resolving references in comments...");

      resolveComments();

      //--- Resolve pending references in all ClassDocImpls

      printNotice("Resolving references in classes...");

      for (Iterator it = classDocMap.values().iterator(); it.hasNext(); ) {
         ClassDoc cd=(ClassDoc)it.next();
         if (cd instanceof ClassDocImpl) {
            ((ClassDocImpl)cd).resolve();
         }
      }

      //--- Resolve pending references in all PackageDocImpls

      printNotice("Resolving references in packages...");

      for (Iterator it = packageDocMap.values().iterator(); it.hasNext(); ) {
         PackageDocImpl pd=(PackageDocImpl)it.next();
         pd.resolve();
      }

      //--- Assemble the array with all specified packages

      specifiedPackages = new LinkedHashSet();
      for (Iterator it = specifiedPackageNames.iterator(); it.hasNext(); ) {
         String specifiedPackageName = (String)it.next();
         PackageDoc specifiedPackageDoc = (PackageDoc)packageDocMap.get(specifiedPackageName);
         if (null!=specifiedPackageDoc) {
            ((PackageDocImpl)specifiedPackageDoc).setIsIncluded(true);
            specifiedPackages.add(specifiedPackageDoc);

            ClassDoc[] packageClassDocs=specifiedPackageDoc.allClasses();
            for (int i=0; i<packageClassDocs.length; ++i) {
               ClassDocImpl specifiedPackageClassDoc=(ClassDocImpl)packageClassDocs[i];

               specifiedPackageClassDoc.setIsIncluded(true);
               classesList.add(specifiedPackageClassDoc);
            }
         }
      }

      //--- Resolve pending references in comment data of all classes

      printNotice("Resolving references in class comments...");

      for (Iterator it=classDocMap.values().iterator(); it.hasNext(); ) {
         ClassDoc cd=(ClassDoc)it.next();
         if (cd instanceof ClassDocImpl) {
            ((ClassDocImpl)cd).resolveComments();
         }
      }

      //--- Resolve pending references in comment data of all packages

      printNotice("Resolving references in package comments...");

      for (Iterator it=packageDocMap.values().iterator(); it.hasNext(); ) {
         PackageDocImpl pd=(PackageDocImpl)it.next();
         pd.resolveComments();
      }

      //--- Create array with all loaded classes

      this.classes=(ClassDocImpl[])classesList.toArray(new ClassDocImpl[0]);
      Arrays.sort(this.classes);

      //--- Close comment cache

      parser = null;
      System.gc();
      System.gc();
   }

   public long writeRawComment(String rawComment) {
      try {
         long pos=rawCommentCache.getFilePointer();
         //rawCommentCache.writeUTF(rawComment);
         byte[] bytes = rawComment.getBytes("utf-8");
         rawCommentCache.writeInt(bytes.length);
         rawCommentCache.write(bytes);
         return pos;
      }
      catch (IOException e) {
         printFatal("Cannot write to comment cache: "+e.getMessage());
         return -1;
      }
   }

   public String readRawComment(long pos) {
      try {
         rawCommentCache.seek(pos);
         int sz = rawCommentCache.readInt();
         byte[] bytes = new byte[sz];
         rawCommentCache.read(bytes);
         return new String(bytes, "utf-8");
         //return rawCommentCache.readUTF();
      }
      catch (IOException e) {
         e.printStackTrace();
         printFatal("Cannot read from comment cache: "+e.getMessage());
         return null;
      }
   }

   List findSourceFiles(String relPath) {

      List result = new LinkedList();
      for (Iterator it = sourcePath.iterator(); it.hasNext(); ) {
         File path = (File)it.next();
         File file = new File(path, relPath);
         if (file.exists()) {
            result.add(file);
         }
      }

      return result;
   }

   PackageDocImpl findOrCreatePackageDoc(String packageName) {
      PackageDocImpl rc=(PackageDocImpl)getPackageDoc(packageName);
      if (null==rc) {
         rc=new PackageDocImpl(packageName);
         if (specifiedPackageNames.contains(packageName)) {
            String packageDirectoryName = packageName.replace('.', File.separatorChar);
            List packageDirectories = findSourceFiles(packageDirectoryName);
            Iterator it = packageDirectories.iterator();
            boolean packageDocFound = false;
            while (it.hasNext()) {
               File packageDirectory = (File)it.next();
               File packageDocFile = new File(packageDirectory, "package.html");
               rc.setPackageDirectory(packageDirectory);
               packageDocFound = true;
               if (null!=packageDocFile && packageDocFile.exists()) {
                  try {
                     rc.setRawCommentText(readHtmlBody(packageDocFile));
                  }
                  catch (IOException e) {
                     printWarning("Error while reading documentation for package "+packageName+": "+e.getMessage());
                  }
                  break;
               }
            }
            if (!packageDocFound) {
               printNotice("No description found for package "+packageName);
            }
         }
         addPackageDoc(rc);
      }
      return rc;
   }

   public void addClassDoc(ClassDoc cd) {
      classDocMap.put(cd.qualifiedName(), cd);
   }

   public void addClassDocRecursive(ClassDoc cd) {
      classDocMap.put(cd.qualifiedName(), cd);
      ClassDoc[] innerClasses = cd.innerClasses(false);
      for (int i=0; i<innerClasses.length; ++i) {
         addClassDocRecursive(innerClasses[i]);
      }
   }

   public void addPackageDoc(PackageDoc pd) {
      packageDocMap.put(pd.name(), pd);
   }

   public PackageDocImpl getPackageDoc(String name) {
      return (PackageDocImpl)packageDocMap.get(name);
   }

   public ClassDocImpl getClassDoc(String qualifiedName) {
      return (ClassDocImpl)classDocMap.get(qualifiedName);
   }

   class ScheduledClass {

      ClassDoc contextClass;
      String qualifiedName;
      ScheduledClass(ClassDoc contextClass, String qualifiedName) {
         this.contextClass=contextClass;
         this.qualifiedName=qualifiedName;
      }

      public String toString() { return "ScheduledClass{"+qualifiedName+"}"; }
   }

   public void scheduleClass(ClassDoc context, String qualifiedName) throws ParseException, IOException {

      if (classDocMap.get(qualifiedName)==null) {

         //Debug.log(9,"Scheduling "+qualifiedName+", context "+context+".");
         //System.err.println("Scheduling " + qualifiedName + ", context " + context);

         scheduledClasses.add(new ScheduledClass(context, qualifiedName));
      }
   }

   /**
    *  Load all classes that were implictly referenced by the classes
    *  (already loaded) that the user explicitly specified on the
    *  command line.
    *
    *  For example, if the user generates Documentation for his simple
    *  'class Test {}', which of course 'extends java.lang.Object',
    *  then 'java.lang.Object' is implicitly referenced because it is
    *  the base class of Test.
    *
    *  Gjdoc needs a ClassDocImpl representation of all classes
    *  implicitly referenced through derivation (base class),
    *  or implementation (interface), or field type, method argument
    *  type, or method return type.
    *
    *  The task of this method is to ensure that Gjdoc has all this
    *  information at hand when it exits.
    *
    *
    */
   public void loadScheduledClasses(Parser parser) throws ParseException, IOException {

      // Because the referenced classes could in turn reference other
      // classes, this method runs as long as there are still unloaded
      // classes.

      while (!scheduledClasses.isEmpty()) {

         // Make a copy of scheduledClasses and empty it. This
         // prevents any Concurrent Modification issues.
         // As the copy won't need to grow (as it won't change)
         // we make it an Array for performance reasons.

         ScheduledClass[] scheduledClassesArr = (ScheduledClass[])scheduledClasses.toArray(new ScheduledClass[0]);
         scheduledClasses.clear();

         // Load each class specified in our array copy

         for (int i=0; i<scheduledClassesArr.length; ++i) {

            // The name of the class we are looking for. This name
            // needs not be fully qualified.

            String scheduledClassName=scheduledClassesArr[i].qualifiedName;

            // The ClassDoc in whose context the scheduled class was looked for.
            // This is necessary in order to resolve non-fully qualified
            // class names.
            ClassDoc scheduledClassContext=scheduledClassesArr[i].contextClass;

            // If there already is a class doc with this name, skip. There's
            // nothing to do for us.
            if (classDocMap.get(scheduledClassName)!=null) {
               continue;
            }

            try {
               // Try to load the class
               //printNotice("Trying to load " + scheduledClassName);
               loadScheduledClass(parser, scheduledClassName, scheduledClassContext);
            }
            catch (ParseException e) {

               /**********************************************************

               // Check whether the following is necessary at all.


               if (scheduledClassName.indexOf('.')>0) {

               // Maybe the dotted notation doesn't mean a package
               // name but instead an inner class, as in 'Outer.Inner'.
               // so let's assume this and try to load the outer class.

                  String outerClass="";
                  for (StringTokenizer st=new StringTokenizer(scheduledClassName,"."); st.hasMoreTokens(); ) {
                     if (outerClass.length()>0) outerClass+=".";
                     outerClass+=st.nextToken();
                     if (!st.hasMoreTokens()) break;
                     try {
                        loadClass(outerClass);
                        //FIXME: shouldn't this be loadScheduledClass(outerClass, scheduledClassContext); ???
                        continue;
                     }
                     catch (Exception ee) {
                     // Ignore: try next level
                     }
                  }
               }

               **********************************************************/

               // If we arrive here, the class could not be found

               printWarning("Couldn't load class "+scheduledClassName+" referenced by "+scheduledClassContext);

               //FIXME: shouldn't this be throw new Error("cannot load: "+scheduledClassName);
            }
         }
      }
   }

   private void loadScheduledClass(Parser parser, String scheduledClassName, ClassDoc scheduledClassContext) throws ParseException, IOException {

      ClassDoc loadedClass=(ClassDoc)scheduledClassContext.findClass(scheduledClassName);

      if (loadedClass==null || loadedClass instanceof ClassDocProxy) {

         ClassDoc classDoc = findScheduledClassFile(scheduledClassName, scheduledClassContext);
         if (null != classDoc) {

            if (classDoc instanceof ClassDocReflectedImpl) {
               Main.getRootDoc().addClassDocRecursive(classDoc);
            }

            if (Main.DESCEND_SUPERCLASS
                && null != classDoc.superclass()
                && (classDoc.superclass() instanceof ClassDocProxy)) {
               scheduleClass(classDoc, classDoc.superclass().qualifiedName());
            }
         }
         else {
            // It might be an inner class of one of the outer/super classes.
            // But we can only check that when they are all fully loaded.
            boolean retryLater = false;

            int numberOfProcessedFilesBefore = parser.getNumberOfProcessedFiles();

            ClassDoc cc = scheduledClassContext.containingClass();
            while (cc != null && !retryLater) {
               ClassDoc sc = cc.superclass();
               while (sc != null && !retryLater) {
                  if (sc instanceof ClassDocProxy) {
                     ((ClassDocImpl)cc).resolve();
                     retryLater = true;
                  }
                  sc = sc.superclass();
               }
               cc = cc.containingClass();
            }

            // Now that outer/super references have been resolved, try again
            // to find the class.

            loadedClass = (ClassDoc)scheduledClassContext.findClass(scheduledClassName);

            int numberOfProcessedFilesAfter = parser.getNumberOfProcessedFiles();

            boolean filesWereProcessed = numberOfProcessedFilesAfter > numberOfProcessedFilesBefore;

            // Only re-schedule class if additional files have been processed
            // If there haven't, there's no point in re-scheduling.
            // Will avoid infinite loops of re-scheduling
            if (null == loadedClass && retryLater && filesWereProcessed)
               scheduleClass(scheduledClassContext, scheduledClassName);

            /* A warning needn't be emitted - this is normal, can happen
               if the scheduled class is in a package which is not
               included on the command line.

               else if (null == loadedClass)
               printWarning("Can't find scheduled class '"
               + scheduledClassName
               + "' in context '"
               + scheduledClassContext.qualifiedName()
               + "'");
            */
         }
      }
   }

   private static interface ResolvedImport
   {
      public String match(String name);
      public boolean mismatch(String name);
      public ClassDoc tryFetch(String name);
   }

   private class ResolvedImportNotFound
      implements ResolvedImport
   {
      private String importSpecifier;
      private String name;

      ResolvedImportNotFound(String importSpecifier)
      {
         this.importSpecifier = importSpecifier;
         int ndx = importSpecifier.lastIndexOf('.');
         if (ndx >= 0) {
            this.name = importSpecifier.substring(ndx + 1);
         }
         else {
            this.name = importSpecifier;
         }
      }

      public String toString()
      {
         return "ResolvedImportNotFound{" + importSpecifier + "}";
      }

      public String match(String name)
      {
         if ((name.equals(this.name)) || (importSpecifier.equals(name)))
            return this.name;
         // FIXME: note that we don't handle on-demand imports here.
         return null;
      }

      public boolean mismatch(String name)
      {
         return true; // FIXME!
      }

      public ClassDoc tryFetch(String name)
      {
         return null;
      }
   }

   private class ResolvedImportPackageFile
      implements ResolvedImport
   {
      private Set topLevelClassNames;
      private File packageFile;
      private String packageName;
      private Map cache = new HashMap();

      ResolvedImportPackageFile(File packageFile, String packageName)
      {
         this.packageFile = packageFile;
         this.packageName = packageName;
         topLevelClassNames = new HashSet();
         File[] files = packageFile.listFiles();
         for (int i=0; i<files.length; ++i) {
            if (!files[i].isDirectory() && files[i].getName().endsWith(".java")) {
               String topLevelClassName = files[i].getName();
               topLevelClassName
                  = topLevelClassName.substring(0, topLevelClassName.length() - 5);
               topLevelClassNames.add(topLevelClassName);
            }
         }
      }

      public String match(String name)
      {
         ClassDoc loadedClass = classNamed(packageName + "." + name);
         if (null != loadedClass) {
            return loadedClass.qualifiedName();
         }
         else {
            String topLevelName = name;
            int ndx = topLevelName.indexOf('.');
            String innerClassName = null;
            if (ndx > 0) {
               innerClassName = topLevelName.substring(ndx + 1);
               topLevelName = topLevelName.substring(0, ndx);
            }

            if (topLevelClassNames.contains(topLevelName)) {
               //System.err.println(this + ".match returns " + packageName + "." + name);
               return packageName + "." + name;
            }
            // FIXME: inner classes
            else {
               return null;
            }
         }
      }

      public boolean mismatch(String name)
      {
         return null == match(name);
      }

      public ClassDoc tryFetch(String name)
      {
         ClassDoc loadedClass = classNamed(packageName + "." + name);
         if (null != loadedClass) {
            return loadedClass;
         }
         else if (null != match(name)) {

            String topLevelName = name;
            int ndx = topLevelName.indexOf('.');
            String innerClassName = null;
            if (ndx > 0) {
               innerClassName = topLevelName.substring(ndx + 1);
               topLevelName = topLevelName.substring(0, ndx);
            }

            ClassDoc topLevelClass = (ClassDoc)cache.get(topLevelName);
            if (null == topLevelClass) {
               File classFile = new File(packageFile, topLevelName + ".java");
               try {
                  // FIXME: inner classes
                  topLevelClass = parser.processSourceFile(classFile, false, sourceEncoding, null);
               }
               catch (Exception ignore) {
                  printWarning("Could not parse source file " + classFile);
               }
               cache.put(topLevelName, topLevelClass);
            }
            if (null == innerClassName) {
               return topLevelClass;
            }
            else {
               return getInnerClass(topLevelClass, innerClassName);
            }
         }
         else {
            return null;
         }
      }

      public String toString()
      {
         return "ResolvedImportPackageFile{" + packageFile + "," + packageName + "}";
      }
   }

   private ClassDoc getInnerClass(ClassDoc topLevelClass, String innerClassName)
   {
      StringTokenizer st = new StringTokenizer(innerClassName, ".");
   outer:

      while (st.hasMoreTokens()) {
         String innerClassNameComponent = st.nextToken();
         ClassDoc[] innerClasses = topLevelClass.innerClasses();
         for (int i=0; i<innerClasses.length; ++i) {
            if (innerClasses[i].name().equals(innerClassNameComponent)) {
               topLevelClass = innerClasses[i];
               continue outer;
            }
         }
         printWarning("Could not find inner class " + innerClassName + " in class " + topLevelClass.qualifiedName());
         return null;
      }
      return topLevelClass;
   }

   private class ResolvedImportClassFile
      implements ResolvedImport
   {
      private File classFile;
      private String innerClassName;
      private String name;
      private ClassDoc classDoc;
      private boolean alreadyFetched;
      private String qualifiedName;

      ResolvedImportClassFile(File classFile, String innerClassName, String name, String qualifiedName)
      {
         this.classFile = classFile;
         this.innerClassName = innerClassName;
         this.name = name;
         this.qualifiedName = qualifiedName;
      }

      public String toString()
      {
         return "ResolvedImportClassFile{" + classFile + "," + innerClassName +  "}";
      }

      public String match(String name)
      {
         String topLevelName = name;
         int ndx = topLevelName.indexOf('.');

         String _innerClassName = null;
         if (ndx > 0) {
            _innerClassName = topLevelName.substring(ndx + 1);
            topLevelName = topLevelName.substring(0, ndx);
         }

         if (this.name.equals(topLevelName)) {
            if (null == _innerClassName) {
               return qualifiedName;
            }
            else {
               return qualifiedName + "." + _innerClassName;
            }
         }
         else {
            return null;
         }
      }

      public boolean mismatch(String name)
      {
         return null == match(name);
      }

      public ClassDoc tryFetch(String name)
      {
         if (null != match(name)) {
            ClassDoc topLevelClass = null;
            if (alreadyFetched) {
               topLevelClass = classDoc;
            }
            else {
               alreadyFetched = true;
               try {
                  topLevelClass = parser.processSourceFile(classFile, false, sourceEncoding, null);
               }
               catch (Exception ignore) {
                  printWarning("Could not parse source file " + classFile);
               }
            }
            if (null == topLevelClass) {
               return null;
            }
            else {
               return getInnerClass(topLevelClass, innerClassName);
            }
         }
         else {
            return null;
         }
      }

      public String getName()
      {
         if (innerClassName != null) {
            return name + innerClassName;
         }
         else {
            return name;
         }
      }
   }

   private class ResolvedImportReflectionClass
      implements ResolvedImport
   {
      private Class clazz;
      private String name;

      ResolvedImportReflectionClass(Class clazz)
      {
         this.clazz = clazz;
         String className = clazz.getName();
         int ndx = className.lastIndexOf('.');
         if (ndx >= 0) {
            this.name = className.substring(ndx + 1);
         }
         else {
            this.name = className;
         }
      }

      public String toString()
      {
         return "ResolvedImportReflectionClass{" + clazz.getName() + "}";
      }

      public String match(String name)
      {
         if ((this.name.equals(name)) || (clazz.getName().equals(name))) {
            return clazz.getName();
         }
         else {
            return null;
         }
      }

      public boolean mismatch(String name)
      {
         return null == match(name);
      }

      public ClassDoc tryFetch(String name)
      {
         if (null != match(name)) {
            return new ClassDocReflectedImpl(clazz);
         }
         // FIXME: inner classes?
         else {
            return null;
         }
      }

      public String getName()
      {
         return name;
      }
   }

   private class ResolvedImportReflectionPackage
      implements ResolvedImport
   {
      private String packagePrefix;

      ResolvedImportReflectionPackage(String packagePrefix)
      {
         this.packagePrefix = packagePrefix;
      }

      public String toString()
      {
         return "ResolvedImportReflectionPackage{" + packagePrefix + ".*}";
      }

      public String match(String name)
      {
         try {
            Class clazz = Class.forName(packagePrefix + "." + name);
            return clazz.getName();
         }
         catch (Exception e) {
            return null;
         }
      }

      public boolean mismatch(String name)
      {
         return null == match(name);
      }

      public ClassDoc tryFetch(String name)
      {
         try {
            Class clazz = Class.forName(packagePrefix + name);
            return ClassDocReflectedImpl.newInstance(clazz);
         }
         catch (Exception e) {
            return null;
         }
      }

      public String getName()
      {
         return packagePrefix;
      }
   }

   private List unlocatablePrefixes = new LinkedList();

   private ResolvedImport resolveImport(String importSpecifier)
   {
      ResolvedImport result = resolveImportFileSystem(importSpecifier);
      if (null == result && Main.getInstance().isReflectionEnabled()) {
         result = resolveImportReflection(importSpecifier);
      }
      if (null == result) {
         result = new ResolvedImportNotFound(importSpecifier);
      }
      return result;
   }

   private ResolvedImport resolveImportReflection(String importSpecifier)
   {
      String importedPackageOrClass = importSpecifier;
      if (importedPackageOrClass.endsWith(".*")) {
         importedPackageOrClass = importedPackageOrClass.substring(0, importedPackageOrClass.length() - 2);

         return new ResolvedImportReflectionPackage(importedPackageOrClass);

         //return null;
      }
      else {
         try {
            Class importedClass = Class.forName(importSpecifier);
            return new ResolvedImportReflectionClass(importedClass);
         }
         catch (Throwable ignore) {
            return null;
         }
      }
   }

   private ResolvedImport resolveImportFileSystem(String importSpecifier)
   {
      for (Iterator it = unlocatablePrefixes.iterator(); it.hasNext(); ) {
         String unlocatablePrefix = (String)it.next();
         if (importSpecifier.startsWith(unlocatablePrefix)) {
            return null;
         }
      }

      String longestUnlocatablePrefix = "";

      for (Iterator it=sourcePath.iterator(); it.hasNext(); ) {

         File _sourcePath = (File)it.next();

         StringBuffer packageOrClassPrefix = new StringBuffer();
         StringTokenizer st = new StringTokenizer(importSpecifier, ".");
         while (st.hasMoreTokens() && _sourcePath.isDirectory()) {
            String token = st.nextToken();
            if ("*".equals(token)) {
               return new ResolvedImportPackageFile(_sourcePath,
                                                    packageOrClassPrefix.substring(0, packageOrClassPrefix.length() - 1));
            }
            else {
               packageOrClassPrefix.append(token);
               packageOrClassPrefix.append('.');
               File classFile = new File(_sourcePath, token + ".java");
               //System.err.println("  looking for file " + classFile);
               if (classFile.exists()) {
                  StringBuffer innerClassName = new StringBuffer();
                  while (st.hasMoreTokens()) {
                     token = st.nextToken();
                     if (innerClassName.length() > 0) {
                        innerClassName.append('.');
                     }
                     innerClassName.append(token);
                  }
                  return new ResolvedImportClassFile(classFile, innerClassName.toString(), token, importSpecifier);
               }
               else {
                  _sourcePath = new File(_sourcePath, token);
               }
            }
         }
         if (st.hasMoreTokens()) {
            if (packageOrClassPrefix.length() > longestUnlocatablePrefix.length()) {
               longestUnlocatablePrefix = packageOrClassPrefix.toString();
            }
         }
      }

      if (longestUnlocatablePrefix.length() > 0) {
         unlocatablePrefixes.add(longestUnlocatablePrefix);
      }

      return null;
   }

   private Map resolvedImportCache = new HashMap();

   private ResolvedImport getResolvedImport(String importSpecifier)
   {
      ResolvedImport result
         = (ResolvedImport)resolvedImportCache.get(importSpecifier);
      if (null == result) {
         result = resolveImport(importSpecifier);
         resolvedImportCache.put(importSpecifier, result);
      }
      return result;
   }

   public String resolveClassName(String className, ClassDocImpl context)
   {
      Iterator it = context.getImportSpecifierList().iterator();
      while (it.hasNext()) {
         String importSpecifier = (String)it.next();
         ResolvedImport resolvedImport = getResolvedImport(importSpecifier);
         String resolvedScheduledClassName = resolvedImport.match(className);

         if (null != resolvedScheduledClassName) {
            return resolvedScheduledClassName;
         }
      }
      return className;
   }

   public ClassDoc findScheduledClassFile(String scheduledClassName,
                                          ClassDoc scheduledClassContext)
      throws ParseException, IOException
   {
      String resolvedScheduledClassName = null;

      if (scheduledClassContext instanceof ClassDocImpl) {

         //((ClassDocImpl)scheduledClassContext).resolveReferencedName(scheduledClassName);
         Iterator it = ((ClassDocImpl)scheduledClassContext).getImportSpecifierList().iterator();
         while (it.hasNext()) {
            String importSpecifier = (String)it.next();
            ResolvedImport resolvedImport = getResolvedImport(importSpecifier);
            //System.err.println("  looking in import '" +  resolvedImport + "'");
            resolvedScheduledClassName = resolvedImport.match(scheduledClassName);
            if (null != resolvedScheduledClassName) {
               ClassDoc result = resolvedImport.tryFetch(scheduledClassName);
               if (null != result) {
                  return result;
               }
               else {
                  if (!inaccessibleReportedSet.contains(scheduledClassName)) {
                     inaccessibleReportedSet.add(scheduledClassName);
                     printWarning("Error while loading class " + scheduledClassName);
                  }
                  // FIXME: output resolved class name here
                  return null;
               }
            }
         }
      }
      else {
         System.err.println("findScheduledClassFile for '" + scheduledClassName + "' in proxy for " + scheduledClassContext);
      }

      // interpret as fully qualified name on file system

      ResolvedImport fqImport = resolveImportFileSystem(scheduledClassName);
      if (null != fqImport && fqImport instanceof ResolvedImportClassFile) {
         return fqImport.tryFetch(((ResolvedImportClassFile)fqImport).getName());
      }

      // use reflection, assume fully qualified class name

      if (!unlocatableReflectedClassNames.contains(scheduledClassName)) {
         if (Main.getInstance().isReflectionEnabled()) {
            try {
               Class clazz = Class.forName(scheduledClassName);
               printWarning("Cannot locate class " + scheduledClassName + " on file system, falling back to reflection.");
               ClassDoc result = new ClassDocReflectedImpl(clazz);
               return result;
            }
            catch (Throwable ignore) {
               unlocatableReflectedClassNames.add(scheduledClassName);
            }
         }
         else {
            unlocatableReflectedClassNames.add(scheduledClassName);
         }
      }

      if (null == resolvedScheduledClassName) {
         resolvedScheduledClassName = scheduledClassName;
      }
      if (!unlocatableReportedSet.contains(resolvedScheduledClassName)) {
         unlocatableReportedSet.add(resolvedScheduledClassName);
         printWarning("Cannot locate class " + resolvedScheduledClassName + " referenced in class " + scheduledClassContext.qualifiedName());
      }
      return null;
   }

   private Set unlocatableReflectedClassNames = new HashSet();

   public static boolean recursiveClasses = false;

   public void addSpecifiedPackageName(String packageName) {
      specifiedPackageNames.add(packageName);
   }

   public void addSpecifiedSourceFile(File sourceFile) {
      specifiedSourceFiles.add(sourceFile);
   }

   public boolean hasSpecifiedPackagesOrClasses() {
      return !specifiedPackageNames.isEmpty()
         ||  !specifiedSourceFiles.isEmpty();
   }

   public void setOptions(String[][] customOptionArr) {
      this.customOptionArr = customOptionArr;
   }

   public void setSourcePath(List sourcePath) {
      this.sourcePath = sourcePath;
   }

   public void finalize() throws Throwable {
      super.finalize();
   }

   public void flush()
   {
      try {
         rawCommentCache.close();
      }
      catch (IOException e) {
         printError("Cannot close raw comment cache");
      }

      rawCommentCache = null;
      customOptionArr = null;
      specifiedPackageNames = null;
      classesList = null;
      classDocMap = null;
      packageDocMap = null;
      classes = null;
      specifiedClasses = null;
      specifiedPackages = null;
      scheduledClasses = null;
      sourcePath = null;
      parser = null;
      unlocatableReportedSet = null;
      inaccessibleReportedSet = null;
   }

   public void setSourceEncoding(String sourceEncoding)
   {
      this.sourceEncoding = sourceEncoding;
   }

   public RootDocImpl()
   {
      super(null);
   }

   public static String readHtmlBody(File file)
      throws IOException
   {
      FileReader fr=new FileReader(file);
      long size = file.length();
      char[] packageDocBuf=new char[(int)(size)];
      int index = 0;
      int i = fr.read(packageDocBuf, index, (int)size);
      while (i > 0) {
         index += i;
         size -= i;
         i = fr.read(packageDocBuf, index, (int)size);
      }
      fr.close();

      // We only need the part between the begin and end body tag.
      String html = new String(packageDocBuf);
      int start = html.indexOf("<body");
      if (start == -1)
         start = html.indexOf("<BODY");
      int end = html.indexOf("</body>");
      if (end == -1)
         end = html.indexOf("</BODY>");
      if (start != -1 && end != -1) {
         // Start is end of body tag.
         start = html.indexOf('>', start) + 1;
         if (start != -1 && start < end)
            html = html.substring(start, end);
      }
      return html.trim();
   }

   public Parser getParser()
   {
      return parser;
   }
}
