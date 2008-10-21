/* gnu.classpath.tools.doclets.xmldoclet.Driver
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

package gnu.classpath.tools.doclets.xmldoclet;

import com.sun.javadoc.*;
import java.io.*;

import com.sun.tools.doclets.Taglet;

import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;
import java.lang.reflect.Modifier;

import java.text.DateFormat;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Comparator;
import java.util.HashSet;
import java.util.TreeSet;
import java.util.Iterator;
import java.util.LinkedHashMap;
import java.util.LinkedList;
import java.util.List;
import java.util.Locale;
import java.util.Map;
import java.util.HashMap;
import java.util.Properties;
import java.util.Set;
import java.util.SortedSet;
import java.util.StringTokenizer;
import java.util.TreeMap;

import gnu.classpath.tools.gjdoc.TemporaryStore;
import gnu.classpath.tools.gjdoc.GjdocPackageDoc;

import gnu.classpath.tools.doclets.PackageGroup;
import gnu.classpath.tools.doclets.PackageMatcher;
import gnu.classpath.tools.doclets.InvalidPackageWildcardException;

import gnu.classpath.tools.doclets.xmldoclet.doctranslet.DocTranslet;
import gnu.classpath.tools.doclets.xmldoclet.doctranslet.DocTransletOptions;

import gnu.classpath.tools.taglets.AuthorTaglet;
import gnu.classpath.tools.taglets.VersionTaglet;
import gnu.classpath.tools.taglets.SinceTaglet;
import gnu.classpath.tools.taglets.DeprecatedTaglet;
import gnu.classpath.tools.taglets.GenericTaglet;
import gnu.classpath.tools.doclets.StandardTaglet;

import gnu.classpath.tools.java2xhtml.Java2xhtml;

import gnu.classpath.tools.IOToolkit;
import gnu.classpath.tools.FileSystemClassLoader;

/**
 *  A Doclet which retrieves all information presented by the Doclet
 *  API, dumping it to stdout in XML format.
 *
 *  @author Julian Scheid
 */
public class Driver {

   public static final String XMLDOCLET_VERSION = "0.6.1";

   /**
    *  Used for redirecting error messages to <code>/dev/null</code>.
    */
   private static class NullErrorReporter implements DocErrorReporter {
      public void printError(String ignore) {}
      public void printWarning(String ignore) {}
      public void printNotice(String ignore) {}
   }

   /*
    *  Taglet context constants.
    */
   private static final int CONTEXT_CONSTRUCTOR  = 1;
   private static final int CONTEXT_FIELD        = 2;
   private static final int CONTEXT_METHOD       = 3;
   private static final int CONTEXT_OVERVIEW     = 4;
   private static final int CONTEXT_PACKAGE      = 5;
   private static final int CONTEXT_TYPE         = 6;

   /**
    *  All XML output will go to this stream.
    */
   private PrintWriter out;

   /**
    *  How many spaces to indent each XML node level,
    *  i.e. Tab size for output.
    */
   private static int indentStep = 1;

   /**
    *  Won't output superfluous spaces if set to true.
    *  If set to false, output will be more legible.
    */
   private boolean compress = false;

   /**
    *  Won't output warning messages while fixing
    *  HTML code if set to true.
    */
   private boolean noHTMLWarn = false;

   /**
    *  Won't output warning messages when encountering tags
    *  that look like an email address if set to true.
    */
   private boolean noEmailWarn = false;

   /**
    *  Will fix HTML if necessary so that each comment
    *  contains valid XML code if set to true. If set
    *  to false, HTML code will not be modified and
    *  instead encapsulated in a CDATA section.
    */
   private boolean fixHTML = true;

   /**
    *  User-specified name of the directory where the final version of
    *  the generated files will be written to.
    *
    *  If no XSLT sheet is given, the XML output will go directly into
    *  this directory. Otherwise, XML output will go to a temporary
    *  directory and XSLT output will go to this directory.
    */
   private File targetDirectory = null;

   /**
    *  Directory where XML output will be written to. If no XSLT
    *  sheet was given, this is the target directory specified
    *  by the user. Otherwise, this is a temporary directory.
    */
   private File xmlTargetDirectory;

   /**
    *  Contains a number of TargetContexts which describe which XSLT
    *  sheet to apply to the output of this doclet, to what directory
    *  the XSLT output is written, and which postprocess driver to use
    *  to process XSLT output.
    */
   private List targets = new ArrayList();

   /**
    *  XML text to include at the end of every generated page. Read
    *  from the file specified on the command line using -bottomnote.
    *  If present, this will be written to the main output file
    *  (index.xml) in node /gjdoc:rootDoc/gjdoc:bottomnote.
    */
   private String bottomNote;

   /**
    *  Brief description of the package set. Can be specified on the
    *  command line using -title.  This will be written to the main
    *  output file (index.xml) in node
    *  /gjdoc:rootDoc/gjdoc:title. The HTML generating XSLT sheet
    *  uses this for example in window titles.
    */
   private String title;

   /**
    *  Path to the directory where temporary files should be stored.
    *  Defaults to system tempdir, but can be overridden by user 
    *  with -workpath.
    */
   private String workingPath = System.getProperty("java.io.tmpdir");

   /**
    *  Temporary directory created by this doclet where all 
    *  temporary files will be stored in. If no temporary
    *  files are needed (i.e. no XSLT postprocessing stage
    *  specified by user), this is <code>null</code>.
    */
    private File workingDirectory;

   /**
    *  Whether to deep-copy the doc-files subdirectory.
    */
    private boolean docFilesSubdirsEnabled = false;

   /**
    *  Which direct subdirectories of the doc-files directories to exclude.
    *  Set of String.
    */
    private Set excludeDocFilesSubDirs = new HashSet();

   /**
    *  Stores the Doclet API RootDoc we are operating on.
    */
   private RootDoc rootDoc;

   /**
    *  XML namespace prefix used for all tags, except for HTML
    *  tags copied from Javadoc comments. Excluding colon.
    */
   public static final String tagPrefix = "gjdoc";

   /**
    *  Classpath for loading Taglet classes.
    */
   private String tagletPath = null;

   /**
    *  The current class that is being processed.
    *  Set in outputClassDoc().
    */
   private ClassDoc currentClass;

   /**
    *  The current member that is being processed.
    *  Set in outputMemberDoc().
    */
   private MemberDoc currentMember;

   /**
    *  The current constructor/method that is being processed.
    *  Set in outputExecutableMemberDoc().
    */
   private ExecutableMemberDoc currentExecMember;

   /**
    *  Mapping from tag type to Taglet for user Taglets specified on
    *  the command line.
    */
   private Map tagletMap = new LinkedHashMap();

   /**
    *  Keeps track of the tags mentioned by the user during option
    *  processiong so that an error can be emitted if a tag is
    *  mentioned more than once.
    */
   private List mentionedTags = new LinkedList();

   /**
    *  Stores options to be passed to the DocTranslet.
    */
   private DocTransletOptions docTransletOptions = new DocTransletOptions();   

   /**
    *  Stores the package groups specified in the user
    *  options. Contains objects of type PackageGroup.
    */
   private List packageGroups = new LinkedList();

   private HtmlRepairer htmlRepairer;

   public static boolean start(TemporaryStore _rootDocWrapper) {
      return new Driver().instanceStart((RootDoc)_rootDocWrapper.getAndClear());
   }

   /**
    *  Official Doclet entry point.
    */
   public static boolean start(RootDoc _rootDoc) {

      // Create a new XmlDoclet instance and delegate control.
      TemporaryStore tstore = new TemporaryStore(_rootDoc);
      _rootDoc = null;
      return new Driver().instanceStart((RootDoc)tstore.getAndClear());
   }

   /**
    *  Output an XML tag describing a com.sun.javadoc.Type object.
    *  Assumes that the tag does not have subtags.
    *
    *  @param level  Level of indentation. Will be multiplied by 
    *                <code>indentStep</code> to yield actual amount
    *                of whitespace inserted at start of line.
    *  @param tag    Identifier for the XML tag being output.
    *  @param type   The Javadoc Type to be output.
    */
   protected void outputType(int level, String tag, Type type) {
      outputType(level, tag, type, true);
   }

   protected void outputType(int level, String tag, Type type, boolean atomic) {

      boolean isIncluded = false;
      ClassDoc typeAsClassDoc = type.asClassDoc();
      String packageName = null;
      if (null != typeAsClassDoc) {
         isIncluded = typeAsClassDoc.isIncluded();
         packageName = typeAsClassDoc.containingPackage().name();
      }
      println(level, "<"+tagPrefix+":"+tag + " typename=\""+type.typeName()+"\""+
	      " qualifiedtypename=\""+type.qualifiedTypeName()+"\""
	      +(type.dimension().length()==0?"":" dimension=\""+type.dimension()+"\"")
              +(isIncluded?" isIncluded=\"true\"" : "")
              +((null != packageName)?" package=\"" + packageName + "\"" : "")
	      +(atomic?"/":"")+">");
   }

   protected void outputExecutableMemberDocBody(int level, ExecutableMemberDoc memberDoc) {

      currentExecMember = memberDoc;

      outputMemberDocBody(level, memberDoc);

      Parameter[] parameters = memberDoc.parameters();
      for (int i=0, ilim=parameters.length; i<ilim; ++i) {
	 Parameter parameter = parameters[i];
	 outputType(level, "parameter name=\""+parameter.name()+"\"", parameter.type());
      }

      ClassDoc[] exceptions = memberDoc.thrownExceptions();
      for (int i=0, ilim=exceptions.length; i<ilim; ++i) {
	 ClassDoc exception = exceptions[i];
	 outputType(level, "thrownException", exception);
       }

      printAtomTag(level, "signature full=\""+memberDoc.signature()+"\" flat=\""+memberDoc.flatSignature()+"\"");

      if (memberDoc.isNative()) {
	 printAtomTag(level, "isNative");
      }

      if (memberDoc.isSynchronized()) {
	 printAtomTag(level, "isSynchronized");
      }
   }

   protected void outputMethodDoc(int level, MethodDoc methodDoc) {
      println();
      printOpenTag(level, "methoddoc name=\""+methodDoc.name()+"\"");
      outputExecutableMemberDocBody(level+1, methodDoc);
      outputType(level+1, "returns", methodDoc.returnType());
      printCloseTag(level, "methoddoc");
   }

   protected void outputMemberDocBody(int level, MemberDoc memberDoc) {
      currentMember = memberDoc;
      outputProgramElementDocBody(level, memberDoc);
   }

   protected void outputFieldDocBody(int level, FieldDoc fieldDoc) {
      outputType(level, "type", fieldDoc.type());
      if (fieldDoc.isTransient()) {
	 printAtomTag(level, "isTransient");
      }
      if (fieldDoc.isVolatile()) {
	 printAtomTag(level, "isVolatile");
      }
   }

   private void outputFieldDoc(int level, FieldDoc fieldDoc) {
      println();
      printOpenTag(level, "fielddoc name=\""+fieldDoc.name()+"\"");
      outputMemberDocBody(level+1, fieldDoc);
      outputFieldDocBody(level+1, fieldDoc);
      printCloseTag(level, "fielddoc");
   }

   protected void outputConstructorDoc(int level, ConstructorDoc constructorDoc) {
      println();
      printOpenTag(level, "constructordoc name=\""+constructorDoc.name()+"\"");
      outputExecutableMemberDocBody(level+1, constructorDoc);
      printCloseTag(level, "constructordoc");
   }

   protected void outputSuperInterfacesRec(int level, ClassDoc classDoc) {
      if (null!=classDoc) {
	 ClassDoc[] interfaces = classDoc.interfaces();
         if (null != interfaces) {
            for (int i=0, ilim=interfaces.length; i<ilim; ++i) {
               outputType(level, "superimplements", interfaces[i]);
            }
         }
	 outputSuperInterfacesRec(level, classDoc.superclass());
      }
   }

   protected void outputClassDocSummary(ClassDoc classDoc) {
      println();
      printOpenTag(1, "classdoc name=\""+classDoc.name()+"\" qualifiedtypename=\""+classDoc.qualifiedName()+"\" isIncluded=\"true\"");
      if (null!=classDoc.superclass()) {
	 outputType(2, "superclass", classDoc.superclass());
      }

      ClassDoc[] interfaces = classDoc.interfaces();
      for (int i=0, ilim=interfaces.length; i<ilim; ++i) {
	 outputType(2, "implements", interfaces[i]);
      }
      outputSuperInterfacesRec(2, classDoc.superclass());

      printAtomTag(2, "containingPackage name=\""+classDoc.containingPackage().name()+"\"");
      if (classDoc.isError()) {
         printAtomTag(2, "isError");
      }
      if (classDoc.isException()) {
         printAtomTag(2, "isException");
      }
      if (classDoc.isInterface()) {
         printAtomTag(2, "isInterface");
      }
      if (classDoc.isOrdinaryClass()) {
         printAtomTag(2, "isOrdinaryClass");
      }

      printCloseTag(1, "classdoc");
   }

   protected void outputPackageDoc(PackageDoc packageDoc) {
      println();
      printOpenTag(1, "packagedoc name=\""+packageDoc.name()+"\"");
      if (packageDoc.firstSentenceTags().length > 0) {
	 printOpenTag(2, "firstSentenceTags", false);
	 outputTags(3, packageDoc.firstSentenceTags(), true, CONTEXT_PACKAGE);
	 printCloseTag(0, "firstSentenceTags");
	 printOpenTag(2, "inlineTags", false);
	 outputTags(3, packageDoc.inlineTags(), true, CONTEXT_PACKAGE);
	 printCloseTag(0, "inlineTags");
      }

      if (packageDoc.tags().length > 0) {
	 printOpenTag(2, "tags");
	 outputTags(3, packageDoc.tags(), true, CONTEXT_PACKAGE);
	 printCloseTag(2, "tags");
      }

      if (packageDoc.seeTags().length > 0) {
	 printOpenTag(2, "seeTags");
	 outputTags(3, packageDoc.seeTags(), true, CONTEXT_PACKAGE);
	 printCloseTag(2, "seeTags");
      }

      ClassDoc[] allClasses = (ClassDoc[]) packageDoc.allClasses().clone();
      Arrays.sort(allClasses);

      if (false) {
	 for (int i = 0, ilim = allClasses.length; i < ilim; ++ i) {
	    printAtomTag(2, "containsClass qualifiedtypename=\""+allClasses[i].qualifiedTypeName()+"\"");
	 }
      }

      printCloseTag(1, "packagedoc");
   }

   protected void outputClassDoc(ClassDoc classDoc) throws IOException {

      currentClass = classDoc;

      println();
      printOpenTag(1, "classdoc xmlns=\"http://www.w3.org/TR/REC-html40\" xmlns:"+tagPrefix+"=\"http://www.gnu.org/software/cp-tools/gjdocxml\" name=\""+classDoc.name()+"\" qualifiedtypename=\""+classDoc.qualifiedName()+"\"");

      ClassDoc[] interfaces = classDoc.interfaces();
      for (int i=0, ilim=interfaces.length; i<ilim; ++i) {
	 outputType(2, "implements", interfaces[i]);
      }
      outputSuperInterfacesRec(2, classDoc.superclass());

      outputProgramElementDocBody(2, classDoc);
      if (classDoc.isAbstract())
	 printAtomTag(2, "isAbstract");
      if (classDoc.isSerializable())
	 printAtomTag(2, "isSerializable");
      if (classDoc.isExternalizable())
	 printAtomTag(2, "isExternalizable");
      if (classDoc.definesSerializableFields()) {
	 printAtomTag(2, "definesSerializableFields");
      }

      ConstructorDoc[] constructors = classDoc.constructors();
      for (int i=0, ilim=constructors.length; i<ilim; ++i) {
	 outputConstructorDoc(2, constructors[i]);
      }

      MethodDoc[] methods = classDoc.methods();
      for (int i=0, ilim=methods.length; i<ilim; ++i) {
	 outputMethodDoc(2, methods[i]);
      }

      FieldDoc[] fields = classDoc.fields();
      for (int i=0, ilim=fields.length; i<ilim; ++i) {
	 outputFieldDoc(2, fields[i]);
      }

      if (classDoc.serializableFields().length > 0) {
	 printOpenTag(2, "serializableFields");

         FieldDoc[] sfields = classDoc.serializableFields();
         for (int i=0, ilim=sfields.length; i<ilim; ++i) {
            outputFieldDoc(2, sfields[i]);
         }
	 printCloseTag(2, "serializableFields");
      }

      Java2xhtml java2xhtml = new Java2xhtml();
      Properties properties = new Properties();
      properties.setProperty("isCodeSnippet", "true");
      properties.setProperty("hasLineNumbers", "true");
      java2xhtml.setProperties(properties);

      if (null == classDoc.containingClass() && docTransletOptions.linksource) {
         printOpenTag(2, "source");
         StringWriter sourceBuffer = new StringWriter();
         File sourceFile = new File(((GjdocPackageDoc)classDoc.containingPackage()).packageDirectory(),
                                    classDoc.name() + ".java");
         FileReader sourceReader = new FileReader(sourceFile);
         IOToolkit.copyStream(sourceReader, sourceBuffer);
         print(java2xhtml.makeHTML(sourceBuffer.getBuffer(), sourceFile.getName()));
         printCloseTag(2, "source");      
      }

      ClassDoc superclassDoc = classDoc.superclass();
      while (superclassDoc != null) {
	 outputType(2, "superclass", superclassDoc, false);

         // FIXME: remove the following after adjusting the XSLT sheets:
         printAtomTag(3, "containingPackage name=\"" + superclassDoc.containingPackage().name() + "\"");

         MethodDoc[] superMethods = superclassDoc.methods();
         if (null != superMethods) {
            for (int i=0, ilim=superMethods.length; i<ilim; ++i) {
               printAtomTag(3, "methoddoc name=\"" + superMethods[i].name() + "\" signature=\"" + superMethods[i].signature() + "\"");
            }
         }
         
         FieldDoc[] superFields = superclassDoc.fields();
         if (null != superFields) {
            for (int i=0, ilim=superFields.length; i<ilim; ++i) {
               printAtomTag(3, "fielddoc name=\"" + superFields[i].name() + "\"");
            }
         }
         printCloseTag(2, "superclass");
         
         superclassDoc = superclassDoc.superclass();
      }

      outputUsage(classDoc, 2);

      printCloseTag(1, "classdoc");

      currentClass = null;
      currentMember = null;
      currentExecMember = null;
   }

   protected int outputHeritageOpen(int level, ClassDoc classDoc) {

      ClassDoc superClassDoc = classDoc.superclass();
      if (null != superClassDoc) {
         level = outputHeritageOpen(level, superClassDoc);
         ++ level;
      }
      outputType(level, "heritage", classDoc, false);
      return level;
   }

   protected void outputHeritageClose(int level, ClassDoc classDoc) {

      ClassDoc superClassDoc = classDoc.superclass();
      if (null != superClassDoc) {
         outputHeritageClose(level + 1, superClassDoc);
      }
      printCloseTag(level, "heritage");
   }

   protected void outputDocBody(int level, Doc doc) {

      int context = CONTEXT_TYPE;

      if (doc.isClass()) {
	 printAtomTag(level, "isClass");

         ClassDoc classDoc = (ClassDoc)doc;
         ClassDoc[] classes = rootDoc.classes();
         for (int i=0, ilim=classes.length; i<ilim; ++i) {
            if (classes[i].superclass() == classDoc) {
               outputType(level, "extended-by", classes[i]);
            }
         }

         outputHeritageOpen(level, classDoc);
         outputHeritageClose(level, classDoc);
      }
      if (doc.isConstructor()) {
	 printAtomTag(level, "isConstructor");
         context = CONTEXT_CONSTRUCTOR;
      }
      if (doc.isError()) {
	 printAtomTag(level, "isError");
      }
      if (doc.isException()) {
	 printAtomTag(level, "isException");
      }
      if (doc.isField()) {
	 printAtomTag(level, "isField");
         context = CONTEXT_FIELD;
      }
      if (doc.isIncluded()) {
	 printAtomTag(level, "isIncluded");
      }
      if (doc.isInterface()) {
	 printAtomTag(level, "isInterface");

         ClassDoc classDoc = (ClassDoc)doc;
         ClassDoc[] classes = rootDoc.classes();
         for (int i=0, ilim=classes.length; i<ilim; ++i) {
            ClassDoc[] implementedInterfaces = classes[i].interfaces();
            for (int j=0; j<implementedInterfaces.length; ++j) {
               if (implementedInterfaces[j] == classDoc) {
                  if (classDoc.isInterface()) {
                        outputType(level, "subinterface", classes[i]);
                  }
                  else {
                     outputType(level, "implemented-by", classes[i]);
                  }
                  break;
               }
            }
         }
      }
      if (doc.isMethod()) {
	 printAtomTag(level, "isMethod");
         context = CONTEXT_METHOD;
      }
      if (doc.isOrdinaryClass()) {
	 printAtomTag(level, "isOrdinaryClass");
      }

      if (doc.inlineTags().length > 0) {
	 printOpenTag(level, "inlineTags", false);
	 outputTags(level+1, doc.inlineTags(), true, context);
	 printCloseTag(0, "inlineTags");
      }

      if (doc.firstSentenceTags().length > 0) {
	 printOpenTag(level, "firstSentenceTags", false);
	 outputTags(level+1, doc.firstSentenceTags(), true, context);
	 printCloseTag(0, "firstSentenceTags");
      }

      if (doc.tags().length > 0) {
	 printOpenTag(level, "tags");
         outputTaglets(level+1, doc.tags(), true, context);
	 printCloseTag(level, "tags");
      }

      if (doc.seeTags().length > 0) {
	 printOpenTag(level, "seeTags");
	 outputTags(level+1, doc.seeTags(), true, context);
	 printCloseTag(level, "seeTags");
      }

      SourcePosition position = doc.position();
      if (null != position) {
	 printAtomTag(level, "position file=\"" + position.file().getAbsolutePath() + "\" line=\"" + position.line() + "\" column=\"" + position.column() + "\"");
      }
   }

   protected void outputProgramElementDocBody(int level, ProgramElementDoc programElementDoc) {
      outputDocBody(level, programElementDoc);
      printAtomTag(level, "containingPackage name=\""+programElementDoc.containingPackage().name()+"\"");
      if (null!=programElementDoc.containingClass()) {
	 outputType(level, "containingClass", programElementDoc.containingClass());
      }
      String access;
      if (programElementDoc.isPublic()) 
	 access="public";
      else if (programElementDoc.isProtected()) 
	 access="protected";
      else if (programElementDoc.isPrivate()) 
	 access="private";
      else if (programElementDoc.isPackagePrivate()) 
	 access="package";
      else
	 throw new RuntimeException("Huh? "+programElementDoc+" is neither public, protected, private nor package protected.");
      printAtomTag(level, "access scope=\""+access+"\"");
      if (programElementDoc.isFinal())
	 printAtomTag(level, "isFinal");
      if (programElementDoc.isStatic())
	 printAtomTag(level, "isStatic");
   }

   protected void outputTags(int level, Tag[] tags, boolean descend, int context) {

      for (int i=0; i<tags.length; ++i) {
         outputTag(tags[i], level, descend, context, i == tags.length-1);
      }
   }

   protected void outputTag(Tag tag, int level, boolean descend, int context, boolean lastTag) {

      if (!"Text".equals(tag.name())) {
         printOpenTag(0 /* don't introduce additional whitespace */, 
                      "tag kind=\""+tag.kind()+"\" name=\""+tag.name()+"\"", false);
      }
      if (tag instanceof ThrowsTag) {
         ThrowsTag throwsTag = (ThrowsTag)tag;
         if (null!=throwsTag.exception()) {
            outputType(level+1, "exception", throwsTag.exception());
         }
         else {
            StringBuffer sb = new StringBuffer("Exception ");
            sb.append(throwsTag.exceptionName());
            sb.append(" not found in ");
            if (currentExecMember instanceof MethodDoc) {
               MethodDoc m = (MethodDoc)currentExecMember;
               sb.append(m.returnType().typeName());
               sb.append(m.returnType().dimension());
               sb.append(' ');
            }
            sb.append(currentClass.qualifiedName());
            sb.append('.');
            sb.append(currentExecMember.name());
            sb.append('(');
            Parameter[] params = currentExecMember.parameters();
            for (int j=0; j < params.length; j++) {
               sb.append(params[j].type().typeName());
               sb.append(params[j].type().dimension());
               sb.append(' ');
               sb.append(params[j].name());
               if (j != params.length-1)
                  sb.append(", ");
            }
            sb.append(')');
            printWarning(sb.toString());

            printAtomTag(level+1, "exception typename=\""+throwsTag.exceptionName()+"\"");
         }
      }
      else if (tag instanceof ParamTag) {
         ParamTag paramTag = (ParamTag)tag;
         printAtomTag(level+1, "parameter name=\""+paramTag.parameterName()+"\"");
      }

      if (null != tag.text()) {
         //printOpenTag(level+1, "text", false);
         if (fixHTML) {
            print(htmlRepairer.getWellformedHTML(tag.text()));
         }
         else {
            print("<![CDATA["+cdata(tag.text())+"]]>");
         }
         //printCloseTag(0 /* don't introduce additional whitespace */, "text");
      }
      else {
         printWarning("Tag got null text: "+tag);
      }

      if ((descend && ("@throws".equals(tag.name()) || "@param".equals(tag.name()))) || "@deprecated".equals(tag.name())) {
         if (tag.firstSentenceTags().length>0) {
            printOpenTag(level+1, "firstSentenceTags", false);
            outputTags(level+2, tag.firstSentenceTags(), false, context);
            printCloseTag(0, "firstSentenceTags");
         }
	    
         if (tag.inlineTags().length>0) {
            printOpenTag(level+1, "inlineTags", false);
            outputTags(level+2, tag.firstSentenceTags(), false, context);
            printCloseTag(0, "inlineTags");
         }
      }

      if (fixHTML && lastTag) {
         String terminateText = htmlRepairer.terminateText();
         if (null != terminateText && terminateText.length() > 0) {
            print(terminateText);
         }
      }

      if (!"Text".equals(tag.name())) {

         Taglet inlineTaglet = (Taglet)tagletMap.get(tag.name().substring(1));
         if (null != inlineTaglet && inlineTaglet.isInlineTag()) {
            printOpenTag(0, "inlineTagletText", false);
            print(inlineTaglet.toString(tag));
            printCloseTag(0, "inlineTagletText");
         }

         printCloseTag(0, "tag", false);
      }
   }

   void outputTaglets(int level, Tag[] tags, boolean descend, int context) 
   {
      for (Iterator it = tagletMap.keySet().iterator(); it.hasNext(); ) {
         String tagName = (String)it.next();
         Object o = tagletMap.get(tagName);
         Taglet taglet = (Taglet)o;

         if (!taglet.isInlineTag()
             && ((context != CONTEXT_CONSTRUCTOR || taglet.inConstructor())
                 || (context != CONTEXT_FIELD || taglet.inField())
                 || (context != CONTEXT_METHOD || taglet.inMethod())
                 || (context != CONTEXT_OVERVIEW || taglet.inOverview())
                 || (context != CONTEXT_PACKAGE || taglet.inPackage())
                 || (context != CONTEXT_TYPE || taglet.inType()))) {
            
            List tagsOfThisType = new ArrayList();
            for (int i=0, ilim=tags.length; i<ilim; ++i) {
               if (tags[i].name().substring(1).equals(tagName)) {
                  tagsOfThisType.add(tags[i]);
               }
            }

            if (!tagsOfThisType.isEmpty()) {
               Tag[] tagletTags = (Tag[])tagsOfThisType.toArray(new Tag[tagsOfThisType.size()]);
               if (taglet instanceof StandardTaglet) {
                  Iterator tagIterator = tagsOfThisType.iterator();
                  while (tagIterator.hasNext()) {
                     Tag tag = (Tag)tagIterator.next();
                     outputTag(tag, level, descend, context, !tagIterator.hasNext());
                  }
               }
               else {
                  String tagletString = taglet.toString(tagletTags);
                  if (null != tagletString) {
                     printOpenTag(0, "tag name=\"" + tagName + "\" taglet-generated=\"true\"");
                     if (fixHTML) {
                        print(htmlRepairer.getWellformedHTML(tagletString));
                        print(htmlRepairer.terminateText());
                     }
                     else {
                        print("<![CDATA["+cdata(tagletString)+"]]>");
                     }
                     printCloseTag(0, "tag", false);
                  }
               }
            }
         }
      }
   }

   /**
    *  Inofficial entry point. We got an instance here.
    */
   protected boolean instanceStart(RootDoc _rootDoc) {

      this.rootDoc = _rootDoc;
      _rootDoc = null;

      boolean xmlOnly = true;

      // Set the default Taglet order

      registerTaglet(new VersionTaglet());
      registerTaglet(new AuthorTaglet());
      //registerTaglet(new SinceTaglet());
      registerTaglet(new StandardTaglet("deprecated"));
      registerTaglet(new StandardTaglet("see"));
      registerTaglet(new StandardTaglet("param"));

      // Set the built-in Taglet filter

      AuthorTaglet.setTagletEnabled(false);
      VersionTaglet.setTagletEnabled(false);
      SinceTaglet.setTagletEnabled(true);
      DeprecatedTaglet.setTagletEnabled(true);

      try {
	 {

	    // Process command line options passed through to this doclet
	    
	    TargetContext targetContext = null;
	    
	    TargetContext htmlTargetContext
               = new TargetContext(DocTranslet.fromClasspath("/doctranslets/html/gjdoc.xsl"), 
                                   targetDirectory);

	    for (int i=0, ilim=rootDoc.options().length; i<ilim; ++i) {

	       String[] option = rootDoc.options()[i];
	       String optionTag = option[0];

	       if ("-d".equals(optionTag)) {
		  if (null == targetDirectory) {
		     targetDirectory = new File(option[1]);
		  }
		  if (null != targetContext) {
		     targetContext.setTargetDirectory(targetDirectory);
		  }
	       }

	       else if ("-nofixhtml".equals(optionTag)) {
		  fixHTML = false;
                  printError("-nofixhtml currently not supported.");
                  return false;
	       }
	       else if ("-compress".equals(optionTag)) {
		  compress = true;
	       }
	       else if ("-nohtmlwarn".equals(optionTag)) {
		  noHTMLWarn = true;
	       }
	       else if ("-noemailwarn".equals(optionTag)) {
		  noEmailWarn = true;
	       }
	       else if ("-indentstep".equals(optionTag)) {
		  indentStep = Integer.parseInt(option[1]);
	       }
	       else if ("-doctranslet".equals(optionTag)) {
		  targets.add(targetContext = new TargetContext(DocTranslet.fromJarFile(new File(option[1])), 
                                                                targetDirectory));
	       }
	       else if ("-genhtml".equals(optionTag)) {
		  htmlTargetContext.setTargetDirectory(targetDirectory);
		  targets.add(targetContext = htmlTargetContext);
                  xmlOnly = false;
	       } 
	       else if ("-geninfo".equals(optionTag)) {
                  targetContext
                              = new TargetContext(DocTranslet.fromClasspath("/doctranslets/info/gengj.xsl"), 
                                                  targetDirectory);
		  targets.add(targetContext);
		  if (!fixHTML) {
		     printNotice("NOTE: -geninfo implies -fixhtml.");
		     fixHTML = true;
		  }
                  xmlOnly = false;
	       }
	       else if ("-gendocbook".equals(optionTag)) {
                  targetContext = new TargetContext(DocTranslet.fromClasspath("/doctranslets/docbook/gengj.xsl"), 
                                                    targetDirectory);
		  targets.add(targetContext);
		  if (!fixHTML) {
		     printNotice("NOTE: -gendocbook implies -fixhtml.");
		     fixHTML = true;
		  }
	       }
	       else if ("-genpdf".equals(optionTag)) {
                  targetContext
                     = new TargetContext(DocTranslet.fromClasspath("/doctranslets/docbook/gengj.xsl"), 
                                         targetDirectory);
                                         /** "gnu.classpath.tools.doclets.xmldoclet.DocBookPostprocessor") **/
		  targets.add(targetContext);
		  if (!fixHTML) {
		     printNotice("NOTE: -genpdf implies -fixhtml.");
		     fixHTML = true;
		  }
	       }
	       else if ("-xmlonly".equals(optionTag)) {
		  xmlOnly = true;
	       }
	       else if ("-bottomnote".equals(optionTag)) {

		  FileReader reader = new FileReader(option[1]);
		  StringWriter writer = new StringWriter();
		  char[] buf = new char[256];
		  int nread;
		  while ((nread = reader.read(buf)) >= 0) {
		     writer.write(buf, 0, nread);
		  }
		  writer.flush();
		  bottomNote = writer.toString();
		  writer.close();
		  reader.close();
	       }
	       else if ("-title".equals(optionTag)) {

		  title = option[1];
	       }
	       else if ("-workpath".equals(optionTag)) {

		  workingPath = option[1];
	       }
	       else if ("-tagletpath".equals(optionTag)) {

                  if (null == tagletPath) {
                     tagletPath = option[1];
                  }
                  else {
                     tagletPath = tagletPath + File.pathSeparator + option[1];
                  }
	       }
               else if ("-taglet".equals(optionTag)) {

                  boolean tagletLoaded = false;

                  String useTagletPath = this.tagletPath;
                  if (null == useTagletPath) {
                     useTagletPath = System.getProperty("java.class.path");
                  }

                  try {
                     Class tagletClass;
                     try {
                        tagletClass
                           = new FileSystemClassLoader(useTagletPath).loadClass(option[1]);
                     }
                     catch (ClassNotFoundException e) {
                        // If not found on specified tagletpath, try default classloader
                        tagletClass
                           = Class.forName(option[1]);
                     }
                     Method registerTagletMethod
                        = tagletClass.getDeclaredMethod("register", new Class[] { java.util.Map.class });

                     if (!registerTagletMethod.getReturnType().equals(Void.TYPE)) {
                        printError("Taglet class '" + option[1] + "' found, but register method doesn't return void.");
                     }
                     else if (registerTagletMethod.getExceptionTypes().length > 0) {
                        printError("Taglet class '" + option[1] + "' found, but register method contains throws clause.");
                     }
                     else if ((registerTagletMethod.getModifiers() & (Modifier.STATIC | Modifier.PUBLIC | Modifier.ABSTRACT)) != (Modifier.STATIC | Modifier.PUBLIC)) {
                        printError("Taglet class '" + option[1] + "' found, but register method isn't public static, or is abstract..");
                     }
                     else {
                        Map tempMap = new HashMap();
                        registerTagletMethod.invoke(null, new Object[] { tempMap });
                        tagletLoaded = true;
                        String name = (String)tempMap.keySet().iterator().next();
                        Taglet taglet = (Taglet)tempMap.get(name);
                        tagletMap.put(name, taglet);
                        mentionedTags.add(taglet);
                     }
                  }
                  catch (NoSuchMethodException e) {
                     printError("Taglet class '" + option[1] + "' found, but doesn't contain the register method.");
                  }
                  catch (SecurityException e) {
                     printError("Taglet class '" + option[1] + "' cannot be loaded: " + e.getMessage());
                  }
                  catch (InvocationTargetException e) {
                     printError("Taglet class '" + option[1] + "' found, but register method throws exception: " + e.toString());
                  }
                  catch (IllegalAccessException e) {
                     printError("Taglet class '" + option[1] + "' found, but there was a problem when accessing the register method: " + e.toString());
                  }
                  catch (IllegalArgumentException e) {
                     printError("Taglet class '" + option[1] + "' found, but there was a problem when accessing the register method: " + e.toString());
                  }
                  catch (ClassNotFoundException e) {
                     printError("Taglet class '" + option[1] + "' cannot be found.");
                  }
                  if (!tagletLoaded) {
                     return false;
                  }
               }
	       else if ("-author".equals(optionTag)) {
                  AuthorTaglet.setTagletEnabled(true);
               }
	       else if ("-version".equals(optionTag)) {
                  VersionTaglet.setTagletEnabled(true);
               }
	       else if ("-nosince".equals(optionTag)) {
                  SinceTaglet.setTagletEnabled(false);
               }
	       else if ("-nodeprecated".equals(optionTag)) {
                  DeprecatedTaglet.setTagletEnabled(false);
               }
	       else if ("-authormail".equals(optionTag)) {
                  
		  if ("no-replace".equalsIgnoreCase(option[1])) {
                     AuthorTaglet.setEmailReplacementType(AuthorTaglet.EmailReplacement.NO_REPLACEMENT);
                  }
		  else if ("mailto-name".equalsIgnoreCase(option[1])) {
                     AuthorTaglet.setEmailReplacementType(AuthorTaglet.EmailReplacement.MAILTO_NAME);
                  }
		  else if ("name-mailto-address".equalsIgnoreCase(option[1])) {
                     AuthorTaglet.setEmailReplacementType(AuthorTaglet.EmailReplacement.NAME_MAILTO_ADDRESS);
                  }
		  else if ("name-mangled-address".equalsIgnoreCase(option[1])) {
                     AuthorTaglet.setEmailReplacementType(AuthorTaglet.EmailReplacement.NAME_MANGLED_ADDRESS);
                  }
                  else {
                     printError("Invalid value for option '-authortag-email'. Allowed values are:"
                                + " no-replace, mailto-name, name-mailto-address, name-mangled-address.");
                     return false;
                  }
	       }
	       else if ("-mailmangledot".equals(optionTag)) {
                  AuthorTaglet.setDotReplacement(option[1]);
               }
	       else if ("-mailmangleat".equals(optionTag)) {
                  AuthorTaglet.setAtReplacement(option[1]);
               }
	       else if ("-docfilessubdirs".equals(optionTag)) {
                  docFilesSubdirsEnabled = true;
               }
	       else if ("-excludedocfilessubdir".equals(optionTag)) {
                  StringTokenizer st = new StringTokenizer(option[1]);
                  while (st.hasMoreTokens()) {
                     excludeDocFilesSubDirs.add(st.nextToken());
                  }
               }
	       else if ("-nonavbar".equals(optionTag)) {
                  docTransletOptions.nonavbar = true;
               }
	       else if ("-noindex".equals(optionTag)) {
                  docTransletOptions.noindex = true;
               }
	       else if ("-notree".equals(optionTag)) {
                  docTransletOptions.notree = true;
               }
	       else if ("-nocomment".equals(optionTag)) {
                  docTransletOptions.nocomment = true;
               }
	       else if ("-nohelp".equals(optionTag)) {
                  docTransletOptions.nohelp = true;
               }
	       else if ("-splitindex".equals(optionTag)) {
                  docTransletOptions.splitindex = true;
               }
	       else if ("-linksource".equals(optionTag)) {
                  docTransletOptions.linksource = true;
               }
	       else if ("-windowtitle".equals(optionTag)) {
                  docTransletOptions.windowtitle = option[1];
               }
	       else if ("-helpfile".equals(optionTag)) {
                  docTransletOptions.helpfile = new File(option[1]).toURL().toString();
               }
	       else if ("-stylesheetfile".equals(optionTag)) {
                  docTransletOptions.stylesheetfile = new File(option[1]).toURL().toString();
               }
	       else if ("-header".equals(optionTag)) {
                  docTransletOptions.header = option[1];
               }
	       else if ("-footer".equals(optionTag)) {
                  docTransletOptions.footer = option[1];
               }
	       else if ("-bottom".equals(optionTag)) {
                  docTransletOptions.bottom = option[1];
               }
	       else if ("-doctitle".equals(optionTag)) {
                  docTransletOptions.doctitle = option[1];
               }
	       else if ("-nodeprecatedlist".equals(optionTag)) {
                  docTransletOptions.nodeprecatedlist = true;
               }
	       else if ("-uses".equals(optionTag)) {
                  docTransletOptions.uses = true;
               }
	       else if ("-group".equals(optionTag)) {
                  if (!processGroupOption(option[1], option[2])) {
                     printError("Invalid package wildcard list in -group option \"" + option[1] + "\" " + option[2]);
                     return false;
                  }
               }
	       else if ("-tag".equals(optionTag)) {
                  String tagSpec = option[1];
                  boolean validTagSpec = false;
                  int ndx1 = tagSpec.indexOf(':');
                  if (ndx1 < 0) {
                     Taglet taglet = (Taglet)tagletMap.get(tagSpec);
                     if (null == taglet) {
                        printError("There is no standard tag '" + tagSpec + "'.");
                     }
                     else {
                        if (mentionedTags.contains(taglet)) {
                           printError("Tag '" + tagSpec + "' has been added or moved before.");
                        }
                        else {
                           mentionedTags.add(taglet);
                           
                           // re-append taglet
                           tagletMap.remove(tagSpec);
                           tagletMap.put(tagSpec, taglet);
                        }
                     }
                  }
                  else {
                     int ndx2 = tagSpec.indexOf(':', ndx1 + 1);
                     if (ndx2 > ndx1 && ndx2 < tagSpec.length() - 1) {
                        String tagName = tagSpec.substring(0, ndx1);
                        String tagHead = null;
                        if (tagSpec.charAt(ndx2 + 1) == '\"') {
                           if (tagSpec.charAt(tagSpec.length() - 1) == '\"') {
                              tagHead = tagSpec.substring(ndx2 + 2, tagSpec.length() - 1);
                              validTagSpec = true;
                           }
                        }
                        else {
                           tagHead = tagSpec.substring(ndx2 + 1);
                           validTagSpec = true;
                        }

                        boolean tagScopeOverview = false;
                        boolean tagScopePackages = false;
                        boolean tagScopeTypes = false;
                        boolean tagScopeConstructors = false;
                        boolean tagScopeMethods = false;
                        boolean tagScopeFields = false;
                        boolean tagDisabled = false;
                        
                     tag_option_loop:
                        for (int n=ndx1+1; n<ndx2; ++n) {
                           switch (tagSpec.charAt(n)) {
                           case 'X': 
                              tagDisabled = true;
                              break;
                           case 'a':
                              tagScopeOverview = true;
                              tagScopePackages = true;
                              tagScopeTypes = true;
                              tagScopeConstructors = true;
                              tagScopeMethods = true;
                              tagScopeFields = true;
                              break;
                           case 'o':
                              tagScopeOverview = true;
                              break;
                           case 'p':
                              tagScopePackages = true;
                              break;
                           case 't':
                              tagScopeTypes = true;
                              break;
                           case 'c':
                              tagScopeConstructors = true;
                              break;
                           case 'm':
                              tagScopeMethods = true;
                              break;
                           case 'f':
                              tagScopeFields = true;
                              break;
                           default:
                              validTagSpec = false;
                              break tag_option_loop;
                           }
                        }
                        
                        if (validTagSpec) {
                           GenericTaglet taglet
                              = new GenericTaglet(tagName,
                                                  tagHead,
                                                  tagScopeOverview,
                                                  tagScopePackages,
                                                  tagScopeTypes,
                                                  tagScopeConstructors,
                                                  tagScopeMethods,
                                                  tagScopeFields);
                           taglet.setTagletEnabled(!tagDisabled);
                           taglet.register(tagletMap);
                           mentionedTags.add(taglet);
                        }
                     }
                  }
                  if (!validTagSpec) {
                     printError("Value for option -tag must be in format \"<tagname>:Xaoptcmf:<taghead>\".");
                  }
               }
	    }

            // Use current directory if target directory hasn't been set.
            if (null == targetDirectory) {
               targetDirectory = new File(System.getProperty("user.dir"));
            }
            if (null != targetContext) {
               targetContext.setTargetDirectory(targetDirectory);
            }

	    // It is illegal to specify targets AND -xmlonly.

	    if (xmlOnly && targets.size() > 0) {

	       printError("You can only specify one of -xmlonly and a target format.");
	       return false;
	    }

	    // If no target was specified and XML only was not
	    // requested, use HTML as default target.

	    if (!xmlOnly && targets.size() == 0) {
	       targets.add(targetContext = htmlTargetContext);
	    }

	    // Set the same target directory for all output.

	    // FIXME: Allow separate target directories for different
	    // output formats.

	    for (Iterator it = targets.iterator(); it.hasNext(); ) {
	       TargetContext t = (TargetContext)it.next();
	       t.setTargetDirectory(targetDirectory);
	    }

	    // Create temporary directory if necessary

	    if (xmlOnly) {

	       xmlTargetDirectory = targetDirectory;
	    }
	    else {

	       File workingTopDirectory = new File(workingPath);

	       workingDirectory = new File(workingTopDirectory, "gjdoc.tmp."+System.currentTimeMillis());
	    
	       if (!workingDirectory.mkdir()) {
		  printError("Cannot create temporary directory at "+System.getProperty("java.io.tmpdir"));
		  return false;
	       }

	       File xmlTempDirectory = new File(workingDirectory, "xmloutput");

	       if (!xmlTempDirectory.mkdir()) {
		  printError("Cannot create temporary directory for XML output at "+System.getProperty("java.io.tmpdir"));
		  return false;
	       }

	       xmlTargetDirectory = xmlTempDirectory;
	    }

            // Create target directory if necessary

            if (!targetDirectory.exists()) {
               printNotice("Creating destination directory: \""
                           + targetDirectory + "\"");
               if (!targetDirectory.mkdirs()) {
                  printError("Failed to create destination directory \""
                             + targetDirectory + "\"");
                  return false;
               }
            }

            // Check for deprecation

            boolean hasDeprecatedClasses = false;
            boolean hasDeprecatedInterfaces = false;
            boolean hasDeprecatedExceptions = false;
            boolean hasDeprecatedErrors = false;
            boolean hasDeprecatedMethods = false;
            boolean hasDeprecatedFields = false;

            {
               ClassDoc[] classes = rootDoc.classes();
               for (int i = 0, ilim = classes.length; i < ilim; ++ i) {
                  ClassDoc c = classes[i];
                  Tag[] deprecatedTags = c.tags("deprecated");
                  if (null != deprecatedTags && 0 != deprecatedTags.length) {
                     if (c.isInterface()) {
                        hasDeprecatedInterfaces = true;
                     }
                     else if (c.isException()) {
                        hasDeprecatedExceptions = true;
                     }
                     else if (c.isError()) {
                        hasDeprecatedErrors = true;
                     }
                     else /*if (c.isOrdinaryClass())*/ {
                        hasDeprecatedClasses = true;
                     }
                  }
                  
                  MethodDoc[] methods = c.methods();
                  for (int j = 0, jlim = methods.length; j < jlim; ++ j) {
                     MethodDoc m = methods[j];
                     deprecatedTags = m.tags("deprecated");
                     if (null != deprecatedTags && 0 != deprecatedTags.length) {
                        hasDeprecatedMethods = true;
                     }
                  }
                  
                  FieldDoc[] fields = c.fields();
                  for (int j = 0, jlim = fields.length; j < jlim; ++ j) {
                     FieldDoc f = fields[j];
                     deprecatedTags = f.tags("deprecated");
                     if (null != deprecatedTags && 0 != deprecatedTags.length) {
                        hasDeprecatedFields = true;
                     }
                  }
               }
            }

            htmlRepairer = new HtmlRepairer(rootDoc, noHTMLWarn, noEmailWarn,
                                            currentClass, currentMember,
                                            false);

            collectUsage();

            // Begin XML generation

	    printNotice("Writing XML Index file...");

	    // Assign output stream

	    setTargetFile("index.xml");

	    // Output XML document header

	    println(0, "<?xml version=\"1.0\"?>");
	    println("<!DOCTYPE gjdoc SYSTEM \"dtd/gjdoc.dtd\">");
	    println();
	    printOpenTag(0, "rootdoc xmlns=\"http://www.w3.org/TR/REC-html40\" xmlns:gjdoc=\"http://www.gnu.org/software/cp-tools/gjdocxml\"");

	    println();
	    println(1, "<!-- Tags from overview page, if available -->");

            if (rootDoc.firstSentenceTags().length > 0) {
               printOpenTag(2, "firstSentenceTags", false);
               outputTags(3, rootDoc.firstSentenceTags(), true, CONTEXT_PACKAGE);
               printCloseTag(0, "firstSentenceTags");
            }

            if (rootDoc.inlineTags().length > 0) {
               printOpenTag(2, "inlineTags");
               outputTags(3, rootDoc.inlineTags(), true, CONTEXT_PACKAGE);
               printCloseTag(2, "inlineTags");
            }
	 
	    if (null != bottomNote) {
	       printOpenTag(1, "bottomnote");
	       print(bottomNote);
	       printCloseTag(1, "bottomnote");
	    }

	    if (null != title) {
	       printOpenTag(1, "title");
	       println(2, title);
	       printCloseTag(1, "title");
	    }
	 
	    printOpenTag(1, "created");
	    println(2, DateFormat.getDateInstance(DateFormat.LONG, Locale.US).format(new java.util.Date()));
	    printCloseTag(1, "created");

            if (hasDeprecatedClasses) printAtomTag(1, "hasDeprecatedClasses");
            if (hasDeprecatedInterfaces) printAtomTag(1, "hasDeprecatedInterfaces");
            if (hasDeprecatedExceptions) printAtomTag(1, "hasDeprecatedExceptions");
            if (hasDeprecatedErrors) printAtomTag(1, "hasDeprecatedErrors");
            if (hasDeprecatedMethods) printAtomTag(1, "hasDeprecatedMethods");
            if (hasDeprecatedFields) printAtomTag(1, "hasDeprecatedFields");

	    // Output summary of all classes specified on command line

	    println();
	    println(1, "<!-- Classes specified by user on command line -->");
	    ClassDoc[] specifiedClasses = rootDoc.specifiedClasses();
	    for (int i=0, ilim=specifiedClasses.length; i<ilim; ++i) {
	       ClassDoc sc = specifiedClasses[i];
	       printAtomTag(1, "specifiedclass fqname=\""+sc.qualifiedName()+"\" name=\""+sc.name()+"\"");
	    }
	    specifiedClasses = null;

	    // Output summary of all packages specified on command line

	    println();
	    println(1, "<!-- Packages specified by user on command line -->");
	    PackageDoc[] specifiedPackages = rootDoc.specifiedPackages();
	    for (int i=0, ilim=specifiedPackages.length; i<ilim; ++i) {
	       PackageDoc sp = specifiedPackages[i];
	       printAtomTag(1, "specifiedpackage name=\""+sp.name()+"\"");
	    }
	    specifiedPackages = null;

	    // Output package group information specified on the
	    // command line

	    println();
	    println(1, "<!-- Package groups specified by user on command line -->");
            {
               Iterator packageGroupIt = packageGroups.iterator();
               while (packageGroupIt.hasNext()) {
                  PackageGroup packageGroup = (PackageGroup)packageGroupIt.next();
                  SortedSet groupedPackages = packageGroup.getPackages();
                  if (groupedPackages.isEmpty()) {
                     printWarning("Package group named '" 
                                  + packageGroup.getName() + "' didn't match any packages.");
                  }
                  else {
                     printOpenTag(1, "packagegroup name=\"" + packageGroup.getName() + "\"");
                     Iterator groupedPackageIt = groupedPackages.iterator();
                     while (groupedPackageIt.hasNext()) {
                        PackageDoc groupedPackageDoc = (PackageDoc)groupedPackageIt.next();
                        printAtomTag(2, "package name=\"" + groupedPackageDoc.name() + "\"");
                     }
                     printCloseTag(1, "packagegroup");
                  }
               }
               packageGroups = null;
            }

	    // Output information on all packages for which documentation
	    // has been made available via the Doclet API

	    println();
	    println(1, "<!-- Documentation for all packages -->");
	    PackageDoc[] packages = rootDoc.specifiedPackages();
	    for (int i=0, ilim=packages.length; i<ilim; ++i) {
	       PackageDoc c = packages[i];
	       outputPackageDoc(c);
	    }
	    packages = null;

	    // Output brief summary on all classes for which documentation
	    // has been made available via the Doclet API.
	    //
	    // While this is redundant, it can speed up XSLT
	    // processing by orders of magnitude

	    println();
	    println(1, "<!-- Brief summary for all classes -->");
	    ClassDoc[] sumclasses = rootDoc.classes();
	    for (int i=0, ilim=sumclasses.length; i<ilim; ++i) {
	       ClassDoc c = sumclasses[i];
	       outputClassDocSummary(c);
	    }
	    sumclasses = null;
	    
	    // Output closing tag, finish output stream

	    println();
	    printCloseTag(0, "rootdoc");

	    closeTargetFile();

            createIndexByName();



	    // Output information on all classes for which documentation
	    // has been made available via the Doclet API
	    
	    println();
	    println(1, "<!-- Documentation for all classes -->");
	    ClassDoc[] classes = rootDoc.classes();
            String prevPackageName = null;
	    for (int i = 0, ilim = classes.length; i < ilim; ++ i) {
	       ClassDoc c = classes[i];

               if (isVerbose()) {
                  printNotice("Writing XML information for "+c.qualifiedName()+"...");
               }
               else {
                  String packageName = c.containingPackage().name();
                  if (null == prevPackageName || !packageName.equals(prevPackageName)) {
                     printNotice("Writing XML information for "+packageName+"...");
                     prevPackageName = packageName;
                  }
               }
	       
	       setTargetFile(c.qualifiedName().replace('/','.')+".xml");
	       
	       println("<?xml version=\"1.0\"?>");
               println("<!DOCTYPE gjdoc SYSTEM \"dtd/gjdoc.dtd\">");
	       
	       outputClassDoc(c);
	       
	       closeTargetFile();
	    }
	    classes = null;
	 }
	 
         // Copy DTD files to temporary directory
         
         // FIXME: try to solve this via jar: URLs. but this will
         // probably break libxmlj compatibility (?)
         
         String[] resources = new String[] {
            "gjdoc.dtd",
            "gjdoc-alphaindex.dtd",
            "dbcentx.mod",
            "ent/iso-amsa.ent",
            "ent/iso-amsb.ent",
            "ent/iso-amsc.ent",
            "ent/iso-amsn.ent",
            "ent/iso-amso.ent",
            "ent/iso-amsr.ent",
            "ent/iso-box.ent",
            "ent/iso-cyr1.ent",
            "ent/iso-cyr2.ent",
            "ent/iso-dia.ent",
            "ent/iso-grk1.ent",
            "ent/iso-grk2.ent",
            "ent/iso-grk3.ent",
            "ent/iso-grk4.ent",
            "ent/iso-lat1.ent",
            "ent/iso-lat2.ent",
            "ent/iso-num.ent",
            "ent/iso-pub.ent",
            "ent/iso-tech.ent",
         };

         File tempDtdDirectory = new File(xmlTargetDirectory, "dtd");
         File tempDtdEntDirectory = new File(tempDtdDirectory, "ent");

         if ((tempDtdDirectory.exists() || tempDtdDirectory.mkdir())
             && (tempDtdEntDirectory.exists() || tempDtdEntDirectory.mkdir())) {
            for (int i = 0; i < resources.length; ++ i) {
               copyResourceToFile("/dtd/" + resources[i], 
                                  new File(tempDtdDirectory, resources[i]));
            }
         }
         else {
            printError("Cannot create temporary directories for DTD data at " + tempDtdDirectory);
            return false;
         }

         // Copy package data-dir directory

         {
            PackageDoc[] packages = rootDoc.specifiedPackages();
            for (int i=0, ilim=packages.length; i<ilim; ++i) {
               PackageDoc c = packages[i];
               if (c instanceof GjdocPackageDoc) {
                  copyPackageDataDir((GjdocPackageDoc)c);
               }
            }
         }

	 // All information has been output. Apply stylesheet if given.

	 gnu.classpath.tools.gjdoc.Main.releaseRootDoc();
         
	 this.currentClass = null;
	 this.currentMember = null;
	 this.currentExecMember = null;
	 
	 System.gc();	 

	 // From this point we are only operating on files, so we don't
	 // need this anymore and can free up some memory

         for (Iterator it = targets.iterator(); it.hasNext(); ) {

            TargetContext target = (TargetContext)it.next();

	    // We have XSLT postprocessing, run DocTranslet.

            //DocTranslet docTranslet = DocTranslet.fromClasspath("/doctranslets/html/gjdoc.xsl");
            
            //docTranslet.setOptions(docTransletOptions);

            target.getDocTranslet().setOptions(docTransletOptions);

            target.getDocTranslet().apply(xmlTargetDirectory, 
                                          target.getTargetDirectory(), 
                                          rootDoc);
	 }

	 // Done

	 targets = null;

	 System.gc();
         Runtime.getRuntime().runFinalization();

	 return true;
      }
      catch (Exception e) {

	 // Something went wrong. Report to stderr and pass error to
	 // Javadoc Reporter

	 e.printStackTrace();
	 printError(e.toString());

         Throwable rootCause = e.getCause();
         if (null != rootCause) {
            while (null != rootCause.getCause()) {
               rootCause = rootCause.getCause();
            }
            System.err.println("Root cause:");
            rootCause.printStackTrace();
         }

	 return false;
      }
      finally {

	 // In any case, delete the working directory if we created one

	 if (null != workingDirectory) {

	    if (!deleteRecursive(workingDirectory)) {
	       printWarning("Could not delete temporary directory at "+workingDirectory);
	    }
	 }

	 printNotice("Done.");
      }
   }

   /**
    * Recursively delete the specified directory and its contents,
    * like <code>rm -Rf directory</code>
    *
    * @return <code>true</code> on success
    */
   private static boolean deleteRecursive(File directory) {

      boolean success = true;

      File[] files = directory.listFiles();

      for (int i=0, ilim=files.length; i<ilim; ++i) {

	 File file = files[i];

	 if (file.isDirectory()) {

	    success = deleteRecursive(file) && success;
	 }
	 else {

	    success = file.delete() && success;
	 }
      }

      return directory.delete() && success;
   }

   /**
    *  Prints a string to stdout and appends a newline.  Convenience
    *  method.  
    */
   protected void println(String str) {
      out.println(str);
   }

   /**
    *  Prints a string to stdout without appending a newline.
    *  Convenience method.  
    */
   protected void print(String str) {
      out.print(str);
   }

   /**
    *  In standard mode, prints an empty line to stdout.
    *  In thight mode, nothing happens.
    *  Convenience method.  
    */
   protected void println() {
      if (!compress) {
	 out.println();
      }
   }

   /**
    *  In standard mode, prints the given text indented to stdout and appends newline. 
    *  In tight mode, doesn't print indentation or newlines.
    */
   protected void print(int indentLevel, String msg) {
      if (compress) {
	 out.print(msg);
      }
      else {
	 StringBuffer indentation = new StringBuffer();
	 for (int i=0; i<indentLevel*indentStep; ++i) {
	    indentation.append(' ');
	 }
	 out.print(indentation+msg);
      }
   }
   
   /**
    *  In tight mode, prints a message at a given indentation level.
    *  In standard mode, appends a newline in addition.
    */
   protected void println(int indentLevel, String msg) {
      print(indentLevel, msg);
      if (!compress) out.println();
   }

   /**
    *  Prints an atom tag at the given indentation level.
    */
   protected void printAtomTag(int level, String tag) {
      println(level, "<"+tagPrefix+":"+replaceCharsInTag(tag)+"/>");
   }

   /**
    *  Prints an open tag at the given indentation level.
    */
   protected void printOpenTag(int level, String tag) {
      printOpenTag(level, replaceCharsInTag(tag), true);
   }

   /**
    *  Prints an open tag at the given indentation level and
    *  conditionally appends a newline (if not in tight mode).
    */
   protected void printOpenTag(int level, String tag, boolean appendNewline) {
      if (appendNewline && !compress) {
	 println(level, "<"+tagPrefix+":"+replaceCharsInTag(tag)+">");
      }
      else {
	 print(level, "<"+tagPrefix+":"+replaceCharsInTag(tag)+">");
      }
   }

   /**
    *  Prints a close tag at the given indentation level.
    */
   protected void printCloseTag(int level, String tag) {
      printCloseTag(level, tag, true);
   }

   /**
    *  Prints a close tag at the given indentation level and
    *  conditionally appends a newline (if not in tight mode).
    */
   protected void printCloseTag(int level, String tag, boolean appendNewline) {
      if (appendNewline && !compress) {
	 println(level, "</"+tagPrefix+":"+replaceCharsInTag(tag)+">");
      }
      else {
	 print(level, "</"+tagPrefix+":"+replaceCharsInTag(tag)+">");
      }
   }

   public static int optionLength(String option) {
      if ("-d".equals(option)) return 2;
      else if ("-fixhtml".equals(option)) return 1;
      else if ("-compress".equals(option)) return 1;
      else if ("-nohtmlwarn".equals(option)) return 1;
      else if ("-noemailwarn".equals(option)) return 1;
      else if ("-indentstep".equals(option)) return 2;
      else if ("-xslsheet".equals(option)) return 2;
      else if ("-xsltdriver".equals(option)) return 2;
      else if ("-postprocess".equals(option)) return 2;
      else if ("-genhtml".equals(option)) return 1;
      else if ("-geninfo".equals(option)) return 1;
      else if ("-gendocbook".equals(option)) return 1;
      else if ("-xmlonly".equals(option)) return 1;
      else if ("-bottomnote".equals(option)) return 2;
      else if ("-workpath".equals(option)) return 2;
      else if ("-title".equals(option)) return 2;
      else if ("-tagletpath".equals(option)) return 2;
      else if ("-taglet".equals(option)) return 2;
      else if ("-authormail".equals(option)) return 2;
      else if ("-mailmangledot".equals(option)) return 2;
      else if ("-mailmangleat".equals(option)) return 2;
      else if ("-noindex".equals(option)) return 1;
      else if ("-nocomment".equals(option)) return 1;
      else if ("-notree".equals(option)) return 1;
      else if ("-nohelp".equals(option)) return 1;
      else if ("-nonavbar".equals(option)) return 1;
      else if ("-splitindex".equals(option)) return 1;
      else if ("-author".equals(option)) return 1;
      else if ("-version".equals(option)) return 1;
      else if ("-nosince".equals(option)) return 1;
      else if ("-nodeprecated".equals(option)) return 1;
      else if ("-linksource".equals(option)) return 1;
      else if ("-windowtitle".equals(option)) return 2;
      else if ("-helpfile".equals(option)) return 2;
      else if ("-stylesheetfile".equals(option)) return 2;
      else if ("-tag".equals(option)) return 2;
      else if ("-header".equals(option)) return 2;
      else if ("-footer".equals(option)) return 2;
      else if ("-bottom".equals(option)) return 2;
      else if ("-doctitle".equals(option)) return 2;
      else if ("-nodeprecatedlist".equals(option)) return 1;
      else if ("-uses".equals(option)) return 1;
      else if ("-group".equals(option)) return 3;

      else return -1;
   }

   public static boolean validOptions(String[][] options) {
      return true;
   }


   /**
    *  Workaround for non well-formed comments: fix tag contents
    *  by replacing <code>&lt;</code> with <code>&amp;lt;</code>,
    *  <code>&gt;</code> with <code>&amp;gt;</code> and
    *  <code>&amp;</code> with <code>&amp;amp;</code>.
    *
    *  @param tagContent  String to process
    *
    *  @return given String with all special characters replaced by 
    *          HTML entities.
    */
   private static String replaceCharsInTag(String tagContent) {
      return 
	 replaceString(
	    replaceString(
	       replaceString(
		  tagContent, 
		  "<", "&lt;"
		  ), 
	       ">", "&gt;"
	       ),
	    "&", "&amp;"
	    );
   }

   /**
    *  Replaces all occurences of string <code>needle</code> within string
    *  <code>haystack</code> by string <code>replacement</code>.
    *
    *  @param haystack    The string to search and replace in.
    *  @param needle      The string which is searched for.
    *  @param replacement The string by which every occurence of <code>needle</code> is replaced.
    */
   private static String replaceString(String haystack, String needle, String replacement) {
      int ndx = haystack.indexOf(needle);
      if (ndx<0)
	 return haystack;
      else
	 return haystack.substring(0, ndx) + replacement 
	    + replaceString(haystack.substring(ndx+needle.length()), needle, replacement);
   }

   protected void setTargetFile(String filename) throws IOException {

      OutputStream fileOut = new FileOutputStream(new File(xmlTargetDirectory, filename));
      out = new PrintWriter(new BufferedWriter(new OutputStreamWriter(fileOut, "UTF8")));;
   }

   protected void closeTargetFile() {

      out.flush();
      out.close();
   }

   private String cdata(String str) {

      if (null==str) {
	 return str;
      } // end of if ((null==str)

      StringBuffer rc = new StringBuffer();
      for (int i=0; i<str.length(); ++i) {
	 char c = str.charAt(i);
	 if (c==0x09 || c==0x0a || c==0x0d || (c>=0x20 && c<=0xd7ff) || (c>=0xe000 && c<=0xfffd) || (c>=0x10000 && c<=0x10ffff)) {
	    rc.append(c);
	 }
	 else {
	    printWarning("Invalid Unicode character 0x"+Integer.toString(c, 16)+" in javadoc markup has been stripped.");
	 } // end of else
	 
      }
      return rc.toString();
   }

   static void copyResourceToFile(String resourceName, File target) throws IOException {
      
      InputStream in = Driver.class.getResourceAsStream(resourceName);

      if (null != in) {

	 FileOutputStream out = new FileOutputStream(target);
	 int size;
	 byte[] buffer = new byte[512];
	 while ((size = in.read(buffer)) >= 0) {
	    out.write(buffer, 0, size);
	 }
	 out.close();
      }
      else {

	 throw new IOException("Can't find resource named "+resourceName);
      }
   }

   private void printError(String error) {
      if (null != rootDoc) {
	 rootDoc.printError(error);
      }
      else {
	 System.err.println("ERROR: "+error);
      }
   }

   private void printWarning(String warning) {
      if (null != rootDoc) {
	 rootDoc.printWarning(warning);
      }
      else {
	 System.err.println("WARNING: "+warning);
      }
   }

   private void printNotice(String notice) {
      if (null != rootDoc) {
	 rootDoc.printNotice(notice);
      }
      else {
	 System.err.println(notice);
      }
   }

   /**
    *  Copy the contents of the input directory to the output
    *  directory. The output directory must exist.
    */
   private void copyPackageDataDir(GjdocPackageDoc packageDoc) throws IOException {
      File docFilesSourceDirectory
         = new File(packageDoc.packageDirectory(), "doc-files");
      File docFilesTargetDirectory
         = new File(this.targetDirectory, 
                    packageDoc.name().replace('.', File.separatorChar));
      if (docFilesSourceDirectory.exists()) {
         printNotice("Copying files from " + docFilesSourceDirectory);
         copyDirectory(docFilesSourceDirectory, docFilesTargetDirectory,
                       docFilesSubdirsEnabled,
                       excludeDocFilesSubDirs);
      }
   }

   /**
    *  Recursively copy the contents of the input directory to the
    *  output directory. The output directory must exist.
    */
   private static void copyDirectory(File sourceDir, File targetDir, 
                                     boolean recursive,
                                     Set excludeDirs) throws IOException {
      if (!targetDir.exists() && !targetDir.mkdirs()) {
         throw new IOException("Cannot create directory " + targetDir);
      }

      File[] sourceFiles = sourceDir.listFiles();
      for (int i=0; i<sourceFiles.length; ++i) {
         if (sourceFiles[i].isDirectory()) {
            if (recursive && (null == excludeDirs 
                              || !excludeDirs.contains(sourceFiles[i].getName()))) {
               File targetSubDir = new File(targetDir, 
                                            sourceFiles[i].getName());
               if (targetSubDir.exists() || targetSubDir.mkdir()) {
                  copyDirectory(sourceFiles[i], targetSubDir, recursive, null);
               }
               else {
                  throw new IOException("Cannot create directory " + targetSubDir);
               }
            }
         }
         else {
            copyFile(sourceFiles[i], new File(targetDir, sourceFiles[i].getName()));
         }
      }
   }

   /**
    *  Copy the contents of the input file to the output file. The
    *  output file's parent directory must exist.
    */
   private static void copyFile(File sourceFile, File targetFile) throws IOException {

      InputStream in = new FileInputStream(sourceFile);
      OutputStream out = new FileOutputStream(targetFile);
      int nread;
      byte[] buf = new byte[512];
      while ((nread = in.read(buf)) >= 0) {
         out.write(buf, 0, nread);
      }
      in.close();
      out.close();
   }

   private void createIndexByName() throws IOException {
      // Create index

      // Collect index
            
      Map indexMap = new TreeMap(new Comparator() {
            public int compare(Object o1, Object o2) {
               return o1.toString().toLowerCase().compareTo(o2.toString().toLowerCase());
            }
         });

      // Add packages to index

      PackageDoc[] packages = rootDoc.specifiedPackages();
      for (int i=0, ilim=packages.length; i<ilim; ++i) {
         PackageDoc c = packages[i];
         indexMap.put(c.name(), c);
      }

      // Add classes, fields and methods to index

      ClassDoc[] sumclasses = rootDoc.classes();
      for (int i=0, ilim=sumclasses.length; i<ilim; ++i) {
         ClassDoc c = sumclasses[i];
         if (null == c.containingClass()) {
            indexMap.put(c.name(), c);
         }
         else {
            indexMap.put(c.name().substring(c.containingClass().name().length() + 1), c);
         }
         FieldDoc[] fields = c.fields();
         for (int j=0, jlim=fields.length; j<jlim; ++j) {
            indexMap.put(fields[j].name(), fields[j]);
         }
         MethodDoc[] methods = c.methods();
         for (int j=0, jlim=methods.length; j<jlim; ++j) {
            MethodDoc method = methods[j];
            StringBuffer signature = new StringBuffer();
            signature.append(method.name());
            signature.append('(');
            Parameter[] parameters = method.parameters();
            for (int k=0, klim=parameters.length; k<klim; ++k) {
               if (k > 0) {
                  signature.append(", ");
               }
               signature.append(parameters[k].typeName());
            }
            signature.append(')');
            indexMap.put(signature.toString(), method);
         }
      }

      // Assign output stream

      setTargetFile("alphaindex.xml");

      // Output XML document header

      println(0, "<?xml version=\"1.0\"?>");
      println("<!DOCTYPE gjdoc SYSTEM \"dtd/gjdoc-alphaindex.dtd\">");
      println();
      printOpenTag(0, "alphaindex xmlns=\"http://www.w3.org/TR/REC-html40\" xmlns:gjdoc=\"http://www.gnu.org/software/cp-tools/gjdocxml\"");
      
      Iterator it = indexMap.keySet().iterator();

      char previousCategoryLetter = '\0';
      boolean categoryOpen = false;

      while (it.hasNext()) {
         String key = (String)it.next();
         Doc entry = (Doc)indexMap.get(key);

         char firstChar = Character.toUpperCase(key.charAt(0));
         if (firstChar != previousCategoryLetter) {
            if (categoryOpen) {
               printCloseTag(1, "category");
            }
            printOpenTag(1, "category letter=\"" + firstChar + "\"");
            categoryOpen = true;
            previousCategoryLetter = firstChar;
         }

         printOpenTag(2, "entry name=\"" + key + "\"");
         if (entry instanceof PackageDoc) {
            printAtomTag(3, "isPackage");
         }
         else if (entry instanceof ClassDoc) {
            printAtomTag(3, "isClass");
            ClassDoc centry = (ClassDoc)entry;
            currentClass = centry;
            printAtomTag(3, "containingPackage name=\"" + centry.containingPackage().name() + "\"");
            if (null != centry.containingClass()) {
               printAtomTag(3, "containingClass name=\"" + centry.containingClass().name() + "\"");
            }
            if (centry.isInterface()) {
               printAtomTag(3, "isInterface");
            }
            if (centry.isException()) {
               printAtomTag(3, "isException");
            }
            if (centry.isError()) {
               printAtomTag(3, "isError");
            }
            if (centry.isOrdinaryClass()) {
               printAtomTag(3, "isOrdinaryClass");
            }
         }
         else if (entry instanceof ProgramElementDoc) {
            ProgramElementDoc pentry = (ProgramElementDoc)entry;
            currentClass = pentry.containingClass();
            printAtomTag(3, "containingPackage name=\"" + pentry.containingPackage().name() + "\"");
            printAtomTag(3, "containingClass name=\"" + pentry.containingClass().name() + "\"");
            if (pentry.isMethod()) {
               printAtomTag(3, "isMethod");
               ExecutableMemberDoc mentry = (ExecutableMemberDoc)pentry;
               printAtomTag(3, "signature full=\""+mentry.signature()+"\" flat=\""+mentry.flatSignature()+"\"");
               printAtomTag(3, "method name=\"" + mentry.name() + "\"");
            }
            if (pentry.isField()) {
               printAtomTag(3, "isField");
            }
         }

         Tag[] tags = entry.firstSentenceTags();
         for (int i=0, ilim=tags.length; i<ilim; ++i) {
            Tag tag = tags[i];
            if (tag.firstSentenceTags().length>0) {
               printOpenTag(3, "firstSentenceTags", false);
               outputTags(4, tag.firstSentenceTags(), false, CONTEXT_TYPE);
               printCloseTag(3, "firstSentenceTags");
            }
         }


         printCloseTag(2, "entry");
      }

      if (categoryOpen) {
         printCloseTag(1, "category");
      }

      printCloseTag(0, "alphaindex");

      closeTargetFile();
   }

   private static class UsageType
   {
      public static final UsageType CLASS_DERIVED_FROM = new UsageType("class-derived-from");
      public static final UsageType FIELD_OF_TYPE = new UsageType("field-of-type");
      public static final UsageType METHOD_WITH_RETURN_TYPE = new UsageType("method-with-return-type");
      public static final UsageType METHOD_WITH_PARAMETER_TYPE = new UsageType("method-with-parameter-type");
      public static final UsageType METHOD_WITH_THROWN_TYPE = new UsageType("method-with-thrown-type");
      public static final UsageType CONSTRUCTOR_WITH_PARAMETER_TYPE = new UsageType("constructor-with-parameter-type");
      public static final UsageType CONSTRUCTOR_WITH_THROWN_TYPE = new UsageType("constructor-with-thrown-type");
      private String id;

      private UsageType(String id)
      {
         this.id = id;
      }

      public String toString() { 
         return "UsageType{id=" + id + "}"; 
      }

      public String getId() {
         return id;
      }
   }

   /**
    *  ClassDoc -> (PackageDoc -> (UsageType -> (Set of Doc)))
    */
   private Map usedClassToPackagesMap = new HashMap();

   private void addUsedBy(ClassDoc usedClass, UsageType usageType, Doc user, PackageDoc userPackage)
   {
      Map packageToUsageTypeMap = (Map)usedClassToPackagesMap.get(usedClass);
      if (null == packageToUsageTypeMap) {
         packageToUsageTypeMap = new HashMap();
         usedClassToPackagesMap.put(usedClass, packageToUsageTypeMap);
      }

      Map usageTypeToUsersMap = (Map)packageToUsageTypeMap.get(userPackage);
      if (null == usageTypeToUsersMap) {
         usageTypeToUsersMap = new HashMap();
         packageToUsageTypeMap.put(userPackage, usageTypeToUsersMap);
      }

      Set userSet = (Set)usageTypeToUsersMap.get(usageType);
      if (null == userSet) {
         userSet = new TreeSet(); // FIXME: we need the collator from Main here
         usageTypeToUsersMap.put(usageType, userSet);
      }
      userSet.add(user);
   }

   /**
    *  Create the cross reference database.
    */
   private void collectUsage() {

      ClassDoc[] classes = rootDoc.classes();
      for (int i = 0, ilim = classes.length; i < ilim; ++ i) {
         ClassDoc clazz = classes[i];

         // classes derived from
         for (ClassDoc superclass = clazz.superclass(); superclass != null; 
              superclass = superclass.superclass()) {
            addUsedBy(superclass, UsageType.CLASS_DERIVED_FROM, clazz, clazz.containingPackage());
         }

         FieldDoc[] fields = clazz.fields();
         for (int j = 0, jlim = fields.length; j < jlim; ++ j) {
            FieldDoc field = fields[j];

            // fields of type                  
            ClassDoc fieldType = field.type().asClassDoc();
            if (null != fieldType) {
               addUsedBy(fieldType, UsageType.FIELD_OF_TYPE, 
                         field, clazz.containingPackage());
            }
         }

         MethodDoc[] methods = clazz.methods();
         for (int j = 0, jlim = methods.length; j < jlim; ++ j) {
            MethodDoc method = methods[j];

            // methods with return type

            ClassDoc returnType = method.returnType().asClassDoc();
            if (null != returnType) {
               addUsedBy(returnType, UsageType.METHOD_WITH_RETURN_TYPE, 
                         method, clazz.containingPackage());
            }
            Parameter[] parameters = method.parameters();
            for (int k=0; k<parameters.length; ++k) {

               // methods with parameter type

               Parameter parameter = parameters[k];
               ClassDoc parameterType = parameter.type().asClassDoc();
               if (null != parameterType) {
                  addUsedBy(parameterType, UsageType.METHOD_WITH_PARAMETER_TYPE, 
                            method, clazz.containingPackage());
               }
            }

            // methods which throw

            ClassDoc[] thrownExceptions = method.thrownExceptions();
            for (int k = 0, klim = thrownExceptions.length; k < klim; ++ k) {
               ClassDoc thrownException = thrownExceptions[k];
               addUsedBy(thrownException, UsageType.METHOD_WITH_THROWN_TYPE, 
                         method, clazz.containingPackage());
            }
         }
                  
         ConstructorDoc[] constructors = clazz.constructors();
         for (int j = 0, jlim = constructors.length; j < jlim; ++ j) {

            ConstructorDoc constructor = constructors[j];

            Parameter[] parameters = constructor.parameters();
            for (int k = 0, klim = parameters.length; k < klim; ++ k) {

               // constructors with parameter type
                     
               Parameter parameter = parameters[k];
               ClassDoc parameterType = parameter.type().asClassDoc();
               if (null != parameterType) {
                  addUsedBy(parameterType, UsageType.CONSTRUCTOR_WITH_PARAMETER_TYPE, 
                            constructor, clazz.containingPackage());
               }
            }

            // constructors which throw

            ClassDoc[] thrownExceptions = constructor.thrownExceptions();
            for (int k = 0, klim = thrownExceptions.length; k < klim; ++ k) {
               ClassDoc thrownException = thrownExceptions[k];
               addUsedBy(thrownException, UsageType.CONSTRUCTOR_WITH_THROWN_TYPE, 
                         constructor, clazz.containingPackage());
            }
         }
      }
   }

   private void outputUsage(ClassDoc clazz, int level) {

      Map packageToUsageTypeMap = (Map)usedClassToPackagesMap.get(clazz);
      if (null != packageToUsageTypeMap) {
         printOpenTag(level, "references");

         Iterator packagesIterator = packageToUsageTypeMap.keySet().iterator();

         while (packagesIterator.hasNext()) {
            PackageDoc packageDoc = (PackageDoc)packagesIterator.next();
            printOpenTag(level + 1, "referencing-package name=\"" + packageDoc.name() + "\"");
            Map usageTypeToUsersMap = (Map)packageToUsageTypeMap.get(packageDoc);
            Iterator usageTypeIterator = usageTypeToUsersMap.keySet().iterator();
            while (usageTypeIterator.hasNext()) {
               UsageType usageType = (UsageType)usageTypeIterator.next();
               printOpenTag(level + 2, "usage-type id=\"" + usageType.getId() + "\"");
               Set users = (Set)usageTypeToUsersMap.get(usageType);
               Iterator userIterator = users.iterator();
               while (userIterator.hasNext()) {
                  Doc user = (Doc)userIterator.next();
                  if (user instanceof ClassDoc) {
                     printAtomTag(level + 3, "user"
                                  + " class=\"" + ((ClassDoc)user).name() + "\"");
                  }
                  else if (user instanceof FieldDoc) {
                     FieldDoc fieldDoc = (FieldDoc)user;
                     printAtomTag(level + 3, "user"
                                  + " class=\"" + fieldDoc.containingClass().name() + "\""
                                  + " field=\"" + fieldDoc.name() + "\"");
                  }
                  else if (user instanceof MethodDoc) {
                     MethodDoc methodDoc = (MethodDoc)user;
                     printAtomTag(level + 3, "user"
                                  + " class=\"" + methodDoc.containingClass().name() + "\""
                                  + " method=\"" + methodDoc.name() + "\""
                                  + " signature=\"" + methodDoc.signature() + "\""
                                  + " flatSignature=\"" + methodDoc.flatSignature() + "\"");
                  }
                  else if (user instanceof ConstructorDoc) {
                     ConstructorDoc constructorDoc = (ConstructorDoc)user;
                     printAtomTag(level + 3, "user"
                                  + " class=\"" + constructorDoc.containingClass().name() + "\""
                                  + " signature=\"" + constructorDoc.signature() + "\""
                                  + " flatSignature=\"" + constructorDoc.flatSignature() + "\"");
                  }
               }
               printCloseTag(level +2, "usage-type");
            }
            printCloseTag(level + 1, "referencing-package");
         }

         printCloseTag(level, "references");
      }
   }

   private boolean processGroupOption(String groupName, String colonSeparatedPackageList)
   {
      try {
         PackageMatcher packageMatcher = new PackageMatcher();

         StringTokenizer tokenizer = new StringTokenizer(colonSeparatedPackageList, ":");
         while (tokenizer.hasMoreTokens()) {
            String packageWildcard = tokenizer.nextToken();
            packageMatcher.addWildcard(packageWildcard);
         }

         SortedSet groupPackages = packageMatcher.filter(rootDoc.specifiedPackages());

         packageGroups.add(new PackageGroup(groupName, groupPackages));

         return true;
      }
      catch (InvalidPackageWildcardException e) {
         return false;
      }
   }

   private void registerTaglet(Taglet taglet)
   {
      tagletMap.put(taglet.getName(), taglet);
   }

   private boolean isVerbose()
   {
      return false;
   }
}
