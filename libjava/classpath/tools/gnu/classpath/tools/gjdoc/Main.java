/* gnu.classpath.tools.gjdoc.Main
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
import java.io.*;
import java.util.*;
import java.lang.reflect.*;
import java.text.Collator;

import gnu.classpath.tools.FileSystemClassLoader;

/**
 * Class that will launch the gjdoc tool.
 */
public final class Main
{

  /**
   * Do we load classes that are referenced as base class?
   */
  static final boolean DESCEND_SUPERCLASS = true;

  /**
   * Do we load classes that are referenced as interface?
   */
  static final boolean DESCEND_INTERFACES = false;

  /**
   * Do we load classes that are imported in a source file?
   */
  static final boolean DESCEND_IMPORTED = true;

  /**
   * Document only public members.
   */
  static final int COVERAGE_PUBLIC = 0;

  /**
   * Document only public and protected members.
   */
  static final int COVERAGE_PROTECTED = 1;

  /**
   * Document public, protected and package private members.
   */
  static final int COVERAGE_PACKAGE = 2;

  /**
   * Document all members.
   */
  static final int COVERAGE_PRIVATE = 3;

  /*
   *  FIXME: This should come from a ResourceBundle
   */
  private static final String STRING_TRY_GJDOC_HELP = 
     "Try `gjdoc --help' for more information.";

  /**
   * Grid for looking up whether a particular access level is included in the
   * documentation.
   */
  static final boolean[][] coverageTemplates = new boolean[][]
    { new boolean[]
      { true, false, false, false }, // public
        new boolean[]
          { true, true, false, false }, // protected
        new boolean[]
          { true, true, true, false }, // package
        new boolean[]
          { true, true, true, true }, // private
    };

  /**
   * Holds the Singleton instance of this class.
   */
  private static Main instance = new Main();

  /**
   * Avoid re-instantiation of this class.
   */
  private Main()
  {
  }

  private static RootDocImpl rootDoc;

  private ErrorReporter reporter;

  /**
   * Cache for version string from resource /version.properties
   */
  private String gjdocVersion;

  /**
   * <code>false</code> during Phase I: preparation of the documentation data.
   * <code>true</code> during Phase II: documentation output by doclet.
   */
  boolean docletRunning = false;

  //---- Command line options

  /**
   * Option "-doclet": name of the Doclet class to use.
   */
  private String option_doclet = "gnu.classpath.tools.doclets.htmldoclet.HtmlDoclet";

  /**
   * Option "-overview": path to the special overview file.
   */
  private String option_overview;

  /**
   * Option "-coverage": which members to include in generated documentation.
   */
  private int option_coverage = COVERAGE_PROTECTED;

  /**
   * Option "-help": display command line usage.
   */
  private boolean option_help;

  /**
   * Option "-docletpath": path to doclet classes.
   */
  private String option_docletpath;

  /**
   * Option "-classpath": path to additional classes.
   */
  private String option_classpath;

  /**
   * Option "-sourcepath": path to the Java source files to be documented.
   * FIXME: this should be a list of paths
   */
  private List option_sourcepath = new ArrayList();

  /**
   * Option "-extdirs": path to Java extension files.
   */
  private String option_extdirs;

  /**
   * Option "-verbose": Be verbose when generating documentation.
   */
  private boolean option_verbose;

  /**
   * Option "-nowarn": Do not print warnings.
   */
  private boolean option_nowarn;

  /**
   * Option "-locale:" Specify the locale charset of Java source files.
   */
  private Locale option_locale = new Locale("en", "us");

  /**
   * Option "-encoding": Specify character encoding of Java source files.
   */
  private String option_encoding;

  /**
   * Option "-J": Specify flags to be passed to Java runtime.
   */
  private List option_java_flags = new LinkedList(); //ArrayList();

  /**
   * Option "-source:" should be 1.4 to handle assertions, 1.1 is no
   * longer supported.
   */
  private String option_source = "1.2";

  /**
   * Option "-subpackages": list of subpackages to be recursively
   * added.
   */
  private List option_subpackages = new ArrayList();

  /**
   * Option "-exclude": list of subpackages to exclude.
   */
  private List option_exclude = new ArrayList();

  /**
   * Option "-breakiterator" - whether to use BreakIterator for
   * detecting the end of the first sentence.
   */
  private boolean option_breakiterator;

  /**
   * Option "-licensetext" - whether to copy license text.
   */
  private boolean option_licensetext;

  /**
   * The locale-dependent collator used for sorting.
   */
  private Collator collator;

  /**
   * true when --version has been specified on the command line.
   */
  private boolean option_showVersion;
  
  /**
   * true when -bootclasspath has been specified on the command line.
   */
  private boolean option_bootclasspath_specified;
  
  /**
   * true when -all has been specified on the command line.
   */
  private boolean option_all;

  /**
   * true when -reflection has been specified on the command line.
   */
  private boolean option_reflection;
  
  // TODO: add the rest of the options as instance variables
  
  /**
   * Parse all source files/packages and subsequentially start the Doclet given
   * on the command line.
   * 
   * @param allOptions List of all command line tokens
   */
  private boolean startDoclet(List allOptions)
  {

    try
    {

      //--- Fetch the Class object for the Doclet.

      Debug.log(1, "loading doclet class...");

      Class docletClass;

      if (null != option_docletpath) {
        try {
          FileSystemClassLoader docletPathClassLoader
            = new FileSystemClassLoader(option_docletpath);
          System.err.println("trying to load class  " + option_doclet + " from path " + option_docletpath);
          docletClass = docletPathClassLoader.findClass(option_doclet);
        }
        catch (Exception e) {
          docletClass = Class.forName(option_doclet);
        }
      }
      else {
        docletClass = Class.forName(option_doclet);
      }
      //Object docletInstance = docletClass.newInstance();

      Debug.log(1, "doclet class loaded...");

      Method startTempMethod = null;
      Method startMethod = null;
      Method optionLenMethod = null;
      Method validOptionsMethod = null;

      //--- Try to find the optionLength method in the Doclet class.

      try
      {
        optionLenMethod = docletClass.getMethod("optionLength", new Class[]
          { String.class });
      }
      catch (NoSuchMethodException e)
      {
        // Ignore if not found; it's OK it the Doclet class doesn't define
        // this method.
      }

      //--- Try to find the validOptions method in the Doclet class.

      try
      {
        validOptionsMethod = docletClass.getMethod("validOptions", new Class[]
          { String[][].class, DocErrorReporter.class });
      }
      catch (NoSuchMethodException e)
      {
        // Ignore if not found; it's OK it the Doclet class doesn't define
        // this method.
      }

      //--- Find the start method in the Doclet class; complain if not found

      try
      {
        startTempMethod = docletClass.getMethod("start", new Class[]
          { TemporaryStore.class });
      }
      catch (Exception e)
      {
        // ignore
      }
      startMethod = docletClass.getMethod("start", new Class[]
        { RootDoc.class });

      //--- Feed the custom command line tokens to the Doclet

      // stores all recognized options
      List options = new LinkedList();

      // stores packages and classes defined on the command line
      List packageAndClasses = new LinkedList();

      for (Iterator it = allOptions.iterator(); it.hasNext();)
      {
        String option = (String) it.next();

        Debug.log(9, "parsing option '" + option + "'");

        if (option.startsWith("-"))
        {

          //--- Parse option

          int optlen = optionLength(option);

          //--- Try to get option length from Doclet class

          if (optlen <= 0 && optionLenMethod != null)
          {

            optionLenMethod.invoke(null, new Object[]
              { option });

            Debug.log(3, "invoking optionLen method");

            optlen = ((Integer) optionLenMethod.invoke(null, new Object[]
              { option })).intValue();

            Debug.log(3, "done");
          }

          if (optlen <= 0) {

            if (option.startsWith("-JD")) {
              // Simulate VM option -D
              String propertyValue = option.substring(3);
              int ndx = propertyValue.indexOf('=');
              if (ndx <= 0) {
                reporter.printError("Illegal format in option " + option + ": use -JDproperty=value");
                return false;
              }
              else {
                String property = propertyValue.substring(0, ndx);
                String value = propertyValue.substring(ndx + 1);
                System.setProperty(property, value);
              }
            }
            else if (option.startsWith("-J")) {
              //--- Warn if VM option is encountered
              reporter.printWarning("Ignored option " + option + ". Pass this option to the VM if required.");
            }
            else {
              //--- Complain if not found

              reporter.printError("Unknown option " + option);
              reporter.printNotice(STRING_TRY_GJDOC_HELP);
              return false;
            }
          }
          else
          {

            //--- Read option values

            String[] optionAndValues = new String[optlen];
            optionAndValues[0] = option;
            for (int i = 1; i < optlen; ++i)
            {
              if (!it.hasNext())
              {
                reporter.printError("Missing value for option " + option);
                return false;
              }
              else
              {
                optionAndValues[i] = (String) it.next();
              }
            }

            //--- Store option for processing later

            options.add(optionAndValues);
          }
        }
        else if (option.length() > 0)
        {

          //--- Add to list of packages/classes if not option or option
          // value

          packageAndClasses.add(option);
        }
      }

      Debug.log(9, "options parsed...");

      //--- For each package specified with the -subpackages option on
      //         the command line, recursively find all valid java files
      //         beneath it.

      //--- For each class or package specified on the command line,
      //         check that it exists and find out whether it is a class
      //         or a package

      for (Iterator it = option_subpackages.iterator(); it.hasNext();)
      {
        String subpackage = (String) it.next();
        Set foundPackages = new LinkedHashSet();

        for (Iterator pit = option_sourcepath.iterator(); pit.hasNext(); ) {
          File sourceDir = (File)pit.next();
          File packageDir = new File(sourceDir, subpackage.replace('.', File.separatorChar));
          findPackages(subpackage, packageDir, foundPackages);
        }

        addFoundPackages(subpackage, foundPackages);
      }

      if (option_all) {
        Set foundPackages = new LinkedHashSet();
        for (Iterator pit = option_sourcepath.iterator(); pit.hasNext(); ) {
          File sourceDir = (File)pit.next();
          findPackages("", sourceDir, foundPackages);
        }
        addFoundPackages(null, foundPackages);
        for (Iterator packageIt = foundPackages.iterator(); packageIt.hasNext(); ) {
          String packageName = (String)packageIt.next();
          if (null == packageName) {
            packageName = "";
          }
          rootDoc.addSpecifiedPackageName(packageName);
        }
      }

      for (Iterator it = packageAndClasses.iterator(); it.hasNext();)
      {

        String classOrPackage = (String) it.next();

        boolean foundSourceFile = false;

        if (classOrPackage.endsWith(".java")) {
          for (Iterator pit = option_sourcepath.iterator(); pit.hasNext() && !foundSourceFile; ) {
            File sourceDir = (File)pit.next();
            File sourceFile = new File(sourceDir, classOrPackage);
            if (sourceFile.exists() && !sourceFile.isDirectory()) {
              rootDoc.addSpecifiedSourceFile(sourceFile);
              foundSourceFile = true;
              break;
            }
          }
          if (!foundSourceFile) {
            File sourceFile = new File(classOrPackage);
            if (sourceFile.exists() && !sourceFile.isDirectory()) {
              rootDoc.addSpecifiedSourceFile(sourceFile);
              foundSourceFile = true;
            } 
          }
        }

        if (!foundSourceFile) {
        //--- Check for illegal name

        if (classOrPackage.startsWith(".")
            || classOrPackage.endsWith(".")
            || classOrPackage.indexOf("..") > 0
            || !checkCharSet(classOrPackage,
                "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ1234567890_."))
        {
          throw new ParseException("Illegal class or package name '"
              + classOrPackage + "'");
        }

        //--- Assemble absolute path to package

        String classOrPackageRelPath = classOrPackage.replace('.',
            File.separatorChar);

        //--- Create one file object each for a possible package directory
        //         and a possible class file, and find out if they exist.

        List packageDirs = rootDoc.findSourceFiles(classOrPackageRelPath);
        List sourceFiles = rootDoc.findSourceFiles(classOrPackageRelPath + ".java");

        boolean packageDirExists = !packageDirs.isEmpty();
        boolean sourceFileExists = !sourceFiles.isEmpty();

        //--- Complain if neither exists: not found

        if (!packageDirExists && !sourceFileExists)
        {
          reporter.printError("Class or package " + classOrPackage
              + " not found.");
          return false;
        }

        //--- Complain if both exist: ambigious

        else
          if (packageDirExists && sourceFileExists)
          {
            reporter.printError("Ambigious class/package name "
                + classOrPackage + ".");
            return false;
          }

          //--- Otherwise, if the package directory exists, it is a package

          else
            if (packageDirExists) {
              Iterator packageDirIt = packageDirs.iterator();
              boolean packageDirFound = false;
              while (packageDirIt.hasNext()) {
                File packageDir = (File)packageDirIt.next();
                if (packageDir.isDirectory()) {
                  rootDoc.addSpecifiedPackageName(classOrPackage);
                  packageDirFound = true;
                  break;
                }
              }
              if (!packageDirFound) {
                reporter.printError("No suitable file or directory found for" + classOrPackage);
                return false;
              }
            }

            //--- Otherwise, emit error message

            else {
                reporter.printError("No sources files found for package " + classOrPackage);
            }
        }
      }

      //--- Complain if no packages or classes specified

      if (option_help) {
        usage();
        return true;
      }

      //--- Validate custom options passed on command line
      //         by asking the Doclet if they are OK.

      String[][] customOptionArr = (String[][]) options
          .toArray(new String[0][0]);
      if (validOptionsMethod != null
          && !((Boolean) validOptionsMethod.invoke(null, new Object[]
            { customOptionArr, reporter })).booleanValue())
      {
        // Not ok: shutdown system.
        reporter.printNotice(STRING_TRY_GJDOC_HELP);
        return false;
      }

      if (!rootDoc.hasSpecifiedPackagesOrClasses()) {
        reporter.printError("No packages or classes specified.");
        reporter.printNotice(STRING_TRY_GJDOC_HELP);
        return false;
      }

      rootDoc.setOptions(customOptionArr);

      rootDoc.build();

      //--- Bail out if no classes found

      if (0 == rootDoc.classes().length
          && 0 == rootDoc.specifiedPackages().length
          && 0 == rootDoc.specifiedClasses().length)
      {
        reporter.printError("No packages or classes found(!).");
        return false;
      }

      //--- Our work is done, tidy up memory

      System.gc();
      System.gc();

      //--- Set flag indicating Phase II of documentation generation

      docletRunning = true;

      //--- Invoke the start method on the Doclet: produce output

      reporter.printNotice("Running doclet...");

      TemporaryStore tstore = new TemporaryStore(Main.rootDoc);

      Thread.currentThread().setContextClassLoader(docletClass.getClassLoader());

      if (null != startTempMethod)
      {
        startTempMethod.invoke(null, new Object[]
          { tstore });
      }
      else
      {
        startMethod.invoke(null, new Object[]
          { tstore.getAndClear() });
      }

      //--- Let the user know how many warnings/errors occured

      if (reporter.getWarningCount() > 0)
      {
        reporter.printNotice(reporter.getWarningCount() + " warnings");
      }

      if (reporter.getErrorCount() > 0)
      {
        reporter.printNotice(reporter.getErrorCount() + " errors");
      }

      System.gc();

      //--- Done.
      return true;
    }
    catch (Exception e)
    {
      e.printStackTrace();
      return false;
    }
  }

  private void addFoundPackages(String subpackage, Set foundPackages)
  {        
    if (foundPackages.isEmpty()) {
      reporter.printWarning("No classes found under subpackage " + subpackage);
    }
    else {
      boolean onePackageAdded = false;
      for (Iterator rit = foundPackages.iterator(); rit.hasNext();) {
        String foundPackage = (String)rit.next();
        boolean excludeThisPackage = false;

        for (Iterator eit = option_exclude.iterator(); eit.hasNext();) {
          String excludePackage = (String)eit.next();
          if (foundPackage.equals(excludePackage) ||
              foundPackage.startsWith(excludePackage + ":")) {
            excludeThisPackage = true;
            break;
          }
        }

        if (!excludeThisPackage) {
          rootDoc.addSpecifiedPackageName(foundPackage);
          onePackageAdded = true;
        }
      }
      if (!onePackageAdded) {
        if (null != subpackage) {
          reporter.printWarning("No non-excluded classes found under subpackage " + subpackage);
        }
        else {
          reporter.printWarning("No non-excluded classes found.");
        }
      }
    }
  }

  /**
   *  Verify that the given file is a valid Java source file and that
   *  it specifies the given package.
   */
  private boolean isValidJavaFile(File file,
                                  String expectedPackage)
  {
    try {
      InputStream in = new BufferedInputStream(new FileInputStream(file));

      int ch, prevChar = 0;

      final int STATE_DEFAULT = 0;
      final int STATE_COMMENT = 1;
      final int STATE_LINE_COMMENT = 2;

      int state = STATE_DEFAULT;

      StringBuffer word = new StringBuffer();
      int wordIndex = 0;

      while ((ch = in.read()) >= 0) {
        String completeWord = null;

        switch (state) {
        case STATE_COMMENT:
          if (prevChar == '*' && ch == '/') {
            state = STATE_DEFAULT;
          }
          break;

        case STATE_LINE_COMMENT:
          if (ch == '\n') {
            state = STATE_DEFAULT;
          }
          break;

        case STATE_DEFAULT:
          if (prevChar == '/' && ch == '*') {
            word.deleteCharAt(word.length() - 1);
            if (word.length() > 0) {
              completeWord = word.toString();
              word.setLength(0);
            }
            state = STATE_COMMENT;
          }
          else if (prevChar == '/' && ch == '/') {
            word.deleteCharAt(word.length() - 1);
            if (word.length() > 0) {
              completeWord = word.toString();
              word.setLength(0);
            }
            state = STATE_LINE_COMMENT;
          }
          else if (" \t\r\n".indexOf(ch) >= 0) {
            if (word.length() > 0) {
              completeWord = word.toString();
              word.setLength(0);
            }
          }
          else if (1 == wordIndex && ';' == ch) {
            if (word.length() > 0) {
              completeWord = word.toString();
              word.setLength(0);
            }
            else {
              // empty package name in source file: "package ;" -> invalid source file
              in.close();
              return false;
            }
          }
          else {
            word.append((char)ch);
          }
          break;
        }

        if (null != completeWord) {
          if (0 == wordIndex && !"package".equals(completeWord)) {
            in.close();
            return "".equals(expectedPackage);
          }
          else if (1 == wordIndex) {
            in.close();
            return expectedPackage.equals(completeWord);
          }
          ++ wordIndex;
        }

        prevChar = ch;
      }

      // no package or class found before end-of-file -> invalid source file

      in.close();
      return false;
    }
    catch (IOException e) {
      reporter.printWarning("Could not examine file " + file + ": " + e);
      return false;
    }
  }

  /**
   *  Recursively try to locate valid Java packages under the given
   *  package specified by its name and its directory. Add the names
   *  of all valid packages to the result list.
   */
  private void findPackages(String subpackage, 
                            File packageDir, 
                            Set result)
  {
    File[] files = packageDir.listFiles();
    if (null != files) {
      for (int i=0; i<files.length; ++i) {
        File file = files[i];
        if (!file.isDirectory() && file.getName().endsWith(".java")) {
          if (isValidJavaFile(file, subpackage)) {
            if ("".equals(subpackage)) {
              result.add(null);
            }
            else {
              result.add(subpackage);
            }
            break;
          }
        }
      }
      for (int i=0; i<files.length; ++i) {
        File file = files[i];
        if (file.isDirectory()) {
          String newSubpackage;
          if (null != subpackage && subpackage.length() > 0) {
            newSubpackage = subpackage + "." + file.getName();
          }
          else {
            newSubpackage = file.getName();
          }
          findPackages(newSubpackage, file, result);
        }
      }
    }
  }

  /**
   *
   */
  private static boolean validOptions(String options[][],
      DocErrorReporter reporter)
  {

    boolean foundDocletOption = false;
    for (int i = 0; i < options.length; i++)
    {
      String[] opt = options[i];
      if (opt[0].equalsIgnoreCase("-doclet"))
      {
        if (foundDocletOption)
        {
          reporter.printError("Only one -doclet option allowed.");
          return false;
        }
        else
        {
          foundDocletOption = true;
        }
      }
    }

    return true;
  }

  /**
   * Main entry point. This is the method called when gjdoc is invoked from the
   * command line.
   * 
   * @param args
   *          command line arguments
   */
  public static void main(String[] args)
  {

    try
    {
      //--- Remember current time for profiling purposes

      Timer.setStartTime();

      //--- Handle control to the Singleton instance of this class

      int result = instance.start(args);

      if (result < 0) {
        // fatal error
        System.exit(5);
      }
      else if (result > 0) {
        // errors encountered
        System.exit(1);
      }
      else {
        // success
        System.exit(0);
      }
    }
    catch (Exception e)
    {
      //--- unexpected error
      e.printStackTrace();
      System.exit(1);
    }
  }

  /**
   * Parses command line arguments and subsequentially handles control to the
   * startDoclet() method
   *   
   * @param args The command line parameters.
   */
   public static int execute(String[] args)
   {
     try
     {
       int result = instance.start(args);
       if (result < 0) {
         // fatal error
         return 5;
       }
       else if (result > 0) {
         // errors encountered
         return 1;
       }
       else {
         // success
         return 0;
       }
     }
     catch (Exception e)
     {
       // unexpected error
       return 1;
     }
   }

  /**
   * @param programName Name of the program (for error messages). *disregarded*
   * @param args The command line parameters.
   * @returns The return code.
   */
  public static int execute(String programName,
                            String[] args)
  {
    return execute(args);
  }
 
  /**
   * @param programName Name of the program (for error messages).
   * @param defaultDocletClassName Fully qualified class name.
   * @param args The command line parameters.
   * @returns The return code.
   *//*
  public static int execute(String programName,
                            String defaultDocletClassName,
                            String[] args)
  {
    // not yet implemented
  }*/
 
  /**
   * @param programName Name of the program (for error messages).
   * @param defaultDocletClassName Fully qualified class name.
   * @param args The command line parameters.
   * @returns The return code.
   *//*
  public static int execute(String programName,
                            String defaultDocletClassName,
                            String[] args)
  {
    // not yet implemented
  }*/
 
  /**
   * @param programName Name of the program (for error messages).
   * @param errWriter PrintWriter to receive error messages.
   * @param warnWriter PrintWriter to receive error messages.
   * @param noticeWriter PrintWriter to receive error messages.
   * @param defaultDocletClassName Fully qualified class name.
   * @param args The command line parameters.
   * @returns The return code.
   *//*
  public static int execute(String programName,
                            PrintWriter errWriter,
                            PrintWriter warnWriter,
                            PrintWriter noticeWriter,
                            String defaultDocletClassName,
                            String[] args)
  {
    // not yet implemented
  }*/

  /**
   * Parses command line arguments and subsequentially handles control to the
   * startDoclet() method
   * 
   * @param args
   *          Command line arguments, as passed to the main() method
   * @return {@code -1} in case of a fatal error (invalid arguments),
   * or the number of errors encountered.
   * @exception ParseException
   *              FIXME
   * @exception IOException
   *              if an IO problem occur
   */
  public int start(String[] args) throws ParseException, IOException
  {

    //--- Collect unparsed arguments in array and resolve references
    //         to external argument files.

    List arguments = new ArrayList(args.length);

    for (int i = 0; i < args.length; ++i)
    {
      if (!args[i].startsWith("@"))
      {
        arguments.add(args[i]);
      }
      else
      {
        FileReader reader = new FileReader(args[i].substring(1));
        StreamTokenizer st = new StreamTokenizer(reader);
        st.resetSyntax();
        st.wordChars('\u0000', '\uffff');
        st.quoteChar('\"');
        st.quoteChar('\'');
        st.whitespaceChars(' ', ' ');
        st.whitespaceChars('\t', '\t');
        st.whitespaceChars('\r', '\r');
        st.whitespaceChars('\n', '\n');
        while (st.nextToken() != StreamTokenizer.TT_EOF)
        {
          arguments.add(st.sval);
        }
      }
    }

    //--- Initialize Map for option parsing

    initOptions();

    //--- This will hold all options recognized by gjdoc itself
    //         and their associated arguments.
    //         Contains objects of type String[], where each entry
    //         specifies an option along with its aguments.

    List options = new LinkedList();

    //--- This will hold all command line tokens not recognized
    //         to be part of a standard option.
    //         These options are intended to be processed by the doclet
    //         Contains objects of type String, where each entry is
    //         one unrecognized token.

    List customOptions = new LinkedList();

    rootDoc = new RootDocImpl();
    reporter = rootDoc.getReporter();

    //--- Iterate over all options given on the command line

    for (Iterator it = arguments.iterator(); it.hasNext();)
    {

      String arg = (String) it.next();

      //--- Check if gjdoc recognizes this option as a standard option
      //         and remember the options' argument count

      int optlen = optionLength(arg);

      //--- Argument count == 0 indicates that the option is not recognized.
      //         Add it to the list of custom option tokens

      //--- Otherwise the option is recognized as a standard option.
      //         if all required arguments are supplied. Create a new String
      //         array for the option and its arguments, and store it
      //         in the options array.

      if (optlen > 0)
      {
        String[] option = new String[optlen];
        option[0] = arg;
        boolean optargs_ok = true;
        for (int j = 1; j < optlen && optargs_ok; ++j)
        {
          if (it.hasNext())
          {
            option[j] = (String) it.next();
            if (option[j].startsWith("-"))
            {
              optargs_ok = false;
            }
          }
          else
          {
            optargs_ok = false;
          }
        }
        if (optargs_ok)
          options.add(option);
        else
        {
          //         If the option requires more arguments than given on the
          //         command line, issue a fatal error

          reporter.printFatal("Missing value for option " + arg + ".");
        }
      }
    }

    //--- Create an array of String arrays from the dynamic array built above

    String[][] optionArr = (String[][]) options.toArray(new String[options
        .size()][0]);

    //--- Validate all options and issue warnings/errors

    if (validOptions(optionArr, rootDoc))
    {

      //--- We got valid options; parse them and store the parsed values
      //         in 'option_*' fields.

      readOptions(optionArr);

      //--- Show version and exit if requested by user

      if (option_showVersion) {
        System.out.println("gjdoc " + getGjdocVersion());
        System.exit(0);
      }

      if (option_bootclasspath_specified) {
        reporter.printWarning("-bootclasspath ignored: not supported by"
                              + " gjdoc wrapper script, or no wrapper script in use.");
      }

      // If we have an empty source path list, add the current directory ('.')

      if (option_sourcepath.size() == 0)
        option_sourcepath.add(new File("."));

      //--- We have all information we need to start the doclet at this time
    
      if (null != option_encoding) {
        rootDoc.setSourceEncoding(option_encoding);
      }
      else {
        // be quiet about this for now:
        // reporter.printNotice("No encoding specified, using platform default: " + System.getProperty("file.encoding"));
        rootDoc.setSourceEncoding(System.getProperty("file.encoding"));
      }
      rootDoc.setSourcePath(option_sourcepath);

      //addJavaLangClasses();

      if (!startDoclet(arguments)) {
        return -1;
      }
    }

    return reporter.getErrorCount();
  }

  private void addJavaLangClasses()
    throws IOException
  {
    String resourceName = "/java.lang-classes-" + option_source + ".txt";
    InputStream in = getClass().getResourceAsStream(resourceName);
    BufferedReader reader = new BufferedReader(new InputStreamReader(in));
    String line;
    while ((line = reader.readLine()) != null) {
      
      String className = line.trim();
      if (className.length() > 0) {
        ClassDocImpl classDoc =
          new ClassDocImpl(null, new PackageDocImpl("java.lang"),
                           ProgramElementDocImpl.ACCESS_PUBLIC,
                           false, false, null);
        classDoc.setClass(className);
        rootDoc.addClassDoc(classDoc);
      }
    }
  }

  /**
   * Helper class for parsing command line arguments. An instance of this class
   * represents a particular option accepted by gjdoc (e.g. '-sourcepath') along
   * with the number of expected arguments and behavior to parse the arguments.
   */
  private abstract class OptionProcessor
  {

    /**
     * Number of arguments expected by this option.
     */
    private int argCount;

    /**
     * Initializes this instance.
     * 
     * @param argCount
     *          number of arguments
     */
    public OptionProcessor(int argCount)
    {
      this.argCount = argCount;
    }

    /**
     * Overridden by derived classes with behavior to parse the arguments
     * specified with this option.
     * 
     * @param args
     *          command line arguments
     */
    abstract void process(String[] args);
  }

  /**
   * Maps option tags (e.g. '-sourcepath') to OptionProcessor objects.
   * Initialized only once by method initOptions(). FIXME: Rename to
   * 'optionProcessors'.
   */
  private static Map options = null;

  /**
   * Initialize all OptionProcessor objects needed to scan/parse command line
   * options. This cannot be done in a static initializer block because
   * OptionProcessors need access to the Singleton instance of the Main class.
   */
  private void initOptions()
  {

    options = new HashMap();

    //--- Put one OptionProcessor object into the map
    //         for each option recognized.

    options.put("-overview", new OptionProcessor(2)
      {

        void process(String[] args)
        {
          option_overview = args[0];
        }
      });
    options.put("-public", new OptionProcessor(1)
      {

        void process(String[] args)
        {
          option_coverage = COVERAGE_PUBLIC;
        }
      });
    options.put("-protected", new OptionProcessor(1)
      {

        void process(String[] args)
        {
          option_coverage = COVERAGE_PROTECTED;
        }
      });
    options.put("-package", new OptionProcessor(1)
      {

        void process(String[] args)
        {
          option_coverage = COVERAGE_PACKAGE;
        }
      });
    options.put("-private", new OptionProcessor(1)
      {

        void process(String[] args)
        {
          option_coverage = COVERAGE_PRIVATE;
        }
      });
    OptionProcessor helpProcessor = new OptionProcessor(1)
      {

        void process(String[] args)
        {
          option_help = true;
        }
      };

    options.put("-help", helpProcessor);
    options.put("--help", helpProcessor);
    options.put("-doclet", new OptionProcessor(2)
        {

          void process(String[] args)
          {
            option_doclet = args[0];
          }
        });
    options.put("-docletpath", new OptionProcessor(2)
        {

          void process(String[] args)
          {
            option_docletpath = args[0];
          }
        });
    options.put("-nowarn", new OptionProcessor(1)
        {

          void process(String[] args)
          {
            option_nowarn = true;
          }
        });
    options.put("-source", new OptionProcessor(2)
        {

          void process(String[] args)
          {
            option_source = args[0];
            if (!"1.2".equals(option_source) 
                && !"1.3".equals(option_source)
                && !"1.4".equals(option_source)) {

              throw new RuntimeException("Only he following values are currently"
                                         + " supported for option -source: 1.2, 1.3, 1.4.");
            }
          }
        });
    OptionProcessor sourcePathProcessor = new OptionProcessor(2) {
        void process(String[] args)
        {
          Debug.log(1, "-sourcepath is '" + args[0] + "'");
          for (StringTokenizer st = new StringTokenizer(args[0],
              File.pathSeparator); st.hasMoreTokens();)
          {
            String path = st.nextToken();
            File file = new File(path);
            if (!(file.exists()))
            {
              throw new RuntimeException("The source path " + path
                  + " does not exist.");
            }
            option_sourcepath.add(file);
          }
        }
      };
    options.put("-s", sourcePathProcessor);
    options.put("-sourcepath", sourcePathProcessor);
    options.put("-subpackages", new OptionProcessor(2)
      {
        void process(String[] args)
        {
          StringTokenizer st = new StringTokenizer(args[0], ":"); 
          while (st.hasMoreTokens()) {
            String packageName = st.nextToken();

            if (packageName.startsWith(".")
                || packageName.endsWith(".")
                || packageName.indexOf("..") > 0
                || !checkCharSet(packageName,
                                 "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ1234567890_.")) {
              throw new RuntimeException("Illegal package name '"
                                         + packageName + "'");
            }
            option_subpackages.add(packageName);
          }
        }
      });
    options.put("-exclude", new OptionProcessor(2)
      {
        void process(String[] args)
        {
          StringTokenizer st = new StringTokenizer(args[0], ":"); 
          while (st.hasMoreTokens()) {
            String packageName = st.nextToken();

            if (packageName.startsWith(".")
                || packageName.endsWith(".")
                || packageName.indexOf("..") > 0
                || !checkCharSet(packageName,
                                 "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ1234567890_.")) {
              throw new RuntimeException("Illegal package name '"
                                         + packageName + "'");
            }
            option_exclude.add(packageName);
          }
        }
      });
    // TODO include other options here
    options.put("-verbose", new OptionProcessor(1)
      {

        void process(String[] args)
        {
          option_verbose = true;
          System.err.println("WARNING: Unsupported option -verbose ignored");
        }
      });
    options.put("-quiet", new OptionProcessor(1)
      {

        void process(String[] args)
        {
          reporter.setQuiet(true);
        }
      });
    options.put("-locale", new OptionProcessor(2)
      {

        void process(String[] args)
        {
          String localeName = args[0];
          String language = null;
          String country = null;
          String variant = null;
          StringTokenizer st = new StringTokenizer(localeName, "_");
          if (st.hasMoreTokens()) {
            language = st.nextToken();
          }
          if (st.hasMoreTokens()) {
            country = st.nextToken();
          }
          if (st.hasMoreTokens()) {
            variant = st.nextToken();
          }
          if (variant != null) {
            option_locale = new Locale(language, country, variant);
          }
          else if (country != null) {
             option_locale = new Locale(language, country);
          }
          else if (language != null) {
             option_locale = new Locale(language);
          }
          else {
              throw new RuntimeException("Illegal locale specification '"
                                         + localeName + "'");
          }
        }
      });
    options.put("-encoding", new OptionProcessor(2)
      {

        void process(String[] args)
        {
          option_encoding = args[0];
        }
      });
    options.put("-breakiterator", new OptionProcessor(1)
      {
        void process(String[] args)
        {
          option_breakiterator = true;
        }
      });
    options.put("-licensetext", new OptionProcessor(1)
      {
        void process(String[] args)
        {
          option_licensetext = true;
        }
      });
    options.put("-overview", new OptionProcessor(2)
      {
        void process(String[] args)
        {
          try {
            getRootDoc().setRawCommentText(RootDocImpl.readHtmlBody(new File(args[0])));
          }
          catch (IOException e) { 
            throw new RuntimeException("Cannot read file specified in option -overview: " + e.getMessage());
          }
        }
      });
    options.put("-classpath", new OptionProcessor(2)
      {
        void process(String[] args)
        {
          reporter.printWarning("-classpath option could not be passed to the VM.  Faking it with ");
          reporter.printWarning("    System.setProperty(\"java.class.path\", \"" + args[0] + "\");");
          System.setProperty("java.class.path", args[0]);
        }
      });
    options.put("--version", new OptionProcessor(1)
      {
        void process(String[] args)
        {
          option_showVersion = true;
        }
      });
    options.put("-bootclasspath", new OptionProcessor(1)
      {
        void process(String[] args)
        {
          option_bootclasspath_specified = true;
        }
      });
    options.put("-all", new OptionProcessor(1)
      {
        void process(String[] args)
        {
          option_all = true;
        }
      });
    options.put("-reflection", new OptionProcessor(1)
      {
        void process(String[] args)
        {
          option_reflection = true;
        }
      });
  }

  /**
   * Determine how many arguments the given option requires.
   * 
   * @param option
   *          The name of the option without leading dash.
   */
  private static int optionLength(String option)
  {

    OptionProcessor op = (OptionProcessor) options.get(option.toLowerCase());
    if (op != null)
      return op.argCount;
    else
      return 0;
  }

  /**
   * Process all given options. Assumes that the options have been validated
   * before.
   * 
   * @param optionArr
   *          Each element is a series of Strings where [0] is the name of the
   *          option and [1..n] are the arguments to the option.
   */
  private void readOptions(String[][] optionArr)
  {

    //--- For each option, find the appropriate OptionProcessor
    //        and call its process() method

    for (int i = 0; i < optionArr.length; ++i)
    {
      String[] opt = optionArr[i];
      String[] args = new String[opt.length - 1];
      System.arraycopy(opt, 1, args, 0, opt.length - 1);
      OptionProcessor op = (OptionProcessor) options.get(opt[0].toLowerCase());
      op.process(args);
    }
  }

  /**
   * Print command line usage.
   */
  private static void usage()
  {
    System.out
        .print("\n"
            + "USAGE: gjdoc [options] [packagenames] "
            + "[sourcefiles] [@files]\n\n"
            + "  --version                Show version information and exit\n"
            + "  -all                     Process all source files found in the source path\n"
            + "  -overview <file>         Read overview documentation from HTML file\n"
            + "  -public                  Include only public classes and members\n"
            + "  -protected               Include protected and public classes and members\n"
            + "                           This is the default\n"
            + "  -package                 Include package/protected/public classes and members\n"
            + "  -private                 Include all classes and members\n"
            + "  -help, --help            Show this information\n"
            + "  -doclet <class>          Doclet class to use for generating output\n"
            + "  -docletpath <classpath>  Specifies the search path for the doclet and\n"
            + "                           dependencies\n"
            + "  -source <release>        Provide source compatibility with specified\n"
            + "                           release (1.4 to handle assertion)\n"
            + "  -sourcepath <pathlist>   Where to look for source files\n"
            + "  -s <pathlist>            Alias for -sourcepath\n"
            + "  -subpackages <spkglist>  List of subpackages to recursively load\n"
            + "  -exclude <pkglist>       List of packages to exclude\n"
            + "  -verbose                 Output messages about what Gjdoc is doing [ignored]\n"
            + "  -quiet                   Do not print non-error and non-warning messages\n"
            + "  -locale <name>           Locale to be used, e.g. en_US or en_US_WIN\n"
            + "  -encoding <name>         Source file encoding name\n"
            + "  -breakiterator           Compute first sentence with BreakIterator\n"
            + "  -classpath <pathlist>    Set the path used for loading auxilliary classes\n"
            + "\n"
            + "Standard doclet options:\n"
            + "  -d                      Set target directory\n"
            + "  -use                    Includes the 'Use' page for each documented class\n"
            + "                          and package\n"
            + "  -version                Includes the '@version' tag\n"
            + "  -author                 Includes the '@author' tag\n"
            + "  -splitindex             Splits the index file into multiple files\n"
            + "  -windowtitle <text>     Browser window title\n"
            + "  -doctitle <text>        Title near the top of the overview summary file\n"
            + "                          (HTML allowed)\n"
            + "  -title <text>           Title for this set of API documentation\n"
            + "                          (deprecated, -doctitle should be used instead)\n"
            + "  -header <text>          Text to include in the top navigation bar\n"
            + "                          (HTML allowed)\n"
            + "  -footer <text>          Text to include in the bottom navigation bar\n"
            + "                          (HTML allowed)\n"
            + "  -bottom <text>          Text to include at the bottom of each output file\n"
            + "                          (HTML allowed)\n"
            + "  -link <extdoc URL>      Link to external generated documentation at URL\n"
            + "  -linkoffline <extdoc URL> <packagelistLoc>\n"
            + "                          Link to external generated documentation for\n"
            + "                          the specified package-list\n"
            + "  -linksource             Creates an HTML version of each source file\n"
            + "  -group <groupheading> <packagepattern:packagepattern:...>\n"
            + "                          Separates packages on the overview page into groups\n"
            + "  -nodeprecated           Prevents the generation of any deprecated API\n"
            + "  -nodeprecatedlist       Prevents the generation of the file containing\n"
            + "                          the list of deprecated APIs and the link to the\n"
            + "                          navigation bar to that page\n"
            + "  -nosince                Omit the '@since' tag\n"
            + "  -notree                 Do not generate the class/interface hierarchy page\n"
            + "  -noindex                Do not generate the index file\n"
            + "  -nohelp                 Do not generate the help link\n"
            + "  -nonavbar               Do not generate the navbar, header and footer\n"
            + "  -helpfile <filen>       Path to an alternate help file\n"
            + "  -stylesheetfile <file>  Path to an alternate CSS stylesheet\n"
            + "  -addstylesheet <file>   Path to an additional CSS stylesheet\n"
            + "  -serialwarn             Complain about missing '@serial' tags [ignored]\n"
            + "  -charset <IANACharset>  Specifies the HTML charset\n"
            + "  -docencoding <IANACharset>\n"
            + "                          Specifies the encoding of the generated HTML files\n"
            + "  -tag <tagname>:Xaoptcmf:\"<taghead>\"\n"
            + "                          Enables gjdoc to interpret a custom tag\n"
            + "  -taglet                 Adds a Taglet class to the map of taglets\n"
            + "  -tagletpath             Sets the CLASSPATH to load subsequent Taglets from\n"
            + "  -docfilessubdirs        Enables deep copy of 'doc-files' directories\n"
            + "  -excludedocfilessubdir <name1:name2:...>\n"
            + "                          Excludes 'doc-files' subdirectories with a give name\n"
            + "  -noqualifier all|<packagename1:packagename2:...>\n"
            + "                          Do never fully qualify given package names\n"
            + "  -nocomment              Suppress the entire comment body including the main\n"
            + "                          description and all tags, only generate declarations\n"
            + "\n"
            + "Gjdoc extension options:\n"
            + "  -reflection             Use reflection for resolving unqualified class names\n"
            + "  -licensetext            Include license text from source files\n"
            + "  -validhtml              Use valid HTML/XML names (breaks compatibility)\n"
            + "  -baseurl <url>          Hardwire the given base URL into generated pages\n"
               /**
            + "  -genhtml                Generate HTML code instead of XML code. This is the\n"
            + "                          default.\n"
            + "  -geninfo                Generate Info code instead of XML code.\n"
            + "  -xslsheet <file>        If specified, XML files will be written to a\n"
            + "                          temporary directory and transformed using the\n"
            + "                          given XSL sheet. The result of the transformation\n"
            + "                          is written to the output directory. Not required if\n"
            + "                          -genhtml or -geninfo has been specified.\n"
            + "  -xmlonly                Generate XML code only, do not generate HTML code.\n"
            + "  -bottomnote             HTML code to include at the bottom of each page.\n"
            + "  -nofixhtml              If not specified, heurestics will be applied to\n"
            + "                          fix broken HTML code in comments.\n"
            + "  -nohtmlwarn             Do not emit warnings when encountering broken HTML\n"
            + "                          code.\n"
            + "  -noemailwarn            Do not emit warnings when encountering strings like\n"
            + "                          <abc@foo.com>.\n"
            + "  -indentstep <n>         How many spaces to indent each tag level in\n"
            + "                          generated XML code.\n"
            + "  -xsltdriver <class>     Specifies the XSLT driver to use for transformation.\n"
            + "                          By default, xsltproc is used.\n"
            + "  -postprocess <class>    XmlDoclet postprocessor class to apply after XSL\n"
            + "                          transformation.\n"
            + "  -compress               Generated info pages will be Zip-compressed.\n"
            + "  -workpath               Specify a temporary directory to use.\n"
            + "  -authormail <type>      Specify handling of mail addresses in @author tags.\n"
            + "     no-replace             do not replace mail addresses (default).\n"
            + "     mailto-name            replace by <a>Real Name</a>.\n"
            + "     name-mailto-address    replace by Real Name (<a>abc@foo.com</a>).\n"
            + "     name-mangled-address   replace by Real Name (<a>abc AT foo DOT com</a>).\n"
               **/
            );
  }

  /**
   * The root of the gjdoc tool.
   * 
   * @return all the options of the gjdoc application.
   */
  public static RootDocImpl getRootDoc()
  {
    return rootDoc;
  }

  /**
   * Get the gjdoc singleton.
   * 
   * @return the gjdoc instance.
   */
  public static Main getInstance()
  {
    return instance;
  }

  /**
   * Is this access level covered?
   * 
   * @param accessLevel
   *          the access level we want to know if it is covered.
   * @return true if the access level is covered.
   */
  public boolean includeAccessLevel(int accessLevel)
  {
    return coverageTemplates[option_coverage][accessLevel];
  }

  /**
   * Is the doclet running?
   * 
   * @return true if it's running
   */
  public boolean isDocletRunning()
  {
    return docletRunning;
  }

  /**
   * Check the charset. Check that all the characters of the string 'toCheck'
   * and query if they exist in the 'charSet'. The order does not matter. The
   * number of times a character is in the variable does not matter.
   * 
   * @param toCheck
   *          the charset to check.
   * @param charSet
   *          the reference charset
   * @return true if they match.
   */
  public static boolean checkCharSet(String toCheck, String charSet)
  {
    for (int i = 0; i < toCheck.length(); ++i)
    {
      if (charSet.indexOf(toCheck.charAt(i)) < 0)
        return false;
    }
    return true;
  }

  /**
   * Makes the RootDoc eligible for the GC.
   */
  public static void releaseRootDoc()
  {
    rootDoc.flush();
  }

  /**
   * Return whether the -breakiterator option has been specified.
   */
  public boolean isUseBreakIterator()
  {
    return this.option_breakiterator
      || !getLocale().getLanguage().equals(Locale.ENGLISH.getLanguage());
  }

  /**
   * Return whether boilerplate license text should be copied.
   */
  public boolean isCopyLicenseText()
  {
    return this.option_licensetext;
  }

  /**
   *  Return the locale specified using the -locale option or the
   *  default locale;
   */
  public Locale getLocale()
  {
    return this.option_locale;
  }

  /**
   *  Return the collator to use based on the specified -locale
   *  option. If no collator can be found for the given locale, a
   *  warning is emitted and the default collator is used instead.
   */
  public Collator getCollator()
  {
    if (null == this.collator) {
      Locale locale = getLocale();
      this.collator = Collator.getInstance(locale);
      Locale defaultLocale = Locale.getDefault();
      if (null == this.collator
          && !defaultLocale.equals(locale)) {
        this.collator = Collator.getInstance(defaultLocale);
        if (null != this.collator) {
          reporter.printWarning("No collator found for locale " 
                                + locale.getDisplayName() 
                                + "; using collator for default locale " 
                                + defaultLocale.getDisplayName()
                                + ".");
        }
        else {
          this.collator = Collator.getInstance();
          reporter.printWarning("No collator found for specified locale " 
                                + locale.getDisplayName() 
                                + " or default locale " 
                                + defaultLocale.getDisplayName()
                                + ": using default collator.");
        }
      }
      if (null == this.collator) {
        this.collator = Collator.getInstance();
        reporter.printWarning("No collator found for locale " 
                              + locale.getDisplayName() 
                              + ": using default collator.");
      }
    }
    return this.collator;
  }

  public boolean isCacheRawComments()
  {
    return true;
  }

  public String getGjdocVersion()
  {
    if (null == gjdocVersion) {
      gjdocVersion = gnu.classpath.Configuration.CLASSPATH_VERSION;
    }
    return gjdocVersion;
  }

  public boolean isReflectionEnabled()
  {
    return this.option_reflection;
  }
}
