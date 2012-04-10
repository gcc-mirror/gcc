/* Main.java - javah main program
 Copyright (C) 2006, 2007 Free Software Foundation, Inc.

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
 Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
 02110-1301 USA.

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


package gnu.classpath.tools.javah;

import gnu.classpath.tools.common.ClasspathToolParser;
import gnu.classpath.tools.getopt.Option;
import gnu.classpath.tools.getopt.OptionException;
import gnu.classpath.tools.getopt.Parser;

import java.io.BufferedReader;
import java.io.File;
import java.io.FileFilter;
import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.net.URL;
import java.net.URLClassLoader;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Iterator;
import java.util.Set;
import java.util.Map;

import org.objectweb.asm.ClassReader;
import org.objectweb.asm.tree.InnerClassNode;

public class Main
{
  // This is an option group for classpath-related options,
  // and also is used for loading classes.
  PathOptionGroup classpath = new PathOptionGroup();

  // The output directory.
  String outputDir;

  // The output file name used if/when -o option is used.
  String outFileName;

  // The loader that we use to load class files.
  URLClassLoader loader;

  // In -all mode, the name of the directory to scan.
  String allDirectory;

  // True for verbose mode.
  boolean verbose;

  // True if we're emitting stubs.
  boolean stubs;

  // True if we're emitting CNI code.
  boolean cni;

  // True if we've seen -cni or -jni.
  boolean cniOrJniSeen;

  // True if output files should always be written.
  boolean force;

  // Map class names to class wrappers.
  HashMap<String,ClassWrapper> classMap = new HashMap<String,ClassWrapper>();

  // Map class names to lists of Text objects.
  HashMap<String,ArrayList<Text>> textMap = new HashMap<String,ArrayList<Text>>();

  // Set of classes which have been parsed
  Set<String> parsed = new HashSet<String>();

  void readCommandFile(String textFileName) throws OptionException
  {
    FileInputStream fis;
    try
      {
        fis = new FileInputStream(textFileName);
      }
    catch (FileNotFoundException ignore)
      {
        throw new OptionException("file \"" + textFileName + "\" not found");
      }
    BufferedReader reader = new BufferedReader(new InputStreamReader(fis));
    String currentClass = null;
    ArrayList<Text> currentValues = null;
    while (true)
      {
        String line;
        try
          {
            line = reader.readLine();
          }
        catch (IOException _)
          {
            break;
          }
        if (line == null)
          break;
        line = line.trim();
        if (line.length() == 0 || line.charAt(0) == '#')
          continue;
        int index = line.indexOf(' ');
        String cmd = line.substring(0, index);
        String value = line.substring(index + 1);
        int cmdValue;
        if ("class".equals(cmd))
          {
            if (currentClass != null)
              {
                textMap.put(currentClass, currentValues);
              }
            currentClass = value;
            currentValues = new ArrayList<Text>();
            continue;
          }
        if (currentClass == null)
          throw new OptionException("no class set");
        if ("add".equals(cmd))
          cmdValue = Text.ADD;
        else if ("append".equals(cmd))
          cmdValue = Text.APPEND;
        else if ("prepend".equals(cmd))
          cmdValue = Text.PREPEND;
        else if ("friend".equals(cmd))
          cmdValue = Text.FRIEND;
        else
          throw new OptionException("unrecognized command: " + cmd);
        currentValues.add(new Text(cmdValue, value));
      }
    if (currentClass != null)
      {
        textMap.put(currentClass, currentValues);
      }
  }

  void scanDirectory(File dir, final HashSet<Object> results)
  {
    File[] files = dir.listFiles(new FileFilter()
    {
      public boolean accept(File pathname)
      {
        if (pathname.isDirectory())
          {
            scanDirectory(pathname, results);
            return false;
          }
        return pathname.getName().endsWith(".class");
      }
    });
    if (files != null)
      results.addAll(Arrays.asList(files));
  }

  protected String getName()
  {
    return "javah";
  }

  protected ClasspathToolParser getParser()
  {
    ClasspathToolParser result = new ClasspathToolParser(getName(), true);
    result.setHeader("usage: javah [OPTIONS] CLASS...");
    result.add(classpath);
    result.add(new Option('d', "Set output directory", "DIR")
    {
      public void parsed(String dir) throws OptionException
      {
        if (outputDir != null)
          throw new OptionException("-d already seen");
        if (outFileName != null)
          throw new OptionException("only one of -d or -o may be used");
        outputDir = dir;
      }
    });
    result.add(new Option('o',
                          "Set output file (only one of -d or -o may be used)",
                          "FILE")
    {
      public void parsed(String fileName) throws OptionException
      {
        if (outFileName != null)
          throw new OptionException("-o already seen");
        if (outputDir != null)
          throw new OptionException("only one of -d or -o may be used");
        outFileName = fileName;
      }
    });
    result.add(new Option("cmdfile", "Read command file", "FILE")
    {
      public void parsed(String file) throws OptionException
      {
        readCommandFile(file);
      }
    });
    result.add(new Option("all", "Operate on all class files under directory",
                          "DIR")
    {
      public void parsed(String arg) throws OptionException
      {
        // FIXME: lame restriction...
        if (allDirectory != null)
          throw new OptionException("-all already specified");
        allDirectory = arg;
      }
    });
    result.add(new Option("stubs", "Emit stub implementation")
    {
      public void parsed(String arg0) throws OptionException
      {
        stubs = true;
      }
    });
    result.add(new Option("jni", "Emit JNI stubs or header (default)")
    {
      public void parsed(String arg0) throws OptionException
      {
        if (cniOrJniSeen && cni)
          throw new OptionException("only one of -jni or -cni may be used");
        cniOrJniSeen = true;
        cni = false;
      }
    });
    result.add(new Option("cni", "Emit CNI stubs or header (default JNI)")
    {
      public void parsed(String arg0) throws OptionException
      {
        if (cniOrJniSeen && ! cni)
          throw new OptionException("only one of -jni or -cni may be used");
        cniOrJniSeen = true;
        cni = true;
      }
    });
    result.add(new Option("verbose", 'v', "Set verbose mode")
    {
      public void parsed(String arg0) throws OptionException
      {
        verbose = true;
      }
    });
    result.add(new Option("force", "Output files should always be written")
    {
      public void parsed(String arg0) throws OptionException
      {
        force = true;
      }
    });
    return result;
  }

  private File makeOutputDirectory() throws IOException
  {
    File outputFile;
    if (outputDir == null)
      outputFile = new File(".");
    else
      outputFile = new File(outputDir);
    return outputFile;
  }

  /**
   * @return The {@link File} object where the generated code will be written.
   *         Returns <code>null</code> if the option <code>-force</code> was
   *         specified on the command line and the designated file already
   *         exists.
   * @throws IOException if <code>outFileName</code> is not a writable file.
   */
  private File makeOutputFile() throws IOException
  {
    File result = new File(outFileName);
    if (result.exists())
      {
        if (! result.isFile())
          throw new IOException("'" + outFileName + "' is not a file");
        if (! force)
          {
            if (verbose)
              System.err.println("["+ outFileName
                                 + " already exists.  Use -force to overwrite]");
            return null;
          }
        if (! result.delete())
          throw new IOException("Was unable to delete existing file: "
                                + outFileName);
      }
    return result;
  }

  private void writeHeaders(Map<File,ClassWrapper> klasses, Printer printer)
      throws IOException
  {
    Iterator<Map.Entry<File,ClassWrapper>> i = klasses.entrySet().iterator();
    while (i.hasNext())
      {
        Map.Entry<File,ClassWrapper> e = i.next();
        File file = e.getKey();
        ClassWrapper klass = e.getValue();
        if (verbose)
          System.err.println("[writing " + klass + " as " + file + "]");
        printer.printClass(file, klass);
      }
  }

  private Map<File,ClassWrapper> parseClasses(Iterator<Object> inputs)
    throws IOException
  {
    Map<File,ClassWrapper> results = new HashMap<File,ClassWrapper>();
    while (inputs.hasNext())
      {
        // Let user specify either kind of class name or a
        // file name.
        Object item = inputs.next();
        ClassWrapper klass;
        File filename;
        if (item instanceof File)
          {
            // Load class from file.
            if (verbose)
              System.err.println("[reading file " + item + "]");
            klass = getClass((File) item);
            filename = new File(klass.name);
          }
        else
          {
            // Load class given the class name.
            String className = ((String) item).replace('.', '/');
            if (verbose)
              System.err.println("[reading class " + className + "]");
            // Use the name the user specified, even if it is
            // different from the ultimate class name.
            filename = new File(className);
            klass = getClass(className);
          }
        results.put(filename, klass);
        parsed.add(item.toString());

        // Check to see if there are inner classes to also parse
        Iterator<?> innerClasses = klass.innerClasses.iterator();
        HashSet<Object> innerNames = new HashSet<Object>();
        while (innerClasses.hasNext())
          {
            String innerName = ((InnerClassNode) innerClasses.next()).name;
            if (!parsed.contains(innerName))
              innerNames.add(innerName);
          }
        results.putAll(parseClasses(innerNames.iterator()));
      }
    return results;
  }

  protected void postParse(String[] names)
  {
    // Nothing here.
  }

  protected void run(String[] args) throws IOException
  {
    ClasspathToolParser p = getParser();
    String[] classNames = p.parse(args, true);
    postParse(classNames);
    loader = classpath.getLoader();

    boolean isDirectory = outFileName == null;
    File outputFile = isDirectory ? makeOutputDirectory() : makeOutputFile();
    if (outputFile == null)
      return;

    Printer printer;
    if (! cni)
      {
        if (stubs)
          printer = new JniStubPrinter(this, outputFile, isDirectory, force);
        else
          printer = new JniIncludePrinter(this, outputFile, isDirectory, force);
      }
    else
      {
        if (stubs)
          printer = new CniStubPrinter(this, outputFile, isDirectory, force);
        else
          printer = new CniIncludePrinter(this, outputFile, isDirectory, force);
      }

    // First we load all of the files. That way if
    // there are references between the files we will
    // be loading the set that the user asked for.
    HashSet<Object> klasses = new HashSet<Object>();
    if (allDirectory != null)
      scanDirectory(new File(allDirectory), klasses);
    // Add the command-line arguments. We use the type of
    // an item in 'klasses' to decide how to load each class.
    for (int i = 0; i < classNames.length; ++i)
      {
        if (classNames[i].endsWith(".class"))
          {
            klasses.add(new File(classNames[i]));
          }
        else
          {
            klasses.add(classNames[i]);
          }
      }

    Map<File, ClassWrapper> results = parseClasses(klasses.iterator());

    writeHeaders(results, printer);
  }

  public ArrayList<Text> getClassTextList(String name)
  {
    return textMap.get(name);
  }

  private ClassWrapper readClass(InputStream is) throws IOException
  {
    ClassReader r = new ClassReader(is);
    ClassWrapper result = new ClassWrapper(this);
    r.accept(result, true);
    is.close();
    return result;
  }

  private ClassWrapper getClass(File fileName) throws IOException
  {
    InputStream is = new FileInputStream(fileName);
    ClassWrapper result = readClass(is);
    if (classMap.containsKey(result.name))
      throw new IllegalArgumentException("class " + result.name
                                         + " already loaded");
    classMap.put(result.name, result);
    return result;
  }

  public ClassWrapper getClass(String name) throws IOException
  {
    if (! classMap.containsKey(name))
      {
        String resource = name.replace('.', '/') + ".class";
        URL url = loader.findResource(resource);
        if (url == null)
          throw new IOException("can't find class file " + resource
                                + " in " + loader);
        InputStream is = url.openStream();
        ClassWrapper result = readClass(is);
        classMap.put(name, result);
      }
    return classMap.get(name);
  }

  public static void main(String[] args) throws IOException
  {
    Main m = new Main();
    m.run(args);
  }
}
