/* SourceGiopRmicCompiler -- Central GIOP-based RMI stub and tie compiler class.
   Copyright (C) 2006, 2008, 2012 Free Software Foundation

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
*/

package gnu.classpath.tools.rmic;

import gnu.classpath.tools.rmic.AbstractMethodGenerator;

import java.io.File;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.OutputStream;
import java.lang.reflect.Method;
import java.net.MalformedURLException;
import java.net.URL;
import java.net.URLClassLoader;
import java.rmi.Remote;
import java.rmi.RemoteException;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.Comparator;
import java.util.HashSet;
import java.util.Iterator;
import java.util.Properties;
import java.util.StringTokenizer;
import java.util.TreeSet;

/**
 * Provides the extended rmic functionality to generate the POA - based classes
 * for GIOP (javax.rmi.CORBA package).
 *
 * @author Audrius Meskauskas, Lithuania (audriusa@Bioinformatics.org)
 */
public class SourceGiopRmicCompiler
  extends Generator implements Comparator<AbstractMethodGenerator>, RmicBackend
{
  /** The package name. */
  protected String packag;

  /**
   * The "basic" name (normally, the interface name, unless several Remote -
   * derived interfaces are implemented.
   */
  protected String name;

  /**
   * The name (without package) of the class, passed as the parameter.
   */
  protected String implName;

  /**
   * The proposed name for the stub.
   */
  protected String stubName;

  /**
   * The Remote's, implemented by this class.
   */
  protected Collection<Class<?>> implementedRemotes = new HashSet<Class<?>>();

  /**
   * The extra classes that must be imported.
   */
  protected Collection<String> extraImports = new HashSet<String>();

  /**
   * The methods we must implement.
   */
  protected Collection<AbstractMethodGenerator> methods =
    new HashSet<AbstractMethodGenerator>();

  /**
   * The map of all code generator variables.
   */
  public Properties vars = new Properties();

  /**
   * If this flag is set (true by default), the compiler generates the Servant
   * based classes. If set to false, the compiler generates the old style
   * ObjectImpl based classes.
   */
  protected boolean poaMode = true;

  /**
   * If this flag is set (true by default), the compiler emits warnings.
   */
  protected boolean warnings = true;

  /**
   * If this flag is set (false by default), the compiler does not
   * write output files.
   */
  protected boolean noWrite = false;

  /**
   * If this flag is set (false by default), the compiler keeps source
   * output files.  For SourceGiopRmicCompiler this overrides
   * -nowrite, since -nowrite doesn't apply to sources kept with
   * -keep.
   */
  protected boolean keep = false;

  /**
   * Verbose output
   */
  protected boolean verbose = false;

  /**
   * Force mode - do not check the exceptions
   */
  protected boolean force = false;

  /**
   * The output directory for generated files.
   */
  protected String outputDirectory;

  /**
   * The class loader to load the class being compiled.
   */
  ClassLoader classLoader;

  /**
   * Clear data, preparing for the next compilation.
   */
  public synchronized void reset()
  {
    packag = name = implName = stubName = null;
    implementedRemotes.clear();
    extraImports.clear();
    methods.clear();
    vars.clear();
  }

  /**
   * Set the class path (handle the -classpath key)
   *
   * @param classPath the class path to set.
   */
  public void setClassPath(String classPath)
  {
    classLoader = Thread.currentThread().getContextClassLoader();
    StringTokenizer tok = new StringTokenizer(classPath, File.pathSeparator,
                                              true);
    ArrayList urls = new ArrayList(tok.countTokens());
    String s = null;
    try
      {
        while (tok.hasMoreTokens())
          {
            s = tok.nextToken();
            if (s.equals(File.pathSeparator))
              urls.add(new File(".").toURL());
            else
              {
                urls.add(new File(s).toURL());
                if (tok.hasMoreTokens())
                  {
                    // Skip the separator.
                    tok.nextToken();
                    // If the classpath ended with a separator,
                    // append the current directory.
                    if (! tok.hasMoreTokens())
                      urls.add(new File(".").toURL());
                  }
              }
          }
      }
    catch (MalformedURLException ex)
      {
        System.err.println("Malformed path '" + s + "' in classpath '"
                           + classPath + "'");
        System.exit(1);
      }
    URL[] u = new URL[urls.size()];
    for (int i = 0; i < u.length; i++)
      {
        u[i] = (URL) urls.get(i);
      }

    classLoader = new URLClassLoader(u, classLoader);
  }

  /**
   * Loads the class with the given name (uses class path, if applicable)
   *
   * @param name the name of the class.
   */
  public Class loadClass(String name)
  {
    ClassLoader loader = classLoader;
    if (loader == null)
      loader = Thread.currentThread().getContextClassLoader();
    try
      {
        return loader.loadClass(name);
      }
    catch (ClassNotFoundException e)
      {
        System.err.println(name+" not found on "+loader);
        System.exit(1);
        // Unreacheable code.
        return null;
      }
  }

  /**
   * Compile the given class (the instance of Remote), generating the stub and
   * tie for it.
   *
   * @param remote
   *          the class to compile.
   */
  public synchronized void compile(Class<?> remote)
  {
    reset();
    String s;

    // Get the package.
    s = remote.getName();
    int p = s.lastIndexOf('.');
    if (p < 0)
      {
        // Root package.
        packag = "";
        implName = name = s;
      }
    else
      {
        packag = s.substring(0, p);
        implName = name = s.substring(p + 1);
      }

    name = convertStubName(name);

    stubName = name;

    vars.put("#name", name);
    vars.put("#package", packag);
    vars.put("#implName", implName);

    if (verbose)
      System.out.println("Package " + packag + ", name " + name + " impl "
                         + implName);

    // Get the implemented remotes.
    Class<?>[] interfaces = remote.getInterfaces();

    for (int i = 0; i < interfaces.length; i++)
      {
        if (Remote.class.isAssignableFrom(interfaces[i]))
          {
            if (! interfaces[i].equals(Remote.class))
              {
                implementedRemotes.add(interfaces[i]);
              }
          }
      }

    vars.put("#idList", getIdList(implementedRemotes));

    // Collect and process methods.
    Iterator<Class<?>> iter = implementedRemotes.iterator();

    while (iter.hasNext())
      {
        Class<?> c = iter.next();
        Method[] m = c.getMethods();

        // Check if throws RemoteException.
        for (int i = 0; i < m.length; i++)
          {
            Class[] exc = m[i].getExceptionTypes();
            boolean remEx = false;

            for (int j = 0; j < exc.length; j++)
              {
                if (exc[j].isAssignableFrom(RemoteException.class))
                  {
                    remEx = true;
                    break;
                  }
              }
            if (! remEx && !force)
              throw new CompilationError(m[i].getName() + ", defined in "
                                         + c.getName()
                                         + ", does not throw "
                                         + RemoteException.class.getName());
            AbstractMethodGenerator mm = createMethodGenerator(m[i]);
            methods.add(mm);
          }
      }
  }

  /**
   * Create the method generator for the given method.
   *
   * @param m the method
   *
   * @return the created method generator
   */
  protected AbstractMethodGenerator createMethodGenerator(Method m)
  {
    return new MethodGenerator(m, this);
  }

  /**
   * Get the name of the given class. The class is added to imports, if not
   * already present and not from java.lang and not from the current package.
   *
   * @param nameIt
   *          the class to name
   * @return the name of class as it should appear in java language
   */
  public synchronized String name(Class nameIt)
  {
    if (nameIt.isArray())
      {
        // Mesure dimensions:
        int dimension = 0;
        Class finalComponent = nameIt;
        while (finalComponent.isArray())
          {
            finalComponent = finalComponent.getComponentType();
            dimension++;
          }

        StringBuilder brackets = new StringBuilder();

        for (int i = 0; i < dimension; i++)
          {
            brackets.append("[]");
          }

        return name(finalComponent) + " " + brackets;
      }
    else
      {
        String n = nameIt.getName();
        if (! nameIt.isArray() && ! nameIt.isPrimitive())
          if (! n.startsWith("java.lang")
              && ! (packag != null && n.startsWith(packag)))
            extraImports.add(n);

        int p = n.lastIndexOf('.');
        if (p < 0)
          return n;
        else
          return n.substring(p + 1);
      }
  }

  /**
   * Get the RMI-style repository Id for the given class.
   *
   * @param c
   *          the interface, for that the repository Id must be created.
   * @return the repository id
   */
  public String getId(Class<?> c)
  {
    return "RMI:" + c.getName() + ":0000000000000000";
  }

  /**
   * Get repository Id string array declaration.
   *
   * @param remotes
   *          the collection of interfaces
   * @return the fully formatted string array.
   */
  public String getIdList(Collection<Class<?>> remotes)
  {
    StringBuilder b = new StringBuilder();

    // Keep the Ids sorted, ensuring, that the same order will be preserved
    // between compilations.
    TreeSet<String> sortedIds = new TreeSet<String>();

    Iterator<Class<?>> iter = remotes.iterator();
    while (iter.hasNext())
      {
        sortedIds.add(getId(iter.next()));
      }

    Iterator<String> iterIds = sortedIds.iterator();
    while (iterIds.hasNext())
      {
        b.append("      \"" + iterIds.next() + "\"");
        if (iterIds.hasNext())
          b.append(", \n");
      }
    return b.toString();
  }

  /**
   * Generate stub. Can only be called from {@link #compile}.
   *
   * @return the string, containing the text of the generated stub.
   */
  public String generateStub()
  {
    String template = getResource("Stub.jav");

    // Generate methods.
    StringBuilder b = new StringBuilder();
    Iterator<AbstractMethodGenerator> iter = methods.iterator();
    while (iter.hasNext())
      {
        AbstractMethodGenerator m = iter.next();
        b.append(m.generateStubMethod());
      }

    vars.put("#stub_methods", b.toString());
    vars.put("#imports", getImportStatements());
    vars.put("#interfaces", getAllInterfaces());

    String output = replaceAll(template, vars);
    return output;
  }

  /**
   * Get the list of all interfaces, implemented by the class, that are
   * derived from Remote.
   *
   * @return the string - all interfaces.
   */
  public String getAllInterfaces()
  {
    StringBuilder b = new StringBuilder();
    Iterator iter = implementedRemotes.iterator();

    while (iter.hasNext())
      {
        b.append(name((Class) iter.next()));
        if (iter.hasNext())
          b.append(", ");
      }

    return b.toString();
  }

  /**
   * Generate Tie. Can only be called from {@link #compile}.
   *
   * @return the string, containing the text of the generated Tie.
   */
  public String generateTie()
  {
    String template;
    if (poaMode)
      template = getResource("Tie.jav");
    else
      template = getResource("ImplTie.jav");

    // Generate methods.
    HashFinder hashFinder = new HashFinder();

    // Find the hash character position:
    Iterator<AbstractMethodGenerator> iter = methods.iterator();
    String[] names = new String[methods.size()];
    int p = 0;

    for (int i = 0; i < names.length; i++)
      names[i] = ((MethodGenerator) iter.next()).getGiopMethodName();

    int hashCharPosition = hashFinder.findHashCharPosition(names);

    iter = methods.iterator();
    while (iter.hasNext())
      ((MethodGenerator) iter.next()).hashCharPosition = hashCharPosition;

    vars.put("#hashCharPos", Integer.toString(hashCharPosition));

    ArrayList<AbstractMethodGenerator> sortedMethods =
      new ArrayList<AbstractMethodGenerator>(methods);
    Collections.sort(sortedMethods, this);

    iter = sortedMethods.iterator();

    StringBuilder b = new StringBuilder();

    MethodGenerator prev = null;

    while (iter.hasNext())
      {
        MethodGenerator m = (MethodGenerator) iter.next();
        m.previous = prev;
        m.hashCharPosition = hashCharPosition;
        prev = m;
        b.append(m.generateTieMethod());
      }

    vars.put("#tie_methods", b.toString());

    vars.put("#imports", getImportStatements());

    String output = replaceAll(template, vars);
    return output;
  }

  public int compare(AbstractMethodGenerator ag1, AbstractMethodGenerator ag2)
  {
    MethodGenerator g1 = (MethodGenerator) ag1;
    MethodGenerator g2 = (MethodGenerator) ag2;

    return g1.getHashChar() - g2.getHashChar();
  }

  /**
   * Import the extra classes, used as the method parameters and return values.
   *
   * @return the additional import block.
   */
  protected String getImportStatements()
  {
    TreeSet<String> imp = new TreeSet<String>();

    Iterator<String> it = extraImports.iterator();
    while (it.hasNext())
      {
        String ic = it.next();
        imp.add("import " + ic + ";\n");
      }

    StringBuilder b = new StringBuilder();
    it = imp.iterator();

    while (it.hasNext())
      {
        b.append(it.next());
      }
    return b.toString();
  }

  /**
   * If this flag is set (true by default), the compiler generates the Servant
   * based classes. If set to false, the compiler generates the old style
   * ObjectImpl based classes.
   */
  public void setPoaMode(boolean mode)
  {
    poaMode = mode;
  }

  /**
   * Set the verbose output mode (false by default)
   *
   * @param isVerbose the verbose output mode
   */
  public void setVerbose(boolean isVerbose)
  {
    verbose = isVerbose;
  }

  /**
   * If this flag is set (true by default), the compiler emits warnings.
   */
  public void setWarnings(boolean warn)
  {
    warnings = warn;
  }

  /**
   * Set the error ignore mode.
   */
  public void setForce(boolean isforce)
  {
    force = isforce;
  }

  /**
   * Get the package name.
   */
  public String getPackageName()
  {
    return packag;
  }

  /**
   * Get the proposed stub name
   */
  public String getStubName()
  {
    return stubName;
  }

  /**
   * Additional processing of the stub name.
   */
  public String convertStubName(String name)
  {
    // Drop the Impl suffix, if one exists.
    if (name.endsWith("Impl"))
      return name.substring(0, name.length() - "Impl".length());
    else
      return name;
  }

  /**
   * Assumes that output directory is already created.
   */
  protected boolean outputTie(File fw, Class c)
  {
    try
      {
        String tie = generateTie();
        String tieName = "_" + name(c) + "_Tie.java";

        OutputStream out = new FileOutputStream(new File(fw, tieName));
        out.write(tie.getBytes());
        out.close();
      }
    catch (IOException ioex)
      {
        System.err.println("Output path not accessible");
        ioex.printStackTrace();
        return false;
      }
    return true;
  }

  public void setup(boolean keep, boolean need11Stubs, boolean need12Stubs,
                    boolean iiop, boolean poa, boolean debug, boolean warnings,
                    boolean noWrite, boolean verbose, boolean force, String classpath,
                    String bootclasspath, String extdirs, String outputDirectory)
  {
    setWarnings(warnings);
    setVerbose(verbose);
    setForce(force);
    setClassPath(classpath);
    setPoaMode(poa);
    this.outputDirectory = outputDirectory;
    this.noWrite = noWrite;
    this.keep = keep;
  }

  public boolean run(String[] inputFiles)
  {
    for (int i = 0; i < inputFiles.length; i++)
      {
        reset();
        Class c = loadClass(inputFiles[i]);

        compile(c);
        String packag = getPackageName().replace('.', '/');
        File fw = new File(outputDirectory, packag);

        // Generate stub.
        String stub = generateStub();
        String subName = getStubName() + "_Stub.java";

        // -keep overrides -nowrite for sources.
        if (!noWrite || keep)
          {
            try
              {
                fw.mkdirs();
                OutputStream out = new FileOutputStream(new File(fw,
                                                                 subName));
                out.write(stub.getBytes());
                out.close();

                // Generate tie
                if (!outputTie(fw, c))
                  return false;
              }
            catch (IOException ioex)
              {
                System.err.println("Output path not accessible");
                ioex.printStackTrace();
                return false;
              }
          }
      }
    return true;
  }
}
