/* PathOptionGroup.java - handle classpath-setting options
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

import gnu.classpath.tools.getopt.Option;
import gnu.classpath.tools.getopt.OptionException;
import gnu.classpath.tools.getopt.OptionGroup;

import java.io.File;
import java.io.FilenameFilter;
import java.net.MalformedURLException;
import java.net.URL;
import java.net.URLClassLoader;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Iterator;
import java.util.StringTokenizer;

public class PathOptionGroup
    extends OptionGroup
{
  ArrayList classpath = new ArrayList();

  ArrayList bootclasspath = new ArrayList();

  void setPath(ArrayList list, String path)
  {
    list.clear();
    StringTokenizer st = new StringTokenizer(path, File.pathSeparator);
    while (st.hasMoreTokens())
      {
        list.add(st.nextToken());
      }
  }

  void addExtDirs(ArrayList list, String path)
  {
    StringTokenizer tok = new StringTokenizer(path, File.pathSeparator);
    while (tok.hasMoreTokens())
      {
        File dir = new File(tok.nextToken());
        list.addAll(Arrays.asList(dir.list(new FilenameFilter()
        {
          public boolean accept(File dir, String name)
          {
            return name.endsWith(".zip") || name.endsWith(".jar");
          }
        })));
      }
  }

  public PathOptionGroup()
  {
    super("Class path options");

    // Use the VM's built-in boot class path by default.
    String boot = System.getProperty("sun.boot.class.path");
    if (boot != null)
      setPath(bootclasspath, boot);

    add(new Option("classpath", "Set the class path", "PATH")
    {
      public void parsed(String path) throws OptionException
      {
        setPath(classpath, path);
      }
    });
    add(new Option("cp", "Set the class path", "PATH")
    {
      public void parsed(String path) throws OptionException
      {
        setPath(classpath, path);
      }
    });
    add(new Option('I', "Add directory to class path", "DIR", true)
    {
      public void parsed(String path) throws OptionException
      {
        classpath.add(path);
      }
    });
    add(new Option("bootclasspath", "Set the boot class path", "PATH")
    {
      public void parsed(String path) throws OptionException
      {
        setPath(bootclasspath, path);
      }
    });
    add(new Option("extdirs", "Set the extension directory path", "PATH")
    {
      public void parsed(String path) throws OptionException
      {
        addExtDirs(classpath, path);
      }
    });
  }

  public URLClassLoader getLoader() throws MalformedURLException
  {
    ArrayList urls = new ArrayList();
    classpath.addAll(bootclasspath);
    Iterator i = classpath.iterator();
    while (i.hasNext())
      {
        String f = (String) i.next();
        urls.add(new File(f).toURL());
      }
    URL[] urlArray = (URL[]) urls.toArray(new URL[0]);
    return new URLClassLoader(urlArray);
  }
}
