/* gnu.classpath.tools.SerialVer
 Copyright (C) 1998, 1999, 2000, 2001 Free Software Foundation, Inc.

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

package gnu.classpath.tools.serialver;

import gnu.classpath.tools.common.ClasspathToolParser;
import gnu.classpath.tools.getopt.FileArgumentCallback;
import gnu.classpath.tools.getopt.Option;
import gnu.classpath.tools.getopt.OptionException;
import gnu.classpath.tools.getopt.Parser;

import java.io.File;
import java.io.ObjectStreamClass;
import java.net.URL;
import java.net.URLClassLoader;
import java.text.MessageFormat;
import java.util.ArrayList;
import java.util.Iterator;
import java.util.StringTokenizer;

/**
 * This class is an implementation of the `serialver' program. Any number of
 * class names can be passed as arguments, and the serial version unique
 * identitfier for each class will be printed in a manner suitable for cuting
 * and pasting into a Java source file.
 */
public class SerialVer
{
  // List of classes to load.
  ArrayList<String> classes = new ArrayList<String>();
  // The class path to use.
  String classpath;

  // FIXME: taken from ClassLoader, should share it.
  private static void addFileURL(ArrayList<URL> list, String file)
  {
    try
      {
        list.add(new File(file).toURL());
      }
    catch(java.net.MalformedURLException x)
      {
      }
  }

  private ClassLoader getClassLoader()
  {
    // FIXME: this code is taken from ClassLoader.
    // We should share it somewhere.
    URL[] urls;
    if (classpath == null)
      urls = new URL[0];
    else
      {
        StringTokenizer tok = new StringTokenizer(classpath,
                                                  File.pathSeparator, true);
        ArrayList<URL> list = new ArrayList<URL>();
        while (tok.hasMoreTokens())
          {
            String s = tok.nextToken();
            if (s.equals(File.pathSeparator))
              addFileURL(list, "."); //$NON-NLS-1$
            else
              {
                addFileURL(list, s);
                if (tok.hasMoreTokens())
                  {
                    // Skip the separator.
                    tok.nextToken();
                    // If the classpath ended with a separator,
                    // append the current directory.
                    if (!tok.hasMoreTokens())
                      addFileURL(list, "."); //$NON-NLS-1$
                  }
              }
          }
        urls = new URL[list.size()];
        urls = (URL[]) list.toArray(urls);
      }
    return new URLClassLoader(urls);
  }

  private void printMessage(String format, String klass)
  {
    System.err.println(MessageFormat.format(format, new Object[] { klass }));
  }

  public void run(String[] args)
  {
    Parser p = new ClasspathToolParser("serialver", true) //$NON-NLS-1$
    {
      protected void validate() throws OptionException
      {
        if (classes.isEmpty())
          throw new OptionException(Messages.getString("SerialVer.NoClassesSpecd")); //$NON-NLS-1$
      }
    };
    p.setHeader(Messages.getString("SerialVer.HelpHeader")); //$NON-NLS-1$

    p.add(new Option(Messages.getString("SerialVer.5"), Messages.getString("SerialVer.ClasspathHelp"), "PATH") //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$
    {
      public void parsed(String argument) throws OptionException
      {
        if (classpath != null)
          throw new OptionException(Messages.getString("SerialVer.DupClasspath")); //$NON-NLS-1$
        classpath = argument;
      }
    });

    p.parse(args, new FileArgumentCallback()
    {
      public void notifyFile(String fileArgument) throws OptionException
      {
        classes.add(fileArgument);
      }
    });

    ClassLoader loader = getClassLoader();
    Iterator it = classes.iterator();
    while (it.hasNext())
      {
        String name = (String) it.next();
        try
          {
            Class clazz = loader.loadClass(name);
            ObjectStreamClass osc = ObjectStreamClass.lookup(clazz);
            if (osc != null)
              System.out.println(clazz.getName() + ": " //$NON-NLS-1$
                                 + "static final long serialVersionUID = " //$NON-NLS-1$
                                 + osc.getSerialVersionUID() + "L;"); //$NON-NLS-1$
            else
              printMessage(Messages.getString("SerialVer.ClassNotSerial"), name); //$NON-NLS-1$
          }
        catch (ClassNotFoundException e)
          {
            printMessage(Messages.getString("SerialVer.ClassNotFound"), name); //$NON-NLS-1$
          }
      }
  }

  public static void main(String[] args)
  {
    new SerialVer().run(args);
  }
}
