/* Main.java -- implement com.sun.tools.javac.Main
   Copyright (C) 2007 Free Software Foundation, Inc.

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


package com.sun.tools.javac;

import gnu.classpath.Configuration;
import java.io.File;
import java.io.PrintWriter;
import java.lang.reflect.Constructor;
import java.lang.reflect.Method;
import java.net.MalformedURLException;
import java.net.URL;
import java.net.URLClassLoader;
import java.text.MessageFormat;

public class Main
{
  static Constructor ecjConstructor = null;
  static Method ecjMethod = null;

  static
  {
    String classname = "org.eclipse.jdt.internal.compiler.batch.Main";
    Class klass = null;
    try
      {
        klass = Class.forName(classname);
      }
    catch (ClassNotFoundException e)
      {
        File jar = new File(Configuration.ECJ_JAR);
        if (!jar.exists() || !jar.canRead())
          {
            String message
              = MessageFormat.format(Messages.getString("Main.FailedToRead"),
                                     new Object[] { Configuration.ECJ_JAR });
            System.err.println(message);
          }

        ClassLoader loader = null;
        try
          {
            loader = new URLClassLoader(new URL[] {jar.toURL()});
          }
        catch (MalformedURLException f)
          {
            String message
              = MessageFormat.format(Messages.getString("Main.MalformedURL"),
                                     new Object[] { Configuration.ECJ_JAR });
            System.err.println(message);
            f.printStackTrace();
          }

        try
          {
            klass = loader.loadClass(classname);
          }
        catch (ClassNotFoundException g)
          {
            String message
              = MessageFormat.format(Messages.getString("Main.FailedToLoad"),
                                     new Object[] { classname,
                                                    Configuration.ECJ_JAR });
            System.err.println(message);
            g.printStackTrace();
          }
      }

    try
      {
        ecjConstructor = klass.getConstructor(new Class[] {
                                                PrintWriter.class,
                                                PrintWriter.class,
                                                Boolean.TYPE});
      }
    catch (NoSuchMethodException h)
      {
        System.err.println(Messages.getString("Main.FailedConstructor"));
        h.printStackTrace();
      }

    try
      {
        ecjMethod = klass.getMethod("compile", new Class[] {String[].class});
      }
    catch (NoSuchMethodException i)
      {
        System.err.println(Messages.getString("Main.FailedCompile"));
        i.printStackTrace();
      }
  }

  public static int compile(String[] args, PrintWriter p) throws Exception
  {
    /*
     * This code depends on the patch in Comment #10 in this bug
     * report:
     *
     * https://bugs.eclipse.org/bugs/show_bug.cgi?id=88364
     */
    Object ecjInstance = ecjConstructor.newInstance(new Object[]
        {
          p,
          new PrintWriter(System.err),
          Boolean.FALSE
        });
    return ((Boolean) ecjMethod.invoke(ecjInstance, new Object[]
        { args })).booleanValue() ? 0 : -1;
  }

  public static int compile(String[] args) throws Exception
  {
    return compile(args, new PrintWriter(System.out));
  }

  public static void main(String[] args) throws Exception
  {
    Runtime.getRuntime().exit(Main.compile(args));
  }
}
