/* MethodGenerator.java -- Generates methods for GIOP rmic compiler.
   Copyright (C) 2006 Free Software Foundation

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


package gnu.classpath.tools.rmic;

import gnu.classpath.tools.rmic.AbstractMethodGenerator;

import java.lang.reflect.Method;
import java.util.Properties;

/**
 * Keeps information about the single method and generates the code fragments,
 * related to that method.
 *
 * @author Audrius Meskauskas, Lithuania (audriusa@Bioinformatics.org)
 */
public class MethodGenerator implements AbstractMethodGenerator
{
  /**
   * The method being defined.
   */
  Method method;

  /**
   * The parent code generator.
   */
  SourceGiopRmicCompiler rmic;

  /**
   * The previous method in the list, null for the first element.
   * Used to avoid repretetive inclusion of the same hash code label.
   */
  MethodGenerator previous = null;

  /**
   * The hash character position.
   */
  int hashCharPosition;

  /**
   * Create the new method generator for the given method.
   *
   * @param aMethod
   *          the related method.
   * @param aRmic
   *          the Rmic generator instance, where more class - related
   *          information is defined.
   */
  public MethodGenerator(Method aMethod, SourceGiopRmicCompiler aRmic)
  {
    method = aMethod;
    rmic = aRmic;
  }

  /**
   * Get the method name.
   *
   * @return the name of the method.
   */
  public String getGiopMethodName()
  {
    String m = method.getName();
    if (m.startsWith("get"))
      return "_get_J" + m.substring("get".length());
    else if (m.startsWith("set"))
      return "_set_J" + m.substring("set".length());
    else
      return m;
  }

  /**
   * Get the method parameter declaration.
   *
   * @return the string - method parameter declaration.
   */
  public String getArgumentList()
  {
    StringBuilder b = new StringBuilder();

    Class[] args = method.getParameterTypes();

    for (int i = 0; i < args.length; i++)
      {
        b.append(rmic.name(args[i]));
        b.append(" p" + i);
        if (i < args.length - 1)
          b.append(", ");
      }
    return b.toString();
  }

  /**
   * Get the method parameter list only (no type declarations). This is used to
   * generate the method invocations statement.
   *
   * @return the string - method parameter list.
   */
  public String getArgumentNames()
  {
    StringBuilder b = new StringBuilder();

    Class[] args = method.getParameterTypes();

    for (int i = 0; i < args.length; i++)
      {
        b.append(" p" + i);
        if (i < args.length - 1)
          b.append(", ");
      }
    return b.toString();
  }

  /**
   * Get the list of exceptions, thrown by this method.
   *
   * @return the list of exceptions.
   */
  public String getThrows()
  {
    StringBuilder b = new StringBuilder();

    Class[] args = method.getExceptionTypes();

    for (int i = 0; i < args.length; i++)
      {
        b.append(rmic.name(args[i]));
        if (i < args.length - 1)
          b.append(", ");
      }
    return b.toString();
  }

  /**
   * Generate this method for the Stub class.
   *
   * @return the method body for the stub class.
   */
  public String generateStubMethod()
  {
    String templateName;

    Properties vars = new Properties(rmic.vars);
    vars.put("#return_type", rmic.name(method.getReturnType()));
    vars.put("#method_name", method.getName());
    vars.put("#giop_method_name", getGiopMethodName());
    vars.put("#argument_list", getArgumentList());
    vars.put("#argument_names", getArgumentNames());

    vars.put("#argument_write", getStubParaWriteStatement());

    if (method.getReturnType().equals(void.class))
      vars.put("#read_return", "return;");
    else
      vars.put("#read_return",
               "return "
                   + GiopIo.getReadStatement(method.getReturnType(), rmic));
    String thr = getThrows();
    if (thr.length() > 0)
      vars.put("#throws", "\n    throws " + thr);
    else
      vars.put("#throws", "");

    if (method.getReturnType().equals(void.class))
      templateName = "StubMethodVoid.jav";
    else
      {
        vars.put("#write_result",
                 GiopIo.getWriteStatement(method.getReturnType(), "result",
                                          rmic));
        templateName = "StubMethod.jav";
      }

    String template = rmic.getResource(templateName);
    String generated = rmic.replaceAll(template, vars);
    return generated;
  }

  /**
   * Generate this method handling fragment for the Tie class.
   *
   * @return the fragment to handle this method for the Tie class.
   */
  public String generateTieMethod()
  {
    String templateName;

    Properties vars = new Properties(rmic.vars);
    vars.put("#return_type", rmic.name(method.getReturnType()));
    vars.put("#method_name", method.getName());
    vars.put("#giop_method_name", getGiopMethodName());
    vars.put("#argument_list", getArgumentList());
    vars.put("#argument_names", getArgumentNames());

    vars.put("#argument_write", getStubParaWriteStatement());

    if (previous == null || previous.getHashChar()!=getHashChar())
      vars.put("#hashCodeLabel","    case '"+getHashChar()+"':");
    else
      vars.put("#hashCodeLabel","    // also '"+getHashChar()+"':");

    if (method.getReturnType().equals(void.class))
      templateName = "TieMethodVoid.jav";
    else
      {
        vars.put("#write_result",
                 GiopIo.getWriteStatement(method.getReturnType(), "result",
                                          rmic));
        templateName = "TieMethod.jav";
      }
    vars.put("#read_and_define_args", getRda());

    String template = rmic.getResource(templateName);
    String generated = rmic.replaceAll(template, vars);
    return generated;
  }

  /**
   * Generate sentences for Reading and Defining Arguments.
   *
   * @return the sequence of sentences for reading and defining arguments.
   */
  public String getRda()
  {
    StringBuilder b = new StringBuilder();
    Class[] args = method.getParameterTypes();

    for (int i = 0; i < args.length; i++)
      {
        b.append("                ");
        b.append(rmic.name(args[i]));
        b.append(" ");
        b.append("p"+i);
        b.append(" = ");
        b.append(GiopIo.getReadStatement(args[i], rmic));
        if (i<args.length-1)
          b.append("\n");
      }
    return b.toString();
  }

  /**
   * Get the write statement for writing parameters inside the stub.
   *
   * @return the write statement.
   */
  public String getStubParaWriteStatement()
  {
    StringBuilder b = new StringBuilder();
    Class[] args = method.getParameterTypes();

    for (int i = 0; i < args.length; i++)
      {
        b.append("             ");
        b.append(GiopIo.getWriteStatement(args[i], "p" + i, rmic));
        b.append("\n");
      }
    return b.toString();
  }

  /**
   * Get the hash char.
   */
  public char getHashChar()
  {
    return getGiopMethodName().charAt(hashCharPosition);
  }
}
