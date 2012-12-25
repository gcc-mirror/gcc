/* MethodGenerator.java -- Generates methods for rmi compiler.
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
import gnu.java.rmi.server.RMIHashes;

import java.lang.reflect.Method;
import java.util.Properties;

/**
 * Keeps information about the single method and generates the code fragments,
 * related to that method.
 *
 * @author Audrius Meskauskas, Lithuania (audriusa@Bioinformatics.org)
 */
public class RmiMethodGenerator
    implements AbstractMethodGenerator
{
  /**
   * The method being defined.
   */
  Method method;

  /**
   * The parent code generator.
   */
  SourceRmicCompiler rmic;

  /**
   * Create the new method generator for the given method.
   *
   * @param aMethod the related method.
   * @param aRmic the Rmic generator instance, where more class - related
   *          information is defined.
   */
  public RmiMethodGenerator(Method aMethod, SourceRmicCompiler aRmic)
  {
    method = aMethod;
    rmic = aRmic;
    if (method.getParameterTypes().length == 0)
      rmic.addZeroSizeObjecArray = true;
  }

  /**
   * Get the method parameter declaration.
   *
   * @return the string - method parameter declaration.
   */
  public String getArgumentList()
  {
    StringBuilder b = new StringBuilder();

    Class<?>[] args = method.getParameterTypes();

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

    Class<?>[] args = method.getParameterTypes();

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

    Class<?>[] args = method.getExceptionTypes();

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
    vars.put("#method_hash", getMethodHashCode());
    vars.put("#argument_list", getArgumentList());
    vars.put("#object_arg_list", getArgListAsObjectArray());
    vars.put("#declaring_class", rmic.name(method.getDeclaringClass()));
    vars.put("#class_arg_list", getArgListAsClassArray());

    String thr = getThrows();
    if (thr.length() > 0)
      vars.put("#throws", "\n    throws " + thr);
    else
      vars.put("#throws", "");

    if (method.getReturnType().equals(void.class))
      templateName = "Stub_12MethodVoid.jav";
    else
      {
        templateName = "Stub_12Method.jav";
        vars.put("#return_statement", getReturnStatement());
      }

    String template = rmic.getResource(templateName);
    String generated = rmic.replaceAll(template, vars);
    return generated;
  }

  /**
   * Generate sentences for Reading and Defining Arguments.
   *
   * @return the sequence of sentences for reading and defining arguments.
   */
  public String getStaticMethodDeclarations()
  {
    StringBuilder b = new StringBuilder();
    Class<?>[] args = method.getParameterTypes();

    for (int i = 0; i < args.length; i++)
      {
        b.append("            ");
        b.append(rmic.name(args[i]));
        b.append(" ");
        b.append("p" + i);
        b.append(" = ");
        if (i < args.length - 1)
          b.append("\n");
      }
    return b.toString();
  }

  /**
   * Get the write statement for writing parameters inside the stub.
   *
   * @return the write statement.
   */
  public String getArgListAsObjectArray()
  {
    Class[] args = method.getParameterTypes();

    if (args.length==0)
      return "NO_ARGS";

    StringBuilder b = new StringBuilder("new Object[] {");

    for (int i = 0; i < args.length; i++)
      {
        if (!args[i].isPrimitive())
          b.append("p"+i);
        else
          {
            b.append("new "+rmic.name(WrapUnWrapper.getWrappingClass(args[i])));
            b.append("(p"+i+")");
          }
        if (i<args.length-1)
          b.append(", ");
      }
    b.append("}");
    return b.toString();
  }

  /**
   * Get the return statement, assuming that the returned object is placed into
   * the variable "result".
   */
  public String getReturnStatement()
  {
    Class r = method.getReturnType();
    if (r.equals(void.class))
      return "";
    else
      {
        if (r.isPrimitive())
          {
            String wcd = rmic.name(WrapUnWrapper.getWrappingClass(r));
            return "return ((" + wcd + ") result)."
                   + WrapUnWrapper.getUnwrappingMethod(r) + ";";
          }
        else
          return "return (" + rmic.name(r) + ") result;";
      }
  }

  /**
   * Get argument list as class array.
   */
  public String getArgListAsClassArray()
  {
    StringBuilder b = new StringBuilder();
    Class[] args = method.getParameterTypes();

    for (int i = 0; i < args.length; i++)
      {
        b.append(rmic.name(args[i]));
        b.append(".class");
        if (i < args.length - 1)
          b.append(", ");
      }
    return b.toString();
  }

  /**
   * RMI ties (previously named Skeletons) are no longer used since v 1.2. This
   * method should never be called.
   */
  public String generateTieMethod()
  {
    throw new InternalError();
  }

  /**
   * Get the method hash code.
   */
  public String getMethodHashCode()
  {
    return RMIHashes.getMethodHash(method)+"L";
  }

  /**
   * Additional processing of the stub name (nothing to do for JRMP stubs).
   */
  public String convertStubName(String name)
  {
    return name;
  }

}
