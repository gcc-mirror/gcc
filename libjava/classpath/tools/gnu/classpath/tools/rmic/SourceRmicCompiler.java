/* SourceRmicCompiler.java -- RMI stub generator for java.rmi.*
   Copyright (C) 2006 Free Software Foundation, Inc.

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
*/


package gnu.classpath.tools.rmic;

import java.lang.reflect.Method;
import java.io.File;
import java.util.Iterator;

import gnu.classpath.tools.rmic.AbstractMethodGenerator;

/**
 * RMI stub source code generator, required to support java.rmi.*
 *
 * @author Audrius Meskauskas (AudriusA@Bioinformatics.org) 
 */
public class SourceRmicCompiler extends SourceGiopRmicCompiler
{
  /**
   * If true, the zero size object array is declared in the stub to reduce
   * garbage generation.
   */
  public boolean addZeroSizeObjecArray;
  
  /**
   * Generate a RMI stub.
   * 
   * @return the string, containing the text of the generated stub.
   */
  public String generateStub()
  {
    String template = getResource("Stub_12.jav");

    // Generate methods.
    StringBuilder b = new StringBuilder();
    Iterator iter = methods.iterator();
    while (iter.hasNext())
      {
        RmiMethodGenerator m = (RmiMethodGenerator) iter.next();
        b.append(m.generateStubMethod());
      }

    vars.put("#stub_methods", b.toString());
    vars.put("#imports", getImportStatements());
    vars.put("#interfaces", getAllInterfaces());
    vars.put("#stub_method_declarations", getStubMethodDeclarations());
    vars.put("#stub_method_initializations", getStubMethodInitializations());
    if (addZeroSizeObjecArray)
      {
        vars.put("#zeroSizeObjecArray",
               "private static final Object[] NO_ARGS = new Object[0];");
        vars.put("#zeroSizeClassArray",
               "final Class[]  NO_ARGSc = new Class[0];");
      }
    else
      {
        vars.put("#zeroSizeObjecArray","");
        vars.put("#zeroSizeClassArray","");        
      }

    String output = replaceAll(template, vars);
    return output;
  }

  /**
   * Create a method generator, applicable for RMI stub methods.
   */
  protected AbstractMethodGenerator createMethodGenerator(Method m)
  {
    return new RmiMethodGenerator(m, this);
  } 
  
  /**
   * Get the stub method declarations.
   */
  public String getStubMethodDeclarations()
  {
    StringBuilder b = new StringBuilder();
    
    Iterator iter = methods.iterator();
     
    while (iter.hasNext())
      {
        RmiMethodGenerator method = (RmiMethodGenerator) iter.next();
        b.append("    ");
        b.append("private static final Method met_");
        b.append(method.method.getName());
        b.append(';');
        if (iter.hasNext())
          b.append('\n');
      }
    return b.toString();
  }
  
  /**
   * Get stub method initializations. These must be done in a try-catch
   * statement to catch {@link NoSuchMethodException}.
   */
  public String getStubMethodInitializations()
  {
    StringBuilder b = new StringBuilder();
    
    Iterator iter = methods.iterator();
     
    while (iter.hasNext())
      {
        RmiMethodGenerator method = (RmiMethodGenerator) iter.next();
        b.append("             ");
        b.append("met_");
        b.append(method.method.getName());
        b.append(" =\n               ");
        b.append(name(method.method.getDeclaringClass()));
        b.append(".class.getMethod(");
        b.append('"');
        b.append(method.method.getName());
        b.append("\", ");        
        if (method.method.getParameterTypes().length == 0)
          b.append("NO_ARGSc);");
        else
          {
            b.append("new Class[]\n                 {\n                   ");
            b.append(method.getArgListAsClassArray());
            b.append("\n                 }");
            b.append(");");
          }
        b.append('\n');
      }
    return b.toString();
  }

  /**
   * Prepare for the compilation of the next class.
   */
  public void reset()
  {
    addZeroSizeObjecArray = false;
    super.reset();
  }

  /**
   * Additional processing of the stub name (nothing to do for JRMP stubs).
   */
  public String convertStubName(String name)
  {
    return name;
  }  

  /**
   * Override to do nothing.
   */
  protected boolean outputTie(File fw, Class c)
  {
    return true;
  }
}
