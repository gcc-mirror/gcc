/* Generator.java -- Generic code generator.
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
*/

package gnu.classpath.tools.rmic;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.io.StringReader;
import java.util.Collection;
import java.util.Iterator;
import java.util.Map;

/**
 * Contains basic methods, used in code generation.
 * 
 * @author Audrius Meskauskas, Lithuania (audriusa@Bioinformatics.org)
 */
public class Generator
{
  /**
   * Get resource with the given name, as string.
   * 
   * @param name the resource name
   * @return the resourse string (in subfolder /templates).
   */
  public String getResource(String name)
  {
    String resourcePath = "templates/" + name;
    InputStream in = getClass().getResourceAsStream(resourcePath);

    if (in == null)
      throw new InternalError(getClass().getName() + ": no resource "
                              + resourcePath);

    BufferedReader r = new BufferedReader(new InputStreamReader(in));
    StringBuilder b = new StringBuilder();

    String s;
    try
      {
        while ((s = r.readLine()) != null)
          {
            b.append(s);
            b.append('\n');
          }
        r.close();
      }
    catch (IOException e)
      {
        InternalError ierr = new InternalError("No expected resource " + name);
        ierr.initCause(e);
        throw ierr;
      }

    return b.toString();
  }

  /**
   * Replace the variable references (starting from #) in the template string by
   * the values, present in the given map. The strings, not present in the
   * variable map, are ignored.
   * 
   * @param template
   *          the template string
   * @param variables
   *          the map of variables (name to value) to replace.
   * @return the string with replaced values.
   */
  public String replaceAll(String template, Map variables)
  {
    BufferedReader r = new BufferedReader(new StringReader(template));
    String s;
    StringBuilder b = new StringBuilder(template.length());
    try
      {
        Iterator iter;
        Collection vars = variables.keySet();
        while ((s = r.readLine()) != null)
          {
            // At least one variable must appear in the string to make
            // the string scan sensible.
            if (s.indexOf('#') >= 0)
              {
                iter = vars.iterator();
                String variable;
                while (iter.hasNext())
                  {
                    variable = (String) iter.next();
                    if (s.indexOf(variable) >= 0)
                      s = s.replaceAll(variable,
                                       (String) variables.get(variable));
                  }
              }
            b.append(s);
            b.append('\n');
          }
        r.close();
      }
    catch (IOException e)
      {
        // This should never happen.
        InternalError ierr = new InternalError("");
        ierr.initCause(e);
        throw ierr;
      }
    return b.toString();
  }
}
