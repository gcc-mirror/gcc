/* Native2ASCII.java - native2ascii program
 Copyright (C) 2003, 2007, 2008 Free Software Foundation, Inc.

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


package gnu.classpath.tools.native2ascii;

import gnu.classpath.tools.common.ClasspathToolParser;
import gnu.classpath.tools.getopt.FileArgumentCallback;
import gnu.classpath.tools.getopt.Option;
import gnu.classpath.tools.getopt.OptionException;
import gnu.classpath.tools.getopt.Parser;

import java.io.BufferedReader;
import java.io.BufferedWriter;
import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.io.OutputStream;
import java.io.OutputStreamWriter;
import java.io.PrintWriter;

/**
 * Native2ASCII main program.
 * @author Ito Kazumitsu <kaz@maczuka.gcd.org>
 */
public class Native2ASCII
{
  // Input file.
  String input;
  // Output file.
  String output;
  // Encoding to use.
  String encoding;
  // True for reverse operation.
  boolean reversed;

  private class HandleFile extends FileArgumentCallback
  {
    public HandleFile()
    {
    }

    public void notifyFile(String fileArgument)
      throws OptionException
    {
      if (input == null)
        input = fileArgument;
      else if (output == null)
        output = fileArgument;
      else
        throw new OptionException(Messages.getString("Native2ASCII.TooManyFiles")); //$NON-NLS-1$
    }
  }

  private Parser createParser()
  {
    Parser result = new ClasspathToolParser("native2ascii", true); //$NON-NLS-1$
    result.setHeader(Messages.getString("Native2ASCII.Usage")); //$NON-NLS-1$

    result.add(new Option("encoding", Messages.getString("Native2ASCII.EncodingHelp"), Messages.getString("Native2ASCII.EncodingArgName")) //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$
    {
      public void parsed(String argument) throws OptionException
      {
        if (encoding != null)
          throw new OptionException(Messages.getString("Native2ASCII.EncodingSpecified")); //$NON-NLS-1$
        encoding = argument;
      }
    });
    result.add(new Option("reverse", Messages.getString("Native2ASCII.ReversedHelp")) //$NON-NLS-1$ //$NON-NLS-2$
    {
      public void parsed(String argument) throws OptionException
      {
        reversed = true;
      }
    });

    // We mistakenly added the extra "d" in "reversed"; now we don't
    // want to remove it, for backward compatibility.
    result.add(new Option("reversed", Messages.getString("Native2ASCII.ReversedHelpCompat")) //$NON-NLS-1$ //$NON-NLS-2$
    {
      public void parsed(String argument) throws OptionException
      {
        reversed = true;
      }
    });

    return result;
  }

  private void run(String[] args)
  {
    Parser argParser = createParser();
    argParser.parse(args, new HandleFile());

    if (encoding == null)
      encoding = System.getProperty("file.encoding"); //$NON-NLS-1$
    try
      {
        InputStream is = (input == null ? System.in
                                        : new FileInputStream(input));
        OutputStream os = (output == null ? (OutputStream) System.out
                                          : new FileOutputStream(output));

        BufferedReader rdr = new BufferedReader(new InputStreamReader(is,
                                                                      encoding));
        PrintWriter wtr = new PrintWriter(
                                          new BufferedWriter(
                                                             new OutputStreamWriter(
                                                                                    os,
                                                                                    encoding)));
        while (true)
          {
            String s = rdr.readLine();
            if (s == null)
              break;
            StringBuffer sb = new StringBuffer(s.length() + 80);
            for (int i = 0; i < s.length(); i++)
              {
                char c = s.charAt(i);
                if (reversed
                    && i + 6 <= s.length()
                    && s.charAt(i) == '\\'
                    && s.charAt(i + 1) == 'u')
                  {
                    int num = Integer.parseInt(s.substring(i + 2, i + 6), 16);
                    sb.append((char) num);
                    i += 5;
                  }
                else if ((int)c <= 127 || reversed)
                  {
                    sb.append(c);
                  }
                else
                  {
                    sb.append("\\u"); //$NON-NLS-1$
                    if ((int)c <= 0xff)
                      sb.append("00"); //$NON-NLS-1$
                    else if ((int)c <= 0xfff)
                      sb.append("0"); //$NON-NLS-1$
                    sb.append(Integer.toHexString((int) c));
                  }
              }
            wtr.println(sb.toString());
          }
        rdr.close();
        wtr.flush();
        wtr.close();
      }
    catch (Exception e)
      {
        e.printStackTrace();
      }
  }

  public static void main(String[] args)
  {
    new Native2ASCII().run(args);
  }
}
