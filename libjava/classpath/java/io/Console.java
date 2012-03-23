/* Console.java -- A character-based console device
   Copyright (C) 2012  Free Software Foundation, Inc.

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


package java.io;

/**
 * @since 1.6
 */
public final class Console implements Flushable
{
  private static Console console = new Console();

  public static Console console()
  {
    return console;
  }

  private Console()
  {
  }

  public PrintWriter writer()
  {
    return new PrintWriter(new OutputStreamWriter(System.out));
  }

  public Reader reader()
  {
    return new InputStreamReader(System.in);
  }

  public Console format(String fmt, Object... args)
  {
    System.out.printf(fmt, args);

    return this;
  }

  public Console printf(String fmt, Object... args)
  {
    return format(fmt, args);
  }

  public String readLine(String fmt, Object... args)
  {
    format(fmt, args);
    return readLine();
  }

  public String readLine()
  {
    String result = null;
    try
      {
        result = new BufferedReader(reader()).readLine();
      }
    catch (IOException e)
      {
        throw new IOError(e);
      }
    return result;
  }

  public char[] readPassword(String fmt, Object... args)
  {
    format(fmt, args);
    return readPassword();
  }

  public char[] readPassword()
  {
    String s = VMConsole.readPassword(this);

    System.out.println();

    if (s == null)
      {
        return null;
      }
    return s.toCharArray();
  }

  public void flush() throws IOException
  {
    System.out.flush();
  }
}
