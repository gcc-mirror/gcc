/* IorReader.java --
 Copyright (C) 2005 Free Software Foundation, Inc.

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


package gnu.classpath.examples.CORBA.swing.x5;

import java.io.File;
import java.io.FileReader;
import java.io.IOException;
import java.io.InputStreamReader;
import java.net.MalformedURLException;
import java.net.URL;

import org.omg.CORBA.BAD_PARAM;
import org.omg.CORBA.DATA_CONVERSION;

/**
 * Reads the remote URL. Following formal/04-03-12, CORBA should be able to do
 * this without the help of this class. However some popular class libraries
 * are written using the older CORBA specifications and may not handle
 * functionality, require by this game. This class substitutes the functionality,
 * ensuring that these implementations will also start and we will be able
 * to test the interoperability.
 *
 * @author Audrius Meskauskas (AudriusA@Bioinformatics.org)
 */
public class IorReader
{
  /**
   * Read IOR from the remote URL.
   */
  public static String readUrl(String url)
  {
    URL u;
    try
      {
        u = new URL(url);
      }
    catch (MalformedURLException mex)
      {
        throw new BAD_PARAM("Malformed URL: '" + url + "'");
      }

    try
      {
        InputStreamReader r = new InputStreamReader(u.openStream());

        StringBuilder b = new StringBuilder();
        int c;

        while ((c = r.read()) > 0)
          b.append((char) c);

        return b.toString().trim();
      }
    catch (Exception exc)
      {
        DATA_CONVERSION d = new DATA_CONVERSION("Reading " + url + " failed.");
        throw d;
      }
  }

  /**
   * Read IOR from the file in the local file system.
   */
  public static String readFile(String file)
  {
    File f = new File(file);
    if (!f.exists())
      {
        DATA_CONVERSION err = new DATA_CONVERSION(f.getAbsolutePath()
          + " does not exist.");
        throw err;
      }
    try
      {
        char[] c = new char[(int) f.length()];
        FileReader fr = new FileReader(f);
        fr.read(c);
        fr.close();
        return new String(c).trim();
      }
    catch (IOException ex)
      {
        DATA_CONVERSION d = new DATA_CONVERSION();
        d.initCause(ex);
        throw (d);
      }
  }
}
