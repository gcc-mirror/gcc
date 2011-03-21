/* SpooledDocument.java -- Reurgitate a spooled PostScript file
   Copyright (C) 2006  Free Software Foundation, Inc.

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

package gnu.java.awt.print;

import javax.print.Doc;
import javax.print.DocFlavor;
import javax.print.attribute.DocAttributeSet;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.Reader;
import java.io.InputStream;
import java.io.InputStreamReader;

public class SpooledDocument implements Doc
{
  private FileInputStream fis;

  public SpooledDocument(File file)
  {
    try
      {
        fis = new FileInputStream(file);
      }
    catch (FileNotFoundException ffne)
      {
        // Shouldn't happen.
      }
  }

  public DocAttributeSet getAttributes()
  {
    return null;
  }

  public DocFlavor getDocFlavor()
  {
    return DocFlavor.INPUT_STREAM.POSTSCRIPT;
  }

  public Object getPrintData()
  {
    return fis;
  }

  public Reader getReaderForText()
  {
    return new InputStreamReader(fis);
  }

  public InputStream getStreamForBytes()
  {
    return fis;
 }
}
