/* ObjectDataContentHandler.java -- Data content handler using an object.
   Copyright (C) 2004 Free Software Foundation, Inc.

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

package javax.activation;

import java.awt.datatransfer.DataFlavor;
import java.awt.datatransfer.UnsupportedFlavorException;
import java.io.IOException;
import java.io.OutputStream;

/**
 * Data content handler that uses an existing DCH and reified object.
 *
 * @author <a href='mailto:dog@gnu.org'>Chris Burdess</a>
 * @version 1.1
 */
class ObjectDataContentHandler
  implements DataContentHandler
{

  private DataContentHandler dch;
  private Object object;
  private String mimeType;
  private DataFlavor[] flavors;
  
  public ObjectDataContentHandler(DataContentHandler dch, Object object,
                                  String mimeType)
  {
    this.dch = dch;
    this.object = object;
    this.mimeType = mimeType;
  }
  
  public Object getContent(DataSource ds)
  {
    return object;
  }
  
  public DataContentHandler getDCH()
  {
    return dch;
  }

  public Object getTransferData(DataFlavor flavor, DataSource ds)
    throws UnsupportedFlavorException, IOException
  {
    if (dch != null)
      {
        return dch.getTransferData(flavor, ds);
      }
    if (flavors == null)
      {
        getTransferDataFlavors();
      }
    if (flavor.equals(flavors[0]))
      {
        return object;
      }
    throw new UnsupportedFlavorException(flavor);
  }
  
  public DataFlavor[] getTransferDataFlavors()
  {
    if (flavors == null)
      {
        if (dch != null)
          {
            flavors = dch.getTransferDataFlavors();
          }
        else
          {
            flavors = new DataFlavor[1];
            flavors[0] = new ActivationDataFlavor(object.getClass(),
                                                  mimeType, mimeType);
          }
      }
    return flavors;
  }

  public void writeTo(Object object, String mimeType, OutputStream out)
    throws IOException
  {
    if (dch != null)
      {
        dch.writeTo(object, mimeType, out);
      }
    else
      {
        throw new UnsupportedDataTypeException("no object DCH for MIME type " + mimeType);
      }
  }
  
}

