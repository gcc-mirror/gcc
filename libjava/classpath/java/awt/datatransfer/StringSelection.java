/* StringSelection.java -- Clipboard handler for text.
   Copyright (C) 1999, 2004  Free Software Foundation, Inc.

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


package java.awt.datatransfer;

import java.io.IOException;
import java.io.StringReader;

/**
  * This class transfers a string as plain text using the clipboard.
  *
  * @author Aaron M. Renn (arenn@urbanophile.com)
  */
public class StringSelection implements Transferable, ClipboardOwner
{

/*
 * Class Variables
 */

// List of flavors we support
// XXX: DataFlavor.plainTextFlavor is deprecated.
static final DataFlavor[] supported_flavors 
   = { DataFlavor.stringFlavor,
       DataFlavor.plainTextFlavor };

/*************************************************************************/

/*
 * Instance Variables
 */

// This is the data to transfer
private String data;

  /**
   * Transfer the specfied string as text.
   *
   * @param data the data for the string selection
   */
  public StringSelection(String data)
  {
    this.data = data;
  }

/**
  * Returns a list of supported data flavors.
  *
  * @return A list of supported data flavors.
  */
public DataFlavor[]
getTransferDataFlavors()
{
  return(supported_flavors);
}

/*************************************************************************/

/**
  * Tests whether or not the specified data flavor is supported.
  *
  * @param flavor The data flavor to test.
  *
  * @return <code>true</code> if the data flavor is supported,
  * <code>false</code> otherwise.
  */
public boolean
isDataFlavorSupported(DataFlavor flavor)
{
  for (int i = 0; i < supported_flavors.length; i++)
    if (supported_flavors[i].equals(flavor))
       return(true);

  return(false);
}

/*************************************************************************/

/**
  * This method returns the data in the requested format.
  *
  * @param flavor The desired data flavor.
  *
  * @return The transferred data.
  *
  * @exception UnsupportedFlavorException If the specified flavor is not
  * supported.
  * @exception IOException If any other error occurs.
  */
public Object
getTransferData(DataFlavor flavor) throws UnsupportedFlavorException,
                                          IOException
{
  if (!isDataFlavorSupported(flavor))
    throw new UnsupportedFlavorException(flavor);

  if (DataFlavor.plainTextFlavor == flavor)
      /* The behavior of this method for DataFlavor.plainTextFlavor and
         equivalent DataFlavors is inconsistent with the definition of
         DataFlavor.plainTextFlavor. We choose to do like Sun's implementation
         and return a Reader instead of an InputString. */
      /* return(new StringBufferInputStream(data)); */
      return(new StringReader(data));
  else // DataFlavor.stringFlavor
      return data;
}

/*************************************************************************/

/**
  * Called when ownership of the clipboard object is lost.
  *
  * @param clipboard The affected clipboard.
  * @param contents The clipboard contents.
  */
public void
lostOwnership(Clipboard clipboard, Transferable contents)
{
  // FIXME: What does this do?
}

} // class StringSelection 

