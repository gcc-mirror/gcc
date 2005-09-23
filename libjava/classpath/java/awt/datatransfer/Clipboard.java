/* Clipboard.java -- Class for transferring data via cut and paste.
   Copyright (C) 1999, 2001, 2005 Free Software Foundation, Inc.

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
import java.util.ArrayList;

/**
 * This class allows data to be transferred using a cut and paste type
 * mechanism.
 *
 * @author Aaron M. Renn (arenn@urbanophile.com)
 * @author Mark J. Wielaard (mark@klomp.org)
 */
public class Clipboard
{
  /**
   * The data currently on this clipboard.  For use by
   * subclasses. Also returned by the public method getContents().
   */
  protected Transferable contents;

  /**
   * The owner of this clipboard.
   */
  protected ClipboardOwner owner;

  // The clipboard name
  private final String name;

  // The flavor listeners (most likely small).
  private final ArrayList listeners = new ArrayList(3);

  /**
   * Initializes a new instance of <code>Clipboard</code> with the
   * specified name.
   *
   * @param name The clipboard name.
   */
  public Clipboard(String name)
  {
    this.name = name;
  }

  /**
    * Returns the name of the clipboard.
    */
  public String getName()
  {
    return name;
  }

  /**
   * Returns the contents of the clipboard.
   *
   * @param requestor The object requesting the contents. This
   * implementation ignores this parameter.
   *
   * @exception IllegalStateException If the clipboard is currently unavailable
   */
  public synchronized Transferable getContents(Object requestor)
  {
    return contents;
  }

  /**
   * Sets the content and owner of this clipboard.  If the given owner
   * is different from the current owner then <code>lostOwnership()</code>
   * is called on the current owner with the old contents of the given
   * clipboard.
   *
   * @param contents The new clipboard contents.
   * @param owner The new clipboard owner
   *
   * @exception IllegalStateException If the clipboard is currently unavailable
   */
  public synchronized void setContents(Transferable contents,
				       ClipboardOwner owner)
  {
    Transferable oldContents = getContents(null);
    this.contents = contents;
    if (this.owner != owner)
      {
	ClipboardOwner oldOwner = this.owner;
	this.owner = owner;
	if (oldOwner != null)
	  oldOwner.lostOwnership(this, oldContents);
      }

    FlavorListener[] fs = getFlavorListeners();
    if (fs.length > 0)
      {
	// We are a bit optimistic here. We assume DataFlavors will be
	// given in the same order. If the number of flavors is
	// different or the order of the DataFlavors in the list then
	// fire a change event.
	boolean newFlavors = ((contents != null && oldContents == null)
			      || (contents == null && oldContents != null));
	if (!newFlavors && contents != null && oldContents != null)
	  {
	    DataFlavor[] df1 = contents.getTransferDataFlavors();
	    DataFlavor[] df2 = oldContents.getTransferDataFlavors();
	    newFlavors = df1.length != df2.length;
	    
	    for (int i = 0; !newFlavors && i < df1.length; i++)
	      newFlavors = !df1[i].equals(df2[i]);
	  }

	if (newFlavors)
	  {
	    FlavorEvent e = new FlavorEvent(this);
	    for (int i = 0; i < fs.length; i++)
	      fs[i].flavorsChanged(e);
	  }
      }
  }

  public DataFlavor[] getAvailableDataFlavors()
  {
    Transferable c = getContents(null);
    if (c == null)
      return new DataFlavor[0];
    else
      return c.getTransferDataFlavors();
  }

  public boolean isDataFlavorAvailable(DataFlavor flavor)
  {
    DataFlavor[] fs = getAvailableDataFlavors();
    for (int i = 0; i < fs.length; i++)
      if (flavor.equals(fs[i]))
	return true;

    return false;
  }

  public Object getData(DataFlavor flavor)
    throws UnsupportedFlavorException, IOException
  {
    Transferable c = getContents(null);
    if (c == null)
      throw new UnsupportedFlavorException(flavor);
    else
      return c.getTransferData(flavor);
  }

  public void addFlavorListener(FlavorListener listener)
  {
    synchronized(listeners)
      {
	listeners.add(listener);
      }
  }

  public void removeFlavorListener(FlavorListener listener)
  {
    synchronized(listeners)
      {
	listeners.remove(listener);
      }
  }

  public FlavorListener[] getFlavorListeners()
  {
    synchronized(listeners)
      {
	return (FlavorListener[])
	  listeners.toArray(new FlavorListener[listeners.size()]);
      }
  }
}
