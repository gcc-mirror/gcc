/* Clipboard.java -- Class for transferring data via cut and paste.
   Copyright (C) 1999, 2001 Free Software Foundation, Inc.

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
Free Software Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA
02111-1307 USA.

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

/**
 * This class allows data to be transferred using a cut and paste type
 * mechanism.
 *
 * @author Aaron M. Renn (arenn@urbanophile.com)
 */
public class Clipboard
{
  /**
   * The data being transferred.
   */
  protected Transferable contents;

  /**
   * The owner of this clipboard.
   */
  protected ClipboardOwner owner;

  // The clipboard name
  private String name;

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
   * @param requestor The object requesting the contents.
   *
   * @exception IllegalStateException If the clipboard is currently unavailable
   */
  public synchronized Transferable getContents(Object requestor)
  {
    return contents;
  }

  /**
   * Sets the content and owner of this clipboard.
   * If the given owner is different from the current owner
   * then lostOwnership is called on the current owner.
   * XXX - is this called with the old or new contents.
   *
   * @param contents The new clipboard contents.
   * @param owner The new clipboard owner
   *
   * @exception IllegalStateException If the clipboard is currently unavailable
   */
  public synchronized void setContents(Transferable contents, ClipboardOwner owner)
  {
    if (this.owner != owner)
      if (this.owner != null)
        this.owner.lostOwnership(this, contents);
 
    this.owner = owner;
    this.contents = contents;
  }
}

