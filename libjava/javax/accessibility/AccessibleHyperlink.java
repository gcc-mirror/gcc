/* AccessibleHyperlink.java -- aids in accessibly navigating hypertext
   Copyright (C) 2002, 2005  Free Software Foundation, Inc.

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


package javax.accessibility;

/**
 * This object encapsulates actions associated with navigating hypertext.
 *
 * @author Eric Blake (ebb9@email.byu.edu)
 * @see Accessible
 * @see AccessibleContext
 * @see AccessibleText
 * @see AccessibleContext#getAccessibleText()
 * @since 1.2
 * @status updated to 1.4
 */
public abstract class AccessibleHyperlink implements AccessibleAction
{
  /**
   * The default constructor.
   */
  public AccessibleHyperlink()
  {
  }

  /**
   * Returns whether the document the link references is still valid, as the
   * association may have changed with a text edit.
   *
   * @return true if the link is valid with respect to the AccessibleHypertext
   */
  public abstract boolean isValid();

  /**
   * Get the number possible actions for this object, starting from 0. In
   * general, a hypertext link has only one action, except for an image map,
   * so there isn't really a default action.
   *
   * @return the 0-based number of actions
   */
  public abstract int getAccessibleActionCount();

  /**
   * Perform the specified action. Does nothing if out of bounds.
   *
   * @param i the action to perform, 0-based
   * @return true if the action was performed
   * @see #getAccessibleActionCount()
   */
  public abstract boolean doAccessibleAction(int i);

  /**
   * Get the anchor text of the link, or null if the index is out of bounds.
   * For example, &lt;a href="http://www.gnu.org/"&gt;GNU Home Page&lt;/a&gt;
   * would return "GNU Home Page", while &lt;a HREF="#top"&gt;
   * &lt;img src="top-hat.png" alt="top hat"&gt;&lt;/a&gt; would return
   * "top hat".
   *
   * @param i the link to retrieve, 0-based
   * @return the link anchor text
   * @see #getAccessibleActionCount()
   */
  public abstract String getAccessibleActionDescription(int i);

  /**
   * Get the link location, or null if the index is out of bounds. For
   * example, &lt;a href="http://www.gnu.org/"&gt;GNU Home Page&lt;/a&gt;
   * would return a java.net.URL("http://www.gnu.org/").
   *
   * @param i the link to retrieve, 0-based
   * @return the link location
   * @see #getAccessibleActionCount()
   */
  public abstract Object getAccessibleActionObject(int i);

  /**
   * Get the anchor appropriate for the link, or null if the index is out of
   * bounds. For example, &lt;a href="http://www.gnu.org/"&gt;GNU Home Page
   * &lt;/a&gt; would return "GNU Home Page", while &lt;a HREF="#top"&gt;
   * &lt;img src="top-hat.png" alt="top hat"&gt;&lt;/a&gt; would return
   * an ImageIcon("top-hat.png", "top hat").
   *
   * @param i the link to retrieve, 0-based
   * @return the link anchor object
   * @see #getAccessibleActionCount()
   */
  public abstract Object getAccessibleActionAnchor(int i);

  /**
   * Gets the character index where this link starts in the parent hypertext
   * document.
   *
   * @return the starting index
   */
  public abstract int getStartIndex();

  /**
   * Gets the character index where this link ends in the parent hypertext
   * document.
   *
   * @return the ending index
   */
  public abstract int getEndIndex();
} // class AccessibleAction
