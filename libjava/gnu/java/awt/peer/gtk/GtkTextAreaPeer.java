/* GtkTextAreaPeer.java -- Implements TextAreaPeer with GTK
   Copyright (C) 1998, 1999, 2002 Free Software Foundation, Inc.

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


package gnu.java.awt.peer.gtk;

import java.awt.Dimension;
import java.awt.Font;
import java.awt.FontMetrics;
import java.awt.TextArea;
import java.awt.peer.TextAreaPeer;

public class GtkTextAreaPeer extends GtkTextComponentPeer
  implements TextAreaPeer
{
  private static transient int DEFAULT_ROWS = 10;
  private static transient int DEFAULT_COLS = 80;

  native void create (int width, int height, int scrollbarVisibility);

  native void gtkSetFont (String name, int style, int size);
  native void gtkWidgetRequestFocus ();

  void create ()
  {
    Font f = awtComponent.getFont ();

    // By default, Sun sets a TextArea's font when its peer is
    // created.  If f != null then the peer's font is set by
    // GtkComponent.create.
    if (f == null)
      {
	f = new Font ("Dialog", Font.PLAIN, 12);
	awtComponent.setFont (f);
      }

    FontMetrics fm;
    if (GtkToolkit.useGraphics2D ())
      fm = new GdkClasspathFontPeerMetrics (f);
    else
      fm = new GdkFontMetrics (f);

    TextArea ta = ((TextArea) awtComponent);
    int sizeRows = ta.getRows ();
    int sizeCols = ta.getColumns ();

    sizeRows = sizeRows == 0 ? DEFAULT_ROWS : sizeRows;
    sizeCols = sizeCols == 0 ? DEFAULT_COLS : sizeCols;

    int width = sizeCols * fm.getMaxAdvance ();
    int height = sizeRows * (fm.getMaxDescent () + fm.getMaxAscent ());

    create (width, height, ta.getScrollbarVisibility ());
    setEditable (ta.isEditable ());
  }

  public GtkTextAreaPeer (TextArea ta)
  {
    super (ta);
  }

  public native void insert (String str, int pos);
  public native void replaceRange (String str, int start, int end);

  public Dimension getMinimumSize (int rows, int cols)
  {
    return minimumSize (rows == 0 ? DEFAULT_ROWS : rows,
                        cols == 0 ? DEFAULT_COLS : cols);
  }

  public Dimension getPreferredSize (int rows, int cols)
  {
    return preferredSize (rows == 0 ? DEFAULT_ROWS : rows,
                          cols == 0 ? DEFAULT_COLS : cols);
  }

  native int getHScrollbarHeight ();
  native int getVScrollbarWidth ();

  // Deprecated
  public Dimension minimumSize (int rows, int cols)
  {
    TextArea ta = ((TextArea) awtComponent);
    int height = 0;
    int width = 0;

    if (ta.getScrollbarVisibility () == TextArea.SCROLLBARS_BOTH
	|| ta.getScrollbarVisibility () == TextArea.SCROLLBARS_HORIZONTAL_ONLY)
      height = getHScrollbarHeight ();

    if (ta.getScrollbarVisibility () == TextArea.SCROLLBARS_BOTH
	|| ta.getScrollbarVisibility () == TextArea.SCROLLBARS_VERTICAL_ONLY)
      width = getVScrollbarWidth ();

    Font f = awtComponent.getFont ();
    if (f == null)
      return new Dimension (width, height);

    FontMetrics fm;
    if (GtkToolkit.useGraphics2D ())
      fm = new GdkClasspathFontPeerMetrics (f);
    else
      fm = new GdkFontMetrics (f);

    int sizeRows = rows == 0 ? DEFAULT_ROWS : rows;
    int sizeCols = cols == 0 ? DEFAULT_COLS : cols;

    width += sizeCols * fm.getMaxAdvance ();
    height += sizeRows * (fm.getMaxDescent () + fm.getMaxAscent ());

    return new Dimension (width, height);
  }

  public Dimension preferredSize (int rows, int cols)
  {
    TextArea ta = ((TextArea) awtComponent);
    int height = 0;
    int width = 0;

    if (ta.getScrollbarVisibility () == TextArea.SCROLLBARS_BOTH
	|| ta.getScrollbarVisibility () == TextArea.SCROLLBARS_HORIZONTAL_ONLY)
      height = getHScrollbarHeight ();

    if (ta.getScrollbarVisibility () == TextArea.SCROLLBARS_BOTH
	|| ta.getScrollbarVisibility () == TextArea.SCROLLBARS_VERTICAL_ONLY)
      width = getVScrollbarWidth ();

    Font f = awtComponent.getFont ();
    if (f == null)
      return new Dimension (width, height);

    FontMetrics fm;
    if (GtkToolkit.useGraphics2D ())
      fm = new GdkClasspathFontPeerMetrics (f);
    else
      fm = new GdkFontMetrics (f);

    int sizeRows = rows == 0 ? DEFAULT_ROWS : rows;
    int sizeCols = cols == 0 ? DEFAULT_COLS : cols;

    width += sizeCols * fm.getMaxAdvance ();
    height += sizeRows * (fm.getMaxDescent () + fm.getMaxAscent ());

    return new Dimension (width, height);
  }

  public void replaceText (String str, int start, int end)
  {
    replaceRange (str, start, end);
  }

  public void insertText (String str, int pos)
  {
    insert (str, pos);
  }
}
