/* GtkTextFieldPeer.java -- Implements TextFieldPeer with GTK
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


package gnu.java.awt.peer.gtk;

import java.awt.AWTEvent;
import java.awt.Dimension;
import java.awt.Font;
import java.awt.FontMetrics;
import java.awt.Rectangle;
import java.awt.TextField;
import java.awt.event.KeyEvent;
import java.awt.im.InputMethodRequests;
import java.awt.peer.TextFieldPeer;
import java.awt.peer.TextComponentPeer;

public class GtkTextFieldPeer extends GtkComponentPeer
  implements TextComponentPeer, TextFieldPeer
{
  native void create (int width);
  native void gtkWidgetSetBackground (int red, int green, int blue);
  native void gtkWidgetSetForeground (int red, int green, int blue);

  public native void connectSignals ();

  public native int getCaretPosition ();
  public native void setCaretPosition (int pos);
  public native int getSelectionStart ();
  public native int getSelectionEnd ();
  public native String getText ();
  public native void select (int start, int end);
  public native void setEditable (boolean state);
  public native void setText (String text);

  public int getIndexAtPoint(int x, int y)
  {
    // FIXME
    return 0;
  }

  public Rectangle getCharacterBounds (int pos)
  {
    // FIXME
    return null;
  }

  public long filterEvents (long filter)
  {
    // FIXME
    return filter;
  }

  void create ()
  {
    Font f = awtComponent.getFont ();

    // By default, Sun sets a TextField's font when its peer is
    // created.  If f != null then the peer's font is set by
    // GtkComponent.create.
    if (f == null)
      {
	f = new Font ("Dialog", Font.PLAIN, 12);
	awtComponent.setFont (f);
      }

    FontMetrics fm = getFontMetrics (f);

    TextField tf = ((TextField) awtComponent);
    int cols = tf.getColumns ();

    int text_width = cols * fm.getMaxAdvance ();

    create (text_width);

    setEditable (tf.isEditable ());
  }

  native int gtkEntryGetBorderWidth ();

  native void gtkWidgetModifyFont (String name, int style, int size);

  public GtkTextFieldPeer (TextField tf)
  {
    super (tf);

    setText (tf.getText ());
    setCaretPosition (0);

    if (tf.echoCharIsSet ())
      setEchoChar (tf.getEchoChar ());
  }

  public Dimension getMinimumSize (int cols)
  {
    return minimumSize (cols);
  }

  public Dimension getPreferredSize (int cols)
  {
    return preferredSize (cols);
  }

  public native void setEchoChar (char c);

  // Deprecated
  public Dimension minimumSize (int cols)
  {
    int dim[] = new int[2];

    gtkWidgetGetPreferredDimensions (dim);

    Font f = awtComponent.getFont ();
    if (f == null)
      return new Dimension (2 * gtkEntryGetBorderWidth (), dim[1]);

    FontMetrics fm = getFontMetrics (f);

    int text_width = cols * fm.getMaxAdvance ();

    int width = text_width + 2 * gtkEntryGetBorderWidth ();

    return new Dimension (width, dim[1]);
  }

  public Dimension preferredSize (int cols)
  {
    int dim[] = new int[2];

    gtkWidgetGetPreferredDimensions (dim);

    Font f = awtComponent.getFont ();
    if (f == null)
      return new Dimension (2 * gtkEntryGetBorderWidth (), dim[1]);

    FontMetrics fm = getFontMetrics (f);

    int text_width = cols * fm.getMaxAdvance ();

    int width = text_width + 2 * gtkEntryGetBorderWidth ();

    return new Dimension (width, dim[1]);
  }

  public void setEchoCharacter (char c)
  {
    setEchoChar (c);
  }

  public void handleEvent (AWTEvent e)
  {
    if (e.getID () == KeyEvent.KEY_PRESSED)
      {
        KeyEvent ke = (KeyEvent) e;

        if (!ke.isConsumed ()
            && ke.getKeyCode () == KeyEvent.VK_ENTER)
          postActionEvent (getText (), ke.getModifiersEx ());
      }

    super.handleEvent (e);
  }
  public InputMethodRequests getInputMethodRequests()
  {
      // FIXME: implement
    return null;
  }
}
