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

import java.awt.AWTEvent;
import java.awt.Dimension;
import java.awt.Font;
import java.awt.TextField;
import java.awt.event.KeyEvent;
import java.awt.peer.TextFieldPeer;

public class GtkTextFieldPeer extends GtkTextComponentPeer
  implements TextFieldPeer
{

//    native void create (ComponentPeer parent, String text);

  native void create ();

  native void gtkEntryGetSize (int dims[]);

  native void gtkSetFont(String name, int style, int size);

  public GtkTextFieldPeer (TextField tf)
  {
    super (tf);

    if (tf.echoCharIsSet ())
      setEchoChar (tf.getEchoChar ());
  }

  public Dimension getMinimumSize (int cols)
  {
    int dims[] = new int[2];

    gtkEntryGetSize (dims);

    return (new Dimension (dims[0], dims[1]));
  }

  public Dimension getPreferredSize (int cols)
  {
    int dims[] = new int[2];

    gtkEntryGetSize (dims);

    return (new Dimension (dims[0], dims[1]));
  }
  
  public native void setEchoChar (char c);

  /* Deprecated */

  public Dimension minimumSize (int cols)
  {
    return getMinimumSize (cols);
  }

  public Dimension preferredSize (int cols)
  {
    return getPreferredSize (cols);
  }

  public void setEchoCharacter (char c)
  {
    setEchoChar (c);
  }

  public void setFont (Font f)
  {
    gtkSetFont(f.getName(), f.getStyle(), f.getSize());
  }

  public void handleEvent (AWTEvent e)
  {
    if (e.getID () == KeyEvent.KEY_PRESSED)
      {
        KeyEvent ke = (KeyEvent)e;

        if (!ke.isConsumed()
            && ke.getKeyCode() == KeyEvent.VK_ENTER)
          postActionEvent (getText(), ke.getModifiers ());
      }

    super.handleEvent (e);
  }
}
