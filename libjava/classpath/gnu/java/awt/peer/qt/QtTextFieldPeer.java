/* QtTextFieldPeer.java --
   Copyright (C)  2005  Free Software Foundation, Inc.

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

package gnu.java.awt.peer.qt;

import java.awt.Dimension;
import java.awt.Rectangle;
import java.awt.TextField;
import java.awt.event.TextEvent;
import java.awt.im.InputMethodRequests;
import java.awt.peer.TextFieldPeer;

public class QtTextFieldPeer extends QtComponentPeer implements TextFieldPeer
{
  public QtTextFieldPeer( QtToolkit kit, TextField owner )
  {
    super( kit, owner );
  }

  protected native void init();

  protected void setup()
  {
    super.setup();
    setText(((TextField)owner).getText());
    setEditable(((TextField)owner).isEditable());
  }

  /**
   * Called back on a text edit.
   */
  private void textChanged()
  {
    TextEvent e = new TextEvent(owner, TextEvent.TEXT_VALUE_CHANGED);
    QtToolkit.eventQueue.postEvent(e);
  }

  /**
   * Returns the start (start = true) or end (start = false) of the selection.
   */
  private native int getSelection(boolean start);

  private native Dimension getMinimumSizeNative(int columns);

  private native Dimension getPreferredSizeNative(int columns);

  // ************ Public methods *********************

  public long filterEvents(long e)
  {
    return e;
  }

  public native int getCaretPosition();

  public Rectangle getCharacterBounds(int i)
  {
    return new Rectangle(0,0,0,0);
  }

  public int getIndexAtPoint(int x, int y)
  {
    // FIXME
    return 0;
  }

  public Dimension getMinimumSize(int columns)
  {
    Dimension d = getMinimumSizeNative( columns );
    if ( d == null )
      return new Dimension(10, 10);
    return d;
  }

  public Dimension getPreferredSize(int columns)
  {
    Dimension d = getPreferredSizeNative( columns );
    if ( d == null )
      return owner.getSize();
    return d;
  }

  public int getSelectionEnd()
  {
    return getSelection(false);
  }

  public int getSelectionStart()
  {
    return getSelection(true);
  }

  public native String getText();

  public Dimension minimumSize(int cols)
  {
    return getMinimumSize(cols);
  }

  public Dimension preferredSize(int cols)
  {
    return getPreferredSize(cols);
  }

  public native void select(int selStart, int selEnd);

  public native void setCaretPosition(int pos);

  public void setEchoCharacter(char c)
  {
    setEchoChar(c);
  }

  public native void setEchoChar(char echoChar);

  public native void setEditable(boolean editable);

  public native void setText(String l);

  public InputMethodRequests getInputMethodRequests()
  {
    // TODO Auto-generated method stub
    return null;
  }
}
