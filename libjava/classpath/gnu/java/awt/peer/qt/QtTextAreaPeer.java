/* QtTextAreaPeer.java --
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
import java.awt.TextArea;
import java.awt.event.TextEvent;
import java.awt.im.InputMethodRequests;
import java.awt.peer.TextAreaPeer;

public class QtTextAreaPeer extends QtComponentPeer implements TextAreaPeer
{
  public QtTextAreaPeer( QtToolkit kit, TextArea owner )
  {
    super( kit, owner );
  }

  protected native void init();

  protected void setup()
  {
    super.setup();
    setText(((TextArea)owner).getText());
    setEditable(((TextArea)owner).isEditable());
  }

  /**
   * Returns the start (start = true) or end (start = false) of the selection.
   */
  private native int getSelection(boolean start);

  /**
   * Called back on a text edit.
   */
  private void textChanged()
  {
    TextEvent e = new TextEvent(owner, TextEvent.TEXT_VALUE_CHANGED);
    QtToolkit.eventQueue.postEvent(e);
  }

  // ************ Public methods *********************

  public long filterEvents(long filter)
  {
    return filter;
  }

  public native int getCaretPosition();

  public Rectangle getCharacterBounds(int pos)
  {
    // FIXME
    return new Rectangle(0,0,0,0);
  }

  /**
   * Implemented, but who uses it?
   */
  public native int getIndexAtPoint(int x, int y);

//   public void reshape(int x, int y,
//                    int width, int height)
//   {
//     if(width != 0 || height != 0)
//       super.reshape(x, y, width, height);
//     else
//       super.reshape(x, y, 10, 10);
//   }

  public Dimension getMinimumSize(int rows, int cols)
  {
    // FIXME
    return getMinimumSize();
  }

  public Dimension getPreferredSize(int rows, int cols)
  {
    // FIXME
    return getPreferredSize();
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

  public void insert(String text, int pos)
  {
    // Not very efficient, no.
    String s = getText();
    setText(s.substring(0, pos) + text + s.substring(pos));
  }

  public void insertText(String text, int pos)
  {
    insert(text, pos);
  }

  public Dimension minimumSize(int rows, int cols)
  {
    return getMinimumSize(rows, cols);
  }

  public Dimension preferredSize(int rows, int cols)
  {
    return getPreferredSize(rows, cols);
  }

  public void replaceRange(String insert, int start_pos, int end_pos)
  {
    // Not very efficient, no.
    String text = getText();
    String right = text.substring(0, start_pos);
    String left = text.substring(end_pos);
    setText(right + insert + left);
  }

  public void replaceText(String text, int start_pos, int end_pos)
  {
    replaceRange(text, start_pos, end_pos);
  }

  public native void setText(String text);

  public native void select(int start_pos, int end_pos);

  public native void setEditable(boolean editable);

  public native void setCaretPosition(int pos);

  public InputMethodRequests getInputMethodRequests()
  {
    // TODO Auto-generated method stub
    return null;
  }
}
