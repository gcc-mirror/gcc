/* DefaultEditorKit.java -- 
   Copyright (C) 2002, 2004 Free Software Foundation, Inc.

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

package javax.swing.text;

import java.io.InputStream;
import java.io.OutputStream;
import java.io.Reader;
import java.io.Writer;
import javax.swing.Action;
import javax.swing.JEditorPane;

public class DefaultEditorKit extends EditorKit
{
  public static final String backwardAction = "caret-backward";
  public static final String beepAction = "beep";
  public static final String beginAction = "caret-begin";
  public static final String beginLineAction = "caret-begin-line";
  public static final String beginParagraphAction = "caret-begin-paragraph";
  public static final String beginWordAction = "caret-begin-word";
  public static final String copyAction = "copy-to-clipboard";
  public static final String cutAction = "cut-to-clipboard";
  public static final String defaultKeyTypedAction = "default-typed";
  public static final String deleteNextCharAction = "delete-next";
  public static final String deletePrevCharAction = "delete-previous";
  public static final String downAction = "caret-down";
  public static final String endAction = "caret-end";
  public static final String endLineAction = "caret-end-line";
  public static final String endOfLineStringProperty = "__EndOfLine__";
  public static final String endParagraphAction = "caret-end-paragraph";
  public static final String endWordAction = "caret-end-word";
  public static final String forwardAction = "caret-forward";
  public static final String insertBreakAction = "insert-break";
  public static final String insertContentAction = "insert-content";
  public static final String insertTabAction = "insert-tab";
  public static final String nextWordAction = "caret-next-word";
  public static final String pageDownAction = "page-down";
  public static final String pageUpAction = "page-up";
  public static final String pasteAction = "paste-from-clipboard";
  public static final String previousWordAction = "caret-previous-word";
  public static final String readOnlyAction = "set-read-only";
  public static final String selectAllAction = "select-all";
  public static final String selectionBackwardAction = "selection-backward";
  public static final String selectionBeginAction = "selection-begin";
  public static final String selectionBeginLineAction = "selection-begin-line";
  public static final String selectionBeginParagraphAction = "selection-begin-paragraph";
  public static final String selectionBeginWordAction = "selection-begin-word";
  public static final String selectionDownAction = "selection-down";
  public static final String selectionEndAction = "selection-end";
  public static final String selectionEndLineAction = "selection-end-line";
  public static final String selectionEndParagraphAction = "selection-end-paragraph";
  public static final String selectionEndWordAction = "selection-end-word";
  public static final String selectionForwardAction = "selection-forward";
  public static final String selectionNextWordAction = "selection-next-word";
  public static final String selectionPreviousWordAction = "selection-previous-word";
  public static final String selectionUpAction = "selection-up";
  public static final String selectLineAction = "select-line";
  public static final String selectParagraphAction = "select-paragraph";
  public static final String selectWordAction = "select-word";
  public static final String upAction = "caret-up";
  public static final String writableAction = "set-writable";

    void deinstall(JEditorPane c)
    {
	//      Called when the kit is being removed from the JEditorPane. 
    }
    void install(JEditorPane c)
    {
    }

    Caret createCaret()
    {
	return null;
    }
    Document createDefaultDocument()
    {
        return new PlainDocument();
    }

    Action[] getActions()
    {
	return null;
    }

    String getContentType()
    {
	return "text/plain";
    }
    
    ViewFactory getViewFactory()
    {
	return null;
    }
    void read(InputStream in, Document doc, int pos)
    {
    }
    void read(Reader in, Document doc, int pos)
    {
    }
    void write(OutputStream out, Document doc, int pos, int len)
    {
    }
    void write(Writer out, Document doc, int pos, int len)
    {
    }
}

