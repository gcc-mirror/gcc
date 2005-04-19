/* DefaultEditorKit.java --
   Copyright (C) 2002, 2004, 2005  Free Software Foundation, Inc.

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

import java.awt.Toolkit;
import java.awt.event.ActionEvent;
import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.io.OutputStream;
import java.io.OutputStreamWriter;
import java.io.Reader;
import java.io.Writer;

import javax.swing.Action;

public class DefaultEditorKit extends EditorKit
{
  public static class BeepAction
    extends TextAction
  {
    public BeepAction()
    {
      super(beepAction);
    }

    public void actionPerformed(ActionEvent event)
    {
      Toolkit.getDefaultToolkit().beep();
    }
  }

  public static class CopyAction 
    extends TextAction
  {
    public CopyAction()
    {
      super(copyAction);
    }
    public void actionPerformed(ActionEvent event)
    {
    }
  }

  public static class CutAction 
    extends TextAction
  {
    public CutAction()
    {
      super(cutAction);
    }

    public void actionPerformed(ActionEvent event)
    {
    }
  }

  public static class DefaultKeyTypedAction 
    extends TextAction
  {
    public DefaultKeyTypedAction()
    {
      super(defaultKeyTypedAction);
    }

    public void actionPerformed(ActionEvent event)
    {
      JTextComponent t = getTextComponent(event);
      if (t != null)
        {
          try
            {
              t.getDocument().insertString(t.getCaret().getDot(), event.getActionCommand(), null);
              t.getCaret().setDot(Math.min(t.getCaret().getDot() + 1,
                                           t.getDocument().getEndPosition().getOffset()));
            }
          catch (BadLocationException be)
            {
              // FIXME: we're not authorized to throw this.. swallow it?
            }
        }
    }
  }

  public static class InsertBreakAction 
    extends TextAction
  {
    public InsertBreakAction()
    {
      super(insertBreakAction);
    }

    public void actionPerformed(ActionEvent event)
    {
    }
  }

  public static class InsertContentAction 
    extends TextAction
  {
    public InsertContentAction()
    {
      super(insertContentAction);
    }
    public void actionPerformed(ActionEvent event)
    {
    }
  }

  public static class InsertTabAction 
    extends TextAction
  {
    public InsertTabAction()
    {
      super(insertTabAction);
    }

    public void actionPerformed(ActionEvent event)
    {
    }
  }

  public static class PasteAction 
    extends TextAction
  {
    public PasteAction()
    {
      super(pasteAction);
    }

    public void actionPerformed(ActionEvent event)
    {
    }
  }

  private static final long serialVersionUID = 9017245433028523428L;
  
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
  public static final String EndOfLineStringProperty = "__EndOfLine__";
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
  public static final String selectionBeginParagraphAction =
    "selection-begin-paragraph";
  public static final String selectionBeginWordAction = "selection-begin-word";
  public static final String selectionDownAction = "selection-down";
  public static final String selectionEndAction = "selection-end";
  public static final String selectionEndLineAction = "selection-end-line";
  public static final String selectionEndParagraphAction =
    "selection-end-paragraph";
  public static final String selectionEndWordAction = "selection-end-word";
  public static final String selectionForwardAction = "selection-forward";
  public static final String selectionNextWordAction = "selection-next-word";
  public static final String selectionPreviousWordAction =
    "selection-previous-word";
  public static final String selectionUpAction = "selection-up";
  public static final String selectLineAction = "select-line";
  public static final String selectParagraphAction = "select-paragraph";
  public static final String selectWordAction = "select-word";
  public static final String upAction = "caret-up";
  public static final String writableAction = "set-writable";

  public DefaultEditorKit()
  {
  }

  private static Action[] defaultActions = 
  new Action[] {
    new BeepAction(),
    new CopyAction(),
    new CutAction(),
    new DefaultKeyTypedAction(),
    new InsertBreakAction(),
    new InsertContentAction(),
    new InsertTabAction(),
    new PasteAction(),
    new TextAction(deleteNextCharAction) 
    { 
      public void actionPerformed(ActionEvent event)
      {
        JTextComponent t = getTextComponent(event);
        if (t != null)
          {
            try
              {
                int pos = t.getCaret().getDot();
                if (pos < t.getDocument().getEndPosition().getOffset())
                  {
                    t.getDocument().remove(t.getCaret().getDot(), 1);
                  }
              }
            catch (BadLocationException e)
              {
                // FIXME: we're not authorized to throw this.. swallow it?
              }
          }
      }
    },
    new TextAction(deletePrevCharAction) 
    { 
      public void actionPerformed(ActionEvent event)
      {
        JTextComponent t = getTextComponent(event);
        if (t != null)
          {
            try
              {
                int pos = t.getCaret().getDot();
                if (pos > t.getDocument().getStartPosition().getOffset())
                  {
                    t.getDocument().remove(pos - 1, 1);
                    t.getCaret().setDot(pos - 1);
                  }
              }
            catch (BadLocationException e)
              {
                // FIXME: we're not authorized to throw this.. swallow it?
              }
          }
      }
    },
    new TextAction(backwardAction) 
    { 
      public void actionPerformed(ActionEvent event)
      {
        JTextComponent t = getTextComponent(event);
        if (t != null)
          {
            t.getCaret().setDot(Math.max(t.getCaret().getDot() - 1,
                                         t.getDocument().getStartPosition().getOffset()));
          }
      }
    },
    new TextAction(forwardAction) 
    { 
      public void actionPerformed(ActionEvent event)
      {
        JTextComponent t = getTextComponent(event);
        if (t != null)
          {
            t.getCaret().setDot(Math.min(t.getCaret().getDot() + 1,
                                         t.getDocument().getEndPosition().getOffset()));
          }
      }
    },
    new TextAction(selectionBackwardAction)
    {
      public void actionPerformed(ActionEvent event)
      {
	JTextComponent t = getTextComponent(event);
	if (t != null)
	  {
	    t.getCaret().moveDot(Math.max(t.getCaret().getDot() - 1,
					  t.getDocument().getStartPosition().getOffset()));
	  }
      }
    },
    new TextAction(selectionForwardAction)
    {
      public void actionPerformed(ActionEvent event)
      {
        JTextComponent t = getTextComponent(event);
        if (t != null)
          {
            t.getCaret().moveDot(Math.min(t.getCaret().getDot() + 1,
                                          t.getDocument().getEndPosition().getOffset()));
          }
      }
    },
  };

  public Caret createCaret()
  {
    return new DefaultCaret();
  }

  public Document createDefaultDocument()
  {
    return new PlainDocument();
  }
    
  public Action[] getActions()
  {
    return defaultActions;
  }

  public String getContentType()
  {
    return "text/plain";
  }
  
  public ViewFactory getViewFactory()
  {
    return null;
  }

  public void read(InputStream in, Document document, int offset)
    throws BadLocationException, IOException
  {
    read(new InputStreamReader(in), document, offset);
  }

  public void read(Reader in, Document document, int offset)
    throws BadLocationException, IOException
  {
    BufferedReader reader = new BufferedReader(in);

    String line;
    StringBuffer content = new StringBuffer();

    while ((line = reader.readLine()) != null)
      {
	content.append(line);
	content.append("\n");
      }
    
    document.insertString(offset, content.toString(),
			  SimpleAttributeSet.EMPTY);
  }

  public void write(OutputStream out, Document document, int offset, int len)
    throws BadLocationException, IOException
  {
    write(new OutputStreamWriter(out), document, offset, len);
  }

  public void write(Writer out, Document document, int offset, int len)
    throws BadLocationException, IOException
  {
  }
}
