/* AbstractDocument.java --
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

import java.io.Serializable;
import java.util.Dictionary;
import java.util.Enumeration;
import java.util.EventListener;
import java.util.Vector;

import javax.swing.event.DocumentEvent;
import javax.swing.event.DocumentListener;
import javax.swing.event.EventListenerList;
import javax.swing.event.UndoableEditEvent;
import javax.swing.event.UndoableEditListener;
import javax.swing.tree.TreeNode;
import javax.swing.undo.AbstractUndoableEdit;
import javax.swing.undo.CompoundEdit;
import javax.swing.undo.UndoableEdit;


public abstract class AbstractDocument
  implements Document, Serializable
{
  public abstract class AbstractElement
    implements Element, TreeNode, Serializable
  {
    private static final long serialVersionUID = 1265312733007397733L;
    
    int count;
    int offset;
    AttributeSet attr;
    Vector elts = new Vector();
    String name;
    Element parent;
    Vector kids = new Vector();
    TreeNode tree_parent;

    public AbstractElement(Element p, AttributeSet s)
    {
      parent = p;
      attr = s;
    }

    public Enumeration children()
    {
      return kids.elements();
    }

    public boolean getAllowsChildren()
    {
      return true;
    }

    public TreeNode getChildAt(int index)
    {
      return (TreeNode) kids.elementAt(index);
    }

    public int getChildCount()
    {
      return kids.size();
    }

    public int getIndex(TreeNode node)
    {
      return kids.indexOf(node);
    }

    public TreeNode getParent()
    {
      return tree_parent;
    }

    public AttributeSet getAttributes()
    {
      return attr;
    }

    public Document getDocument()
    {
      return AbstractDocument.this;
    }

    public Element getElement(int index)
    {
      return (Element) elts.elementAt(index);
    }

    public String getName()
    {
      return name;
    }

    public Element getParentElement()
    {
      return parent;
    }

    public abstract boolean isLeaf();

    public abstract int getEndOffset();

    public abstract int getElementCount();

    public abstract int getElementIndex(int offset);

    public abstract int getStartOffset();
  }

  public interface AttributeContext
  {
  }

  public class BranchElement extends AbstractElement
  {
    private static final long serialVersionUID = -8595176318868717313L;
    
    public BranchElement(Element e, AttributeSet a, int s, int end)
    {
      super(e, a);
    }

    public boolean isLeaf()
    {
      return false;
    }

    public int getEndOffset()
    {
      return 0;
    }

    public int getElementCount()
    {
      return 0;
    }

    public int getElementIndex(int offset)
    {
      return 0;
    }

    public int getStartOffset()
    {
      return 0;
    }
  }

  public interface Content
  {
    Position createPosition(int offset) throws BadLocationException;

    int length();

    UndoableEdit insertString(int where, String str)
      throws BadLocationException;

    UndoableEdit remove(int where, int nitems) throws BadLocationException;

    String getString(int where, int len) throws BadLocationException;

    void getChars(int where, int len, Segment txt) throws BadLocationException;
  }

  public class DefaultDocumentEvent extends CompoundEdit
    implements DocumentEvent
  {
    private static final long serialVersionUID = -7406103236022413522L;
    
    public int len;
    public int off;

    public Document getDocument()
    {
      return AbstractDocument.this;
    }

    public int getLength()
    {
      return len;
    }

    public int getOffset()
    {
      return off;
    }

    public DocumentEvent.EventType getType()
    {
      return null;
    }

    public DocumentEvent.ElementChange getChange(Element elem)
    {
      return null;
    }
  }

  public static class ElementEdit extends AbstractUndoableEdit
  {
    private static final long serialVersionUID = -1216620962142928304L;
  }

  public class LeafElement extends AbstractElement
  {
    private static final long serialVersionUID = 5115368706941283802L;
    
    public LeafElement(Element e, AttributeSet a, int s, int end)
    {
      super(e, a);
    }

    public boolean isLeaf()
    {
      return true;
    }

    public int getEndOffset()
    {
      return 0;
    }

    public int getElementCount()
    {
      return 0;
    }

    public int getElementIndex(int offset)
    {
      return 0;
    }

    public int getStartOffset()
    {
      return 0;
    }
  }

  private static final long serialVersionUID = -116069779446114664L;

  protected static final String BAD_LOCATION = "document location failure";

  public static final String BidiElementName = "bidi level";
  public static final String ContentElementName = "content";
  public static final String ParagraphElementName = "paragraph";
  public static final String SectionElementName = "section";
  public static final String ElementNameAttribute = "$ename";
  
  Content content;

  protected AbstractDocument(Content doc)
  {
    this(doc, null);
  }

  protected AbstractDocument(Content doc, AttributeContext context)
  {
    content = doc;
  }

  protected EventListenerList listenerList = new EventListenerList();

  // these still need to be implemented by a derived class:
  public abstract Element getParagraphElement(int pos);

  public abstract Element getDefaultRootElement();

  protected Element createBranchElement(Element parent, AttributeSet a)
  {
    return new BranchElement(parent, a, 0, 0);
  }

  protected Element createLeafElement(Element parent, AttributeSet a, int p0,
                                      int p1)
  {
    return new LeafElement(parent, a, p0, p1 - p0);
  }

  public Position createPosition(int offs)
  {
    final int a = offs;
    return new Position()
      {
	public int getOffset()
	{
	  return a;
	}
      };
  }

  protected void fireChangedUpdate(DocumentEvent event)
  {
    DocumentListener[] listeners = getDocumentListeners();

    for (int index = 0; index < listeners.length; ++index)
      listeners[index].changedUpdate(event);
  }

  protected void fireInsertUpdate(DocumentEvent event)
  {
    DocumentListener[] listeners = getDocumentListeners();

    for (int index = 0; index < listeners.length; ++index)
      listeners[index].insertUpdate(event);
  }

  protected void fireRemoveUpdate(DocumentEvent event)
  {
    DocumentListener[] listeners = getDocumentListeners();

    for (int index = 0; index < listeners.length; ++index)
      listeners[index].removeUpdate(event);
  }

  protected void fireUndoableEditUpdate(UndoableEditEvent event)
  {
    UndoableEditListener[] listeners = getUndoableEditListeners();
    
    for (int index = 0; index < listeners.length; ++index)
      listeners[index].undoableEditHappened(event);
  }

  public int getAsynchronousLoadPriority()
  {
    return 0;
  }

  protected AttributeContext getAttributeContext()
  {
    return null;
  }

  public Element getBidiRootElement()
  {
    return null;
  }

  protected Content getContent()
  {
    return content;
  }

  protected Thread getCurrentWriter()
  {
    return null;
  }

  public Dictionary getDocumentProperties()
  {
    return null;
  }

  public Position getEndPosition()
  {
    return null;
  }

  public int getLength()
  {
    return content.length();
  }

  public EventListener[] getListeners(Class listenerType)
  {
    return listenerList.getListeners(listenerType);
  }

  public Object getProperty(Object key)
  {
    return null;
  }

  public Element[] getRootElements()
  {
    return null;
  }

  public Position getStartPosition()
  {
    return null;
  }

  public String getText(int offset, int length)
  {
    try
      {
	return content.getString(offset, length);
      }
    catch (Exception e)
      {
	System.out.println("Hmmm, fail to getText: " + offset + " -> "
	                   + length);
	return null;
      }
  }

  public void getText(int offset, int length, Segment txt)
  {
    String a = getText(offset, length);

    if (a == null)
      {
	txt.offset = 0;
	txt.count = 0;
	txt.array = new char[0];
	return;
      }

    txt.offset = offset;
    txt.count = length;

    char[] chars = new char[a.length()];

    a.getChars(0, a.length(), chars, 0);

    txt.array = chars;
  }

  public void insertString(int offs, String str, AttributeSet a)
    throws BadLocationException
  {
    content.insertString(offs, str);
  }

  protected void insertUpdate(DefaultDocumentEvent chng, AttributeSet attr)
  {
  }

  protected void postRemoveUpdate(DefaultDocumentEvent chng)
  {
  }

  public void putProperty(Object key, Object value)
  {
  }

  public void readLock()
  {
  }

  public void readUnlock()
  {
  }

  public void remove(int offs, int len)
  {
  }

  /**
   * Adds a <code>DocumentListener</code> object to this document.
   *
   * @param listener the listener to add
   */
  public void addDocumentListener(DocumentListener listener)
  {
    listenerList.add(DocumentListener.class, listener);
  }

  /**
   * Removes a <code>DocumentListener</code> object from this document.
   *
   * @param listener the listener to remove
   */
  public void removeDocumentListener(DocumentListener listener)
  {
    listenerList.remove(DocumentListener.class, listener);
  }

  /**
   * Returns add added <code>DocumentListener</code> objects.
   *
   * @return an array of listeners
   */
  public DocumentListener[] getDocumentListeners()
  {
    return (DocumentListener[]) getListeners(DocumentListener.class);
  }

  /**
   * Adds a <code>UndoableEditListener</code> object to this document.
   *
   * @param listener the listener to add
   */
  public void addUndoableEditListener(UndoableEditListener listener)
  {
    listenerList.add(UndoableEditListener.class, listener);
  }

  /**
   * Removes a <code>UndoableEditListener</code> object from this document.
   *
   * @param listener the listener to remove
   */
  public void removeUndoableEditListener(UndoableEditListener listener)
  {
    listenerList.remove(UndoableEditListener.class, listener);
  }

  /**
   * Returns add added <code>UndoableEditListener</code> objects.
   *
   * @return an array of listeners
   */
  public UndoableEditListener[] getUndoableEditListeners()
  {
    return (UndoableEditListener[]) getListeners(UndoableEditListener.class);
  }

  protected void removeUpdate(DefaultDocumentEvent chng)
  {
  }

  public void render(Runnable r)
  {
  }

  public void setAsynchronousLoadPriority(int p)
  {
  }

  public void setDocumentProperties(Dictionary x)
  {
  }

  protected void writeLock()
  {
  }

  protected void writeUnlock()
  {
  }
}
