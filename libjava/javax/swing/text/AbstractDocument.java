/* AbstractDocument.java --
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

import java.io.PrintStream;
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
  private static final long serialVersionUID = -116069779446114664L;
  
  protected static final String BAD_LOCATION = "document location failure";
  
  public static final String BidiElementName = "bidi level";
  public static final String ContentElementName = "content";
  public static final String ParagraphElementName = "paragraph";
  public static final String SectionElementName = "section";
  public static final String ElementNameAttribute = "$ename";

  Content content;
  AttributeContext context;
  DocumentFilter documentFilter;
  
  protected EventListenerList listenerList = new EventListenerList();

  protected AbstractDocument(Content doc)
  {
    this(doc, StyleContext.getDefaultStyleContext());
  }

  protected AbstractDocument(Content doc, AttributeContext ctx)
  {
    content = doc;
    context = ctx;
  }

  // These still need to be implemented by a derived class:
  public abstract Element getParagraphElement(int pos);

  public abstract Element getDefaultRootElement();

  protected Element createBranchElement(Element parent,
					AttributeSet attributes)
  {
    return new BranchElement(parent, attributes);
  }

  protected Element createLeafElement(Element parent, AttributeSet attributes,
				      int start, int end)
  {
    return new LeafElement(parent, attributes, start, end);
  }

  public Position createPosition(final int offset) throws BadLocationException
  {
    if (offset < 0 || offset > getLength())
      throw new BadLocationException(getText(0, getLength()), offset);

    return new Position()
      {
	public int getOffset()
	{
	  return offset;
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
    return context;
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
    return new Position() 
      {        
        public int getOffset() 
        { 
          return getLength(); 
        } 
      };
  }

  public int getLength()
  {
    return content.length() - 1;
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
    Element[] elements = new Element[1];
    elements[0] = getDefaultRootElement();
    return elements;
  }

  public Position getStartPosition()
  {
    return new Position() 
      {        
        public int getOffset() 
        { 
          return 0; 
        } 
      };
  }

  public String getText(int offset, int length) throws BadLocationException
  {
    return content.getString(offset, length);
  }

  public void getText(int offset, int length, Segment segment)
    throws BadLocationException
  {
    content.getChars(offset, length, segment);
  }

  public void insertString(int offset, String text, AttributeSet attributes)
    throws BadLocationException
  {
    // Just return when no text to insert was given.
    if (text == null || text.length() == 0)
      return;
    
    DefaultDocumentEvent event =
      new DefaultDocumentEvent(offset, text.length(),
			       DocumentEvent.EventType.INSERT);
    content.insertString(offset, text);
    insertUpdate(event, attributes);
    fireInsertUpdate(event);
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

  public void remove(int offset, int length) throws BadLocationException
  {
    DefaultDocumentEvent event =
      new DefaultDocumentEvent(offset, length,
			       DocumentEvent.EventType.REMOVE);
    removeUpdate(event);
    content.remove(offset, length);
    postRemoveUpdate(event);
    fireRemoveUpdate(event);
  }

  /**
   * Replaces some text in the document.
   *
   * @since 1.4
   */
  public void replace(int offset, int length, String text,
		      AttributeSet attributes)
    throws BadLocationException
  {
    remove(offset, length);
    insertString(offset, text, attributes);
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

  /**
   * @since 1.4
   */
  public DocumentFilter getDocumentFilter()
  {
    return documentFilter;
  }

  /**
   * @since 1.4
   */
  public void setDocumentFilter(DocumentFilter filter)
  {
    this.documentFilter = filter;
  }

  public void dump(PrintStream out)
  {
    ((AbstractElement) getDefaultRootElement()).dump(out, 0);
  }

  public interface AttributeContext
  {
    AttributeSet addAttribute(AttributeSet old, Object name, Object value);

    AttributeSet addAttributes(AttributeSet old, AttributeSet attributes);

    AttributeSet getEmptySet();

    void reclaim(AttributeSet attributes);

    AttributeSet removeAttribute(AttributeSet old, Object name);

    AttributeSet removeAttributes(AttributeSet old, AttributeSet attributes);

    AttributeSet removeAttributes(AttributeSet old, Enumeration names);
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

  public abstract class AbstractElement
    implements Element, MutableAttributeSet, TreeNode, Serializable
  {
    private static final long serialVersionUID = 1265312733007397733L;
    int count;
    int offset;

    AttributeSet attributes;

    Element element_parent;

    TreeNode tree_parent;
    Vector tree_children;

    public AbstractElement(Element p, AttributeSet s)
    {
      element_parent = p;
      attributes = s;
    }

    // TreeNode implementation

    public abstract Enumeration children();
      
    public abstract boolean getAllowsChildren();
      
    public TreeNode getChildAt(int index)
    {
      return (TreeNode) tree_children.get(index);
    }
      
    public int getChildCount()
    {
      return tree_children.size();
    }
      
    public int getIndex(TreeNode node)
    {
      return tree_children.indexOf(node);
    }

    public TreeNode getParent()
    {
      return tree_parent;
    }

    public abstract boolean isLeaf();


    // MutableAttributeSet support

    public void addAttribute(Object name, Object value)
    {
      attributes = getAttributeContext().addAttribute(attributes, name, value);
    }

    public void addAttributes(AttributeSet attrs)
    {
      attributes = getAttributeContext().addAttributes(attributes, attrs);
    }

    public void removeAttribute(Object name)
    {
      attributes = getAttributeContext().removeAttribute(attributes, name);
    }

    public void removeAttributes(AttributeSet attrs)
    {
      attributes = getAttributeContext().removeAttributes(attributes, attrs);
    }

    public void removeAttributes(Enumeration names)
    {
      attributes = getAttributeContext().removeAttributes(attributes, names);
    }

    public void setResolveParent(AttributeSet parent)
    {
      attributes = getAttributeContext().addAttribute(attributes, ResolveAttribute, parent);
    }


    // AttributeSet interface support

    public boolean containsAttribute(Object name, Object value)
    {
      return attributes.containsAttribute(name, value);
    }

    public boolean containsAttributes(AttributeSet attrs)
    {
      return attributes.containsAttributes(attrs);
    }

    public AttributeSet copyAttributes()
    {
      return attributes.copyAttributes();
    }

    public Object getAttribute(Object key)
    {
      return attributes.getAttribute(key);
    }

    public int getAttributeCount()
    {
      return attributes.getAttributeCount();
    }
      
    public Enumeration getAttributeNames()
    {
      return attributes.getAttributeNames();
    }
      
    public AttributeSet getResolveParent()
    {
      return attributes.getResolveParent();
    }

    public boolean isDefined(Object attrName)
    {
      return attributes.isDefined(attrName);
    }
      
    public boolean isEqual(AttributeSet attrs) 
    {
      return attributes.isEqual(attrs);
    }

    // Element interface support

    public AttributeSet getAttributes()
    {
      return attributes;
    }

    public Document getDocument()
    {
      return AbstractDocument.this;
    }
      
    public abstract Element getElement(int index);
      
    public String getName()
    {
      return (String) getAttribute(NameAttribute);
    }
      
    public Element getParentElement()
    {
      return element_parent;
    }
      
    public abstract int getEndOffset();
      
    public abstract int getElementCount();
      
    public abstract int getElementIndex(int offset);
      
    public abstract int getStartOffset();

    private void dumpElement(PrintStream stream, String indent, Element element)
    {
      System.out.println(indent + "<" + element.getName() +">");
      
      if (element.isLeaf())
	{
	  int start = element.getStartOffset();
	  int end = element.getEndOffset();
	  String text = "";
	  try
	    {
	      text = getContent().getString(start, end - start);
	    }
	  catch (BadLocationException e)
	    {
	    }
	  System.out.println(indent + "  ["
			     + start + ","
			     + end + "]["
			     + text + "]");
	}
      else
	{
	  for (int i = 0; i < element.getElementCount(); ++i)
	    dumpElement(stream, indent + "  ", element.getElement(i));
	}
    }
    
    public void dump(PrintStream stream, int indent)
    {
      String indentStr = "";
      for (int i = 0; i < indent; ++i)
	indentStr += "  ";
      dumpElement(stream, indentStr, this);
    }
  }

  public class BranchElement extends AbstractElement
  {
    private static final long serialVersionUID = -8595176318868717313L;
    
    private Element[] children = new Element[0];

    public BranchElement(Element parent, AttributeSet attributes)
    {
      super(parent, attributes);
    }

    public Enumeration children()
    {
      if (children.length == 0)
        return null;

      Vector tmp = new Vector();

      for (int index = 0; index < children.length; ++index)
	tmp.add(children[index]);
      
      return tmp.elements();
    }

    public boolean getAllowsChildren()
    {
      return true;
    }

    public Element getElement(int index)
    {
      if (index < 0 || index >= children.length)
	return null;

      return children[index];
    }

    public int getElementCount()
    {
      return children.length;
    }

    public int getElementIndex(int offset)
    {
      // XXX: There is surely a better algorithm
      // as beginning from first element each time.
      for (int index = 0; index < children.length; ++index)
        {
	  Element elem = children[index];

	  if ((elem.getStartOffset() <= offset)
	      && (offset < elem.getEndOffset()))
	    return index;
        }

      return 0;
    }

    public int getEndOffset()
    {
      return children[children.length - 1].getEndOffset();
    }

    public String getName()
    {
      return ParagraphElementName;
    }

    public int getStartOffset()
    {
      return children[0].getStartOffset();
    }

    public boolean isLeaf()
    {
      return false;
    }

    public Element positionToElement(int position)
    {
      // XXX: There is surely a better algorithm
      // as beginning from first element each time.
      for (int index = 0; index < children.length; ++index)
        {
	  Element elem = children[index];

	  if ((elem.getStartOffset() <= position)
	      && (position < elem.getEndOffset()))
	    return elem;
        }

      return null;
    }

    public void replace(int offset, int length, Element[] elements)
    {
      Element[] target = new Element[children.length - length
				     + elements.length];
      System.arraycopy(children, 0, target, 0, offset);
      System.arraycopy(elements, 0, target, offset, elements.length);
      System.arraycopy(children, offset + length, target,
		       offset + elements.length,
		       children.length - offset - length);
      children = target;
    }

    public String toString()
    {
      return ("BranchElement(" + getName() + ") "
	      + getStartOffset() + "," + getEndOffset() + "\n");
    }
  }

  public class DefaultDocumentEvent extends CompoundEdit
    implements DocumentEvent
  {
    private static final long serialVersionUID = -7406103236022413522L;
    
    private int offset;
    private int length;
    private DocumentEvent.EventType type;

    public DefaultDocumentEvent(int offset, int length,
				DocumentEvent.EventType type)
    {
      this.offset = offset;
      this.length = length;
      this.type = type;
    }

    public Document getDocument()
    {
      return AbstractDocument.this;
    }

    public int getLength()
    {
      return length;
    }

    public int getOffset()
    {
      return offset;
    }

    public DocumentEvent.EventType getType()
    {
      return type;
    }

    public DocumentEvent.ElementChange getChange(Element elem)
    {
      return null;
    }
  }

  public static class ElementEdit extends AbstractUndoableEdit
    implements DocumentEvent.ElementChange
  {
    private static final long serialVersionUID = -1216620962142928304L;

    private Element elem;
    private int index;
    private Element[] removed;
    private Element[] added;
    
    public ElementEdit(Element elem, int index,
		       Element[] removed, Element[] added)
    {
      this.elem = elem;
      this.index = index;
      this.removed = removed;
      this.added = added;
    }

    public Element[] getChildrenAdded()
    {
      return added;
    }
    
    public Element[] getChildrenRemoved()
    {
      return removed;
    }

    public Element getElement()
    {
      return elem;
    }

    public int getIndex()
    {
      return index;
    }
  }

  public class LeafElement extends AbstractElement
  {
    private static final long serialVersionUID = 5115368706941283802L;
    private int start;
    private int end;

    public LeafElement(Element parent, AttributeSet attributes, int start,
                       int end)
    {
      super(parent, attributes);
      this.start = start;
      this.end = end;
    }

    public Enumeration children()
    {
      return null;
    }

    public boolean getAllowsChildren()
    {
      return false;
    }

    public Element getElement(int index)
    {
      return null;
    }

    public int getElementCount()
    {
      return 0;
    }

    public int getElementIndex(int offset)
    {
      return -1;
    }

    public int getEndOffset()
    {
      return end;
    }

    public String getName()
    {
      return ContentElementName;
    }

    public int getStartOffset()
    {
      return start;
    }

    public boolean isLeaf()
    {
      return true;
    }

    public String toString()
    {
      return ("LeafElement(" + getName() + ") "
	      + getStartOffset() + "," + getEndOffset() + "\n");
    }
  }
}
