/* DefaultStyledDocument.java --
   Copyright (C) 2004, 2005 Free Software Foundation, Inc.

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


package javax.swing.text;

import java.awt.Color;
import java.awt.Font;
import java.io.Serializable;
import java.util.Enumeration;
import java.util.Stack;
import java.util.Vector;

import javax.swing.event.ChangeEvent;
import javax.swing.event.ChangeListener;
import javax.swing.event.DocumentEvent;
import javax.swing.event.UndoableEditEvent;
import javax.swing.undo.AbstractUndoableEdit;
import javax.swing.undo.UndoableEdit;

/**
 * The default implementation of {@link StyledDocument}. The document is
 * modeled as an {@link Element} tree, which has a {@link SectionElement} as
 * single root, which has one or more {@link AbstractDocument.BranchElement}s
 * as paragraph nodes and each paragraph node having one or more
 * {@link AbstractDocument.LeafElement}s as content nodes.
 * 
 * @author Michael Koch (konqueror@gmx.de)
 * @author Roman Kennke (roman@kennke.org)
 */
public class DefaultStyledDocument extends AbstractDocument implements
    StyledDocument
{

  /**
   * An {@link UndoableEdit} that can undo attribute changes to an element.
   * 
   * @author Roman Kennke (kennke@aicas.com)
   */
  public static class AttributeUndoableEdit extends AbstractUndoableEdit
  {
    /**
     * A copy of the old attributes.
     */
    protected AttributeSet copy;

    /**
     * The new attributes.
     */
    protected AttributeSet newAttributes;

    /**
     * If the new attributes replaced the old attributes or if they only were
     * added to them.
     */
    protected boolean isReplacing;

    /**
     * The element that has changed.
     */
    protected Element element;

    /**
     * Creates a new <code>AttributeUndoableEdit</code>.
     * 
     * @param el
     *          the element that changes attributes
     * @param newAtts
     *          the new attributes
     * @param replacing
     *          if the new attributes replace the old or only append to them
     */
    public AttributeUndoableEdit(Element el, AttributeSet newAtts,
                                 boolean replacing)
    {
      element = el;
      newAttributes = newAtts;
      isReplacing = replacing;
      copy = el.getAttributes().copyAttributes();
    }

    /**
     * Undos the attribute change. The <code>copy</code> field is set as
     * attributes on <code>element</code>.
     */
    public void undo()
    {
      super.undo();
      AttributeSet atts = element.getAttributes();
      if (atts instanceof MutableAttributeSet)
        {
          MutableAttributeSet mutable = (MutableAttributeSet) atts;
          mutable.removeAttributes(atts);
          mutable.addAttributes(copy);
        }
    }

    /**
     * Redos an attribute change. This adds <code>newAttributes</code> to the
     * <code>element</code>'s attribute set, possibly clearing all attributes
     * if <code>isReplacing</code> is true.
     */
    public void redo()
    {
      super.undo();
      AttributeSet atts = element.getAttributes();
      if (atts instanceof MutableAttributeSet)
        {
          MutableAttributeSet mutable = (MutableAttributeSet) atts;
          if (isReplacing)
            mutable.removeAttributes(atts);
          mutable.addAttributes(newAttributes);
        }
    }
  }

  /**
   * Carries specification information for new {@link Element}s that should be
   * created in {@link ElementBuffer}. This allows the parsing process to be
   * decoupled from the <code>Element</code> creation process.
   */
  public static class ElementSpec
  {
    /**
     * This indicates a start tag. This is a possible value for {@link #getType}.
     */
    public static final short StartTagType = 1;

    /**
     * This indicates an end tag. This is a possible value for {@link #getType}.
     */
    public static final short EndTagType = 2;

    /**
     * This indicates a content element. This is a possible value for
     * {@link #getType}.
     */
    public static final short ContentType = 3;

    /**
     * This indicates that the data associated with this spec should be joined
     * with what precedes it. This is a possible value for {@link #getDirection}.
     */
    public static final short JoinPreviousDirection = 4;

    /**
     * This indicates that the data associated with this spec should be joined
     * with what follows it. This is a possible value for {@link #getDirection}.
     */
    public static final short JoinNextDirection = 5;

    /**
     * This indicates that the data associated with this spec should be used to
     * create a new element. This is a possible value for {@link #getDirection}.
     */
    public static final short OriginateDirection = 6;

    /**
     * This indicates that the data associated with this spec should be joined
     * to the fractured element. This is a possible value for
     * {@link #getDirection}.
     */
    public static final short JoinFractureDirection = 7;

    /**
     * The type of the tag.
     */
    short type;

    /**
     * The direction of the tag.
     */
    short direction;

    /**
     * The offset of the content.
     */
    int offset;

    /**
     * The length of the content.
     */
    int length;

    /**
     * The actual content.
     */
    char[] content;

    /**
     * The attributes for the tag.
     */
    AttributeSet attributes;

    /**
     * Creates a new <code>ElementSpec</code> with no content, length or
     * offset. This is most useful for start and end tags.
     * 
     * @param a
     *          the attributes for the element to be created
     * @param type
     *          the type of the tag
     */
    public ElementSpec(AttributeSet a, short type)
    {
      this(a, type, 0);
    }

    /**
     * Creates a new <code>ElementSpec</code> that specifies the length but
     * not the offset of an element. Such <code>ElementSpec</code>s are
     * processed sequentially from a known starting point.
     * 
     * @param a
     *          the attributes for the element to be created
     * @param type
     *          the type of the tag
     * @param len
     *          the length of the element
     */
    public ElementSpec(AttributeSet a, short type, int len)
    {
      this(a, type, null, 0, len);
    }

    /**
     * Creates a new <code>ElementSpec</code> with document content.
     * 
     * @param a
     *          the attributes for the element to be created
     * @param type
     *          the type of the tag
     * @param txt
     *          the actual content
     * @param offs
     *          the offset into the <code>txt</code> array
     * @param len
     *          the length of the element
     */
    public ElementSpec(AttributeSet a, short type, char[] txt, int offs, int len)
    {
      attributes = a;
      this.type = type;
      offset = offs;
      length = len;
      content = txt;
      direction = OriginateDirection;
    }

    /**
     * Sets the type of the element.
     * 
     * @param type
     *          the type of the element to be set
     */
    public void setType(short type)
    {
      this.type = type;
    }

    /**
     * Returns the type of the element.
     * 
     * @return the type of the element
     */
    public short getType()
    {
      return type;
    }

    /**
     * Sets the direction of the element.
     * 
     * @param dir
     *          the direction of the element to be set
     */
    public void setDirection(short dir)
    {
      direction = dir;
    }

    /**
     * Returns the direction of the element.
     * 
     * @return the direction of the element
     */
    public short getDirection()
    {
      return direction;
    }

    /**
     * Returns the attributes of the element.
     * 
     * @return the attributes of the element
     */
    public AttributeSet getAttributes()
    {
      return attributes;
    }

    /**
     * Returns the actual content of the element.
     * 
     * @return the actual content of the element
     */
    public char[] getArray()
    {
      return content;
    }

    /**
     * Returns the offset of the content.
     * 
     * @return the offset of the content
     */
    public int getOffset()
    {
      return offset;
    }

    /**
     * Returns the length of the content.
     * 
     * @return the length of the content
     */
    public int getLength()
    {
      return length;
    }

    /**
     * Returns a String representation of this <code>ElementSpec</code>
     * describing the type, direction and length of this
     * <code>ElementSpec</code>.
     * 
     * @return a String representation of this <code>ElementSpec</code>
     */
    public String toString()
    {
      StringBuilder b = new StringBuilder();
      switch (type)
        {
        case StartTagType:
          b.append("StartTag");
          break;
        case EndTagType:
          b.append("EndTag");
          break;
        case ContentType:
          b.append("Content");
          break;
        default:
          b.append("??");
          break;
        }

      b.append(':');

      switch (direction)
        {
        case JoinPreviousDirection:
          b.append("JoinPrevious");
          break;
        case JoinNextDirection:
          b.append("JoinNext");
          break;
        case OriginateDirection:
          b.append("Originate");
          break;
        case JoinFractureDirection:
          b.append("Fracture");
          break;
        default:
          b.append("??");
          break;
        }

      b.append(':');
      b.append(length);

      return b.toString();
    }
  }

  /**
   * Performs all <em>structural</code> changes to the <code>Element</code>
   * hierarchy.  This class was implemented with much help from the document:
   * http://java.sun.com/products/jfc/tsc/articles/text/element_buffer/index.html.
   */
  public class ElementBuffer implements Serializable
  {
    /** The serialization UID (compatible with JDK1.5). */
    private static final long serialVersionUID = 1688745877691146623L;

    /** The root element of the hierarchy. */
    private Element root;

    /** Holds the offset for structural changes. */
    private int offset;

    /** Holds the end offset for structural changes. */
    private int endOffset;

    /** Holds the length of structural changes. */
    private int length;

    /** Holds the position of the change. */
    private int pos;

    /** Holds the element that was last fractured. */
    private Element lastFractured;
    
    /** True if a fracture was not created during a insertFracture call. */
    private boolean fracNotCreated;

    /**
     * The current position in the element tree. This is used for bulk inserts
     * using ElementSpecs.
     */
    private Stack elementStack;

    /**
     * The ElementChange that describes the latest changes.
     */
    DefaultDocumentEvent documentEvent;

    /**
     * Creates a new <code>ElementBuffer</code> for the specified
     * <code>root</code> element.
     * 
     * @param root
     *          the root element for this <code>ElementBuffer</code>
     */
    public ElementBuffer(Element root)
    {
      this.root = root;
      elementStack = new Stack();
    }

    /**
     * Returns the root element of this <code>ElementBuffer</code>.
     * 
     * @return the root element of this <code>ElementBuffer</code>
     */
    public Element getRootElement()
    {
      return root;
    }

    /**
     * Removes the content. This method sets some internal parameters and
     * delegates the work to {@link #removeUpdate}.
     * 
     * @param offs
     *          the offset from which content is remove
     * @param len
     *          the length of the removed content
     * @param ev
     *          the document event that records the changes
     */
    public void remove(int offs, int len, DefaultDocumentEvent ev)
    {
      if (len == 0)
        return;
      offset = offs;
      length = len;
      pos = offset;
      documentEvent = ev;
      removeUpdate();
    }

    /**
     * Updates the element structure of the document in response to removal of
     * content. It removes the affected {@link Element}s from the document
     * structure.
     */
    protected void removeUpdate()
    {
      int startParagraph = root.getElementIndex(offset);
      int endParagraph = root.getElementIndex(offset + length);
      Element[] empty = new Element[0];
      int removeStart = -1;
      int removeEnd = -1;
      for (int i = startParagraph; i < endParagraph; i++)
        {
          BranchElement paragraph = (BranchElement) root.getElement(i);
          int contentStart = paragraph.getElementIndex(offset);
          int contentEnd = paragraph.getElementIndex(offset + length);
          if (contentStart == paragraph.getStartOffset()
              && contentEnd == paragraph.getEndOffset())
            {
              // In this case we only need to remove the whole paragraph. We
              // do this in one go after this loop and only record the indices
              // here.
              if (removeStart == -1)
                {
                  removeStart = i;
                  removeEnd = i;
                }
              else
                removeEnd = i;
            }
          else
            {
              // In this case we remove a couple of child elements from this
              // paragraph.
              int removeLen = contentEnd - contentStart;
              Element[] removed = new Element[removeLen];
              for (int j = contentStart; j < contentEnd; j++)
                removed[j] = paragraph.getElement(j);
              Edit edit = getEditForParagraphAndIndex(paragraph, contentStart);
              edit.addRemovedElements(removed);
            }
        }
      // Now we remove paragraphs from the root that have been tagged for
      // removal.
      if (removeStart != -1)
        {
          int removeLen = removeEnd - removeStart;
          Element[] removed = new Element[removeLen];
          for (int i = removeStart; i < removeEnd; i++)
            removed[i] = root.getElement(i);
          Edit edit = getEditForParagraphAndIndex((BranchElement) root,
                                                  removeStart);
          edit.addRemovedElements(removed);
        }
    }

    /**
     * Performs the actual work for {@link #change}. The elements at the
     * interval boundaries are split up (if necessary) so that the interval
     * boundaries are located at element boundaries.
     */
    protected void changeUpdate()
    {
      // Split up the element at the start offset if necessary.
      Element el = getCharacterElement(offset);
      Element[] res = split(el, offset, 0, el.getElementIndex(offset));
      BranchElement par = (BranchElement) el.getParentElement();
      int index = par.getElementIndex(offset);
      Edit edit = getEditForParagraphAndIndex(par, index);
      if (res[1] != null)
        {
          Element[] removed;
          Element[] added;
          if (res[0] == null)
            {
              removed = new Element[0];
              added = new Element[] { res[1] };
              index++;
            }
          else
            {
              removed = new Element[] { el };
              added = new Element[] { res[0], res[1] };
            }
          edit.addRemovedElements(removed);

          edit.addAddedElements(added);
        }

      int endOffset = offset + length;
      el = getCharacterElement(endOffset);
      res = split(el, endOffset, 0, el.getElementIndex(endOffset));
      par = (BranchElement) el.getParentElement();
      if (res[0] != null)
        {
          Element[] removed;
          Element[] added;
          if (res[1] == null)
            {
              removed = new Element[0];
              added = new Element[] { res[1] };
            }
          else
            {
              removed = new Element[] { el };
              added = new Element[] { res[0], res[1] };
            }
          edit.addRemovedElements(removed);
          edit.addAddedElements(added);
        }
    }

    /**
     * Modifies the element structure so that the specified interval starts and
     * ends at an element boundary. Content and paragraph elements are split and
     * created as necessary. This also updates the
     * <code>DefaultDocumentEvent</code> to reflect the structural changes.
     * The bulk work is delegated to {@link #changeUpdate()}.
     * 
     * @param offset
     *          the start index of the interval to be changed
     * @param length
     *          the length of the interval to be changed
     * @param ev
     *          the <code>DefaultDocumentEvent</code> describing the change
     */
    public void change(int offset, int length, DefaultDocumentEvent ev)
    {
      if (length == 0)
        return;
      this.offset = offset;
      this.pos = offset;
      this.length = length;
      documentEvent = ev;
      changeUpdate();
    }

    /**
     * Creates and returns a deep clone of the specified <code>clonee</code>
     * with the specified parent as new parent.
     *
     * This method can only clone direct instances of {@link BranchElement}
     * or {@link LeafElement}.
     *
     * @param parent the new parent
     * @param clonee the element to be cloned
     *
     * @return the cloned element with the new parent
     */
    public Element clone(Element parent, Element clonee)
    {
      Element clone = clonee;
      // We can only handle AbstractElements here.
      if (clonee instanceof BranchElement)
        {
          BranchElement branchEl = (BranchElement) clonee;
          BranchElement branchClone =
            new BranchElement(parent, branchEl.getAttributes());
          // Also clone all of the children.
          int numChildren = branchClone.getElementCount();
          Element[] cloneChildren = new Element[numChildren];
          for (int i = 0; i < numChildren; ++i)
            {
              cloneChildren[i] = clone(branchClone,
                                       branchClone.getElement(i));
            }
          branchClone.replace(0, 0, cloneChildren);
          clone = branchClone;
        }
      else if (clonee instanceof LeafElement)
        {
          clone = new LeafElement(parent, clonee.getAttributes(),
                                  clonee.getStartOffset(),
                                  clonee.getEndOffset());
        }
      return clone;
    }

    /**
     * Inserts new <code>Element</code> in the document at the specified
     * position. Most of the work is done by {@link #insertUpdate}, after some
     * fields have been prepared for it.
     * 
     * @param offset
     *          the location in the document at which the content is inserted
     * @param length
     *          the length of the inserted content
     * @param data
     *          the element specifications for the content to be inserted
     * @param ev
     *          the document event that is updated to reflect the structural
     *          changes
     */
    public void insert(int offset, int length, ElementSpec[] data,
                       DefaultDocumentEvent ev)
    {
      if (length == 0)
        return;
      
      this.offset = offset;
      this.pos = offset;
      this.endOffset = offset + length;
      this.length = length;
      documentEvent = ev;
      
      edits.removeAllElements();
      elementStack.removeAllElements();
      lastFractured = null;
      fracNotCreated = false;
      insertUpdate(data);
      // This for loop applies all the changes that were made and updates the
      // DocumentEvent.
      int size = edits.size();
      for (int i = 0; i < size; i++)
        {
          Edit curr = (Edit) edits.get(i);
          BranchElement e = (BranchElement) curr.e;
          Element[] removed = curr.getRemovedElements();
          Element[] added = curr.getAddedElements();
          // FIXME: We probably shouldn't create the empty Element[] in the
          // first place.
          if (removed.length > 0 || added.length > 0)
            {
              if (curr.index + removed.length <= e.getElementCount())
                {
                  e.replace(curr.index, removed.length, added);
                  ElementEdit ee = new ElementEdit(e, curr.index, removed, added);
                  ev.addEdit(ee);
                }
              else
                {
                  System.err.println("WARNING: Tried to replace elements ");
                  System.err.print("beyond boundaries: elementCount: ");
                  System.err.println(e.getElementCount());
                  System.err.print("index: " + curr.index);
                  System.err.println(", removed.length: " + removed.length);
                }
            }
        }
    }

    /**
     * Inserts new content
     * 
     * @param data
     *          the element specifications for the elements to be inserted
     */
    protected void insertUpdate(ElementSpec[] data)
    {
      // Push the root and the paragraph at offset onto the element stack.
      Element current = root;
      int index;
      while (!current.isLeaf())
        {
          index = current.getElementIndex(offset);
          elementStack.push(current);
          current = current.getElement(index);
        }
      
      int i = 0;
      int type = data[0].getType();
      if (type == ElementSpec.ContentType)
        {
          // If the first tag is content we must treat it separately to allow
          // for joining properly to previous Elements and to ensure that
          // no extra LeafElements are erroneously inserted.
          insertFirstContentTag(data);
          pos += data[0].length;
          i = 1;
        }
      else
        {
          createFracture(data);
          i = 0;
        }
      
      // Handle each ElementSpec individually.
      for (; i < data.length; i++)
        {
          BranchElement paragraph = (BranchElement) elementStack.peek();
          switch (data[i].getType())
            {
            case ElementSpec.StartTagType:
              switch (data[i].getDirection())
                {
                case ElementSpec.JoinFractureDirection:
                  // Fracture the tree and ensure the appropriate element
                  // is on top of the stack.
                  fracNotCreated = false;
                  insertFracture(data[i]);
                  if (fracNotCreated)
                    {
                      if (lastFractured != null)
                        elementStack.push(lastFractured.getParentElement());
                      else
                        elementStack.push(paragraph.getElement(0));
                    }
                  break;
                case ElementSpec.JoinNextDirection:
                  // Push the next paragraph element onto the stack so
                  // future insertions are added to it.
                  int ix = paragraph.getElementIndex(pos) + 1;
                  elementStack.push(paragraph.getElement(ix));
                  break;
                default:
                  Element br = null;
                  if (data.length > i + 1)
                    {
                      // leaves will be added to paragraph later
                      int x = 0;
                      if (paragraph.getElementCount() > 0)
                        x = paragraph.getElementIndex(pos) + 1;
                      Edit e = getEditForParagraphAndIndex(paragraph, x);
                      br = (BranchElement) createBranchElement(paragraph,
                                                               data[i].getAttributes());
                      e.added.add(br);
                      elementStack.push(br);
                    }
                  else
                    // need to add leaves to paragraph now
                    br = insertParagraph(paragraph, pos);
                  break;
                }
              break;
            case ElementSpec.EndTagType:
              elementStack.pop();
              break;
            case ElementSpec.ContentType:
              insertContentTag(data[i]);
              offset = pos;
              break;
            }
        }
    }
    
    /**
     * Inserts a new paragraph.
     * 
     * @param par -
     *          the parent
     * @param offset -
     *          the offset
     * @return the new paragraph
     */
    private Element insertParagraph(BranchElement par, int offset)
    {
      int index = par.getElementIndex(offset);
      Element current = par.getElement(index);
      Element[] res = split(current, offset, 0, 0);
      Edit e = getEditForParagraphAndIndex(par, index + 1);
      Element ret;
      if (res[1] != null)
        {
          Element[] removed;
          Element[] added;
          if (res[0] == null)
            {
              removed = new Element[0];
              if (res[1] instanceof BranchElement)
                {
                  added = new Element[] { res[1] };
                  ret = res[1];
                }
              else
                {
                  ret = createBranchElement(par, null);
                  added = new Element[] { ret, res[1] };
                }
              index++;
            }
          else
            {
              removed = new Element[] { current };
              if (res[1] instanceof BranchElement)
                {
                  ret = res[1];
                  added = new Element[] { res[0], res[1] };
                }
              else
                {
                  ret = createBranchElement(par, null);
                  added = new Element[] { res[0], ret, res[1] };
                }
            }

          e.addAddedElements(added);
          e.addRemovedElements(removed);
        }
      else
        {
          ret = createBranchElement(par, null);
          e.addAddedElement(ret);
        }
      return ret;
    }
    
    /**
     * Inserts the first tag into the document.
     * 
     * @param data -
     *          the data to be inserted.
     */
    private void insertFirstContentTag(ElementSpec[] data)
    {
      ElementSpec first = data[0];
      BranchElement paragraph = (BranchElement) elementStack.peek();
      int index = paragraph.getElementIndex(pos);
      Element current = paragraph.getElement(index);
      int newEndOffset = pos + first.length;
      boolean onlyContent = data.length == 1;
      Edit edit = getEditForParagraphAndIndex(paragraph, index);
      switch (first.getDirection())
        {
        case ElementSpec.JoinPreviousDirection:
          if (current.getEndOffset() != newEndOffset && !onlyContent)
            {
              Element newEl1 = createLeafElement(paragraph,
                                                 current.getAttributes(),
                                                 current.getStartOffset(),
                                                 newEndOffset);
              edit.addAddedElement(newEl1);
              edit.addRemovedElement(current);
              offset = newEndOffset;
            }
          break;
        case ElementSpec.JoinNextDirection:
          if (pos != 0)
            {
              Element newEl1 = createLeafElement(paragraph,
                                                 current.getAttributes(),
                                                 current.getStartOffset(),
                                                 pos);
              edit.addAddedElement(newEl1);
              Element next = paragraph.getElement(index + 1);

              if (onlyContent)
                newEl1 = createLeafElement(paragraph, next.getAttributes(),
                                           pos, next.getEndOffset());
              else
                {
                  newEl1 = createLeafElement(paragraph, next.getAttributes(),
                                           pos, newEndOffset);
                  pos = newEndOffset;
                }
              edit.addAddedElement(newEl1);
              edit.addRemovedElement(current);
              edit.addRemovedElement(next);
            }
          break;
        default:
          if (current.getStartOffset() != pos)
            {
              Element newEl = createLeafElement(paragraph,
                                                current.getAttributes(),
                                                current.getStartOffset(),
                                                pos);
              edit.addAddedElement(newEl);
            }
          edit.addRemovedElement(current);
          Element newEl1 = createLeafElement(paragraph, first.getAttributes(),
                                             pos, newEndOffset);
          edit.addAddedElement(newEl1);
          if (current.getEndOffset() != endOffset)
            recreateLeaves(newEndOffset, paragraph, onlyContent);
          else
            offset = newEndOffset;
          break;
        }
    }

    /**
     * Inserts a content element into the document structure.
     * 
     * @param tag -
     *          the element spec
     */
    private void insertContentTag(ElementSpec tag)
    {
      BranchElement paragraph = (BranchElement) elementStack.peek();
      int len = tag.getLength();
      int dir = tag.getDirection();
      AttributeSet tagAtts = tag.getAttributes();
      
      if (dir == ElementSpec.JoinNextDirection)
        {
          int index = paragraph.getElementIndex(pos);
          Element target = paragraph.getElement(index);
          Edit edit = getEditForParagraphAndIndex(paragraph, index);
          
          if (paragraph.getStartOffset() > pos)
            {
              Element first = paragraph.getElement(0);
              Element newEl = createLeafElement(paragraph,
                                                first.getAttributes(), pos,
                                                first.getEndOffset());
              edit.addAddedElement(newEl);
              edit.addRemovedElement(first);
            }
          else if (paragraph.getElementCount() > (index + 1)
                   && (pos == target.getStartOffset() && !target.equals(lastFractured)))
            {
              Element next = paragraph.getElement(index + 1);
              Element newEl = createLeafElement(paragraph,
                                                next.getAttributes(), pos,
                                                next.getEndOffset());
              edit.addAddedElement(newEl);
              edit.addRemovedElement(next);
              edit.addRemovedElement(target);
            }
          else
            {
              BranchElement parent = (BranchElement) paragraph.getParentElement();
              int i = parent.getElementIndex(pos);
              BranchElement next = (BranchElement) parent.getElement(i + 1);
              AttributeSet atts = tag.getAttributes();
              
              if (next != null)
                {
                  Element nextLeaf = next.getElement(0);
                  Edit e = getEditForParagraphAndIndex(next, 0);   
                  Element newEl2 = createLeafElement(next, atts, pos, nextLeaf.getEndOffset());
                  e.addAddedElement(newEl2);
                  e.addRemovedElement(nextLeaf);
                }
            }
        }
      else 
        {
          int end = pos + len;
          Element leaf = createLeafElement(paragraph, tag.getAttributes(), pos, end);
          
          // Check for overlap with other leaves/branches
          if (paragraph.getElementCount() > 0)
            {
              int index = paragraph.getElementIndex(pos);
              Element target = paragraph.getElement(index);
              boolean onlyContent = target.isLeaf();
              
              BranchElement toRec = paragraph;
              if (!onlyContent)
                toRec = (BranchElement) target;

              // Check if we should place the leaf before or after target
              if (pos > target.getStartOffset())
                index++;

              Edit edit = getEditForParagraphAndIndex(paragraph, index);
              edit.addAddedElement(leaf);

              if (end != toRec.getEndOffset())
                {
                  recreateLeaves(end, toRec, onlyContent);
                  
                  if (onlyContent)
                    edit.addRemovedElement(target);
                }
            }
          else
            paragraph.replace(0, 0, new Element[] { leaf });
        }
                            
      pos += len;
    }

    /**
     * This method fractures the child at offset.
     * 
     * @param data
     *          the ElementSpecs used for the entire insertion
     */
    private void createFracture(ElementSpec[] data)
    {
      BranchElement paragraph = (BranchElement) elementStack.peek();
      int index = paragraph.getElementIndex(offset);
      Element child = paragraph.getElement(index);
      Edit edit = getEditForParagraphAndIndex(paragraph, index);
      AttributeSet atts = child.getAttributes();
      
      if (offset != 0)
        {
          Element newEl1 = createLeafElement(paragraph, atts,
                                             child.getStartOffset(), offset);
          edit.addAddedElement(newEl1);
          edit.addRemovedElement(child);
        }
    }

    /**
     * Recreates a specified part of a the tree after a new leaf
     * has been inserted.
     * 
     * @param start - where to start recreating from
     * @param paragraph - the paragraph to recreate
     * @param onlyContent - true if this is the only content
     */
    private void recreateLeaves(int start, BranchElement paragraph, boolean onlyContent)
    {
      int index = paragraph.getElementIndex(start);
      Element child = paragraph.getElement(index);
      AttributeSet atts = child.getAttributes();
      
      if (!onlyContent)
        {
          BranchElement newBranch = (BranchElement) createBranchElement(paragraph,
                                                                        atts);
          Element newLeaf = createLeafElement(newBranch, atts, start, 
                                              child.getEndOffset());
          newBranch.replace(0, 0, new Element[] { newLeaf });
          
          BranchElement parent = (BranchElement) paragraph.getParentElement();
          int parSize = parent.getElementCount();
          Edit edit = getEditForParagraphAndIndex(parent, parSize);
          edit.addAddedElement(newBranch);
            
          int paragraphSize = paragraph.getElementCount();
          Element[] removed = new Element[paragraphSize - (index + 1)];
          int s = 0;
          for (int j = index + 1; j < paragraphSize; j++)
            removed[s++] = paragraph.getElement(j);
          
          edit = getEditForParagraphAndIndex(paragraph, index);
          edit.addRemovedElements(removed);
          Element[] added = recreateAfterFracture(removed, newBranch, 0, child.getEndOffset());
          newBranch.replace(1, 0, added);
          
          lastFractured = newLeaf;
          pos = newBranch.getEndOffset();
        }
      else
        {
          Element newLeaf = createLeafElement(paragraph, atts, start, 
                                              child.getEndOffset());
          Edit edit = getEditForParagraphAndIndex(paragraph, index);
          edit.addAddedElement(newLeaf);
        }
    }
    
    /**
     * Splits an element if <code>offset</code> is not already at its
     * boundary.
     * 
     * @param el
     *          the Element to possibly split
     * @param offset
     *          the offset at which to possibly split
     * @param space
     *          the amount of space to create between the splitted parts
     * @param editIndex 
     *          the index of the edit to use
     * @return An array of elements which represent the split result. This array
     *         has two elements, the two parts of the split. The first element
     *         might be null, which means that the element which should be
     *         splitted can remain in place. The second element might also be
     *         null, which means that the offset is already at an element
     *         boundary and the element doesn't need to be splitted.
     */
    private Element[] split(Element el, int offset, int space, int editIndex)
    {
      // If we are at an element boundary, then return an empty array.
      if ((offset == el.getStartOffset() || offset == el.getEndOffset())
          && space == 0 && el.isLeaf())
        return new Element[2];

      // If the element is an instance of BranchElement, then we
      // recursivly
      // call this method to perform the split.
      Element[] res = new Element[2];
      if (el instanceof BranchElement)
        {
          int index = el.getElementIndex(offset);
          Element child = el.getElement(index);
          Element[] result = split(child, offset, space, editIndex);
          Element[] removed;
          Element[] added;
          Element[] newAdded;

          int count = el.getElementCount();
          if (result[1] != null)
            {
              // This is the case when we can keep the first element.
              if (result[0] == null)
                {
                  removed = new Element[count - index - 1];
                  newAdded = new Element[count - index - 1];
                  added = new Element[] {};

                }
              // This is the case when we may not keep the first
              // element.
              else
                {
                  removed = new Element[count - index];
                  newAdded = new Element[count - index];
                  added = new Element[] { result[0] };
                }
              newAdded[0] = result[1];
              for (int i = index; i < count; i++)
                {
                  Element el2 = el.getElement(i);
                  int ind = i - count + removed.length;
                  removed[ind] = el2;
                  if (ind != 0)
                    newAdded[ind] = el2;
                }
              
              Edit edit = getEditForParagraphAndIndex((BranchElement) el, editIndex);
              edit.addRemovedElements(removed);
              edit.addAddedElements(added);
              
              BranchElement newPar =
                (BranchElement) createBranchElement(el.getParentElement(),
                                                    el.getAttributes());
              newPar.replace(0, 0, newAdded);
              res = new Element[] { null, newPar };
            }
          else
            {
              removed = new Element[count - index];
              for (int i = index; i < count; ++i)
                removed[i - index] = el.getElement(i);
              
              Edit edit = getEditForParagraphAndIndex((BranchElement) el, editIndex);
              edit.addRemovedElements(removed);
              
              BranchElement newPar = (BranchElement) createBranchElement(el.getParentElement(),
                                                                         el.getAttributes());
              newPar.replace(0, 0, removed);
              res = new Element[] { null, newPar };
            }
        }
      else if (el instanceof LeafElement)
        {
          BranchElement par = (BranchElement) el.getParentElement();
          Element el1 = createLeafElement(par, el.getAttributes(),
                                          el.getStartOffset(), offset);

          Element el2 = createLeafElement(par, el.getAttributes(), 
                                          offset + space,
                                          el.getEndOffset());
          res = new Element[] { el1, el2 };
        }
      return res;
    }

    /**
     * Inserts a fracture into the document structure.
     * 
     * @param tag -
     *          the element spec.
     */
    private void insertFracture(ElementSpec tag)
    {
      // insert the fracture at offset.
      BranchElement parent = (BranchElement) elementStack.peek();
      int parentIndex = parent.getElementIndex(pos);
      AttributeSet parentAtts = parent.getAttributes();
      Element toFracture = parent.getElement(parentIndex);
      int parSize = parent.getElementCount();
      Edit edit = getEditForParagraphAndIndex(parent, parentIndex);
      Element frac = toFracture;
      int leftIns = 0;
      int indexOfFrac = toFracture.getElementIndex(pos);
      int size = toFracture.getElementCount();

      // gets the leaf that falls along the fracture
      frac = toFracture.getElement(indexOfFrac);
      while (!frac.isLeaf())
        frac = frac.getElement(frac.getElementIndex(pos));

      AttributeSet atts = frac.getAttributes();
      int fracStart = frac.getStartOffset();
      int fracEnd = frac.getEndOffset();
      if (pos >= fracStart && pos < fracEnd)
        {
          // recreate left-side of branch and all its children before offset
          // add the fractured leaves to the right branch
          BranchElement rightBranch =
            (BranchElement) createBranchElement(parent, parentAtts);
          
          // Check if left branch has already been edited. If so, we only
          // need to create the right branch.
          BranchElement leftBranch = null;
          Element[] added = null;
          if (edit.added.size() > 0 || edit.removed.size() > 0)
            {
              added = new Element[] { rightBranch };
              
              // don't try to remove left part of tree
              parentIndex++;
            }
          else
            {
              leftBranch =
                (BranchElement) createBranchElement(parent, parentAtts);
              added = new Element[] { leftBranch, rightBranch };

              // add fracture to leftBranch
              if (fracStart != pos)
                {
                  Element leftFracturedLeaf =
                    createLeafElement(leftBranch, atts, fracStart, pos);
                  leftBranch.replace(leftIns, 0,
                                     new Element[] { leftFracturedLeaf });
                }
            }

          if (!toFracture.isLeaf())
            {
              // add all non-fracture elements to the branches
              if (indexOfFrac > 0 && leftBranch != null)
                {
                  Element[] add = new Element[indexOfFrac];
                  for (int i = 0; i < indexOfFrac; i++)
                    add[i] = toFracture.getElement(i);
                  leftIns = add.length;
                  leftBranch.replace(0, 0, add);
                }

              int count = size - indexOfFrac - 1;
              if (count > 0)
                {
                  Element[] add = new Element[count];
                  int j = 0;
                  int i = indexOfFrac + 1;
                  while (j < count)
                    add[j++] = toFracture.getElement(i++);
                  rightBranch.replace(0, 0, add);
                }
            }
          
          // add to fracture to rightBranch          
          // Check if we can join the right frac leaf with the next leaf
          int rm = 0;
          int end = fracEnd;
          Element next = rightBranch.getElement(0);
          if (next != null && next.isLeaf()
              && next.getAttributes().isEqual(atts))
            {
              end = next.getEndOffset();
              rm = 1;
            }

          Element rightFracturedLeaf = createLeafElement(rightBranch, atts,
                                                         pos, end);
          rightBranch.replace(0, rm, new Element[] { rightFracturedLeaf });

          // recreate those elements after parentIndex and add/remove all
          // new/old elements to parent
          int remove = parSize - parentIndex;
          Element[] removed = new Element[0];
          Element[] added2 = new Element[0];
          if (remove > 0)
            {
              removed = new Element[remove];
              int s = 0;
              for (int j = parentIndex; j < parSize; j++)
                removed[s++] = parent.getElement(j);
              edit.addRemovedElements(removed);
              added2 = recreateAfterFracture(removed, parent, 1,
                                            rightBranch.getEndOffset());
            }
          
          edit.addAddedElements(added);
          edit.addAddedElements(added2);
          elementStack.push(rightBranch);
          lastFractured = rightFracturedLeaf;
        }
      else
        fracNotCreated = true;
    }

    /**
     * Recreates all the elements from the parent to the element on the top of
     * the stack, starting from startFrom with the starting offset of
     * startOffset.
     * 
     * @param recreate -
     *          the elements to recreate
     * @param parent -
     *          the element to add the new elements to
     * @param startFrom -
     *          where to start recreating from
     * @param startOffset -
     *          the offset of the first element
     * @return the array of added elements         
     */
    private Element[] recreateAfterFracture(Element[] recreate,
                                       BranchElement parent, int startFrom,
                                       int startOffset)
    {
      Element[] added = new Element[recreate.length - startFrom];
      int j = 0;
      for (int i = startFrom; i < recreate.length; i++)
        {
          Element curr = recreate[i];
          int len = curr.getEndOffset() - curr.getStartOffset();
          if (curr instanceof LeafElement)
            added[j] = createLeafElement(parent, curr.getAttributes(),
                                         startOffset, startOffset + len);
          else
            {
              BranchElement br =
                (BranchElement) createBranchElement(parent,
                                                    curr.getAttributes());
              int bSize = curr.getElementCount();
              for (int k = 0; k < bSize; k++)
                {
                  Element bCurr = curr.getElement(k);
                  Element[] add = recreateAfterFracture(new Element[] { bCurr }, br, 0,
                                        startOffset);
                  br.replace(0, 0, add);
                  
                }
              added[j] = br;
            }
          startOffset += len;
          j++;
        }

      return added;
    }
  }

  /**
   * This method looks through the Vector of Edits to see if there is already an
   * Edit object associated with the given paragraph. If there is, then we
   * return it. Otherwise we create a new Edit object, add it to the vector, and
   * return it. Note: this method is package private to avoid accessors.
   * 
   * @param index
   *          the index associated with the Edit we want to create
   * @param para
   *          the paragraph associated with the Edit we want
   * @return the found or created Edit object
   */
  Edit getEditForParagraphAndIndex(BranchElement para, int index)
  {
    Edit curr;
    int size = edits.size();
    for (int i = 0; i < size; i++)
      {
        curr = (Edit) edits.elementAt(i);
        if (curr.e.equals(para))
          return curr;
      }
    curr = new Edit(para, index, null, null);
    edits.add(curr);
    
    return curr;
  }
  /**
   * Instance of all editing information for an object in the Vector. This class
   * is used to add information to the DocumentEvent associated with an
   * insertion/removal/change as well as to store the changes that need to be
   * made so they can be made all at the same (appropriate) time.
   */
  class Edit
  {
    /** The element to edit . */
    Element e;

    /** The index of the change. */
    int index;

    /** The removed elements. */
    Vector removed = new Vector();

    /** The added elements. */
    Vector added = new Vector();

    /**
     * Return an array containing the Elements that have been removed from the
     * paragraph associated with this Edit.
     * 
     * @return an array of removed Elements
     */
    public Element[] getRemovedElements()
    {
      int size = removed.size();
      Element[] removedElements = new Element[size];
      for (int i = 0; i < size; i++)
        removedElements[i] = (Element) removed.elementAt(i);
      return removedElements;
    }

    /**
     * Return an array containing the Elements that have been added to the
     * paragraph associated with this Edit.
     * 
     * @return an array of added Elements
     */
    public Element[] getAddedElements()
    {
      int size = added.size();
      Element[] addedElements = new Element[size];
      for (int i = 0; i < size; i++)
        addedElements[i] = (Element) added.elementAt(i);
      return addedElements;
    }
    
    /** 
     * Checks if e is already in the vector.
     * 
     * @param e - the Element to look for
     * @param v - the vector to search
     * @return true if e is in v.
     */
    private boolean contains(Vector v, Element e)
    {
      if (e == null)
        return false;
      
      int i = v.size();
      for (int j = 0; j < i; j++)
        {
          Element e1 = (Element) v.get(j);
          if ((e1 != null) && (e1.getAttributes().isEqual(e.getAttributes()))
              && (e1.getName().equals(e.getName()))
              && (e1.getStartOffset() == e.getStartOffset())
              && (e1.getEndOffset() == e.getEndOffset())
              && (e1.getParentElement().equals(e.getParentElement()))
              && (e1.getElementCount() == e.getElementCount()))
            return true;
        }
      return false;
    }

    /**
     * Adds one Element to the vector of removed Elements.
     * 
     * @param e
     *          the Element to add
     */
    public void addRemovedElement(Element e)
    {
      if (!contains(removed, e))
        removed.add(e);
    }

    /**
     * Adds each Element in the given array to the vector of removed Elements
     * 
     * @param e
     *          the array containing the Elements to be added
     */
    public void addRemovedElements(Element[] e)
    {
      if (e == null || e.length == 0)
        return;
      for (int i = 0; i < e.length; i++)
        {
          if (!contains(removed, e[i]))
            removed.add(e[i]);
        }
    }

    /**
     * Adds one Element to the vector of added Elements.
     * 
     * @param e
     *          the Element to add
     */
    public void addAddedElement(Element e)
    {
      if (!contains(added, e))
        added.add(e);
    }

    /**
     * Adds each Element in the given array to the vector of added Elements.
     * 
     * @param e
     *          the array containing the Elements to be added
     */
    public void addAddedElements(Element[] e)
    {
      if (e == null || e.length == 0)
        return;
      for (int i = 0; i < e.length; i++)
        {
          if (!contains(added, e[i]))
            added.add(e[i]);
        }
    }

    /**
     * Creates a new Edit object with the given parameters
     * 
     * @param e
     *          the paragraph Element associated with this Edit
     * @param i
     *          the index within the paragraph where changes are started
     * @param removed
     *          an array containing Elements that should be removed from the
     *          paragraph Element
     * @param added
     *          an array containing Elements that should be added to the
     *          paragraph Element
     */
    public Edit(Element e, int i, Element[] removed, Element[] added)
    {
      this.e = e;
      this.index = i;
      addRemovedElements(removed);
      addAddedElements(added);
    }
  }

  /**
   * An element type for sections. This is a simple BranchElement with a unique
   * name.
   */
  protected class SectionElement extends BranchElement
  {
    /**
     * Creates a new SectionElement.
     */
    public SectionElement()
    {
      super(null, null);
    }

    /**
     * Returns the name of the element. This method always returns
     * &quot;section&quot;.
     * 
     * @return the name of the element
     */
    public String getName()
    {
      return SectionElementName;
    }
  }

  /**
   * Receives notification when any of the document's style changes and calls
   * {@link DefaultStyledDocument#styleChanged(Style)}.
   * 
   * @author Roman Kennke (kennke@aicas.com)
   */
  private class StyleChangeListener implements ChangeListener
  {

    /**
     * Receives notification when any of the document's style changes and calls
     * {@link DefaultStyledDocument#styleChanged(Style)}.
     * 
     * @param event
     *          the change event
     */
    public void stateChanged(ChangeEvent event)
    {
      Style style = (Style) event.getSource();
      styleChanged(style);
    }
  }

  /** The serialization UID (compatible with JDK1.5). */
  private static final long serialVersionUID = 940485415728614849L;

  /**
   * The default size to use for new content buffers.
   */
  public static final int BUFFER_SIZE_DEFAULT = 4096;

  /**
   * The <code>EditorBuffer</code> that is used to manage to
   * <code>Element</code> hierarchy.
   */
  protected DefaultStyledDocument.ElementBuffer buffer;

  /**
   * Listens for changes on this document's styles and notifies styleChanged().
   */
  private StyleChangeListener styleChangeListener;

  /**
   * Vector that contains all the edits. Maybe replace by a HashMap.
   */
  Vector edits = new Vector();

  /**
   * Creates a new <code>DefaultStyledDocument</code>.
   */
  public DefaultStyledDocument()
  {
    this(new GapContent(BUFFER_SIZE_DEFAULT), new StyleContext());
  }

  /**
   * Creates a new <code>DefaultStyledDocument</code> that uses the specified
   * {@link StyleContext}.
   * 
   * @param context
   *          the <code>StyleContext</code> to use
   */
  public DefaultStyledDocument(StyleContext context)
  {
    this(new GapContent(BUFFER_SIZE_DEFAULT), context);
  }

  /**
   * Creates a new <code>DefaultStyledDocument</code> that uses the specified
   * {@link StyleContext} and {@link Content} buffer.
   * 
   * @param content
   *          the <code>Content</code> buffer to use
   * @param context
   *          the <code>StyleContext</code> to use
   */
  public DefaultStyledDocument(AbstractDocument.Content content,
                               StyleContext context)
  {
    super(content, context);
    buffer = new ElementBuffer(createDefaultRoot());
    setLogicalStyle(0, context.getStyle(StyleContext.DEFAULT_STYLE));
  }

  /**
   * Adds a style into the style hierarchy. Unspecified style attributes can be
   * resolved in the <code>parent</code> style, if one is specified. While it
   * is legal to add nameless styles (<code>nm == null</code),
   * you must be aware that the client application is then responsible
   * for managing the style hierarchy, since unnamed styles cannot be
   * looked up by their name.
   *
   * @param nm the name of the style or <code>null</code> if the style should
   *           be unnamed
   * @param parent the parent in which unspecified style attributes are
   *           resolved, or <code>null</code> if that is not necessary
   *
   * @return the newly created <code>Style</code>
   */
  public Style addStyle(String nm, Style parent)
  {
    StyleContext context = (StyleContext) getAttributeContext();
    Style newStyle = context.addStyle(nm, parent);

    // Register change listener.
    if (styleChangeListener == null)
      styleChangeListener = new StyleChangeListener();
    newStyle.addChangeListener(styleChangeListener);

    return newStyle;
  }

  /**
   * Create the default root element for this kind of <code>Document</code>.
   * 
   * @return the default root element for this kind of <code>Document</code>
   */
  protected AbstractDocument.AbstractElement createDefaultRoot()
  {
    Element[] tmp;
    SectionElement section = new SectionElement();

    BranchElement paragraph = new BranchElement(section, null);
    tmp = new Element[1];
    tmp[0] = paragraph;
    section.replace(0, 0, tmp);

    Element leaf = new LeafElement(paragraph, null, 0, 1);
    tmp = new Element[1];
    tmp[0] = leaf;
    paragraph.replace(0, 0, tmp);

    return section;
  }

  /**
   * Returns the <code>Element</code> that corresponds to the character at the
   * specified position.
   * 
   * @param position
   *          the position of which we query the corresponding
   *          <code>Element</code>
   * @return the <code>Element</code> that corresponds to the character at the
   *         specified position
   */
  public Element getCharacterElement(int position)
  {
    Element element = getDefaultRootElement();

    while (!element.isLeaf())
      {
        int index = element.getElementIndex(position);
        element = element.getElement(index);
      }

    return element;
  }

  /**
   * Extracts a background color from a set of attributes.
   * 
   * @param attributes
   *          the attributes from which to get a background color
   * @return the background color that correspond to the attributes
   */
  public Color getBackground(AttributeSet attributes)
  {
    StyleContext context = (StyleContext) getAttributeContext();
    return context.getBackground(attributes);
  }

  /**
   * Returns the default root element.
   * 
   * @return the default root element
   */
  public Element getDefaultRootElement()
  {
    return buffer.getRootElement();
  }

  /**
   * Extracts a font from a set of attributes.
   * 
   * @param attributes
   *          the attributes from which to get a font
   * @return the font that correspond to the attributes
   */
  public Font getFont(AttributeSet attributes)
  {
    StyleContext context = (StyleContext) getAttributeContext();
    return context.getFont(attributes);
  }

  /**
   * Extracts a foreground color from a set of attributes.
   * 
   * @param attributes
   *          the attributes from which to get a foreground color
   * @return the foreground color that correspond to the attributes
   */
  public Color getForeground(AttributeSet attributes)
  {
    StyleContext context = (StyleContext) getAttributeContext();
    return context.getForeground(attributes);
  }

  /**
   * Returns the logical <code>Style</code> for the specified position.
   * 
   * @param position
   *          the position from which to query to logical style
   * @return the logical <code>Style</code> for the specified position
   */
  public Style getLogicalStyle(int position)
  {
    Element paragraph = getParagraphElement(position);
    AttributeSet attributes = paragraph.getAttributes();
    AttributeSet a = attributes.getResolveParent();
    // If the resolve parent is not of type Style, we return null.
    if (a instanceof Style)
      return (Style) a;
    return null;
  }

  /**
   * Returns the paragraph element for the specified position. If the position
   * is outside the bounds of the document's root element, then the closest
   * element is returned. That is the last paragraph if
   * <code>position >= endIndex</code> or the first paragraph if
   * <code>position < startIndex</code>.
   * 
   * @param position
   *          the position for which to query the paragraph element
   * @return the paragraph element for the specified position
   */
  public Element getParagraphElement(int position)
  {
    Element e = getDefaultRootElement();
    while (!e.isLeaf())
      e = e.getElement(e.getElementIndex(position));

    if (e != null)
      return e.getParentElement();
    return e;
  }

  /**
   * Looks up and returns a named <code>Style</code>.
   * 
   * @param nm
   *          the name of the <code>Style</code>
   * @return the found <code>Style</code> of <code>null</code> if no such
   *         <code>Style</code> exists
   */
  public Style getStyle(String nm)
  {
    StyleContext context = (StyleContext) getAttributeContext();
    return context.getStyle(nm);
  }

  /**
   * Removes a named <code>Style</code> from the style hierarchy.
   * 
   * @param nm
   *          the name of the <code>Style</code> to be removed
   */
  public void removeStyle(String nm)
  {
    StyleContext context = (StyleContext) getAttributeContext();
    context.removeStyle(nm);
  }

  /**
   * Sets text attributes for the fragment specified by <code>offset</code>
   * and <code>length</code>.
   * 
   * @param offset
   *          the start offset of the fragment
   * @param length
   *          the length of the fragment
   * @param attributes
   *          the text attributes to set
   * @param replace
   *          if <code>true</code>, the attributes of the current selection
   *          are overridden, otherwise they are merged
   */
  public void setCharacterAttributes(int offset, int length,
                                     AttributeSet attributes, boolean replace)
  {
    // Exit early if length is 0, so no DocumentEvent is created or fired.
    if (length == 0)
      return;
    try
      {
        // Must obtain a write lock for this method. writeLock() and
        // writeUnlock() should always be in try/finally block to make
        // sure that locking happens in a balanced manner.
        writeLock();
        DefaultDocumentEvent ev = new DefaultDocumentEvent(offset,
                                                           length,
                                                           DocumentEvent.EventType.CHANGE);

        // Modify the element structure so that the interval begins at an
        // element
        // start and ends at an element end.
        buffer.change(offset, length, ev);

        Element root = getDefaultRootElement();
        // Visit all paragraph elements within the specified interval
        int end = offset + length;
        Element curr;
        for (int pos = offset; pos < end;)
          {
            // Get the CharacterElement at offset pos.
            curr = getCharacterElement(pos);
            if (pos == curr.getEndOffset())
              break;

            MutableAttributeSet a = (MutableAttributeSet) curr.getAttributes();
            ev.addEdit(new AttributeUndoableEdit(curr, attributes, replace));
            // If replace is true, remove all the old attributes.
            if (replace)
              a.removeAttributes(a);
            // Add all the new attributes.
            a.addAttributes(attributes);
            // Increment pos so we can check the next CharacterElement.
            pos = curr.getEndOffset();
          }
        fireChangedUpdate(ev);
        fireUndoableEditUpdate(new UndoableEditEvent(this, ev));
      }
    finally
      {
        writeUnlock();
      }
  }

  /**
   * Sets the logical style for the paragraph at the specified position.
   * 
   * @param position
   *          the position at which the logical style is added
   * @param style
   *          the style to set for the current paragraph
   */
  public void setLogicalStyle(int position, Style style)
  {
    Element el = getParagraphElement(position);
    // getParagraphElement doesn't return null but subclasses might so
    // we check for null here.
    if (el == null)
      return;
    try
      {
        writeLock();
        if (el instanceof AbstractElement)
          {
            AbstractElement ael = (AbstractElement) el;
            ael.setResolveParent(style);
            int start = el.getStartOffset();
            int end = el.getEndOffset();
            DefaultDocumentEvent ev = new DefaultDocumentEvent(start,
                                                               end - start,
                                                               DocumentEvent.EventType.CHANGE);
            fireChangedUpdate(ev);
            fireUndoableEditUpdate(new UndoableEditEvent(this, ev));
          }
        else
          throw new AssertionError(
                                   "paragraph elements are expected to be"
                                       + "instances of AbstractDocument.AbstractElement");
      }
    finally
      {
        writeUnlock();
      }
  }

  /**
   * Sets text attributes for the paragraph at the specified fragment.
   * 
   * @param offset
   *          the beginning of the fragment
   * @param length
   *          the length of the fragment
   * @param attributes
   *          the text attributes to set
   * @param replace
   *          if <code>true</code>, the attributes of the current selection
   *          are overridden, otherwise they are merged
   */
  public void setParagraphAttributes(int offset, int length,
                                     AttributeSet attributes, boolean replace)
  {
    try
      {
        // Must obtain a write lock for this method. writeLock() and
        // writeUnlock() should always be in try/finally blocks to make
        // sure that locking occurs in a balanced manner.
        writeLock();

        // Create a DocumentEvent to use for changedUpdate().
        DefaultDocumentEvent ev = new DefaultDocumentEvent(offset,
                                                           length,
                                                           DocumentEvent.EventType.CHANGE);

        // Have to iterate through all the _paragraph_ elements that are
        // contained or partially contained in the interval
        // (offset, offset + length).
        Element rootElement = getDefaultRootElement();
        int startElement = rootElement.getElementIndex(offset);
        int endElement = rootElement.getElementIndex(offset + length - 1);
        if (endElement < startElement)
          endElement = startElement;

        for (int i = startElement; i <= endElement; i++)
          {
            Element par = rootElement.getElement(i);
            MutableAttributeSet a = (MutableAttributeSet) par.getAttributes();
            // Add the change to the DocumentEvent.
            ev.addEdit(new AttributeUndoableEdit(par, attributes, replace));
            // If replace is true remove the old attributes.
            if (replace)
              a.removeAttributes(a);
            // Add the new attributes.
            a.addAttributes(attributes);
          }
        fireChangedUpdate(ev);
        fireUndoableEditUpdate(new UndoableEditEvent(this, ev));
      }
    finally
      {
        writeUnlock();
      }
  }

  /**
   * Called in response to content insert actions. This is used to update the
   * element structure.
   * 
   * @param ev
   *          the <code>DocumentEvent</code> describing the change
   * @param attr
   *          the attributes for the change
   */
  protected void insertUpdate(DefaultDocumentEvent ev, AttributeSet attr)
  {
    super.insertUpdate(ev, attr);
    // If the attribute set is null, use an empty attribute set.
    if (attr == null)
      attr = SimpleAttributeSet.EMPTY;
    int offset = ev.getOffset();
    int length = ev.getLength();
    int endOffset = offset + length;
    AttributeSet paragraphAttributes = getParagraphElement(endOffset).getAttributes();
    Segment txt = new Segment();
    try
      {
        getText(offset, length, txt);
      }
    catch (BadLocationException ex)
      {
        AssertionError ae = new AssertionError("Unexpected bad location");
        ae.initCause(ex);
        throw ae;
      }

    int len = 0;
    Vector specs = new Vector();
    ElementSpec finalStartTag = null;
    short finalStartDirection = ElementSpec.OriginateDirection;
    boolean prevCharWasNewline = false;
    Element prev = getCharacterElement(offset);
    Element next = getCharacterElement(endOffset);
    Element prevParagraph = getParagraphElement(offset);
    Element paragraph = getParagraphElement(endOffset);

    int segmentEnd = txt.offset + txt.count;

    // Check to see if we're inserting immediately after a newline.
    if (offset > 0)
      {
        try
          {
            String s = getText(offset - 1, 1);
            if (s.equals("\n"))
              {
                finalStartDirection = handleInsertAfterNewline(specs, offset,
                                                               endOffset,
                                                               prevParagraph,
                                                               paragraph,
                                                               paragraphAttributes);

                prevCharWasNewline = true;
                // Find the final start tag from the ones just created.
                for (int i = 0; i < specs.size(); i++)
                  if (((ElementSpec) specs.get(i)).getType() == ElementSpec.StartTagType)
                    finalStartTag = (ElementSpec) specs.get(i);
              }
          }
        catch (BadLocationException ble)
          {
            // This shouldn't happen.
            AssertionError ae = new AssertionError();
            ae.initCause(ble);
            throw ae;
          }
      }

    for (int i = txt.offset; i < segmentEnd; ++i)
      {
        len++;
        if (txt.array[i] == '\n')
          {
            // Add the ElementSpec for the content.
            specs.add(new ElementSpec(attr, ElementSpec.ContentType, len));

            // Add ElementSpecs for the newline.
            specs.add(new ElementSpec(null, ElementSpec.EndTagType));
            finalStartTag = new ElementSpec(paragraphAttributes,
                                            ElementSpec.StartTagType);
            specs.add(finalStartTag);
            len = 0;
          }
      }

    // Create last element if last character hasn't been a newline.
    if (len > 0)
      specs.add(new ElementSpec(attr, ElementSpec.ContentType, len));

    // Set the direction of the last spec of type StartTagType.
    // If we are inserting after a newline then this value comes from
    // handleInsertAfterNewline.
    if (finalStartTag != null)
      {
        if (prevCharWasNewline)
          finalStartTag.setDirection(finalStartDirection);
        else if (prevParagraph.getEndOffset() != endOffset)
          finalStartTag.setDirection(ElementSpec.JoinFractureDirection);
        else
          {
            // If there is an element AFTER this one, then set the
            // direction to JoinNextDirection.
            Element parent = prevParagraph.getParentElement();
            int index = parent.getElementIndex(offset);
            if (index + 1 < parent.getElementCount()
                && !parent.getElement(index + 1).isLeaf())
              finalStartTag.setDirection(ElementSpec.JoinNextDirection);
          }
      }

    // If we are at the last index, then check if we could probably be
    // joined with the next element.
    // This means:
    // - we must be a ContentTag
    // - if there is a next Element, we must have the same attributes
    // - if there is no next Element, but one will be created,
    // we must have the same attributes as the higher-level run.
    ElementSpec last = (ElementSpec) specs.lastElement();
    if (last.getType() == ElementSpec.ContentType)
      {
        Element currentRun = prevParagraph.getElement(prevParagraph.getElementIndex(offset));
        if (currentRun.getEndOffset() == endOffset)
          {
            if (endOffset < getLength() && next.getAttributes().isEqual(attr)
                && last.getType() == ElementSpec.ContentType)
              last.setDirection(ElementSpec.JoinNextDirection);
          }
        else
          {
            if (finalStartTag != null
                && finalStartTag.getDirection() == ElementSpec.JoinFractureDirection
                && currentRun.getAttributes().isEqual(attr))
              {
                last.setDirection(ElementSpec.JoinNextDirection);
              }
          }
      }

    // If we are at the first new element, then check if it could be
    // joined with the previous element.
    ElementSpec first = (ElementSpec) specs.firstElement();
    if (prev.getAttributes().isEqual(attr)
        && first.getType() == ElementSpec.ContentType)
      first.setDirection(ElementSpec.JoinPreviousDirection);

    ElementSpec[] elSpecs = (ElementSpec[]) specs.toArray(new ElementSpec[specs.size()]);
    buffer.insert(offset, length, elSpecs, ev);
  }

  /**
   * A helper method to set up the ElementSpec buffer for the special case of an
   * insertion occurring immediately after a newline.
   * 
   * @param specs
   *          the ElementSpec buffer to initialize.
   */
  short handleInsertAfterNewline(Vector specs, int offset, int endOffset,
                                 Element prevParagraph, Element paragraph,
                                 AttributeSet a)
  {
    if (prevParagraph.getParentElement() == paragraph.getParentElement())
      {
        specs.add(new ElementSpec(a, ElementSpec.EndTagType));
        specs.add(new ElementSpec(a, ElementSpec.StartTagType));
        if (paragraph.getStartOffset() != endOffset)
          return ElementSpec.JoinFractureDirection;
        // If there is an Element after this one, use JoinNextDirection.
        Element parent = paragraph.getParentElement();
        if (parent.getElementCount() > (parent.getElementIndex(offset) + 1))
          return ElementSpec.JoinNextDirection;
      }
    return ElementSpec.OriginateDirection;
  }

  /**
   * Updates the document structure in response to text removal. This is
   * forwarded to the {@link ElementBuffer} of this document. Any changes to the
   * document structure are added to the specified document event and sent to
   * registered listeners.
   * 
   * @param ev
   *          the document event that records the changes to the document
   */
  protected void removeUpdate(DefaultDocumentEvent ev)
  {
    super.removeUpdate(ev);
    buffer.remove(ev.getOffset(), ev.getLength(), ev);
  }

  /**
   * Returns an enumeration of all style names.
   * 
   * @return an enumeration of all style names
   */
  public Enumeration getStyleNames()
  {
    StyleContext context = (StyleContext) getAttributeContext();
    return context.getStyleNames();
  }

  /**
   * Called when any of this document's styles changes.
   * 
   * @param style
   *          the style that changed
   */
  protected void styleChanged(Style style)
  {
    // Nothing to do here. This is intended to be overridden by subclasses.
  }

  /**
   * Inserts a bulk of structured content at once.
   * 
   * @param offset
   *          the offset at which the content should be inserted
   * @param data
   *          the actual content spec to be inserted
   */
  protected void insert(int offset, ElementSpec[] data)
      throws BadLocationException
  {
    if (data == null || data.length == 0)
      return;
    try
      {
        // writeLock() and writeUnlock() should always be in a try/finally
        // block so that locking balance is guaranteed even if some
        // exception is thrown.
        writeLock();

        // First we collect the content to be inserted.
        StringBuffer contentBuffer = new StringBuffer();
        for (int i = 0; i < data.length; i++)
          {
            // Collect all inserts into one so we can get the correct
            // ElementEdit
            ElementSpec spec = data[i];
            if (spec.getArray() != null && spec.getLength() > 0)
              contentBuffer.append(spec.getArray(), spec.getOffset(),
                                   spec.getLength());
          }

        int length = contentBuffer.length();

        // If there was no content inserted then exit early.
        if (length == 0)
          return;

        UndoableEdit edit = content.insertString(offset,
                                                 contentBuffer.toString());

        // Create the DocumentEvent with the ElementEdit added
        DefaultDocumentEvent ev = new DefaultDocumentEvent(offset,
                                                           length,
                                                           DocumentEvent.EventType.INSERT);
        ev.addEdit(edit);

        // Finally we must update the document structure and fire the insert
        // update event.
        buffer.insert(offset, length, data, ev);
        fireInsertUpdate(ev);
        fireUndoableEditUpdate(new UndoableEditEvent(this, ev));
      }
    finally
      {
        writeUnlock();
      }
  }

  /**
   * Initializes the <code>DefaultStyledDocument</code> with the specified
   * data.
   * 
   * @param data
   *          the specification of the content with which the document is
   *          initialized
   */
  protected void create(ElementSpec[] data)
  {
    writeLock();
    try
      {
        // Clear content if there is some.
        int len = getLength();
        if (len > 0)
          remove(0, len);

        // Now we insert the content.
        StringBuilder b = new StringBuilder();
        for (int i = 0; i < data.length; ++i)
          {
            ElementSpec el = data[i];
            if (el.getArray() != null && el.getLength() > 0)
              b.append(el.getArray(), el.getOffset(), el.getLength());
          }
        Content content = getContent();
        UndoableEdit cEdit = content.insertString(0, b.toString());

        DefaultDocumentEvent ev =
          new DefaultDocumentEvent(0, b.length(),
                                   DocumentEvent.EventType.INSERT);
        ev.addEdit(cEdit);

        // We do a little trick here to get the new structure: We instantiate
        // a new ElementBuffer with a new root element, insert into that root
        // and then reparent the newly created elements to the old root
        // element.
        BranchElement createRoot =
          (BranchElement) createBranchElement(null, null);
        Element dummyLeaf = createLeafElement(createRoot, null, 0, 1);
        createRoot.replace(0, 0, new Element[]{ dummyLeaf });
        ElementBuffer createBuffer = new ElementBuffer(createRoot);
        createBuffer.insert(0, b.length(), data, new DefaultDocumentEvent(0, b.length(), DocumentEvent.EventType.INSERT));
        // Now the new root is the first child of the createRoot.
        Element newRoot = createRoot.getElement(0);
        BranchElement root = (BranchElement) getDefaultRootElement();
        Element[] added = new Element[newRoot.getElementCount()];
        for (int i = 0; i < added.length; ++i)
          {
            added[i] = newRoot.getElement(i);
            ((AbstractElement) added[i]).element_parent = root;
          }
        Element[] removed = new Element[root.getElementCount()];
        for (int i = 0; i < removed.length; ++i)
          removed[i] = root.getElement(i);

        // Replace the old elements in root with the new and update the event.
        root.replace(0, removed.length, added);
        ev.addEdit(new ElementEdit(root, 0, removed, added));

        fireInsertUpdate(ev);
        fireUndoableEditUpdate(new UndoableEditEvent(this, ev));
      }
    catch (BadLocationException ex)
      {
        AssertionError err = new AssertionError("Unexpected bad location");
        err.initCause(ex);
        throw err;
      }
    finally
      {
        writeUnlock();
      }
  }
}
