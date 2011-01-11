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

import gnu.java.lang.CPStringBuilder;

import java.awt.Color;
import java.awt.Font;
import java.io.Serializable;
import java.util.ArrayList;
import java.util.Enumeration;
import java.util.Iterator;
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
      CPStringBuilder b = new CPStringBuilder();
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
      ArrayList removed = new ArrayList();

      /** The added elements. */
      ArrayList added = new ArrayList();

      /**
       * Indicates if this edit contains a fracture.
       */
      boolean isFracture;

      /**
       * Creates a new Edit for the specified element at index i.
       *
       * @param el the element
       * @param i the index
       */
      Edit(Element el, int i)
      {
        this(el, i, false);
      }

      /**
       * Creates a new Edit for the specified element at index i.
       *
       * @param el the element
       * @param i the index
       * @param frac if this is a fracture edit or not
       */
      Edit(Element el, int i, boolean frac)
      {
        e = el;
        index = i;
        isFracture = frac;
      }

    }

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

    /**
     * The parent of the fracture.
     */
    private Element fracturedParent;

    /**
     * The fractured child.
     */
    private Element fracturedChild;

    /**
     * Indicates if a fracture has been created.
     */
    private boolean createdFracture;

    /**
     * The current position in the element tree. This is used for bulk inserts
     * using ElementSpecs.
     */
    private Stack elementStack;

    private Edit[] insertPath;

    private boolean recreateLeafs;

    /**
     * Vector that contains all the edits. Maybe replace by a HashMap.
     */
    private ArrayList edits;

    private boolean offsetLastIndex;
    private boolean offsetLastIndexReplace;

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
      prepareEdit(offs, len);
      removeUpdate();
      finishEdit(ev);
    }

    /**
     * Updates the element structure of the document in response to removal of
     * content. It removes the affected {@link Element}s from the document
     * structure.
     */
    protected void removeUpdate()
    {
      removeElements(root, offset, endOffset);
    }

    private boolean removeElements(Element elem, int rmOffs0, int rmOffs1)
    {
      boolean ret = false;
      if (! elem.isLeaf())
        {
          // Update stack for changes.
          int index0 = elem.getElementIndex(rmOffs0);
          int index1 = elem.getElementIndex(rmOffs1);
          elementStack.push(new Edit(elem, index0));
          Edit ec = (Edit) elementStack.peek();

          // If the range is contained by one element,
          // we just forward the request
          if (index0 == index1)
            {
              Element child0 = elem.getElement(index0);
              if(rmOffs0 <= child0.getStartOffset()
                  && rmOffs1 >= child0.getEndOffset())
                {
                  // Element totally removed.
                  ec.removed.add(child0);
                }
              else if (removeElements(child0, rmOffs0, rmOffs1))
                {
                  ec.removed.add(child0);
                }
            }
          else
            {
              // The removal range spans elements.  If we can join
              // the two endpoints, do it.  Otherwise we remove the
              // interior and forward to the endpoints.
              Element child0 = elem.getElement(index0);
              Element child1 = elem.getElement(index1);
              boolean containsOffs1 = (rmOffs1 < elem.getEndOffset());
          if (containsOffs1 && canJoin(child0, child1))
            {
              // Remove and join.
              for (int i = index0; i <= index1; i++)
                {
                  ec.removed.add(elem.getElement(i));
                }
              Element e = join(elem, child0, child1, rmOffs0, rmOffs1);
              ec.added.add(e);
            }
          else
            {
              // Remove interior and forward.
              int rmIndex0 = index0 + 1;
              int rmIndex1 = index1 - 1;
              if (child0.getStartOffset() == rmOffs0
                  || (index0 == 0 && child0.getStartOffset() > rmOffs0
                      && child0.getEndOffset() <= rmOffs1))
                {
                  // Start element completely consumed.
                  child0 = null;
                  rmIndex0 = index0;
                }
              if (! containsOffs1)
                {
                  child1 = null;
                  rmIndex1++;
              }
              else if (child1.getStartOffset() == rmOffs1)
                {
                  // End element not touched.
                  child1 = null;
                }
              if (rmIndex0 <= rmIndex1)
                {
                  ec.index = rmIndex0;
                }
              for (int i = rmIndex0; i <= rmIndex1; i++)
                {
                  ec.removed.add(elem.getElement(i));
                }
              if (child0 != null)
                {
                  if(removeElements(child0, rmOffs0, rmOffs1))
                    {
                      ec.removed.add(0, child0);
                      ec.index = index0;
                    }
                }
              if (child1 != null)
                {
                  if(removeElements(child1, rmOffs0, rmOffs1))
                    {
                      ec.removed.add(child1);
                    }
                }
            }
            }

          // Perform changes.
          pop();

          // Return true if we no longer have any children.
          if(elem.getElementCount() == (ec.removed.size() - ec.added.size()))
            ret = true;
        }
      return ret;
    }

    /**
     * Creates a document in response to a call to
     * {@link DefaultStyledDocument#create(ElementSpec[])}.
     *
     * @param len the length of the inserted text
     * @param data the specs for the elements
     * @param ev the document event
     */
    void create(int len, ElementSpec[] data, DefaultDocumentEvent ev)
    {
      prepareEdit(offset, len);
      Element el = root;
      int index = el.getElementIndex(0);
      while (! el.isLeaf())
        {
          Element child = el.getElement(index);
          Edit edit = new Edit(el, index, false);
          elementStack.push(edit);
          el = child;
          index = el.getElementIndex(0);
        }
      Edit ed = (Edit) elementStack.peek();
      Element child = ed.e.getElement(ed.index);
      ed.added.add(createLeafElement(ed.e, child.getAttributes(), getLength(),
                                     child.getEndOffset()));
      ed.removed.add(child);
      while (elementStack.size() > 1)
        pop();
      int n = data.length;

      // Reset root element's attributes.
      AttributeSet newAtts = null;
      if (n > 0 && data[0].getType() == ElementSpec.StartTagType)
        newAtts = data[0].getAttributes();
      if (newAtts == null)
        newAtts = SimpleAttributeSet.EMPTY;
      MutableAttributeSet mAtts = (MutableAttributeSet) root.getAttributes();
      ev.addEdit(new AttributeUndoableEdit(root, newAtts, true));
      mAtts.removeAttributes(mAtts);
      mAtts.addAttributes(newAtts);

      // Insert the specified elements.
      for (int i = 1; i < n; i++)
        insertElement(data[i]);

      // Pop remaining stack.
      while (elementStack.size() > 0)
        pop();

      finishEdit(ev);
    }

    private boolean canJoin(Element e0, Element e1)
    {
      boolean ret = false;
      if ((e0 != null) && (e1 != null))
        {
          // Don't join a leaf to a branch.
          boolean isLeaf0 = e0.isLeaf();
          boolean isLeaf1 = e1.isLeaf();
          if(isLeaf0 == isLeaf1)
            {
              if (isLeaf0)
                {
                  // Only join leaves if the attributes match, otherwise
                  // style information will be lost.
                  ret = e0.getAttributes().isEqual(e1.getAttributes());
                }
              else
                {
                  // Only join non-leafs if the names are equal. This may result
                  // in loss of style information, but this is typically
                  // acceptable for non-leafs.
                  String name0 = e0.getName();
                  String name1 = e1.getName();
                  if (name0 != null)
                    ret = name0.equals(name1);
                  else if (name1 != null)
                    ret = name1.equals(name0);
                  else // Both names null.
                    ret = true;
                }
            }
        }
      return ret;
    }

    private Element join(Element p, Element left, Element right, int rmOffs0,
                         int rmOffs1)
    {
      Element joined = null;
      if (left.isLeaf() && right.isLeaf())
        {
          joined = createLeafElement(p, left.getAttributes(),
                                     left.getStartOffset(),
                                     right.getEndOffset());
        }
      else if ((! left.isLeaf()) && (! right.isLeaf()))
        {
          // Join two branch elements.  This copies the children before
          // the removal range on the left element, and after the removal
          // range on the right element.  The two elements on the edge
          // are joined if possible and needed.
          joined = createBranchElement(p, left.getAttributes());
          int ljIndex = left.getElementIndex(rmOffs0);
          int rjIndex = right.getElementIndex(rmOffs1);
          Element lj = left.getElement(ljIndex);
          if (lj.getStartOffset() >= rmOffs0)
            {
              lj = null;
            }
          Element rj = right.getElement(rjIndex);
          if (rj.getStartOffset() == rmOffs1)
            {
              rj = null;
            }
          ArrayList children = new ArrayList();
          // Transfer the left.
          for (int i = 0; i < ljIndex; i++)
            {
              children.add(clone(joined, left.getElement(i)));
            }

          // Transfer the join/middle.
          if (canJoin(lj, rj))
            {
              Element e = join(joined, lj, rj, rmOffs0, rmOffs1);
              children.add(e);
            }
          else
            {
              if (lj != null)
                {
                  children.add(cloneAsNecessary(joined, lj, rmOffs0, rmOffs1));
                }
              if (rj != null)
                {
                  children.add(cloneAsNecessary(joined, rj, rmOffs0, rmOffs1));
                }
            }

          // Transfer the right.
          int n = right.getElementCount();
          for (int i = (rj == null) ? rjIndex : rjIndex + 1; i < n; i++)
            {
              children.add(clone(joined, right.getElement(i)));
            }

          // Install the children.
          Element[] c = new Element[children.size()];
          c = (Element[]) children.toArray(c);
          ((BranchElement) joined).replace(0, 0, c);
        }
      else
        {
          assert false : "Must not happen";
        }
      return joined;
    }

    /**
     * Performs the actual work for {@link #change}. The elements at the
     * interval boundaries are split up (if necessary) so that the interval
     * boundaries are located at element boundaries.
     */
    protected void changeUpdate()
    {
      boolean didEnd = split(offset, length);
      if (! didEnd)
        {
          // need to do the other end
          while (elementStack.size() != 0)
            {
              pop();
            }
          split(offset + length, 0);
        }
      while (elementStack.size() != 0)
        {
          pop();
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
      prepareEdit(offset, length);
      changeUpdate();
      finishEdit(ev);
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

    private Element cloneAsNecessary(Element parent, Element clonee,
                                     int rmOffs0, int rmOffs1)
    {
      Element cloned;
      if (clonee.isLeaf())
        {
          cloned = createLeafElement(parent, clonee.getAttributes(),
                                     clonee.getStartOffset(),
                                     clonee.getEndOffset());
        }
      else
        {
          Element e = createBranchElement(parent, clonee.getAttributes());
          int n = clonee.getElementCount();
          ArrayList childrenList = new ArrayList(n);
          for (int i = 0; i < n; i++)
            {
              Element elem = clonee.getElement(i);
              if (elem.getStartOffset() < rmOffs0
                  || elem.getEndOffset() > rmOffs1)
                {
                  childrenList.add(cloneAsNecessary(e, elem, rmOffs0,
                                                    rmOffs1));
                }
            }
          Element[] children = new Element[childrenList.size()];
          children = (Element[]) childrenList.toArray(children);
          ((BranchElement) e).replace(0, 0, children);
          cloned = e;
        }
      return cloned;
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
      if (length > 0)
        {
          prepareEdit(offset, length);
          insertUpdate(data);
          finishEdit(ev);
        }
    }

    /**
     * Prepares the state of this object for performing an insert.
     *
     * @param offset the offset at which is inserted
     * @param length the length of the inserted region
     */
    private void prepareEdit(int offset, int length)
    {
      this.offset = offset;
      this.pos = offset;
      this.endOffset = offset + length;
      this.length = length;

      if (edits == null)
        edits = new ArrayList();
      else
        edits.clear();

      if (elementStack == null)
        elementStack = new Stack();
      else
        elementStack.clear();

      fracturedParent = null;
      fracturedChild = null;
      offsetLastIndex = false;
      offsetLastIndexReplace = false;
    }

    /**
     * Finishes an insert. This applies all changes and updates
     * the DocumentEvent.
     *
     * @param ev the document event
     */
    private void finishEdit(DefaultDocumentEvent ev)
    {
      // This for loop applies all the changes that were made and updates the
      // DocumentEvent.
      for (Iterator i = edits.iterator(); i.hasNext();)
        {
          Edit edits = (Edit) i.next();
          Element[] removed = new Element[edits.removed.size()];
          removed = (Element[]) edits.removed.toArray(removed);
          Element[] added = new Element[edits.added.size()];
          added = (Element[]) edits.added.toArray(added);
          int index = edits.index;
          BranchElement parent = (BranchElement) edits.e;
          parent.replace(index, removed.length, added);
          ElementEdit ee = new ElementEdit(parent, index, removed, added);
          ev.addEdit(ee);
        }
      edits.clear();
      elementStack.clear();
    }

    /**
     * Inserts new content.
     *
     * @param data the element specifications for the elements to be inserted
     */
    protected void insertUpdate(ElementSpec[] data)
    {
      // Push the current path to the stack.
      Element current = root;
      int index = current.getElementIndex(offset);
      while (! current.isLeaf())
        {
          Element child = current.getElement(index);
          int editIndex = child.isLeaf() ? index : index + 1;
          Edit edit = new Edit(current, editIndex);
          elementStack.push(edit);
          current = child;
          index = current.getElementIndex(offset);
        }

      // Create a copy of the original path.
      insertPath = new Edit[elementStack.size()];
      insertPath = (Edit[]) elementStack.toArray(insertPath);

      // No fracture yet.
      createdFracture = false;

      // Insert first content tag.
      int i = 0;
      recreateLeafs = false;
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
          insertElement(data[i]);
        }

      // Fracture if we haven't done yet.
      if (! createdFracture)
        fracture(-1);

      // Pop the remaining stack.
      while (elementStack.size() != 0)
        pop();

      // Offset last index if necessary.
      if (offsetLastIndex && offsetLastIndexReplace)
        insertPath[insertPath.length - 1].index++;

      // Make sure we havea an Edit for each path item that has a change.
      for (int p = insertPath.length - 1; p >= 0; p--)
        {
          Edit edit = insertPath[p];
          if (edit.e == fracturedParent)
            edit.added.add(fracturedChild);
          if ((edit.added.size() > 0 || edit.removed.size() > 0)
              && ! edits.contains(edit))
            edits.add(edit);
        }

      // Remove element that would be created by an insert at 0 with
      // an initial end tag.
      if (offset == 0 && fracturedParent != null
          && data[0].getType() == ElementSpec.EndTagType)
        {
          int p;
          for (p = 0;
               p < data.length && data[p].getType() == ElementSpec.EndTagType;
               p++)
            ;

          Edit edit = insertPath[insertPath.length - p - 1];
          edit.index--;
          edit.removed.add(0, edit.e.getElement(edit.index));
        }
    }

    private void pop()
    {
      Edit edit = (Edit) elementStack.peek();
      elementStack.pop();
      if ((edit.added.size() > 0) || (edit.removed.size() > 0))
        {
          edits.add(edit);
        }
      else if (! elementStack.isEmpty())
        {
          Element e = edit.e;
          if (e.getElementCount() == 0)
            {
              // If we pushed a branch element that didn't get
              // used, make sure its not marked as having been added.
              edit = (Edit) elementStack.peek();
              edit.added.remove(e);
          }
      }
    }

    private void insertElement(ElementSpec spec)
    {
      if (elementStack.isEmpty())
        return;

      Edit edit = (Edit) elementStack.peek();
      switch (spec.getType())
        {
        case ElementSpec.StartTagType:
          switch (spec.getDirection())
            {
            case ElementSpec.JoinFractureDirection:
              // Fracture the tree and ensure the appropriate element
              // is on top of the stack.
              if (! createdFracture)
                {
                  fracture(elementStack.size() - 1);
                }
              if (! edit.isFracture)
                {
                  // If the parent isn't a fracture, then the fracture is
                  // in fracturedChild.
                  Edit newEdit = new Edit(fracturedChild, 0, true);
                  elementStack.push(newEdit);
                }
              else
                {
                  // Otherwise use the parent's first child.
                  Element el = edit.e.getElement(0);
                  Edit newEdit = new Edit(el, 0, true);
                  elementStack.push(newEdit);
                }
              break;
            case ElementSpec.JoinNextDirection:
              // Push the next paragraph element onto the stack so
              // future insertions are added to it.
              Element parent = edit.e.getElement(edit.index);
              if (parent.isLeaf())
                {
                  if (edit.index + 1 < edit.e.getElementCount())
                    parent = edit.e.getElement(edit.index + 1);
                  else
                    assert false; // Must not happen.
                }
              elementStack.push(new Edit(parent, 0, true));
              break;
            default:
              Element branch = createBranchElement(edit.e,
                                                   spec.getAttributes());
              edit.added.add(branch);
              elementStack.push(new Edit(branch, 0));
              break;
            }
          break;
        case ElementSpec.EndTagType:
          pop();
          break;
        case ElementSpec.ContentType:
          insertContentTag(spec, edit);
          break;
        }
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
      Edit edit = (Edit) elementStack.peek();
      Element current = edit.e.getElement(edit.index);
      int firstEndOffset = offset + first.length;
      boolean onlyContent = data.length == 1;
      switch (first.getDirection())
        {
        case ElementSpec.JoinPreviousDirection:
          if (current.getEndOffset() != firstEndOffset && ! onlyContent)
            {
              Element newEl1 = createLeafElement(edit.e,
                                                 current.getAttributes(),
                                                 current.getStartOffset(),
                                                 firstEndOffset);
              edit.added.add(newEl1);
              edit.removed.add(current);
              if (current.getEndOffset() != endOffset)
                recreateLeafs = true;
              else
                offsetLastIndex = true;
            }
          else
            {
              offsetLastIndex = true;
              offsetLastIndexReplace = true;
            }
          break;
        case ElementSpec.JoinNextDirection:
          if (offset != 0)
            {
              Element newEl1 = createLeafElement(edit.e,
                                                 current.getAttributes(),
                                                 current.getStartOffset(),
                                                 offset);
              edit.added.add(newEl1);
              Element next = edit.e.getElement(edit.index + 1);
              if (onlyContent)
                newEl1 = createLeafElement(edit.e, next.getAttributes(),
                                           offset, next.getEndOffset());
              else
                {
                  newEl1 = createLeafElement(edit.e, next.getAttributes(),
                                             offset, firstEndOffset);
                }
              edit.added.add(newEl1);
              edit.removed.add(current);
              edit.removed.add(next);
            }
          break;
        default: // OriginateDirection.
          if (current.getStartOffset() != offset)
            {
              Element newEl = createLeafElement(edit.e,
                                                current.getAttributes(),
                                                current.getStartOffset(),
                                                offset);
              edit.added.add(newEl);
            }
          edit.removed.add(current);
          Element newEl1 = createLeafElement(edit.e, first.getAttributes(),
                                             offset, firstEndOffset);
          edit.added.add(newEl1);
          if (current.getEndOffset() != endOffset)
            recreateLeafs = true;
          else
            offsetLastIndex = true;
          break;
        }
    }

    /**
     * Inserts a content element into the document structure.
     *
     * @param tag -
     *          the element spec
     */
    private void insertContentTag(ElementSpec tag, Edit edit)
    {
      int len = tag.getLength();
      int dir = tag.getDirection();
      if (dir == ElementSpec.JoinNextDirection)
        {
          if (! edit.isFracture)
            {
              Element first = null;
              if (insertPath != null)
                {
                  for (int p = insertPath.length - 1; p >= 0; p--)
                    {
                      if (insertPath[p] == edit)
                        {
                          if (p != insertPath.length - 1)
                            first = edit.e.getElement(edit.index);
                          break;
                        }
                    }
                }
              if (first == null)
                first = edit.e.getElement(edit.index + 1);
              Element leaf = createLeafElement(edit.e, first.getAttributes(),
                                               pos, first.getEndOffset());
              edit.added.add(leaf);
              edit.removed.add(first);
            }
          else
            {
              Element first = edit.e.getElement(0);
              Element leaf = createLeafElement(edit.e, first.getAttributes(),
                                               pos, first.getEndOffset());
              edit.added.add(leaf);
              edit.removed.add(first);
            }
        }
      else
        {
          Element leaf = createLeafElement(edit.e, tag.getAttributes(), pos,
                                           pos + len);
          edit.added.add(leaf);
        }

      pos += len;

    }

    /**
     * This method fractures bottomost leaf in the elementStack. This
     * happens when the first inserted tag is not content.
     *
     * @param data
     *          the ElementSpecs used for the entire insertion
     */
    private void createFracture(ElementSpec[] data)
    {
      Edit edit = (Edit) elementStack.peek();
      Element child = edit.e.getElement(edit.index);
      if (offset != 0)
        {
          Element newChild = createLeafElement(edit.e, child.getAttributes(),
                                               child.getStartOffset(), offset);
          edit.added.add(newChild);
        }
      edit.removed.add(child);
      if (child.getEndOffset() != endOffset)
        recreateLeafs = true;
      else
        offsetLastIndex = true;
    }

    private void fracture(int depth)
    {
      int len = insertPath.length;
      int lastIndex = -1;
      boolean recreate = recreateLeafs;
      Edit lastEdit = insertPath[len - 1];
      boolean childChanged = lastEdit.index + 1 < lastEdit.e.getElementCount();
      int deepestChangedIndex = recreate ? len : - 1;
      int lastChangedIndex = len - 1;
      createdFracture = true;
      for (int i = len - 2; i >= 0; i--)
        {
          Edit edit = insertPath[i];
          if (edit.added.size() > 0 || i == depth)
            {
              lastIndex = i;
              if (! recreate && childChanged)
                {
                  recreate = true;
                  if (deepestChangedIndex == -1)
                    deepestChangedIndex = lastChangedIndex + 1;
                }
            }
          if (! childChanged && edit.index < edit.e.getElementCount())
            {
              childChanged = true;
              lastChangedIndex = i;
            }
        }
      if (recreate)
        {
          if (lastIndex == -1)
            lastIndex = len - 1;
          recreate(lastIndex, deepestChangedIndex);
        }
    }

    private void recreate(int startIndex, int endIndex)
    {
      // Recreate the element representing the inserted index.
      Edit edit = insertPath[startIndex];
      Element child;
      Element newChild;
      int changeLength = insertPath.length;

      if (startIndex + 1 == changeLength)
        child = edit.e.getElement(edit.index);
      else
        child = edit.e.getElement(edit.index - 1);

      if(child.isLeaf())
        {
          newChild = createLeafElement(edit.e, child.getAttributes(),
                                   Math.max(endOffset, child.getStartOffset()),
                                   child.getEndOffset());
        }
      else
        {
          newChild = createBranchElement(edit.e, child.getAttributes());
        }
      fracturedParent = edit.e;
      fracturedChild = newChild;

      // Recreate all the elements to the right of the insertion point.
      Element parent = newChild;
      while (++startIndex < endIndex)
        {
          boolean isEnd = (startIndex + 1) == endIndex;
          boolean isEndLeaf = (startIndex + 1) == changeLength;

          // Create the newChild, a duplicate of the elment at
          // index. This isn't done if isEnd and offsetLastIndex are true
          // indicating a join previous was done.
          edit = insertPath[startIndex];

          // Determine the child to duplicate, won't have to duplicate
          // if at end of fracture, or offseting index.
          if(isEnd)
            {
              if(offsetLastIndex || ! isEndLeaf)
                child = null;
              else
                child = edit.e.getElement(edit.index);
            }
          else
            {
              child = edit.e.getElement(edit.index - 1);
            }

          // Duplicate it.
          if(child != null)
            {
              if(child.isLeaf())
                {
                  newChild = createLeafElement(parent, child.getAttributes(),
                                   Math.max(endOffset, child.getStartOffset()),
                                   child.getEndOffset());
                }
              else
                {
                  newChild = createBranchElement(parent,
                                                 child.getAttributes());
                }
            }
          else
            newChild = null;

        // Recreate the remaining children (there may be none).
        int childrenToMove = edit.e.getElementCount() - edit.index;
        Element[] children;
        int moveStartIndex;
        int childStartIndex = 1;

        if (newChild == null)
          {
            // Last part of fracture.
            if (isEndLeaf)
              {
                childrenToMove--;
                moveStartIndex = edit.index + 1;
              }
            else
              {
                moveStartIndex = edit.index;
              }
            childStartIndex = 0;
            children = new Element[childrenToMove];
          }
        else
          {
            if (! isEnd)
              {
                // Branch.
                childrenToMove++;
                moveStartIndex = edit.index;
            }
            else
              {
                // Last leaf, need to recreate part of it.
                moveStartIndex = edit.index + 1;
              }
            children = new Element[childrenToMove];
            children[0] = newChild;
        }

        for (int c = childStartIndex; c < childrenToMove; c++)
          {
            Element toMove = edit.e.getElement(moveStartIndex++);
            children[c] = recreateFracturedElement(parent, toMove);
            edit.removed.add(toMove);
          }
        ((BranchElement) parent).replace(0, 0, children);
        parent = newChild;
      }

    }

    private Element recreateFracturedElement(Element parent, Element toCopy)
    {
      Element recreated;
      if(toCopy.isLeaf())
        {
          recreated = createLeafElement(parent, toCopy.getAttributes(),
                                  Math.max(toCopy.getStartOffset(), endOffset),
                                  toCopy.getEndOffset());
        }
      else
        {
          Element newParent = createBranchElement(parent,
                                                  toCopy.getAttributes());
          int childCount = toCopy.getElementCount();
          Element[] newChildren = new Element[childCount];
          for (int i = 0; i < childCount; i++)
            {
              newChildren[i] = recreateFracturedElement(newParent,
                                                        toCopy.getElement(i));
            }
          ((BranchElement) newParent).replace(0, 0, newChildren);
          recreated = newParent;
        }
      return recreated;
    }

    private boolean split(int offs, int len)
    {
      boolean splitEnd = false;
      // Push the path to the stack.
      Element e = root;
      int index = e.getElementIndex(offs);
      while (! e.isLeaf())
        {
          elementStack.push(new Edit(e, index));
          e = e.getElement(index);
          index = e.getElementIndex(offs);
        }

      Edit ec = (Edit) elementStack.peek();
      Element child = ec.e.getElement(ec.index);
      // Make sure there is something to do. If the
      // offset is already at a boundary then there is
      // nothing to do.
      if (child.getStartOffset() < offs && offs < child.getEndOffset())
        {
          // We need to split, now see if the other end is within
          // the same parent.
          int index0 = ec.index;
          int index1 = index0;
          if (((offs + len) < ec.e.getEndOffset()) && (len != 0))
            {
              // It's a range split in the same parent.
              index1 = ec.e.getElementIndex(offs+len);
              if (index1 == index0)
                {
                  // It's a three-way split.
                  ec.removed.add(child);
                  e = createLeafElement(ec.e, child.getAttributes(),
                                        child.getStartOffset(), offs);
                  ec.added.add(e);
                  e = createLeafElement(ec.e, child.getAttributes(),
                                        offs, offs + len);
                  ec.added.add(e);
                  e = createLeafElement(ec.e, child.getAttributes(),
                                        offs + len, child.getEndOffset());
                  ec.added.add(e);
                  return true;
                }
              else
                {
                  child = ec.e.getElement(index1);
                  if ((offs + len) == child.getStartOffset())
                    {
                      // End is already on a boundary.
                      index1 = index0;
                    }
                }
              splitEnd = true;
            }

          // Split the first location.
          pos = offs;
          child = ec.e.getElement(index0);
          ec.removed.add(child);
          e = createLeafElement(ec.e, child.getAttributes(),
                                child.getStartOffset(), pos);
          ec.added.add(e);
          e = createLeafElement(ec.e, child.getAttributes(),
                                pos, child.getEndOffset());
          ec.added.add(e);

          // Pick up things in the middle.
          for (int i = index0 + 1; i < index1; i++)
            {
              child = ec.e.getElement(i);
              ec.removed.add(child);
              ec.added.add(child);
            }

          if (index1 != index0)
            {
              child = ec.e.getElement(index1);
              pos = offs + len;
              ec.removed.add(child);
              e = createLeafElement(ec.e, child.getAttributes(),
                                    child.getStartOffset(), pos);
              ec.added.add(e);
              e = createLeafElement(ec.e, child.getAttributes(),
                                    pos, child.getEndOffset());

              ec.added.add(e);
            }
        }
      return splitEnd;

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
    int offs = ev.getOffset();
    int len = ev.getLength();
    int endOffs = offs + len;
    if (attr == null)
      attr = SimpleAttributeSet.EMPTY;

    // Paragraph attributes are fetched from the point _after_ the insertion.
    Element paragraph = getParagraphElement(endOffs);
    AttributeSet pAttr = paragraph.getAttributes();
    // Character attributes are fetched from the actual insertion point.
    Element paragraph2 = getParagraphElement(offs);
    int contIndex = paragraph2.getElementIndex(offs);
    Element content = paragraph2.getElement(contIndex);
    AttributeSet cAttr = content.getAttributes();

    boolean insertAtBoundary = content.getEndOffset() == endOffs;
    try
      {
        Segment s = new Segment();
        ArrayList buf = new ArrayList();
        ElementSpec lastStartTag = null;
        boolean insertAfterNewline = false;
        short lastStartDir = ElementSpec.OriginateDirection;

        // Special handle if we are inserting after a newline.
        if (offs > 0)
          {
            getText(offs - 1, 1, s);
            if (s.array[s.offset] == '\n')
              {
                insertAfterNewline = true;
                lastStartDir = insertAfterNewline(paragraph, paragraph2,
                                                  pAttr, buf, offs,
                                                  endOffs);
                // Search last start tag.
                for (int i = buf.size() - 1; i >= 0 && lastStartTag == null;
                     i--)
                  {
                    ElementSpec tag = (ElementSpec) buf.get(i);
                    if (tag.getType() == ElementSpec.StartTagType)
                      {
                        lastStartTag = tag;
                      }
                  }
              }

          }

        // If we are not inserting after a newline, the paragraph attributes
        // come from the paragraph under the insertion point.
        if (! insertAfterNewline)
          pAttr = paragraph2.getAttributes();

        // Scan text and build up the specs.
        getText(offs, len, s);
        int end = s.offset + s.count;
        int last = s.offset;
        for (int i = s.offset; i < end; i++)
          {
            if (s.array[i] == '\n')
              {
                int breakOffs = i + 1;
                buf.add(new ElementSpec(attr, ElementSpec.ContentType,
                                        breakOffs - last));
                buf.add(new ElementSpec(null, ElementSpec.EndTagType));
                lastStartTag = new ElementSpec(pAttr,
                                               ElementSpec.StartTagType);
                buf.add(lastStartTag);
                last = breakOffs;
              }
          }

        // Need to add a tailing content tag if we didn't finish at a boundary.
        if (last < end)
          {
            buf.add(new ElementSpec(attr, ElementSpec.ContentType,
                                    end - last));
          }

        // Now we need to fix up the directions of the specs.
        ElementSpec first = (ElementSpec) buf.get(0);
        int doclen = getLength();

        // Maybe join-previous the first tag if it is content and has
        // the same attributes as the previous character run.
        if (first.getType() == ElementSpec.ContentType && cAttr.isEqual(attr))
          first.setDirection(ElementSpec.JoinPreviousDirection);

        // Join-fracture or join-next the last start tag if necessary.
        if (lastStartTag != null)
          {
            if (insertAfterNewline)
              lastStartTag.setDirection(lastStartDir);
            else if (paragraph2.getEndOffset() != endOffs)
              lastStartTag.setDirection(ElementSpec.JoinFractureDirection);
            else
              {
                Element par = paragraph2.getParentElement();
                int par2Index = par.getElementIndex(offs);
                if (par2Index + 1 < par.getElementCount()
                    && ! par.getElement(par2Index + 1).isLeaf())
                  lastStartTag.setDirection(ElementSpec.JoinNextDirection);
              }
          }

        // Join-next last tag if possible.
        if (insertAtBoundary && endOffs < doclen)
          {
            ElementSpec lastTag = (ElementSpec) buf.get(buf.size() - 1);
            if (lastTag.getType() == ElementSpec.ContentType
                && ((lastStartTag == null
                     && (paragraph == paragraph2 || insertAfterNewline))
                    || (lastStartTag != null
             && lastStartTag.getDirection() != ElementSpec.OriginateDirection)))
              {
                int nextIndex = paragraph.getElementIndex(endOffs);
                Element nextRun = paragraph.getElement(nextIndex);
                if (nextRun.isLeaf() && attr.isEqual(nextRun.getAttributes()))
                  lastTag.setDirection(ElementSpec.JoinNextDirection);
              }
          }

        else if (! insertAtBoundary && lastStartTag != null
           && lastStartTag.getDirection() == ElementSpec.JoinFractureDirection)
          {
            ElementSpec lastTag = (ElementSpec) buf.get(buf.size() - 1);
            if (lastTag.getType() == ElementSpec.ContentType
                && lastTag.getDirection() != ElementSpec.JoinPreviousDirection
                && attr.isEqual(cAttr))
              {
                lastTag.setDirection(ElementSpec.JoinNextDirection);
              }
          }

        ElementSpec[] specs = new ElementSpec[buf.size()];
        specs = (ElementSpec[]) buf.toArray(specs);
        buffer.insert(offs, len, specs, ev);
      }
    catch (BadLocationException ex)
      {
        // Ignore this. Comment out for debugging.
        ex.printStackTrace();
      }
    super.insertUpdate(ev, attr);
  }

  private short insertAfterNewline(Element par1, Element par2,
                                   AttributeSet attr, ArrayList buf,
                                   int offs, int endOffs)
  {
    short dir = 0;
    if (par1.getParentElement() == par2.getParentElement())
      {
        ElementSpec tag = new ElementSpec(attr, ElementSpec.EndTagType);
        buf.add(tag);
        tag = new ElementSpec(attr, ElementSpec.StartTagType);
        buf.add(tag);
        if (par2.getEndOffset() != endOffs)
          dir = ElementSpec.JoinFractureDirection;
        else
          {
            Element par = par2.getParentElement();
            if (par.getElementIndex(offs) + 1 < par.getElementCount())
              dir = ElementSpec.JoinNextDirection;
          }
      }
    else
      {
        // For text with more than 2 levels, find the common parent of
        // par1 and par2.
        ArrayList parentsLeft = new ArrayList();
        ArrayList parentsRight = new ArrayList();
        Element e = par2;
        while (e != null)
          {
            parentsLeft.add(e);
            e = e.getParentElement();
          }
        e = par1;
        int leftIndex = -1;
        while (e != null && (leftIndex = parentsLeft.indexOf(e)) == 1)
          {
            parentsRight.add(e);
            e = e.getParentElement();
          }

        if (e != null)

          {
            // e is now the common parent.
            // Insert the end tags.
            for (int c = 0; c < leftIndex; c++)
              {
                buf.add(new ElementSpec(null, ElementSpec.EndTagType));
              }
            // Insert the start tags.
            for (int c = parentsRight.size() - 1; c >= 0; c--)
              {
                Element el = (Element) parentsRight.get(c);
                ElementSpec tag = new ElementSpec(el.getAttributes(),
                                                  ElementSpec.StartTagType);
                if (c > 0)
                  tag.setDirection(ElementSpec.JoinNextDirection);
                buf.add(tag);
              }
            if (parentsRight.size() > 0)
              dir = ElementSpec.JoinNextDirection;
            else
              dir = ElementSpec.JoinFractureDirection;
          }
        else
          assert false;
      }
    return dir;
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
  public Enumeration<?> getStyleNames()
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
        CPStringBuilder contentBuffer = new CPStringBuilder();
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

        Content c = getContent();
        UndoableEdit edit = c.insertString(offset,
                                           contentBuffer.toString());

        // Create the DocumentEvent with the ElementEdit added
        DefaultDocumentEvent ev = new DefaultDocumentEvent(offset,
                                                           length,
                                                           DocumentEvent.EventType.INSERT);

        ev.addEdit(edit);

        // Finally we must update the document structure and fire the insert
        // update event.
        buffer.insert(offset, length, data, ev);

        super.insertUpdate(ev, null);

        ev.end();
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
    try
      {

        // Clear content if there is some.
        int len = getLength();
        if (len > 0)
          remove(0, len);

        writeLock();

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

        len = b.length();
        DefaultDocumentEvent ev =
          new DefaultDocumentEvent(0, b.length(),
                                   DocumentEvent.EventType.INSERT);
        ev.addEdit(cEdit);

        buffer.create(len, data, ev);

        // For the bidi update.
        super.insertUpdate(ev, null);

        ev.end();
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
