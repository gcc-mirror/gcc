/* JTextComponent.java -- 
   Copyright (C) 2002 Free Software Foundation, Inc.

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

import java.awt.AWTEvent;
import java.awt.Component;
import java.awt.Dimension;
import java.awt.Image;
import java.awt.Insets;
import java.awt.Rectangle;
import java.awt.Point;
import javax.accessibility.Accessible;
import javax.accessibility.AccessibleContext;
import javax.accessibility.AccessibleRole;
import javax.accessibility.AccessibleStateSet;
import javax.accessibility.AccessibleText;
import javax.swing.Icon;
import javax.swing.JComponent;
import javax.swing.KeyStroke;
import javax.swing.Scrollable;
import javax.swing.UIManager;
import javax.swing.event.CaretEvent;
import javax.swing.event.CaretListener;
import javax.swing.event.DocumentEvent;
import javax.swing.event.DocumentListener;
import javax.swing.plaf.TextUI;

public abstract class JTextComponent extends JComponent
  implements Scrollable, Accessible
{
//    public class AccessibleJTextComponent extends AccessibleJComponent
//      implements AccessibleText, CaretListener, DocumentListener,
//                 AccessibleAction, AccessibleEditableText
//    {
//    } // class AccessibleJTextComponent

	/**
	 * AccessibleJTextComponent
	 */
	public class AccessibleJTextComponent extends AccessibleJComponent
		implements AccessibleText, CaretListener, DocumentListener {

		//-------------------------------------------------------------
		// Variables --------------------------------------------------
		//-------------------------------------------------------------

		/**
		 * caretPos
		 */
		int caretPos;


		//-------------------------------------------------------------
		// Initialization ---------------------------------------------
		//-------------------------------------------------------------

		/**
		 * Constructor AccessibleJTextComponent
		 * @param component TODO
		 */
		public AccessibleJTextComponent(JTextComponent component) {
			super(component);
			// TODO
		} // AccessibleJTextComponent()


		//-------------------------------------------------------------
		// Methods ----------------------------------------------------
		//-------------------------------------------------------------

		/**
		 * getCaretPosition
		 * @returns int
		 */
		public int getCaretPosition() {
			return 0; // TODO
		} // getCaretPosition()

		/**
		 * getSelectedText
		 * @returns String
		 */
		public String getSelectedText() {
			return null; // TODO
		} // getSelectedText()

		/**
		 * getSelectionStart
		 * @returns int
		 */
		public int getSelectionStart() {
			return 0; // TODO
		} // getSelectionStart()

		/**
		 * getSelectionEnd
		 * @returns int
		 */
		public int getSelectionEnd() {
			return 0; // TODO
		} // getSelectionEnd()

		/**
		 * caretUpdate
		 * @param value0 TODO
		 */
		public void caretUpdate(CaretEvent value0) {
			// TODO
		} // caretUpdate()

		/**
		 * getAccessibleStateSet
		 * @returns AccessibleStateSet
		 */
		public AccessibleStateSet getAccessibleStateSet() {
			return null; // TODO
		} // getAccessibleStateSet()

		/**
		 * getAccessibleRole
		 * @returns AccessibleRole
		 */
		public AccessibleRole getAccessibleRole() {
			return null; // TODO
		} // getAccessibleRole()

		/**
		 * getAccessibleText
		 * @returns AccessibleText
		 */
		public AccessibleText getAccessibleText() {
			return null; // TODO
		} // getAccessibleText()

		/**
		 * insertUpdate
		 * @param value0 TODO
		 */
		public void insertUpdate(DocumentEvent value0) {
			// TODO
		} // insertUpdate()

		/**
		 * removeUpdate
		 * @param value0 TODO
		 */
		public void removeUpdate(DocumentEvent value0) {
			// TODO
		} // removeUpdate()

		/**
		 * changedUpdate
		 * @param value0 TODO
		 */
		public void changedUpdate(DocumentEvent value0) {
			// TODO
		} // changedUpdate()

		/**
		 * getIndexAtPoint
		 * @param value0 TODO
		 * @returns int
		 */
		public int getIndexAtPoint(Point value0) {
			return 0; // TODO
		} // getIndexAtPoint()

		/**
		 * getRootEditorRect
		 * @returns Rectangle
		 */
		Rectangle getRootEditorRect() {
			return null; // TODO
		} // getRootEditorRect()

		/**
		 * getCharacterBounds
		 * @param value0 TODO
		 * @returns Rectangle
		 */
		public Rectangle getCharacterBounds(int value0) {
			return null; // TODO
		} // getCharacterBounds()

		/**
		 * getCharCount
		 * @returns int
		 */
		public int getCharCount() {
			return 0; // TODO
		} // getCharCount()

		/**
		 * getCharacterAttribute
		 * @param value0 TODO
		 * @returns AttributeSet
		 */
		public AttributeSet getCharacterAttribute(int value0) {
			return null; // TODO
		} // getCharacterAttribute()

		/**
		 * getAtIndex
		 * @param value0 TODO
		 * @param value1 TODO
		 * @returns String
		 */
		public String getAtIndex(int value0, int value1) {
			return null; // TODO
		} // getAtIndex()

		/**
		 * getAfterIndex
		 * @param value0 TODO
		 * @param value1 TODO
		 * @returns String
		 */
		public String getAfterIndex(int value0, int value1) {
			return null; // TODO
		} // getAfterIndex()

		/**
		 * getBeforeIndex
		 * @param value0 TODO
		 * @param value1 TODO
		 * @returns String
		 */
		public String getBeforeIndex(int value0, int value1) {
			return null; // TODO
		} // getBeforeIndex()


	} // AccessibleJTextComponent



  public static class KeyBinding
  {
    public KeyStroke key;
    public String actionName;
    public KeyBinding(KeyStroke key, String actionName)
    {
      this.key = key;
      this.actionName = actionName;
    }
  } // class KeyBinding

  int icon_gap;
    Icon icon;
    int align;
    Document doc;

    public JTextComponent()
    {
	this("", null, 0);
    }

    public JTextComponent(Icon image)
    {
	this("", image, 0);
    }

    public JTextComponent(Icon image, int horizontalAlignment)
    {
	this("", image, horizontalAlignment);
    }

    public JTextComponent(String text)
    {
	this(text, null, 0);
    }

    public JTextComponent(String text, int horizontalAlignment)
    {
	this(text, null, horizontalAlignment);
    }

    public JTextComponent(String text, Icon icon, int horizontalAlignment)
    {
	setDocument(new PlainDocument());

	// do the work.....
	setText(text);
	this.icon  = icon;
	this.align     = horizontalAlignment;
	
        // its an editor, so:
        enableEvents(AWTEvent.KEY_EVENT_MASK);
        updateUI();
    }

    public void setDocument(Document s)
    {
	doc = s;
	revalidate();
	repaint();
    }

    public Document getDocument()
    {
	if (doc == null)
	    System.out.println("doc == null !!!");
	return doc;
    }

    protected  int checkHorizontalKey(int key, String message)
    {
	//    Verify that key is a legal value for the horizontalAlignment properties. 
	return 0;
    }
    protected  int checkVerticalKey(int key, String message)
    {
	//      Verify that key is a legal value for the verticalAlignment or verticalTextPosition properties.  
	return 0;
    }
    public AccessibleContext getAccessibleContext()
    {
	//          Get the AccessibleContext of this object 
	return null;
    }
    public Icon getDisabledIcon()
    {
	return null;
    }
    public int getDisplayedMnemonic()
    {
	//          Return the keycode that indicates a mnemonic key.   
	return 0;
    }
    public int getHorizontalAlignment()
    {
	//          Returns the alignment of the label's contents along the X axis.   
	return 0;
    }
    public int getHorizontalTextPosition()
    {
	//          Returns the horizontal position of the label's text, relative to its image.    
	return 0;
    }

    public Icon getIcon()
    {	return icon;    }
    public int getIconTextGap()
    {	return icon_gap;    }


    Component getLabelFor()
    {
	//          Get the component this is labelling.  
	return null;
    }

    public Insets getMargin()
    {
        // FIXME: Not implemented.
        return null;
    }

    public void setText(String text)
    {
	getDocument().remove(0,doc.getLength());
	getDocument().insertString(0, text, null);
    }
  
    public String getText()
    {
	return getDocument().getText(0, 
				     getDocument().getLength());
    }

    public String getUIClassID()
    {
	//          Returns a string that specifies the name of the l&f class that renders this component.  
	return "JTextComponent";
    }
    public int getVerticalAlignment()
    {
	//          Returns the alignment of the label's contents along the Y axis. 
	return 0;
    }
    public int getVerticalTextPosition()
    {
	//          Returns the vertical position of the label's text, relative to its image. 
	return 0;
    }

    public boolean imageUpdate(Image img, int infoflags, int x, int y, int w, int h)
    {
	//          This is overriden to return false if the current Icon's Image is not equal to the passed in Image img. 
	return (img == icon);
    }
    protected  String paramString()
    {
	//          Returns a string representation of this JTextComponent.  
	return "JTextComponent";
    }
    void setDisabledIcon(Icon disabledIcon)
    {
	//          Set the icon to be displayed if this JTextComponent is "disabled" (JTextComponent.setEnabled(false)).  
    }
    void setDisplayedMnemonic(char aChar)
    {
	//          Specifies the displayedMnemonic as a char value.  
    }
    void setDisplayedMnemonic(int key)
    {
	//          Specify a keycode that indicates a mnemonic key.  
    }
    void setHorizontalAlignment(int alignment)
    {
	//          Sets the alignment of the label's contents along the X axis.  
    }
    void setHorizontalTextPosition(int textPosition)
    {
	//          Sets the horizontal position of the label's text, relative to its image.  
    }
    void setIcon(Icon icon)
    {
	//          Defines the icon this component will display.  
    }
    public void setIconTextGap(int iconTextGap)
    {
	//          If both the icon and text properties are set, this property defines the space between them.  
    }
  
    public void setLabelFor(Component c)
    {
	//          Set the component this is labelling.  
    }
    
    public void setVerticalAlignment(int alignment)
    {
	//          Sets the alignment of the label's contents along the Y axis.  
    }
    public void setVerticalTextPosition(int textPosition)
    {
	//          Sets the vertical position of the label's text, relative to its image.  
    }

    public TextUI getUI()
    {	return (TextUI) ui;
    }

    public void updateUI()
    {
	TextUI b = (TextUI)UIManager.getUI(this);
	setUI(b);
    }

  public Dimension getPreferredScrollableViewportSize()
  {
    return null;
  }
  public int getScrollableUnitIncrement(Rectangle visible, int orientation,
                                        int direction)
  {
    return 0;
  }
  public int getScrollableBlockIncrement(Rectangle visible, int orientation,
                                         int direction)
  {
    return 0;
  }
} // class JTextComponent












