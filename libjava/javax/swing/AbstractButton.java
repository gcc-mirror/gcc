/* AbstractButton.java -- Provides basic button functionality.
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

package javax.swing;

import java.awt.*;
import java.awt.event.*;

import javax.swing.event.*;
import javax.swing.plaf.*;
import javax.swing.text.*;
import javax.accessibility.*;

import java.util.*;
import java.beans.*;

/**
 * Provides basic button functionality
 *
 * @author Ronald Veldema (rveldema@cs.vu.nl)
 */
public abstract class AbstractButton extends JComponent
			implements ItemSelectable, SwingConstants
{
	Icon default_icon, pressed_button, disabled_button,
	selected_button, disabled_selected_button, current_icon;
	String text;

	int vert_align = CENTER;
	int hori_align = CENTER;
	int hori_text_pos = CENTER;
	int vert_text_pos = CENTER;

	boolean paint_border = true, paint_focus;
	Action action_taken;
	ButtonModel model;
	Insets margin;


	public static final String FOCUS_PAINTED_CHANGED_PROPERTY = "focusPainted";

	/**
	 * AccessibleAbstractButton
	 */
	protected abstract class AccessibleAbstractButton 
		extends AccessibleJComponent 
		implements AccessibleAction, AccessibleValue, AccessibleText {

		//-------------------------------------------------------------
		// Initialization ---------------------------------------------
		//-------------------------------------------------------------

		/**
		 * Constructor AccessibleAbstractButton
		 * @param component TODO
		 */
		protected AccessibleAbstractButton(AbstractButton component) {
			super(component);
			// TODO
		} // AccessibleAbstractButton()


		//-------------------------------------------------------------
		// Methods ----------------------------------------------------
		//-------------------------------------------------------------

		/**
		 * getAccessibleStateSet
		 * @returns AccessibleStateSet
		 */
		public AccessibleStateSet getAccessibleStateSet() {
			return null; // TODO
		} // getAccessibleStateSet()

		/**
		 * getAccessibleName
		 * @returns String
		 */
		public String getAccessibleName() {
			return null; // TODO
		} // getAccessibleName()

		/**
		 * getAccessibleIcon
		 * @returns AccessibleIcon[]
		 */
		public AccessibleIcon[] getAccessibleIcon() {
			return null; // TODO
		} // getAccessibleIcon()

		/**
		 * getAccessibleRelationSet
		 * @returns AccessibleRelationSet
		 */
		public AccessibleRelationSet getAccessibleRelationSet() {
			return null; // TODO
		} // getAccessibleRelationSet()

		/**
		 * getAccessibleAction
		 * @returns AccessibleAction
		 */
		public AccessibleAction getAccessibleAction() {
			return null; // TODO
		} // getAccessibleAction()

		/**
		 * getAccessibleValue
		 * @returns AccessibleValue
		 */
		public AccessibleValue getAccessibleValue() {
			return null; // TODO
		} // getAccessibleValue()

		/**
		 * getAccessibleActionCount
		 * @returns int
		 */
		public int getAccessibleActionCount() {
			return 0; // TODO
		} // getAccessibleActionCount()

		/**
		 * getAccessibleActionDescription
		 * @param value0 TODO
		 * @returns String
		 */
		public String getAccessibleActionDescription(int value0) {
			return null; // TODO
		} // getAccessibleActionDescription()

		/**
		 * doAccessibleAction
		 * @param value0 TODO
		 * @returns boolean
		 */
		public boolean doAccessibleAction(int value0) {
			return false; // TODO
		} // doAccessibleAction()

		/**
		 * getCurrentAccessibleValue
		 * @returns Number
		 */
		public Number getCurrentAccessibleValue() {
			return null; // TODO
		} // getCurrentAccessibleValue()

		/**
		 * setCurrentAccessibleValue
		 * @param value0 TODO
		 * @returns boolean
		 */
		public boolean setCurrentAccessibleValue(Number value0) {
			return false; // TODO
		} // setCurrentAccessibleValue()

		/**
		 * getMinimumAccessibleValue
		 * @returns Number
		 */
		public Number getMinimumAccessibleValue() {
			return null; // TODO
		} // getMinimumAccessibleValue()

		/**
		 * getMaximumAccessibleValue
		 * @returns Number
		 */
		public Number getMaximumAccessibleValue() {
			return null; // TODO
		} // getMaximumAccessibleValue()

		/**
		 * getAccessibleText
		 * @returns AccessibleText
		 */
		public AccessibleText getAccessibleText() {
			return null; // TODO
		} // getAccessibleText()

		/**
		 * getIndexAtPoint
		 * @param value0 TODO
		 * @returns int
		 */
		public int getIndexAtPoint(Point value0) {
			return 0; // TODO
		} // getIndexAtPoint()

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
		 * getCaretPosition
		 * @returns int
		 */
		public int getCaretPosition() {
			return 0; // TODO
		} // getCaretPosition()

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

		/**
		 * getCharacterAttribute
		 * @param value0 TODO
		 * @returns AttributeSet
		 */
		public AttributeSet getCharacterAttribute(int value0) {
			return null; // TODO
		} // getCharacterAttribute()

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
		 * getSelectedText
		 * @returns String
		 */
		public String getSelectedText() {
			return null; // TODO
		} // getSelectedText()

		/**
		 * getTextRectangle
		 * @returns Rectangle
		 */
		private Rectangle getTextRectangle() {
			return null; // TODO
		} // getTextRectangle()


	} // AccessibleAbstractButton


	static private class JFocusListener implements FocusListener
	{
		AbstractButton c;

		JFocusListener(AbstractButton c)
		{
			this.c = c;
		}

		public void focusLost(FocusEvent event)
		{
			c.getModel().setArmed(false);

			System.out.println("LOST FOCUS");
			if (c.isFocusPainted())
			{
				c.repaint();
			}
		}
		public void focusGained(FocusEvent event)
		{
			System.out.println("GAIN FOCUS");
		}
	}


	/**********************************************
	 *
	 * 
	 *       Constructors
	 *
	 *
	 ****************/

	AbstractButton()
	{
		this("",null);
	}

	AbstractButton(String text,
	               Icon icon)
	{
		this.text    = text;
		setIcon(icon);

		setAlignmentX(LEFT_ALIGNMENT);
		setAlignmentY(CENTER_ALIGNMENT);

		addFocusListener( new JFocusListener(this) );

		setModel(new DefaultButtonModel(this));

		updateUI(); // get a proper ui
	}


	/**********************************************
	 *
	 * 
	 *       Actions etc
	 *
	 *
	 ****************/

	public ButtonModel getModel()
	{	return model;    }
	public void setModel(ButtonModel newModel)
	{	model = newModel;    }

	public String getActionCommand()
	{	return getModel().getActionCommand();    }
	public void setActionCommand(String aCommand)
	{   getModel().setActionCommand(aCommand);   }

	public void addActionListener(ActionListener l)
	{	getModel().addActionListener(l);    }
	public void removeActionListener(ActionListener l)
	{	getModel().removeActionListener(l);    }

	public void addChangeListener(ChangeListener l)
	{   getModel().addChangeListener(l);     }
	public void removeChangeListener(ChangeListener l)
	{  getModel().removeChangeListener(l);    }

        public void addItemListener(ItemListener l)
	{  getModel().addItemListener(l);    }
        public void removeItemListener(ItemListener l)
	{  getModel().removeItemListener(l);  }

	public int getHorizontalAlignment()
	{	return hori_align;    }
	public int getHorizontalTextPosition()
	{	return hori_text_pos;    }
	public int getVerticalAlignment()
	{	return vert_align;   }
	public int getVerticalTextPosition()
	{	return vert_text_pos;  }


	protected  void fireItemStateChanged(ItemEvent event)
	{    getModel().fireItemStateChanged(event);    }
	protected  void fireStateChanged(ChangeEvent event)
	{	 getModel().fireStateChanged(event);    }
	protected void fireActionPerformed(ActionEvent event)
	{	getModel().fireActionPerformed(event);    }

	public void setVerticalAlignment(int alignment)
	{	vert_align = alignment;    }
	public void setHorizontalAlignment(int alignment)
	{   hori_align = alignment;   }
	public void setVerticalTextPosition(int textPosition)
	{	vert_text_pos = textPosition;    }
	public void setHorizontalTextPosition(int textPosition)
	{   hori_text_pos = textPosition;   }

	public int getMnemonic()
	{	return getModel().getMnemonic();    }
	public void setMnemonic(char mne)
	{	getModel().setMnemonic(mne);    }
	public void setMnemonic(int mne)
	{	getModel().setMnemonic(mne);    }

	public void setRolloverEnabled(boolean b)
	{    getModel().setRollover(b);    }
	public boolean isRolloverEnabled()
	{    return getModel().isRollover();     }


	public boolean isBorderPainted()
	{	return paint_border;    }
	public void setBorderPainted(boolean b)
	{
		if (b != paint_border)
		{
			paint_border = b;
			revalidate();
			repaint();
		}
	}

	public Action getAction()
	{	return action_taken;    }
	public void setAction(Action a)
	{
		action_taken = a;
		revalidate();
		repaint();
	}

	public void setSelected(boolean b)
	{	getModel().setSelected(b);    }
	public boolean isSelected()
	{	return getModel().isSelected();     }


        public Icon getIcon()
	{	return default_icon;    }
	public void setIcon(Icon defaultIcon)
	{
		if (default_icon == defaultIcon)
			return;

		default_icon = defaultIcon;
		if (default_icon != null)
		{
                  // XXX FIXME - icons do not know their parent
//  			default_icon.setParent(this);
		}
		revalidate();
		repaint();
	}

	public String getText()
	{	return text;    }
	public void setLabel(String label)
	{	setText(label);    }
	public String getLabel()
	{	return getText();    }
	public void setText(String text)
	{
		this.text = text;
		revalidate();
		repaint();
	}


        public 	Insets getMargin()
	{      return margin; }
	public void setMargin(Insets m)
	{
		margin = m;
		revalidate();
		repaint();
	}

	public void setEnabled(boolean b)
	{
		super.setEnabled(b);
		getModel().setEnabled(b);
		repaint();
	}

	public Icon getPressedIcon()
	{	return pressed_button;    }
	public void setPressedIcon(Icon pressedIcon)
	{
		pressed_button = pressedIcon;
		revalidate();
		repaint();
	}


	public Icon getDisabledIcon()
	{	return disabled_button;    }
	public void setDisabledIcon(Icon disabledIcon)
	{
		disabled_button = disabledIcon;
		revalidate();
		repaint();
	}

	public boolean isFocusPainted()
	{   return paint_focus;   }
	public void setFocusPainted(boolean b)
	{
		boolean old = paint_focus;
		paint_focus = b;

		firePropertyChange(FOCUS_PAINTED_CHANGED_PROPERTY,
		                   old,
		                   b);
		if (hasFocus())
		{
			revalidate();
			repaint();
		}
	}

	public boolean isFocusTraversable()
	{
		//Identifies whether or not this component can receive the focus.
		return true;
	}


	protected  int checkHorizontalKey(int key, String exception)
	{
		//       Verify that key is a legal value for the horizontalAlignment properties.
		return 0;
	}
	protected  int checkVerticalKey(int key, String exception)
	{
		//       Ensures that the key is a valid.
		return 0;
	}
	protected  void configurePropertiesFromAction(Action a)
	{
		//Factory method which sets the ActionEvent source's properties according to values from the Action instance.
	}

	protected  ActionListener createActionListener()
	{
		return new ActionListener()
		       {
			       public void actionPerformed(ActionEvent e) { }
		       };
	}

	protected  PropertyChangeListener createActionPropertyChangeListener(Action a)
	{
		//Factory method which creates the PropertyChangeListener used to update the ActionEvent source as properties change on its Action instance.
		return null;
	}
	protected  ChangeListener createChangeListener()
	{
		//       Subclasses that want to handle ChangeEvents differently can override this to return another ChangeListener implementation.
		return new ChangeListener()
		       {
			       public void stateChanged(ChangeEvent e) { }
		       };
	}

	protected  ItemListener createItemListener()
	{
		return new ItemListener()
		       {
			       public void itemStateChanged(ItemEvent e) { }
		       };
	}


	public void doClick()
	{
		doClick(100);
	}
	public void doClick(int pressTime)
	{
	    //Toolkit.tlkBeep ();
		//Programmatically perform a "click".
	}


	public Icon getDisabledSelectedIcon()
	{
		//Returns the icon used by the button when it's disabled and selected.
		return disabled_selected_button;
	}


	public Icon getRolloverIcon()
	{
		//       Returns the rollover icon for the button.
		return null;
	}

	Icon getRolloverSelectedIcon()
	{
		//       Returns the rollover selection icon for the button.
		return null;
	}
	Icon getSelectedIcon()
	{
		//       Returns the selected icon for the button.
		return selected_button;
	}


	public Object[] getSelectedObjects()
	{
		//Returns an array (length 1) containing the label or null if the button is not selected.
		return null;
	}


	public boolean imageUpdate(Image img, int infoflags, int x, int y, int w, int h)
	{
		//This is overridden to return false if the current Icon's Image is not equal to the passed in Image img.
		return current_icon == img;
	}

        public boolean isContentAreaFilled()
	{
		//       Checks whether the "content area" of the button should be filled.
		return false;
	}



	protected  void paintBorder(Graphics g)
	{
		//       Paint the button's border if BorderPainted property is true.
		if (isBorderPainted())
			super.paintBorder(g);
	}
	protected  String paramString()
	{
		//        Returns a string representation of this AbstractButton.
		return "AbstractButton";
	}


	public void setContentAreaFilled(boolean b)
	{
		//Sets whether the button should paint the content area or leave it transparent.
	}


	public void setDisabledSelectedIcon(Icon disabledSelectedIcon)
	{
		//          Sets the disabled selection icon for the button.
	}

	public void setRolloverIcon(Icon rolloverIcon)
	{
		//       Sets the rollover icon for the button.
	}
        public void setRolloverSelectedIcon(Icon rolloverSelectedIcon)
	{
		//       Sets the rollover selected icon for the button.
	}


	public void setSelectedIcon(Icon selectedIcon)
	{
		//       Sets the selected icon for the button.
	}


	public void setUI(ButtonUI ui)
	{	//       Sets the L&F object that renders this component.
		super.setUI(ui);
	}

	public ButtonUI getUI()
	{
		//Returns the L&F object that renders this component.
		return (ButtonUI) ui;
	}

        public void updateUI()
	{
		/*
		  //          Notification from the UIFactory that the L&F has changed.
		  if (getUI() == null)
		  {
		  setUI(getUI());
		  }
		*/
	}

	protected void processActionEvent(ActionEvent e)
	{
		System.out.println("PROCESS-ACTION-EVENT: " + e);
	}


	protected void processMouseEvent(MouseEvent e)
	{
		//	System.out.println("PROCESS-MOUSE-EVENT: " + e + ", PRESSED-IN-MODEL="+getModel().isPressed());

		switch (e.getID())
		{
		case MouseEvent.MOUSE_MOVED:
			{
				break;
			}
		case MouseEvent.MOUSE_PRESSED:
			{
				if (! isEnabled())
				{
					System.out.println("button not enabled, ignoring press");
				}
				else
				{
					System.out.println("telling model:press: " + getModel());
					getModel().setPressed(true);
					repaint();
				}
				break;
			}

		case MouseEvent.MOUSE_RELEASED:
			{
				if (! isEnabled())
				{
					System.out.println("button not enabled, ignoring release");
				}
				else
				{
					int flags = 0;

					System.out.println("        XXX--> " + getActionCommand());

					fireActionPerformed(new ActionEvent(this,
					                                    ActionEvent.ACTION_PERFORMED,
					                                    getActionCommand(),
					                                    flags));

					//System.out.println("telling model:release");
					getModel().setPressed(false);
					repaint();
				}
				break;
			}
		case MouseEvent.MOUSE_CLICKED:
			{
				break;
			}
		}
	}
}
