/* JLabel.java --
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


package javax.swing;

import java.awt.Component;
import java.awt.Font;
import java.awt.Image;
import java.awt.event.KeyEvent;

import javax.accessibility.Accessible;
import javax.accessibility.AccessibleContext;
import javax.swing.plaf.LabelUI;

/**
 * A swing widget that displays a text message and/or an icon.
 */
public class JLabel extends JComponent implements Accessible, SwingConstants
{
  /** DOCUMENT ME! */
  private static final long serialVersionUID = 5496508283662221534L;

  /**
   * The Component the label will give focus to when its mnemonic is
   * activated.
   */
  protected Component labelFor;

  /** The label's text. */
  private transient String text;

  /** Where the label will be positioned horizontally. */
  private transient int horizontalAlignment = LEADING;

  /** Where the label text will be placed horizontally relative to the icon. */
  private transient int horizontalTextPosition = TRAILING;

  /** Where the label will be positioned vertically. */
  private transient int verticalAlignment = CENTER;

  /** Where the label text will be place vertically relative to the icon. */
  private transient int verticalTextPosition = CENTER;

  /** The icon painted when the label is enabled. */
  private transient Icon icon;

  /** The icon painted when the label is disabled. */
  private transient Icon disabledIcon;

  /** The label's mnemnonic key. */
  private transient int displayedMnemonic = KeyEvent.VK_UNDEFINED;

  /** The index of the menemonic character in the text. */
  private transient int displayedMnemonicIndex = -1;

  /** The gap between the icon and the text. */
  private transient int iconTextGap = 4;

  /**
   * Creates a new vertically centered, horizontally on the leading edge
   * JLabel object with text and no icon.
   */
  public JLabel()
  {
    this(null, null, LEADING);
  }

  /**
   * Creates a new vertically and horizontally centered
   * JLabel object with no text and the given icon.
   *
   * @param image The icon to use with the label.
   */
  public JLabel(Icon image)
  {
    this(null, image, CENTER);
  }

  /**
   * Creates a new vertically centered JLabel object with no text and the
   * given icon and horizontal alignment. By default, the text is TRAILING
   * the image.
   *
   * @param image The icon to use with the label.
   * @param horizontalAlignment The horizontal alignment of the label.
   */
  public JLabel(Icon image, int horizontalAlignment)
  {
    this(null, image, horizontalAlignment);
  }

  /**
   * Creates a new horizontally leading and vertically centered JLabel 
   * object with no icon and the given text.
   *
   * @param text The text to use with the label.
   */
  public JLabel(String text)
  {
    this(text, null, LEADING);
  }

  /**
   * Creates a new vertically centered JLabel object with no icon and the
   * given text and horizontal alignment.
   *
   * @param text The text to use with the label.
   * @param horizontalAlignment The horizontal alignment of the label.
   */
  public JLabel(String text, int horizontalAlignment)
  {
    this(text, null, horizontalAlignment);
  }

  /**
   * Creates a new vertically centered JLabel object with the given text,
   * icon, and horizontal alignment.
   *
   * @param text The text to use with the label.
   * @param icon The icon to use with the label.
   * @param horizontalAlignment The horizontal alignment of the label.
   */
  public JLabel(String text, Icon icon, int horizontalAlignment)
  {
    this.text = text;
    this.icon = icon;
    this.horizontalAlignment = horizontalAlignment;
    updateUI();
  }

  /**
   * This method returns the label's UI delegate.
   *
   * @return The label's UI delegate.
   */
  public LabelUI getUI()
  {
    return (LabelUI) ui;
  }

  /**
   * This method sets the label's UI delegate.
   *
   * @param ui The label's UI delegate.
   */
  public void setUI(LabelUI ui)
  {
    super.setUI(ui);
  }

  /**
   * This method resets the label's UI delegate to the default UI for the
   * current look and feel.
   */
  public void updateUI()
  {
    setUI((LabelUI) UIManager.getUI(this));
  }

  /**
   * This method returns a name to identify which look and feel class will be
   * the UI delegate for this label.
   *
   * @return The UIClass identifier. "LabelUI"
   */
  public String getUIClassID()
  {
    return "LabelUI";
  }

  /**
   * This method is used primarily for debugging purposes and returns a string
   * that can be used to represent this label.
   *
   * @return A string to represent this label.
   */
  protected String paramString()
  {
    return "JLabel";
  }

  /**
   * This method returns the label text.
   *
   * @return The label text.
   */
  public String getText()
  {
    return text;
  }

  /**
   * This method changes the "text" property. The given text will be painted
   * in the label.
   *
   * @param newText The label's text.
   */
  public void setText(String newText)
  {
    if (text != newText)
      {
	String oldText = text;
	text = newText;
	firePropertyChange("text", oldText, newText);

	if (text != null && text.length() <= displayedMnemonicIndex)
	  setDisplayedMnemonicIndex(text.length() - 1);
      }
  }

  /**
   * This method returns the active icon. The active icon is painted when the
   * label is enabled.
   *
   * @return The active icon.
   */
  public Icon getIcon()
  {
    return icon;
  }

  /**
   * This method changes the "icon" property. This icon (the active icon) will
   * be the one displayed when the label is enabled.
   *
   * @param newIcon The active icon.
   */
  public void setIcon(Icon newIcon)
  {
    if (icon != newIcon)
      {
	Icon oldIcon = icon;
	icon = newIcon;
	firePropertyChange("icon", oldIcon, newIcon);
      }
  }

  /**
   * This method returns the disabled icon. The disabled icon is painted when
   * the label is disabled. If the disabled icon is null and the active icon
   * is an ImageIcon, this method returns a grayed version of the icon. The
   * grayed  version of the icon becomes the disabledIcon.
   *
   * @return The disabled icon.
   */
  public Icon getDisabledIcon()
  {
    if (disabledIcon == null && icon instanceof ImageIcon)
      disabledIcon = new ImageIcon(GrayFilter.createDisabledImage(((ImageIcon) icon)
                                                                  .getImage()));

    return disabledIcon;
  }

  /**
   * This method changes the "disabledIcon" property. This icon (the disabled
   * icon) will be the one displayed when the label is disabled.
   *
   * @param newIcon The disabled icon.
   */
  public void setDisabledIcon(Icon newIcon)
  {
    if (disabledIcon != newIcon)
      {
	Icon oldIcon = disabledIcon;
	disabledIcon = newIcon;
	firePropertyChange("disabledIcon", oldIcon, newIcon);
      }
  }

  /**
   * This method sets the keycode that will be the label's mnemonic. If the
   * label is used as a label for another component, the label will give
   * focus to that component when the mnemonic is activated.
   *
   * @param mnemonic The keycode to use for the mnemonic.
   */
  public void setDisplayedMnemonic(int mnemonic)
  {
    if (displayedMnemonic != mnemonic)
      {
	firePropertyChange("displayedMnemonic",
	                   displayedMnemonic, mnemonic);
	displayedMnemonic = mnemonic;

	if (text != null)
	  setDisplayedMnemonicIndex(text.toUpperCase().indexOf(mnemonic));
      }
  }

  /**
   * This method sets the character that will be the mnemonic used. If the
   * label is used as a label for another component, the label will give
   * focus to that component when the mnemonic is activated.
   *
   * @param mnemonic The character to use for the mnemonic.
   */
  public void setDisplayedMnemonic(char mnemonic)
  {
    setDisplayedMnemonic((int) Character.toUpperCase(mnemonic));
  }

  /**
   * This method returns the keycode that is used for the label's mnemonic.
   *
   * @return The keycode that is used for the label's mnemonic.
   */
  public int getDisplayedMnemonic()
  {
    return (int) displayedMnemonic;
  }

  /**
   * This method sets which character in the text will be the underlined
   * character. If the given index is -1, then this indicates  that there is
   * no mnemonic. If the index is less than -1 or if the index is equal to
   * the length, this method will throw an IllegalArgumentException.
   *
   * @param newIndex The index of the character to underline.
   *
   * @throws IllegalArgumentException If index less than -1 or index equals
   *         length.
   */
  public void setDisplayedMnemonicIndex(int newIndex)
    throws IllegalArgumentException
  {
    if (newIndex < -1 || (text != null && newIndex >= text.length()))
      throw new IllegalArgumentException();

    if (newIndex == -1
        || text == null
	|| text.charAt(newIndex) != displayedMnemonic)
      newIndex = -1;

    if (newIndex != displayedMnemonicIndex)
      {
	int oldIndex = displayedMnemonicIndex;
	displayedMnemonicIndex = newIndex;
	firePropertyChange("displayedMnemonicIndex",
	                   oldIndex, newIndex);
      }
  }

  /**
   * This method returns which character in the text will be  the underlined
   * character.
   *
   * @return The index of the character that will be underlined.
   */
  public int getDisplayedMnemonicIndex()
  {
    return displayedMnemonicIndex;
  }

  /**
   * This method ensures that the key is valid as a horizontal alignment.
   * Valid keys are: LEFT, CENTER, RIGHT, LEADING, TRAILING
   *
   * @param key The key to check.
   * @param message The message of the exception to be thrown if the key is
   *        invalid.
   *
   * @return The key if it's valid.
   *
   * @throws IllegalArgumentException If the key is invalid.
   */
  protected int checkHorizontalKey(int key, String message)
  {
    if (key != LEFT && key != CENTER && key != RIGHT && key != LEADING
        && key != TRAILING)
      throw new IllegalArgumentException(message);
    else
      return key;
  }

  /**
   * This method ensures that the key is valid as a  vertical alignment. Valid
   * keys are: TOP, CENTER, and BOTTOM.
   *
   * @param key The key to check.
   * @param message The message of the exception to be thrown if the key is
   *        invalid.
   *
   * @return The key if it's valid.
   *
   * @throws IllegalArgumentException If the key is invalid.
   */
  protected int checkVerticalKey(int key, String message)
  {
    if (key != TOP && key != BOTTOM && key != CENTER)
      throw new IllegalArgumentException(message);
    else
      return key;
  }

  /**
   * This method returns the gap between the icon and the text.
   *
   * @return The gap between the icon and the text.
   */
  public int getIconTextGap()
  {
    return iconTextGap;
  }

  /**
   * This method changes the "iconTextGap" property. The iconTextGap
   * determines how much space there is between the icon and the text.
   *
   * @param newGap The gap between the icon and the text.
   */
  public void setIconTextGap(int newGap)
  {
    if (iconTextGap != newGap)
      {
	firePropertyChange("iconTextGap", iconTextGap, newGap);
	iconTextGap = newGap;
      }
  }

  /**
   * This method returns the vertical alignment of the label.
   *
   * @return The vertical alignment of the label.
   */
  public int getVerticalAlignment()
  {
    return verticalAlignment;
  }

  /**
   * This method changes the "verticalAlignment" property of the label. The
   * vertical alignment determines how where the label will be placed
   * vertically. If the alignment is not valid, it will default to the
   * center.
   *
   * @param alignment The vertical alignment of the label.
   */
  public void setVerticalAlignment(int alignment)
  {
    if (alignment == verticalAlignment)
      return;

    int oldAlignment = verticalAlignment;
    verticalAlignment = checkVerticalKey(alignment, "verticalAlignment");
    firePropertyChange("verticalAlignment", oldAlignment, verticalAlignment);
  }

  /**
   * This method returns the horziontal alignment of the label.
   *
   * @return The horizontal alignment of the label.
   */
  public int getHorizontalAlignment()
  {
    return horizontalAlignment;
  }

  /**
   * This method changes the "horizontalAlignment" property. The horizontal
   * alignment determines where the label will be placed horizontally.
   *
   * @param alignment The horizontal alignment of the label.
   */
  public void setHorizontalAlignment(int alignment)
  {
    if (horizontalAlignment == alignment)
      return;
    
    int oldAlignment = horizontalAlignment;
    horizontalAlignment = checkHorizontalKey(alignment, "horizontalAlignment");
    firePropertyChange("horizontalAlignment", oldAlignment,
                       horizontalAlignment);
  }

  /**
   * This method returns the vertical text position of the label.
   *
   * @return The vertical text position of the label.
   */
  public int getVerticalTextPosition()
  {
    return verticalTextPosition;
  }

  /**
   * This method changes the "verticalTextPosition" property of the label. The
   * vertical text position determines where the text will be placed
   * vertically relative to the icon.
   *
   * @param textPosition The vertical text position.
   */
  public void setVerticalTextPosition(int textPosition)
  {
    if (textPosition != verticalTextPosition)
      {
	int oldPos = verticalTextPosition;
	verticalTextPosition = checkVerticalKey(textPosition,
	                                        "verticalTextPosition");
	firePropertyChange("verticalTextPosition", oldPos,
	                   verticalTextPosition);
      }
  }

  /**
   * This method returns the horizontal text position of the label.
   *
   * @return The horizontal text position.
   */
  public int getHorizontalTextPosition()
  {
    return horizontalTextPosition;
  }

  /**
   * This method changes the "horizontalTextPosition" property of the label.
   * The horizontal text position determines where the text will be placed
   * horizontally relative to the icon.
   *
   * @param textPosition The horizontal text position.
   */
  public void setHorizontalTextPosition(int textPosition)
  {
    if (textPosition != horizontalTextPosition)
      {
	int oldPos = horizontalTextPosition;
	horizontalTextPosition = checkHorizontalKey(textPosition,
	                                            "horizontalTextPosition");
	firePropertyChange("horizontalTextPosition", oldPos,
	                   horizontalTextPosition);
      }
  }

  /**
   * This method simply returns false if the current icon image (current  icon
   * will depend on whether the label is enabled) is not equal to the passed
   * in image.
   *
   * @param img The image to check.
   * @param infoflags The bitwise inclusive OR of ABORT, ALLBITS, ERROR,
   *        FRAMEBITS, HEIGHT, PROPERTIES, SOMEBITS, and WIDTH
   * @param x The x position
   * @param y The y position
   * @param w The width
   * @param h The height
   *
   * @return Whether the current icon image is equal to the image given.
   */
  public boolean imageUpdate(Image img, int infoflags, int x, int y, int w,
                             int h)
  {
    Icon currIcon = isEnabled() ? icon : disabledIcon;

    // XXX: Is this the correct way to check for image equality?
    if (currIcon != null && currIcon instanceof ImageIcon)
      return (((ImageIcon) currIcon).getImage() == img);

    return false;
  }

  /**
   * This method returns the component that the label gives focus to  when the
   * mnemonic is activated.
   *
   * @return The component that gets focus when the label's mnemonic is
   *         activated.
   */
  public Component getLabelFor()
  {
    return labelFor;
  }

  /**
   * This method changes the "labelFor" property. The component that the label
   * is acting as a label for will request focus when the label's  mnemonic
   * is activated.
   *
   * @param c The component that gets focus when the label's mnemonic is
   *        activated.
   */
  public void setLabelFor(Component c)
  {
    if (c != labelFor)
      {
	Component oldLabelFor = labelFor;
	labelFor = c;
	firePropertyChange("labelFor", oldLabelFor, labelFor);
      }
  }

  /**
   * This method overrides setFont so that we can call for a repaint after the
   * font is changed.
   *
   * @param f The font for this label.
   */
  public void setFont(Font f)
  {
    super.setFont(f);
    repaint();
  }

  /**
   * DOCUMENT ME!
   *
   * @return
   */
  public AccessibleContext getAccessibleContext()
  {
    return null;
  }
}
