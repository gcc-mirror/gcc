/* JLabel.java --
   Copyright (C) 2002, 2004, 2005, 2006,  Free Software Foundation, Inc.

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

import gnu.java.lang.CPStringBuilder;

import java.awt.Component;
import java.awt.Font;
import java.awt.FontMetrics;
import java.awt.Image;
import java.awt.Insets;
import java.awt.Point;
import java.awt.Rectangle;
import java.awt.Shape;
import java.awt.event.KeyEvent;
import java.beans.PropertyChangeEvent;

import javax.accessibility.Accessible;
import javax.accessibility.AccessibleContext;
import javax.accessibility.AccessibleExtendedComponent;
import javax.accessibility.AccessibleRole;
import javax.accessibility.AccessibleText;
import javax.swing.plaf.LabelUI;
import javax.swing.plaf.basic.BasicHTML;
import javax.swing.text.AttributeSet;
import javax.swing.text.BadLocationException;
import javax.swing.text.Position;
import javax.swing.text.SimpleAttributeSet;
import javax.swing.text.View;

/**
 * A component that displays a static text message and/or an icon.
 */
public class JLabel extends JComponent implements Accessible, SwingConstants
{

  /**
   * Provides the accessibility features for the <code>JLabel</code>
   * component.
   */
  protected class AccessibleJLabel
    extends JComponent.AccessibleJComponent
    implements AccessibleText, AccessibleExtendedComponent
  {
    
    /**
     * Returns the accessible name.
     * 
     * @return The accessible name.
     */
    public String getAccessibleName()
    {
      if (accessibleName != null)
        return accessibleName;
      if (text != null)
        return text;
      else
        return super.getAccessibleName();
    }
    
    /**
     * Returns the accessible role for the <code>JLabel</code> component.
     *
     * @return {@link AccessibleRole#LABEL}.
     */
    public AccessibleRole getAccessibleRole()
    {
      return AccessibleRole.LABEL;
    }
    
    /**
     * Returns the selected text. This is null since JLabels
     * are not selectable.
     *
     * @return <code>null</code> because JLabels cannot have selected text
     */
    public String getSelectedText()
    {
      // We return null here since JLabel's text is not selectable.
      return null;
    }

    /**
     * Returns the start index of the selected text.
     *
     * @return the start index of the selected text
     */
    public int getSelectionStart()
    {
      // JLabel don't have selected text, so we return -1 here.
      return -1;
    }

    /**
     * Returns the end index of the selected text.
     *
     * @return the end index of the selected text
     */
    public int getSelectionEnd()
    {
      // JLabel don't have selected text, so we return -1 here.
      return -1;
    }

    /**
     * Returns an {@link AttributeSet} that reflects the text attributes of
     * the specified character. We return an empty
     * <code>AttributeSet</code> here, because JLabels don't support text
     * attributes (at least not yet).
     *
     * @param index the index of the character
     *
     * @return an {@link AttributeSet} that reflects the text attributes of
     *         the specified character
     */
    public AttributeSet getCharacterAttribute(int index)
    {
      // FIXME: Return null here for simple labels, and query the HTML
      // view for HTML labels.
      return new SimpleAttributeSet();
    }

    /**
     * Returns the character, word or sentence at the specified index. The
     * <code>part</code> parameter determines what is returned, the character,
     * word or sentence after the index.
     *
     * @param part one of {@link AccessibleText#CHARACTER},
     *             {@link AccessibleText#WORD} or
     *             {@link AccessibleText#SENTENCE}, specifying what is returned
     * @param index the index
     *
     * @return the character, word or sentence after <code>index</code>
     */
    public String getAtIndex(int part, int index)
    {
      String result = "";
      int startIndex = -1;
      int endIndex = -1;
      switch(part)
        {
        case AccessibleText.CHARACTER:
          result = String.valueOf(text.charAt(index));
          break;
        case AccessibleText.WORD:
          startIndex = text.lastIndexOf(' ', index);
          endIndex = text.indexOf(' ', startIndex + 1);
          if (endIndex == -1)
            endIndex = startIndex + 1;
          result = text.substring(startIndex + 1, endIndex);
          break;
        case AccessibleText.SENTENCE:
        default:
          startIndex = text.lastIndexOf('.', index);
          endIndex = text.indexOf('.', startIndex + 1);
          if (endIndex == -1)
            endIndex = startIndex + 1;
          result = text.substring(startIndex + 1, endIndex);
          break;
        }
      return result;
    }

    /**
     * Returns the character, word or sentence after the specified index. The
     * <code>part</code> parameter determines what is returned, the character,
     * word or sentence after the index.
     *
     * @param part one of {@link AccessibleText#CHARACTER},
     *             {@link AccessibleText#WORD} or
     *             {@link AccessibleText#SENTENCE}, specifying what is returned
     * @param index the index
     *
     * @return the character, word or sentence after <code>index</code>
     */
    public String getAfterIndex(int part, int index)
    {
      String result = "";
      int startIndex = -1;
      int endIndex = -1;
      switch(part)
        {
        case AccessibleText.CHARACTER:
          result = String.valueOf(text.charAt(index + 1));
          break;
        case AccessibleText.WORD:
          startIndex = text.indexOf(' ', index);
          endIndex = text.indexOf(' ', startIndex + 1);
          if (endIndex == -1)
            endIndex = startIndex + 1;
          result = text.substring(startIndex + 1, endIndex);
          break;
        case AccessibleText.SENTENCE:
        default:
          startIndex = text.indexOf('.', index);
          endIndex = text.indexOf('.', startIndex + 1);
          if (endIndex == -1)
            endIndex = startIndex + 1;
          result = text.substring(startIndex + 1, endIndex);
          break;
        }
      return result;
    }

    /**
     * Returns the character, word or sentence before the specified index. The
     * <code>part</code> parameter determines what is returned, the character,
     * word or sentence before the index.
     *
     * @param part one of {@link AccessibleText#CHARACTER},
     *             {@link AccessibleText#WORD} or
     *             {@link AccessibleText#SENTENCE}, specifying what is returned
     * @param index the index
     *
     * @return the character, word or sentence before <code>index</code>
     */
    public String getBeforeIndex(int part, int index)
    {
      String result = "";
      int startIndex = -1;
      int endIndex = -1;
      switch(part)
        {
        case AccessibleText.CHARACTER:
          result = String.valueOf(text.charAt(index - 1));
          break;
        case AccessibleText.WORD:
          endIndex = text.lastIndexOf(' ', index);
          if (endIndex == -1)
            endIndex = 0;
          startIndex = text.lastIndexOf(' ', endIndex - 1);
          result = text.substring(startIndex + 1, endIndex);
          break;
        case AccessibleText.SENTENCE:
        default:
          endIndex = text.lastIndexOf('.', index);
          if (endIndex == -1)
            endIndex = 0;
          startIndex = text.lastIndexOf('.', endIndex - 1);
          result = text.substring(startIndex + 1, endIndex);
          break;
        }
      return result;
    }

    /**
     * Returns the caret position. This method returns -1 because JLabel don't
     * have a caret.
     *
     * @return the caret position
     */
    public int getCaretPosition()
    {
      return -1;
    }

    /**
     * Returns the number of characters that are displayed by the JLabel.
     *
     * @return the number of characters that are displayed by the JLabel
     */
    public int getCharCount()
    {
      // FIXME: Query HTML view for HTML labels.
      return text.length();
    }

    /**
     * Returns the bounding box of the character at the specified index.
     *
     * @param index the index of the character that we return the
     *        bounds for
     *
     * @return the bounding box of the character at the specified index
     */
    public Rectangle getCharacterBounds(int index)
    {
      Rectangle bounds = null;
      View view = (View) getClientProperty(BasicHTML.propertyKey);
      if (view != null)
        {
          Rectangle textR = getTextRectangle();
          try
            {
              Shape s = view.modelToView(index, textR, Position.Bias.Forward);
              bounds = s.getBounds();
            }
          catch (BadLocationException ex)
            {
              // Can't return something reasonable in this case.
            }
        }
      return bounds;
    }

    /**
     * Returns the rectangle inside the JLabel, in which the actual text is
     * rendered. This method has been adopted from the Mauve testcase
     * gnu.testlet.javax.swing.JLabel.AccessibleJLabel.getCharacterBounds.
     *
     * @return the rectangle inside the JLabel, in which the actual text is
     *         rendered
     */
    private Rectangle getTextRectangle()
    {
      JLabel l = JLabel.this;
      Rectangle textR = new Rectangle();
      Rectangle iconR = new Rectangle();
      Insets i = l.getInsets();
      int w = l.getWidth();
      int h = l.getHeight();
      Rectangle viewR = new Rectangle(i.left, i.top, w - i.left - i.right,
                                      h - i.top - i.bottom);
      FontMetrics fm = l.getFontMetrics(l.getFont());
      SwingUtilities.layoutCompoundLabel(l, fm, l.getText(), l.getIcon(),
                                         l.getVerticalAlignment(),
                                         l.getHorizontalAlignment(),
                                         l.getVerticalTextPosition(),
                                         l.getHorizontalTextPosition(),
                                         viewR, iconR, textR,
                                         l.getIconTextGap());
      return textR;
    }

    /**
     * Returns the index of the character that is located at the specified
     * point.
     *
     * @param point the location that we lookup the character for
     *
     * @return the index of the character that is located at the specified
     *         point
     */
    public int getIndexAtPoint(Point point)
    {
      int index = -1;
      View view = (View) getClientProperty(BasicHTML.propertyKey);
      if (view != null)
        {
          Rectangle r = getTextRectangle();
          index = view.viewToModel(point.x, point.y, r, new Position.Bias[0]);
        }
      return index;
    }
  }

  private static final long serialVersionUID = 5496508283662221534L;

  static final String LABEL_PROPERTY = "labeledBy";

  /**
   * The Component the label will give focus to when its mnemonic is
   * activated.
   */
  protected Component labelFor;

  /** The label's text. */
  transient String text;

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

  /** The index of the mnemonic character in the text. */
  private transient int displayedMnemonicIndex = -1;

  /** The gap between the icon and the text. */
  private transient int iconTextGap = 4;

  /**
   * Creates a new vertically centered, horizontally on the leading edge
   * JLabel object with text and no icon.
   */
  public JLabel()
  {
    this("", null, LEADING);
  }

  /**
   * Creates a new vertically and horizontally centered
   * JLabel object with no text and the given icon.
   *
   * @param image The icon to use with the label, <code>null</code> permitted.
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
   * @param image The icon to use with the label, <code>null</code> premitted.
   * @param horizontalAlignment The horizontal alignment of the label, must be
   * either <code>CENTER</code>, <code>LEFT</code>, <code>RIGHT</code>,
   * <code>LEADING</code> or <code>TRAILING</code>.
   */
  public JLabel(Icon image, int horizontalAlignment)
  {
    this(null, image, horizontalAlignment);
  }

  /**
   * Creates a new horizontally leading and vertically centered JLabel 
   * object with no icon and the given text.
   *
   * @param text The text to use with the label, <code>null</code> permitted.
   */
  public JLabel(String text)
  {
    this(text, null, LEADING);
  }

  /**
   * Creates a new vertically centered JLabel object with no icon and the
   * given text and horizontal alignment.
   *
   * @param text The text to use with the label, <code>null</code> permitted.
   * @param horizontalAlignment The horizontal alignment of the label, must be
   * either <code>CENTER</code>, <code>LEFT</code>, <code>RIGHT</code>,
   * <code>LEADING</code> or <code>TRAILING</code>.
   */
  public JLabel(String text, int horizontalAlignment)
  {
    this(text, null, horizontalAlignment);
  }

  /**
   * Creates a new vertically centered JLabel object with the given text,
   * icon, and horizontal alignment.
   *
   * @param text The text to use with the label, <code>null</code> permitted.
   * @param icon The icon to use with the label, <code>null</code> premitted.
   * @param horizontalAlignment The horizontal alignment of the label, must be
   * either <code>CENTER</code>, <code>LEFT</code>, <code>RIGHT</code>,
   * <code>LEADING</code> or <code>TRAILING</code>.
   */
  public JLabel(String text, Icon icon, int horizontalAlignment)
  {
    if (horizontalAlignment != SwingConstants.LEFT  
        && horizontalAlignment != SwingConstants.RIGHT 
        && horizontalAlignment != SwingConstants.CENTER 
        && horizontalAlignment != SwingConstants.LEADING 
        && horizontalAlignment != SwingConstants.TRAILING)
      throw new IllegalArgumentException();
    
    this.text = text;
    this.icon = icon;
    this.horizontalAlignment = horizontalAlignment;
    setAlignmentX(0.0F);
    setInheritsPopupMenu(true);
    updateUI();
  }

  /**
   * Returns the label's UI delegate.
   *
   * @return The label's UI delegate.
   */
  public LabelUI getUI()
  {
    return (LabelUI) ui;
  }

  /**
   * Sets the label's UI delegate.
   *
   * @param ui The label's UI delegate (<code>null</code> not permitted).
   */
  public void setUI(LabelUI ui)
  {
    super.setUI(ui);
  }

  /**
   * Resets the label's UI delegate to the default UI for the current look and 
   * feel.
   */
  public void updateUI()
  {
    setUI((LabelUI) UIManager.getUI(this));
  }

  /**
   * Returns a name to identify which look and feel class will be
   * the UI delegate for this label.
   *
   * @return <code>"LabelUI"</code>
   */
  public String getUIClassID()
  {
    return "LabelUI";
  }

  /**
   * Returns a string describing the attributes for the <code>JLabel</code>
   * component, for use in debugging.  The return value is guaranteed to be 
   * non-<code>null</code>, but the format of the string may vary between
   * implementations.
   *
   * @return A string describing the attributes of the <code>JLabel</code>.
   */
  protected String paramString()
  {
    CPStringBuilder sb = new CPStringBuilder(super.paramString());
    sb.append(",defaultIcon=");
    if (icon != null)
      sb.append(icon);
    sb.append(",disabledIcon=");
    if (disabledIcon != null)
      sb.append(disabledIcon);
    sb.append(",horizontalAlignment=");
    sb.append(SwingUtilities.convertHorizontalAlignmentCodeToString(
        horizontalAlignment));
    sb.append(",horizontalTextPosition=");
    sb.append(SwingUtilities.convertHorizontalAlignmentCodeToString(
        horizontalTextPosition));
    sb.append(",iconTextGap=").append(iconTextGap);
    sb.append(",labelFor=");
    if (labelFor != null)
      sb.append(labelFor);
    sb.append(",text=");
    if (text != null)
      sb.append(text);
    sb.append(",verticalAlignment=");
    sb.append(SwingUtilities.convertVerticalAlignmentCodeToString(
        verticalAlignment));
    sb.append(",verticalTextPosition=");
    sb.append(SwingUtilities.convertVerticalAlignmentCodeToString(
        verticalTextPosition));
    return sb.toString();
  }

  /**
   * Returns the text displayed by the label.
   *
   * @return The label text (possibly <code>null</code>).
   * 
   * @see #setText(String)
   */
  public String getText()
  {
    return text;
  }

  /**
   * Sets the text for the label and sends a {@link PropertyChangeEvent} (with
   * the name 'text') to all registered listeners.  This method will also 
   * update the <code>displayedMnemonicIndex</code>, if necessary.
   *
   * @param newText The text (<code>null</code> permitted).
   * 
   * @see #getText()
   * @see #getDisplayedMnemonicIndex()
   */
  public void setText(String newText)
  {
    if (text == null && newText == null)
      return;
    if (text != null && text.equals(newText))
      return;

    String oldText = text;
    text = newText;
    firePropertyChange("text", oldText, newText);

    if (text != null)
      setDisplayedMnemonicIndex(text.toUpperCase().indexOf(displayedMnemonic));
    else
      setDisplayedMnemonicIndex(-1);
    revalidate();
    repaint();
  }

  /**
   * Returns the active icon. The active icon is painted when the label is 
   * enabled.
   *
   * @return The active icon.
   * 
   * @see #setIcon(Icon)
   * @see #getDisabledIcon()
   */
  public Icon getIcon()
  {
    return icon;
  }

  /**
   * Sets the icon for the label (this is a bound property with the name 
   * 'icon'). This icon will be displayed when the label is enabled.
   *
   * @param newIcon The icon (<code>null</code> permitted).
   * 
   * @see #getIcon()
   * @see #setDisabledIcon(Icon)
   */
  public void setIcon(Icon newIcon)
  {
    if (icon != newIcon)
      {
        Icon oldIcon = icon;
        icon = newIcon;
        firePropertyChange("icon", oldIcon, newIcon);
        repaint();
      }
  }

  /**
   * Returns the disabled icon. The disabled icon is painted when the label is 
   * disabled. If the disabled icon is <code>null</code> and the active icon
   * is an {@link ImageIcon}, this method returns a grayed version of the icon. 
   * The grayed version of the icon becomes the <code>disabledIcon</code>.
   *
   * @return The disabled icon.
   * 
   * @see #setDisabledIcon(Icon)
   */
  public Icon getDisabledIcon()
  {
    if (disabledIcon == null && icon instanceof ImageIcon)
      disabledIcon = new ImageIcon(
          GrayFilter.createDisabledImage(((ImageIcon) icon).getImage()));

    return disabledIcon;
  }

  /**
   * Sets the icon displayed when the label is disabled (this is a bound
   * property with the name 'disabledIcon').
   *
   * @param newIcon The disabled icon (<code>null</code> permitted).
   * 
   * @see #getDisabledIcon()
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
   * Sets the keycode that will be the label's mnemonic (this is a bound
   * property with the name 'displayedMnemonic').  If the label is used as a 
   * label for another component, the label will give focus to that component 
   * when the mnemonic is activated.
   *
   * @param mnemonic The keycode to use for the mnemonic.
   * 
   * @see #getDisplayedMnemonic()
   */
  public void setDisplayedMnemonic(int mnemonic)
  {
    if (displayedMnemonic != mnemonic)
      {
        int old = displayedMnemonic;
        displayedMnemonic = mnemonic;
        firePropertyChange("displayedMnemonic", old, displayedMnemonic);
        if (text != null)
          setDisplayedMnemonicIndex(text.toUpperCase().indexOf(mnemonic));
      }
  }

  /**
   * Sets the character that will be the label's mnemonic. If the
   * label is used as a label for another component, the label will give
   * focus to that component when the mnemonic is activated via the keyboard.
   *
   * @param mnemonic The character to use for the mnemonic (this will be
   *     converted to the equivalent upper case character).
   *     
   * @see #getDisplayedMnemonic()
   */
  public void setDisplayedMnemonic(char mnemonic)
  {
    setDisplayedMnemonic((int) Character.toUpperCase(mnemonic));
  }

  /**
   * Returns the keycode that is used for the label's mnemonic.
   *
   * @return The keycode that is used for the label's mnemonic.
   * 
   * @see #setDisplayedMnemonic(int)
   */
  public int getDisplayedMnemonic()
  {
    return displayedMnemonic;
  }

  /**
   * Sets the index of the character in the text that will be underlined to
   * indicate that it is the mnemonic character for the label.  You only need
   * to call this method if you wish to override the automatically calculated
   * character index.  For instance, for a label "Find Next" with the mnemonic
   * character 'n', you might wish to underline the second occurrence of 'n'
   * rather than the first (which is the default).
   * <br><br>
   * Note that this method does not validate the character at the specified 
   * index to ensure that it matches the key code returned by
   * {@link #getDisplayedMnemonic()}.
   *
   * @param newIndex The index of the character to underline.
   *
   * @throws IllegalArgumentException If index less than -1 or index is greater
   *         than or equal to the label length.
   *         
   * @see #getDisplayedMnemonicIndex()
   * @since 1.4
   */
  public void setDisplayedMnemonicIndex(int newIndex)
    throws IllegalArgumentException
  {
    int maxValid = -1;
    if (text != null)
      maxValid = text.length() - 1;
    if (newIndex < -1 || newIndex > maxValid)
      throw new IllegalArgumentException();

    if (newIndex != displayedMnemonicIndex)
      {
        int oldIndex = displayedMnemonicIndex;
        displayedMnemonicIndex = newIndex;
        firePropertyChange("displayedMnemonicIndex", oldIndex, newIndex);
      }
  }

  /**
   * Returns the index of the character in the label's text that will be
   * underlined (to indicate that it is the mnemonic character), or -1 if no
   * character is to be underlined.
   *
   * @return The index of the character that will be underlined.
   * 
   * @see #setDisplayedMnemonicIndex(int)
   * @since 1.4
   */
  public int getDisplayedMnemonicIndex()
  {
    return displayedMnemonicIndex;
  }

  /**
   * Checks the specified key to ensure that it is valid as a horizontal 
   * alignment, throwing an {@link IllegalArgumentException} if the key is
   * invalid.  Valid keys are {@link #LEFT}, {@link #CENTER}, {@link #RIGHT}, 
   * {@link #LEADING} and {@link #TRAILING}.
   *
   * @param key The key to check.
   * @param message The message of the exception to be thrown if the key is
   *        invalid.
   *
   * @return The key if it is valid.
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
   * Checks the specified key to ensure that it is valid as a vertical 
   * alignment, throwing an {@link IllegalArgumentException} if the key is
   * invalid.  Valid keys are {@link #TOP}, {@link #CENTER} and {@link #BOTTOM}.
   *
   * @param key The key to check.
   * @param message The message of the exception to be thrown if the key is
   *        invalid.
   *
   * @return The key if it is valid.
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
   * Returns the gap between the icon and the text.
   *
   * @return The gap between the icon and the text.
   * 
   * @see #setIconTextGap(int)
   */
  public int getIconTextGap()
  {
    return iconTextGap;
  }

  /**
   * Sets the gap between the icon and the text, in the case that both are 
   * visible (this is a bound property with the name 'iconTextGap'). 
   *
   * @param newGap The gap (in pixels).
   * 
   * @see #getIconTextGap()
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
   * Returns the vertical alignment of the label (one of
   * {@link #TOP}, {@link #CENTER} and {@link #BOTTOM}).  The default value
   * depends on the installed look and feel, but is usually {@link #CENTER}.
   *
   * @return The vertical alignment.
   * 
   * @see #setVerticalAlignment(int)
   */
  public int getVerticalAlignment()
  {
    return verticalAlignment;
  }

  /**
   * Sets the vertical alignment for the label (this is a bound property with
   * the name 'verticalAlignment').  The vertical alignment determines where 
   * the label (icon and text) will be placed vertically within the component 
   * bounds.  Valid alignment codes are {@link #TOP}, {@link #CENTER} and 
   * {@link #BOTTOM}.
   *
   * @param alignment The vertical alignment of the label.
   * 
   * @throws IllegalArgumentException if <code>alignment</code> is not one of 
   *     the specified values.
   *     
   * @see #getVerticalAlignment()
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
   * Returns the horizontal alignment of the label (one of {@link #LEFT}, 
   * {@link #CENTER}, {@link #RIGHT}, {@link #LEADING} and {@link #TRAILING}).
   * The default value depends on the installed look and feel, but is usually 
   * {@link #LEFT}.
   *
   * @return The horizontal alignment.
   * 
   * @see #setHorizontalAlignment(int)
   */
  public int getHorizontalAlignment()
  {
    return horizontalAlignment;
  }

  /**
   * Sets the horizontal alignment for the label (this is a bound property with
   * the name 'horizontalAlignment').  The horizontal alignment determines where 
   * the label (icon and text) will be placed horizontally within the 
   * component bounds.  Valid alignment codes are {@link #LEFT}, 
   * {@link #CENTER}, {@link #RIGHT}, {@link #LEADING} and {@link #TRAILING}.
   *
   * @param alignment The horizontal alignment of the label.
   * 
   * @throws IllegalArgumentException if <code>alignment</code> is not one of 
   *     the specified values.
   *     
   * @see #getHorizontalAlignment()
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
   * Returns the vertical position of the label's text relative to the icon. 
   * This will be one of {@link #TOP}, {@link #CENTER} and {@link #BOTTOM}.
   * 
   * @return The vertical position of the label's text relative to the icon.
   * 
   * @see #setVerticalTextPosition(int)
   */
  public int getVerticalTextPosition()
  {
    return verticalTextPosition;
  }

  /**
   * Sets the vertical position of the label's text relative to the icon (this
   * is a bound property with the name 'verticalTextPosition').  Valid 
   * positions are {@link #TOP}, {@link #CENTER} and {@link #BOTTOM}.
   *
   * @param textPosition The vertical text position.
   * 
   * @throws IllegalArgumentException if <code>textPosition</code> is not one
   *     of the specified values.
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
   * Returns the horizontal position of the label's text relative to the icon. 
   * This will be one of {@link #LEFT}, {@link #CENTER}, {@link #RIGHT}, 
   * {@link #LEADING} and {@link #TRAILING}.
   * 
   * @return The horizontal position of the label's text relative to the icon.
   * 
   * @see #setHorizontalTextPosition(int)
   */
  public int getHorizontalTextPosition()
  {
    return horizontalTextPosition;
  }

  /**
   * Sets the horizontal position of the label's text relative to the icon (this
   * is a bound property with the name 'horizontalTextPosition').  Valid 
   * positions are {@link #LEFT}, {@link #CENTER}, {@link #RIGHT}, 
   * {@link #LEADING} and {@link #TRAILING}.
   *
   * @param textPosition The horizontal text position.
   * 
   * @throws IllegalArgumentException if <code>textPosition</code> is not one
   *     of the specified values.
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
   * Returns false if the current icon image (current icon will depend on 
   * whether the label is enabled) is not equal to the passed in image.
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
   * Returns the component that this <code>JLabel</code> is providing the label
   * for.  This component will typically receive the focus when the label's 
   * mnemonic key is activated via the keyboard.
   *
   * @return The component (possibly <code>null</code>).
   */
  public Component getLabelFor()
  {
    return labelFor;
  }

  /**
   * Sets the component that this <code>JLabel</code> is providing the label
   * for (this is a bound property with the name 'labelFor').  This component
   * will typically receive the focus when the label's mnemonic key is 
   * activated via the keyboard.
   *
   * @param c  the component (<code>null</code> permitted).
   * 
   * @see #getLabelFor()
   */
  public void setLabelFor(Component c)
  {
    if (c != labelFor)
      {
        Component oldLabelFor = labelFor;
        labelFor = c;
        firePropertyChange("labelFor", oldLabelFor, labelFor);

        // We put the label into the client properties for the labeled
        // component so that it can be read by the AccessibleJComponent.
        // The other option would be to reserve a default visible field
        // in JComponent, but since this is relatively seldomly used, it
        // would be unnecessary waste of memory to do so.
        if (oldLabelFor instanceof JComponent)
          {
            ((JComponent) oldLabelFor).putClientProperty(LABEL_PROPERTY, null);
          }

        if (labelFor instanceof JComponent)
          {
            ((JComponent) labelFor).putClientProperty(LABEL_PROPERTY, this);
          }

      }
  }

  /**
   * Sets the font for the label (this a bound property with the name 'font').
   *
   * @param f The font (<code>null</code> permitted).
   */
  public void setFont(Font f)
  {
    super.setFont(f);
    repaint();
  }

  /**
   * Returns the object that provides accessibility features for this
   * <code>JLabel</code> component.
   *
   * @return The accessible context (an instance of {@link AccessibleJLabel}).
   */
  public AccessibleContext getAccessibleContext()
  {
    if (accessibleContext == null)
      accessibleContext = new AccessibleJLabel();
    return accessibleContext;
  }
}
