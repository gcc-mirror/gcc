/* JPasswordField.java --
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


package javax.swing;

import java.io.IOException;
import java.io.ObjectOutputStream;

import javax.accessibility.AccessibleContext;
import javax.accessibility.AccessibleRole;
import javax.swing.text.BadLocationException;
import javax.swing.text.Document;

/**
 * class JPasswordField
 * 
 * @author Andrew Selkirk
 * @version 1.0
 */
public class JPasswordField extends JTextField
{
  /**
   * AccessibleJPasswordField
   */
  protected class AccessibleJPasswordField extends AccessibleJTextField
  {
    private static final long serialVersionUID = -8477039424200681086L;

    /**
     * Constructor AccessibleJPasswordField
     */
    protected AccessibleJPasswordField()
    {
    }

    /**
     * getAccessibleRole
     * 
     * @return AccessibleRole
     */
    public AccessibleRole getAccessibleRole()
    {
      return AccessibleRole.PASSWORD_TEXT;
    }
  }

  /**
   * echoChar.  Default is 0.
   */
  private char echoChar = 0;

  /**
   * Creates a <code>JPasswordField</code> object.
   */
  public JPasswordField()
  {
    this(null, null, 0);
  }

  /**
   * Creates a <code>JPasswordField</code> object.
   * 
   * @param text the initial text
   */
  public JPasswordField(String text)
  {
    this(null, text, 0);
  }

  /**
   * Creates a <code>JPasswordField</code> object.
   * 
   * @param columns the number of columns
   */
  public JPasswordField(int columns)
  {
    this(null, null, columns);
  }

  /**
   * Creates a <code>JPasswordField</code> object.
   * 
   * @param text the initial text
   * @param columns the number of columns
   */
  public JPasswordField(String text, int columns)
  {
    this(null, text, columns);
  }

  /**
   * Creates a <code>JPasswordField</code> object.
   * 
   * @param document the document to use
   * @param text the initial text
   * @param columns the number of columns
   */
  public JPasswordField(Document document, String text, int columns)
  {
    super(document, text, columns);
  }

  /**
   * writeObject
   * 
   * @param stream the stream to write to
   * 
   * @exception IOException if an error occurs
   */
  private void writeObject(ObjectOutputStream stream) throws IOException
  {
    // TODO: Implement me.
  }

  /**
   * Returns the <code>UIClassID</code>
   * 
   * @return the string "PasswordFieldUI"
   */
  public String getUIClassID()
  {
    return "PasswordFieldUI";
  }

  /**
   * getEchoChar
   * 
   * @return the echo char
   */
  public char getEchoChar()
  {
    return echoChar;
  }

  /**
   * setEchoChar
   * 
   * @param echo the echo char
   */
  public void setEchoChar(char echo)
  {
    this.echoChar = echo;
  }

  /**
   * echoCharIsSet
   * 
   * @return <code>true</code> if the echo char is set,
   * <code>false</code> otherwise.
   */
  public boolean echoCharIsSet()
  {
    return echoChar == 0;
  }

  /**
   * Copies the selected text into the clipboard. This operation is not
   * allowed in a password input field.
   */
  public void copy()
  {
    UIManager.getLookAndFeel().provideErrorFeedback(this);
  }

  /**
   * Cuts the selected text and puts it into the clipboard. This operation
   * is not allowed in a password input field.
   */
  public void cut()
  {
    UIManager.getLookAndFeel().provideErrorFeedback(this);
  }

  /**
   * getText
   * 
   * @return String
   * 
   * @deprecated
   */
  public String getText()
  {
    return null; // TODO
  }

  /**
   * getText
   * 
   * @param offset TODO
   * @param length TODO
   * 
   * @return String
   * 
   * @exception BadLocationException TODO
   *
   * @deprecated
   */
  public String getText(int offset, int length) throws BadLocationException
  {
    return null; // TODO
  }

  /**
   * getPassword
   * 
   * @return char[]
   */
  public char[] getPassword()
  {
    return new char[0]; // TODO
  }

  /**
   * paramString
   * 
   * @return String
   */
  protected String paramString()
  {
    return null; // TODO
  }

  /**
   * getAccessibleContext
   * 
   * @return the <code>AccessibleContext</code> object
   */
  public AccessibleContext getAccessibleContext()
  {
    if (accessibleContext == null)
      accessibleContext = new AccessibleJPasswordField();

    return accessibleContext;
  }
}
