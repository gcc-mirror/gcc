/* JPasswordField.java --
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

// Imports
import java.io.*;
import javax.accessibility.*;
import javax.swing.text.*;

/**
 * JPasswordField
 * @author	Andrew Selkirk
 * @version	1.0
 */
public class JPasswordField extends JTextField {

	//-------------------------------------------------------------
	// Classes ----------------------------------------------------
	//-------------------------------------------------------------

	/**
	 * AccessibleJPasswordField
	 */
	protected class AccessibleJPasswordField extends AccessibleJTextField {

		//-------------------------------------------------------------
		// Variables --------------------------------------------------
		//-------------------------------------------------------------


		//-------------------------------------------------------------
		// Initialization ---------------------------------------------
		//-------------------------------------------------------------

		/**
		 * Constructor AccessibleJPasswordField
		 * @param component TODO
		 */
		protected AccessibleJPasswordField(JPasswordField component) {
			super(component);
			// TODO
		} // AccessibleJPasswordField()


		//-------------------------------------------------------------
		// Methods ----------------------------------------------------
		//-------------------------------------------------------------

		/**
		 * getAccessibleRole
		 * @returns AccessibleRole
		 */
		public AccessibleRole getAccessibleRole() {
			return AccessibleRole.PASSWORD_TEXT;
		} // getAccessibleRole()


	} // AccessibleJPasswordField


	//-------------------------------------------------------------
	// Variables --------------------------------------------------
	//-------------------------------------------------------------

	/**
	 * uiClassID
	 */
	private static final String uiClassID = "PasswordFIeldUI";

	/**
	 * echoChar.  Default is 0
	 */
	private char echoChar = 0;


	//-------------------------------------------------------------
	// Initialization ---------------------------------------------
	//-------------------------------------------------------------

	/**
	 * Constructor JPasswordField
	 */
	public JPasswordField() {
		// TODO
	} // JPasswordField()

	/**
	 * Constructor JPasswordField
	 * @param text TODO
	 */
	public JPasswordField(String text) {
		// TODO
	} // JPasswordField()

	/**
	 * Constructor JPasswordField
	 * @param columns TODO
	 */
	public JPasswordField(int columns) {
		// TODO
	} // JPasswordField()

	/**
	 * Constructor JPasswordField
	 * @param text TODO
	 * @param columns TODO
	 */
	public JPasswordField(String text, int columns) {
		// TODO
	} // JPasswordField()

	/**
	 * Constructor JPasswordField
	 * @param document TODO
	 * @param text TODO
	 * @param columns TODO
	 */
	public JPasswordField(Document document, String text, int columns) {
		// TODO
	} // JPasswordField()


	//-------------------------------------------------------------
	// Methods ----------------------------------------------------
	//-------------------------------------------------------------

	/**
	 * writeObject
	 * @param stream TODO
	 * @exception IOException TODO
	 */
	private void writeObject(ObjectOutputStream stream) throws IOException {
		// TODO
	} // writeObject()

	/**
	 * copy
	 */
	public void copy() {
		// TODO
	} // copy()

	/**
	 * getUIClassID
	 * @returns String
	 */
	public String getUIClassID() {
		return uiClassID;
	} // getUIClassID()

	/**
	 * getEchoChar
	 * @returns char
	 */
	public char getEchoChar() {
		return echoChar;
	} // getEchoChar()

	/**
	 * setEchoChar
	 * @param echo TODO
	 */
	public void setEchoChar(char echo) {
		this.echoChar = echo;
		// TODO
	} // setEchoChar()

	/**
	 * echoCharIsSet
	 * @returns boolean
	 */
	public boolean echoCharIsSet() {
		return (echoChar == 0);
	} // echoCharIsSet()

	/**
	 * cut
	 */
	public void cut() {
		// TODO
	} // cut()

	/**
	 * getText
	 * @returns String
	 */
	public String getText() {
		return null; // TODO
	} // getText()

	/**
	 * getText
	 * @param offset TODO
	 * @param length TODO
	 * @exception BadLocationException TODO
	 * @returns String
	 */
	public String getText(int offset, int length) throws BadLocationException {
		return null; // TODO
	} // getText()

	/**
	 * getPassword
	 * @returns char[]
	 */
	public char[] getPassword() {
		return null; // TODO
	} // getPassword()

	/**
	 * paramString
	 * @returns String
	 */
	protected String paramString() {
		return null; // TODO
	} // paramString()

	/**
	 * getAccessibleContext
	 * @returns AccessibleContext
	 */
	public AccessibleContext getAccessibleContext() {
		if (accessibleContext == null) {
			accessibleContext = new AccessibleJPasswordField(this);
		} // if
		return accessibleContext;
	} // getAccessibleContext()


} // JPasswordField
