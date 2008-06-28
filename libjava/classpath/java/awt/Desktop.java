/* Desktop.java -- enable desktop integration between java programs and system
 programs.
 Copyright (C) 2006 Free Software Foundation, Inc.

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


package java.awt;

import java.awt.peer.DesktopPeer;
import java.io.File;
import java.io.IOException;
import java.net.URI;

/**
 * This class enables Java application to access system commands to perform
 * desktop oriented operations, like writing and sending emails, or surfing
 * webpages with the system browser or editing/printing files with a default
 * editor. Methods are provided to handle these common operations, plus an
 * <code>open</code> command selects a default registered application for the
 * specified file type. For example, opening an odf file results in launching
 * OpenOffice. If an operation is not supported, or the application fails to
 * launch, an exception is generated.
 * 
 * <strong>Implementation note: </strong>As this class is used to manage Desktop
 * integration, we provide some extension to configure the behaviour of this
 * class depending on the type of dektop that is detected.<br />
 * 
 * First of all, we support 5 system properties that can be used to define
 * the application to launch in any given case. These properties are:<br />
 * <br />
 * <code>gnu.java.awt.peer.Desktop.html.command</code><br />
 * <code>gnu.java.awt.peer.Desktop.mail.command</code><br />
 * <code>gnu.java.awt.peer.Desktop.edit.command</code><br />
 * <code>gnu.java.awt.peer.Desktop.print.command</code><br />
 * <code>gnu.java.awt.peer.Desktop.open.command</code><br />
 * <br />
 * <br />
 * These can be specified from the command line and have priority on every
 * other setting.<br />
 * <br />
 * The second method supported is defining a Java preference.<br />
 * The main preference node is a <strong>user node</strong> relative to the
 * class <code>gnu.java.awt.peer.ClasspathDesktopPeer</code>. This node
 * contains a child for each supported operation. The key for each type is
 * always <code>command</code>:
 * <br /><br />
 * <code>gnu.java.awt.peer.Desktop.html.command</code><br />
 * <code>gnu.java.awt.peer.Desktop.mail.command</code><br />
 * <code>gnu.java.awt.peer.Desktop.edit.command</code><br />
 * <code>gnu.java.awt.peer.Desktop.print.command</code><br />
 * <code>gnu.java.awt.peer.Desktop.open.command</code><br />
 * <br />
 * <br />
 * The access to these keys is done with the Preference API or, if outside
 * of the java scope, is done in a backend dependent way. For example,
 * with the GConf backend, you can access these properties
 * with (may not be accurate on your system):
 * <br /><br />
 * <code>
 * gconftool-2 -g /apps/classpath/gnu/java/awt/peer/Desktop/html/command
 * </code> 
 * 
 * @author Mario Torre <neugens@limasoftware.net>
 * @since 1.6
 */
public class Desktop
{
  /**
   * Represents an action type supported by a platform.
   * 
   * To determine if a given action is supported by the platform,
   * use the <code>Desktop.isSupported(java.awt.Desktop.Action)</code>
   * method.
   * 
   * @author Mario Torre <neugens@limasoftware.net>
   */
  public enum Action
  {
    BROWSE, EDIT, MAIL, OPEN, PRINT
  }

  private DesktopPeer peer;

  private Desktop()
  {
    /* nothing to be done */
  }
  
  /**
   * Returns an instance of the Desktop Class.
   * 
   * If this implementation does not support Desktop, an 
   * UnsupportedOperationException will be thrown.
   * Also, an HeadlessException will be generated if
   * GraphicsEnvironment.isHeadless() returns true.
   * 
   * @throws UnsupportedOperationException
   * @throws HeadlessException
   */
  public static Desktop getDesktop() throws UnsupportedOperationException,
      HeadlessException
  {
    if (GraphicsEnvironment.isHeadless())
      throw new HeadlessException();
    
    if (!Desktop.isDesktopSupported())
      throw new UnsupportedOperationException();
    
    Desktop desktop = new Desktop();
    desktop.peer = Toolkit.getDefaultToolkit().createDesktopPeer(desktop);
    
    return desktop;
  }

  /**
   * Check if this implementation supports Desktop.
   * If true, use getDesktop to get an instance of this class.
   * 
   * This implementation will return false when GraphicsEnvironment.isHeadless
   * returns true.
   * 
   * @return true if this class is supported on the current platform;
   * false otherwise
   */
  public static boolean isDesktopSupported()
  {
    if (GraphicsEnvironment.isHeadless())
      return false;
    
    return true;
  }
  
  /**
   * Check if the given Action is supported by this implementation.
   * 
   * @param action
   * @return
   */
  public boolean isSupported(Desktop.Action action)
  {
    return peer.isSupported(action);
  }
  
  /**
   * Launches the Desktop default browser to open the given <code>uri</code>.
   * 
   * If a security manager exists and denies
   * AWTPermission("showWindowWithoutWarningBanner"),a SecurityException will
   * be generated.
   * 
   * @param uri
   * @throws IOException
   */
  public void browse(URI uri)
    throws IOException
  {
    peer.browse(uri);
  }
  
  /**
   * Launch the edit command to edit this file.
   * File should already exist before the editing starts. 
   * 
   * If a security manager exists and
   * SecurityManager.checkRead(java.lang.String) method denies read access to
   * the file, or SecurityManager.checkPrintJobAccess() method denies the
   * permission to print the file, a SecurityException will be generated.
   * 
   * @param file
   * @throws IOException
   */
  public void edit(File file)
    throws IOException
  {
    peer.edit(file);
  }
  
  /**
   * Launches the Desktop default mailer.
   * 
   * If a security manager exists and denies
   * AWTPermission("showWindowWithoutWarningBanner"), a SecurityException will
   * be generated.
   *
   * @throws IOException
   */
  public void mail()
    throws IOException
  {
    peer.mail();
  }
  
  /**
   * Launches the Desktop default mailer, with the given mailtoURI
   * as agrument. The <code>mailtoURI</code> must conform to the
   * {@link http://www.ietf.org/rfc/rfc2368.txt The mailto URL scheme (RFC 2368)} 
   * 
   * If a security manager exists and denies
   * AWTPermission("showWindowWithoutWarningBanner"), a SecurityException will
   * be generated.
   * 
   * @param mailtoURI
   * @throws IOException
   */
  public void mail(URI mailtoURI)
    throws IOException
  {
    peer.mail(mailtoURI);
  }
  
  /**
   * Launches the Desktop default application to open the given File.
   * If <code>file</code> is a directory, a file manager is launched.
   * 
   * @param file
   * @throws IOException
   */
  public void open(File file)
    throws IOException
  {
    peer.open(file);
  }

  /**
   * Launch the print program to print this file.
   * 
   * @param file
   * @throws IOException
   */
  public void print(File file)
    throws IOException
  {
    peer.print(file);
  }
}
