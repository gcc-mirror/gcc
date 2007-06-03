/* ClasspathDesktopPeer.java -- Offers a concrete implementation for DesktopPeer
 Copyright (C) 2006, 2007 Free Software Foundation, Inc.

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

package gnu.java.awt.peer;

import java.awt.AWTPermission;
import java.awt.Desktop.Action;
import java.awt.peer.DesktopPeer;

import java.io.File;
import java.io.IOException;

import java.net.URI;

import java.util.prefs.Preferences;

/**
 * Offers a common implementation for the Desktop peers, that enables
 * access to default system application within java processes.
 * 
 * @author Mario Torre <neugens@limasoftware.net>
 */
public class ClasspathDesktopPeer
    implements DesktopPeer
{
  /** This is the fallback browser, if no desktop was detected. */
  protected static final String _DEFAULT_BROWSER = "firefox";
  
  /** gnu.java.awt.peer.Desktop.html.command */
  protected static final String _BROWSE = "html";
  
  /** gnu.java.awt.peer.Desktop.mail.command */
  protected static final String _MAIL = "mail";
  
  /** gnu.java.awt.peer.Desktop.edit.command */
  protected static final String _EDIT = "edit";
  
  /** gnu.java.awt.peer.Desktop.print.command */
  protected static final String _PRINT = "print";
  
  /** gnu.java.awt.peer.Desktop.open.command */
  protected static final String _OPEN = "open";
  
  /** */
  protected static final KDEDesktopPeer kde = new KDEDesktopPeer();
  
  /** */
  protected static final GnomeDesktopPeer gnome = new GnomeDesktopPeer();
  
  /** */
  protected static final ClasspathDesktopPeer classpath =
    new ClasspathDesktopPeer(); 
  
  /**
   * Preference subsystem. Packagers and users can override the default
   * behaviour of this class via preferences and system properties.
   */
  protected Preferences prefs =
    Preferences.userNodeForPackage(ClasspathDesktopPeer.class).node("Desktop");
  
  /**
   * @param target
   */
  protected ClasspathDesktopPeer()
  {
    /* nothing to do */
  }

  public boolean isSupported(Action action)
  {
    String check = null;
    
    switch(action)
    {
      case BROWSE:
        check = _BROWSE;
        break;
        
      case MAIL:
        check = _MAIL;
        break;
        
      case EDIT:
        check = _EDIT;
        break;
        
      case PRINT:
        check = _PRINT;
        break;
      
      case OPEN: default:
        check = _OPEN;
        break;
    }
    
    return this.supportCommand(check);
  }

  public void browse(URI url) throws IOException
  {
    checkPermissions();
    
    String browser = getCommand(_BROWSE);
    
    if (browser == null)
      throw new UnsupportedOperationException();
    
    browser = browser + " " + url.toString();
    
    Runtime.getRuntime().exec(browser);
  }

  public void edit(File file) throws IOException
  {
    checkPermissions(file, false);
    
    String edit = getCommand(_EDIT);
    
    if (edit == null)
      throw new UnsupportedOperationException();
    
    edit = edit + " " + file.getAbsolutePath(); 
    Runtime.getRuntime().exec(edit);
  }

  public void mail(URI mailtoURL) throws IOException
  {
    checkPermissions();
    
    String scheme = mailtoURL.getScheme();
    if (scheme == null || !scheme.equalsIgnoreCase("mailto"))
      throw new IllegalArgumentException("URI Scheme not of type mailto");
    
    String mail = getCommand(_MAIL);
    
    if (mail == null)
      throw new UnsupportedOperationException();
    
    mail = mail + " " + mailtoURL.toString();
    
    Runtime.getRuntime().exec(mail);
  }

  public void mail() throws IOException
  {
    checkPermissions();
    
    String mail = getCommand(_MAIL);
    
    if (mail == null)
      throw new UnsupportedOperationException();
    
    Runtime.getRuntime().exec(mail);
  }
  
  public void open(File file) throws IOException
  {
    checkPermissions(file, true);
    
    String open = getCommand(_OPEN);
    
    if (open == null)
      throw new UnsupportedOperationException();
    
    open = open + " " + file.getAbsolutePath(); 
    Runtime.getRuntime().exec(open);
  }

  public void print(File file) throws IOException
  {
    checkPrintPermissions(file);
    
    String print = getCommand(_PRINT);
    
    if (print == null)
      throw new UnsupportedOperationException();
    
    print = print + " " + file.getAbsolutePath(); 
    Runtime.getRuntime().exec(print);
  }
  
  protected String getCommand(String action)
  {
    // check if a system property exist
    String command =
      System.getProperty("gnu.java.awt.peer.Desktop." + action + ".command");
    
    // otherwise, get it from preferences, if any
    if (command == null)
      {
        command = prefs.node(action).get("command", null);
      }
    
    return command;
  }
  
  /**
   * Note: Checks for AWTPermission("showWindowWithoutWarningBanner") only.
   */
  protected void checkPermissions()
  {
    SecurityManager sm = System.getSecurityManager();
    if (sm != null) {
        sm.checkPermission(new AWTPermission("showWindowWithoutWarningBanner"));
    }
  }
  
  /**
   * Calls checkPermissions() and checks for SecurityManager.checkRead() 
   * and, if readOnly is false, for SecurityManager.checkWrite() 
   */
  protected void checkPermissions(File file, boolean readOnly)
  {
    checkPermissions();
    
    SecurityManager sm = System.getSecurityManager();
    if (sm != null) {
        sm.checkRead(file.toString());
        if (!readOnly) sm.checkWrite(file.toString());
    }
  }
  
  /**
   * Calls checkPermissions(file, true) and checks for
   * SecurityManager.checkPrintJobAccess()
   */
  protected void checkPrintPermissions(File file)
  {
    checkPermissions(file, true);
    
    SecurityManager sm = System.getSecurityManager();
    if (sm != null) {
      sm.checkPrintJobAccess();
    }  
  }
  
  /**
   * @param check
   * @return
   */
  protected boolean supportCommand(String check)
  {
    return ((this.getCommand(check) != null) ?  true : false);
  }
  
  /**
   * @return
   */
  public static DesktopPeer getDesktop()
  {
    //  check if we are under Gnome or KDE or anything else
    String desktopSession = System.getenv("GNOME_DESKTOP_SESSION_ID");
    if (desktopSession == null)
      { 
        desktopSession = System.getenv("KDE_FULL_SESSION");
        if (desktopSession != null)
          return kde;
      }
    else
      {
        return gnome;
      }
    
    // revert to this class for default values
    return classpath;
  }
}
