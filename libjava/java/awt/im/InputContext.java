/* InputContext.java -- provides the context for text input
   Copyright (C) 2002, 2003 Free Software Foundation, Inc.

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

package java.awt.im;

import java.awt.AWTEvent;
import java.awt.AWTException;
import java.awt.Component;
import java.awt.im.spi.InputMethod;
import java.awt.im.spi.InputMethodDescriptor;
import java.io.BufferedReader;
import java.io.InputStreamReader;
import java.io.IOException;
import java.net.URL;
import java.util.ArrayList;
import java.util.Enumeration;
import java.util.HashMap;
import java.util.Locale;
import gnu.java.util.EmptyEnumeration;

/**
 * Provides a context for controlling input methods and keyboard layouts.
 * This class provides the communication layer between the client component,
 * and the various locale-dependent text entry input methods that can be used
 * for the client. By default, there is one instance per Window, shared among
 * all components, but this limits text entry to one component at a time.
 * Thus, text components can create their own instance to allow text entry
 * in multiple components at a time.
 *
 * <p>By using the interfaces of {@link java.awt.im.spi}, you can install
 * extensions which allow additional input methods. Some of these may use
 * platform native input methods, or keyboard layouts provided by the platform.
 * Input methods are unavailable if none have been installed and the platform
 * has no underlying native input methods. Extensions are installed as jar
 * files, usually accessed in the default extension location or specified by
 * the -extdir VM flag. The jar must contain a file named
 * "META_INF/services/java.awt.im.spi.InputMethodDescriptor" which lists,
 * one entry per line in UTF-8 encoding, each class in the jar that implements
 * java.awt.im.spi.InputMethodDescriptor.
 *
 * @author Eric Blake <ebb9@email.byu.edu>
 * @see Component#getInputContext()
 * @see Component#enableInputMethods(boolean)
 * @since 1.2
 * @status updated to 1.4, but unverified
 */
public class InputContext
{
  /**
   * The list of installed input method descriptors.
   */
  private static final ArrayList descriptors = new ArrayList();
  static
  {
    Enumeration enum;
    try
      {
        enum = ClassLoader.getSystemResources
          ("META_INF/services/java.awt.im.spi.InputMethodDescriptor");
      }
    catch (IOException ex)
      {
        // XXX Should we do something else?
        enum = EmptyEnumeration.getInstance();
      }
    while (enum.hasMoreElements())
      {
        URL url = (URL) enum.nextElement();
        BufferedReader in;
        String line;
        try
          {
            in = new BufferedReader
              (new InputStreamReader(url.openConnection().getInputStream(),
                                     "UTF-8"));
            line = in.readLine().trim();
          }
        catch (IOException ignored)
          {
            continue;
          }
      outer:
        while (line != null)
          {
            try
              {
                if (line.charAt(0) != '#')
                  {
                    Class c = Class.forName(line);
                    descriptors.add((InputMethodDescriptor) c.newInstance());
                  }
                line = in.readLine().trim();
              }
            catch (IOException e)
              {
                continue outer;
              }
            catch (Exception ignored)
              {
              }
          }
      }
  }

  /** The current input method; null if no input methods are installed. */
  private InputMethod im;

  /** Map of locales to the most recently selected input method. */
  private final HashMap recent = new HashMap();

  /** The list of acceptable character subsets. */
  private Character.Subset[] subsets;

  /**
   * Construct an InputContext. This is protected, so clients must use
   * {@link #getInstance()} instead.
   */
  protected InputContext()
  {
  }

  /**
   * Returns a new InputContext.
   *
   * @return a new instance, initialized to the default locale if available
   */
  public static InputContext getInstance()
  {
    InputContext ic = new InputContext();
    ic.selectInputMethod(Locale.getDefault());
    return ic;
  }

  /**
   * Attempts to select an input method or keyboard layout which supports the
   * given locale. This returns true if a locale is available and was selected.
   * The following steps are taken in choosing an input method:<ul>
   * <li>If the currently selected input method or keyboard layout supports
   * the requested locale, it remains selected.</li>
   * <li>If there is no input method or keyboard layout available that
   * supports the requested locale, the current input method or keyboard
   * layout remains selected.</li>
   * <li>If the user has previously selected an input method or keyboard
   * layout for the requested locale from the user interface, then the most
   * recently selected such input method or keyboard layout is reselected.</li>
   * <li>Otherwise, an input method or keyboard layout that supports the
   * requested locale is selected in an implementation dependent way. This
   * implementation chooses the first input method which supports the requested
   * locale based on the InputMethodDescriptors loaded from the extensions
   * installed on the CLASSPATH.</li>
   * </ul>
   *
   * <p>Before switching away from an input method, any currently uncommitted
   * text is committed. Not all host operating systems provide API to
   * determine the locale of the currently selected native input method or
   * keyboard layout, and to select a native input method or keyboard layout
   * by locale. For host operating systems that don't provide such API,
   * selectInputMethod assumes that native input methods or keyboard layouts
   * provided by the host operating system support only the system's default
   * locale.
   *
   * <p>An example of where this may be called is in a multi-language document,
   * when moving the insertion point between sections of different locale, so
   * that the user may use the input method appropriate to that section of the
   * document.
   *
   * @param locale the desired new locale
   * @return true if the new locale is active
   * @throws NullPointerException if locale is null
   */
  public boolean selectInputMethod(Locale locale)
  {
    if (im != null && im.setLocale(locale))
      {
        recent.put(locale, im);
        return true;
      }
    InputMethod next = (InputMethod) recent.get(locale);
  outer:
    if (next != null)
      for (int i = 0, limit = descriptors.size(); i < limit; i++)
        {
          InputMethodDescriptor d = (InputMethodDescriptor) descriptors.get(i);
          Locale[] list;
          try
            {
              list = d.getAvailableLocales();
            }
          catch (AWTException ignored)
            {
              continue;
            }
          for (int j = list.length; --j >= 0; )
            if (locale.equals(list[j]))
              {
                try
                  {
                    next = d.createInputMethod();
                    recent.put(locale, next);
                  }
                catch (Exception ignored)
                  {
                    continue;
                  }
              }
        }
    if (next == null)
      return false;
    // XXX I'm not sure if this does all the necessary steps in the switch.
    if (im != null)
      {
        try
          {
            next.setCompositionEnabled(im.isCompositionEnabled());
          }
        catch (UnsupportedOperationException ignored)
          {
          }
        im.endComposition();
        im.deactivate(false);
        im.hideWindows();
      }
    im = next;
    im.setLocale(locale);
    im.setCharacterSubsets(subsets);
    return true;
  }

  /**
   * Returns the current locale of the current input method or keyboard
   * layout. Returns null if the input context does not have a current input
   * method or keyboard layout or if the current input method's
   * {@link InputMethod#getLocale()} method returns null. Not all host
   * operating systems provide API to determine the locale of the currently
   * selected native input method or keyboard layout. For host operating
   * systems that don't provide such API, getLocale assumes that the current
   * locale of all native input methods or keyboard layouts provided by the
   * host operating system is the system's default locale.
   *
   * @return the locale of the current input method, or null
   * @since 1.3
   */
  public Locale getLocale()
  {
    return im == null ? null : im.getLocale();
  }

  /**
   * Sets the subsets of Unicode characters allowed to be input by the current
   * input method, as well as subsequent input methods. The value of null
   * implies all characters are legal. Applications should not rely on this
   * behavior, since native host input methods may not allow restrictions.
   * If no current input method is available, this has no immediate effect.
   *
   * @param subsets the set of Unicode subsets to accept, or null
   */
  public void setCharacterSubsets(Character.Subset[] subsets)
  {
    this.subsets = subsets;
    if (im != null)
      im.setCharacterSubsets(subsets);
  }

  /**
   * Changes the enabled status of the current input method. An input method
   * that is enabled for composition interprets incoming events for both
   * composition and control purposes, while a disabled input method only
   * interprets control commands (including commands to enable itself).
   *
   * @param enable whether to enable the input method
   * @throws UnsupportedOperationException if there is no current input method,
   *         or the input method does not support enabling
   * @see #isCompositionEnabled()
   * @since 1.3
   */
  public void setCompositionEnabled(boolean enable)
  {
    if (im == null)
      throw new UnsupportedOperationException();
    im.setCompositionEnabled(enable);
  }

  /**
   * Find out if the current input method is enabled.
   *
   * @return true if the current input method is enabled
   * @throws UnsupportedOperationException if there is no current input method,
   *         or the input method does not support enabling
   * @see #setCompositionEnabled(boolean)
   * @since 1.3
   */
  public boolean isCompositionEnabled()
  {
    if (im == null)
      throw new UnsupportedOperationException();
    return im.isCompositionEnabled();
  }

  /**
   * Starts a reconversion operation in the current input method. The input
   * method gets theh text to reconvert from the client component, using
   * {@link InputMethodRequests#getSelectedText(Attribute[])}. Then the
   * composed and committed text produced by the operation is sent back to
   * the client using a sequence of InputMethodRequests.
   *
   * @throws UnsupportedOperationException if there is no current input method,
   *         or the input method does not support reconversion
   * @since 1.3
   */
  public void reconvert()
  {
    if (im == null)
      throw new UnsupportedOperationException();
    im.reconvert();
  }

  /**
   * Dispatches an event to the current input method. This is called
   * automatically by AWT. If no input method is available, then the event
   * will never be consumed.
   *
   * @param event the event to dispatch
   * @throws NullPointerException if event is null
   */
  public void dispatchEvent(AWTEvent event)
  {
    if (im != null)
      im.dispatchEvent(event);
  }

  /**
   * Notifies the input context that a client component has been removed from
   * its containment hierarchy, or that input method support has been disabled
   * for the component. This method is usually called from the client
   * component's {@link Component#removeNotify()} method. Potentially pending
   * input from input methods for this component is discarded. If no input
   * methods are available, then this method has no effect.
   *
   * @param client the client component
   * @throws NullPointerException if client is null
   */
  public void removeNotify(Component client)
  {
    // XXX What to do with client information?
    if (im != null)
      {
        im.deactivate(false);
        im.removeNotify();
      }
  }

  /**
   * Ends any input composition that may currently be going on in this
   * context. Depending on the platform and possibly user preferences, this
   * may commit or delete uncommitted text. Any changes to the text are
   * communicated to the active component using an input method event. If no
   * input methods are available, then this method has no effect. This may
   * be called for a variety of reasons, such as when the user moves the
   * insertion point in the client text outside the range of the composed text,
   * or when text is saved to file.
   */
  public void endComposition()
  {
    if (im != null)
      im.endComposition();
  }

  /**
   * Disposes of the input context and release the resources used by it.
   * Called automatically by AWT for the default input context of each
   * Window. If no input methods are available, then this method has no
   * effect.
   */
  public void dispose()
  {
    if (im != null)
      {
        im.deactivate(false);
        im.dispose();
      }
  }

  /**
   * Returns a control object from the current input method, or null. A
   * control object provides implementation-dependent methods that control
   * the behavior of the input method or obtain information from the input
   * method. Clients have to compare the result against known input method
   * control object types. If no input methods are available or the current
   * input method does not provide an input method control object, then null
   * is returned. 
   *
   * @return the control object, or null
   */
  public Object getInputMethodControlObject()
  {
    return im == null ? null : im.getControlObject();
  }
} // class InputContext
