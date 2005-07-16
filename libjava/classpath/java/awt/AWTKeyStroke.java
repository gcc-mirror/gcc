/* AWTKeyStroke.java -- an immutable key stroke
   Copyright (C) 2002, 2004, 2005  Free Software Foundation

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

import java.awt.event.InputEvent;
import java.awt.event.KeyEvent;
import java.io.ObjectStreamException;
import java.io.Serializable;
import java.lang.reflect.Constructor;
import java.lang.reflect.Field;
import java.lang.reflect.InvocationTargetException;
import java.security.AccessController;
import java.security.PrivilegedAction;
import java.security.PrivilegedActionException;
import java.security.PrivilegedExceptionAction;
import java.util.HashMap;
import java.util.LinkedHashMap;
import java.util.Map;
import java.util.StringTokenizer;

/**
 * This class mirrors KeyEvents, representing both low-level key presses and
 * key releases, and high level key typed inputs. However, this class forms
 * immutable strokes, and can be efficiently reused via the factory methods
 * for creating them.
 *
 * <p>For backwards compatibility with Swing, this supports a way to build
 * instances of a subclass, using reflection, provided the subclass has a
 * no-arg constructor (of any accessibility).
 *
 * @author Eric Blake (ebb9@email.byu.edu)
 * @see #getAWTKeyStroke(char)
 * @since 1.4
 * @status updated to 1.4
 */
public class AWTKeyStroke implements Serializable
{
  /**
   * Compatible with JDK 1.4+.
   */
  private static final long serialVersionUID = -6430539691155161871L;

  /** The mask for modifiers. */
  private static final int MODIFIERS_MASK = 0x3fef;

  /**
   * The cache of recently created keystrokes. This maps KeyStrokes to
   * KeyStrokes in a cache which removes the least recently accessed entry,
   * under the assumption that garbage collection of a new keystroke is
   * easy when we find the old one that it matches in the cache.
   */
  private static final LinkedHashMap cache = new LinkedHashMap(11, 0.75f, true)
  {
    /** The largest the keystroke cache can grow. */
    private static final int MAX_CACHE_SIZE = 2048;

    /** Prune stale entries. */
    protected boolean removeEldestEntry(Map.Entry eldest)
    {	// XXX - FIXME Use Map.Entry, not just Entry  as gcj 3.1 workaround.
      return size() > MAX_CACHE_SIZE;
    }
  };

  /** The most recently generated keystroke, or null. */
  private static AWTKeyStroke recent;

  /**
   * The no-arg constructor of a subclass, or null to use AWTKeyStroke. Note
   * that this will be left accessible, to get around private access; but
   * it should not be a security risk as it is highly unlikely that creating
   * protected instances of the subclass via reflection will do much damage.
   */
  private static Constructor ctor;

  /**
   * A table of keyCode names to values.  This is package-private to
   * avoid an accessor method.
   *
   * @see #getAWTKeyStroke(String)
   */
  static final HashMap vktable = new HashMap();
  static
  {
    // Using reflection saves the hassle of keeping this in sync with KeyEvent,
    // at the price of an expensive initialization.
    AccessController.doPrivileged(new PrivilegedAction()
      {
        public Object run()
        {
          Field[] fields = KeyEvent.class.getFields();
          int i = fields.length;
          try
            {
              while (--i >= 0)
                {
                  Field f = fields[i];
                  String name = f.getName();
                  if (name.startsWith("VK_"))
                    vktable.put(name.substring(3), f.get(null));
                }
            }
          catch (Exception e)
            {
              throw (Error) new InternalError().initCause(e);
            }
          return null;
        }
      });
  }

  /**
   * The typed character, or CHAR_UNDEFINED for key presses and releases.
   *
   * @serial the keyChar
   */
  private char keyChar;

  /**
   * The virtual key code, or VK_UNDEFINED for key typed. Package visible for
   * use by Component.
   *
   * @serial the keyCode
   */
  int keyCode;

  /**
   * The modifiers in effect. To match Sun, this stores the old style masks
   * for shift, control, alt, meta, and alt-graph (but not button1); as well
   * as the new style of extended modifiers for all modifiers.
   *
   * @serial bitwise or of the *_DOWN_MASK modifiers
   */
  private int modifiers;

  /**
   * True if this is a key release; should only be true if keyChar is
   * CHAR_UNDEFINED.
   *
   * @serial true to distinguish key pressed from key released
   */
  private boolean onKeyRelease;

  /**
   * Construct a keystroke with default values: it will be interpreted as a
   * key typed event with an invalid character and no modifiers. Client code
   * should use the factory methods instead.
   *
   * @see #getAWTKeyStroke(char)
   * @see #getAWTKeyStroke(Character, int)
   * @see #getAWTKeyStroke(int, int, boolean)
   * @see #getAWTKeyStroke(int, int)
   * @see #getAWTKeyStrokeForEvent(KeyEvent)
   * @see #getAWTKeyStroke(String)
   */
  protected AWTKeyStroke()
  {
    keyChar = KeyEvent.CHAR_UNDEFINED;
  }

  /**
   * Construct a keystroke with the given values. Client code should use the
   * factory methods instead.
   *
   * @param keyChar the character entered, if this is a key typed
   * @param keyCode the key pressed or released, or VK_UNDEFINED for key typed
   * @param modifiers the modifier keys for the keystroke, in old or new style
   * @param onKeyRelease true if this is a key release instead of a press
   * @see #getAWTKeyStroke(char)
   * @see #getAWTKeyStroke(Character, int)
   * @see #getAWTKeyStroke(int, int, boolean)
   * @see #getAWTKeyStroke(int, int)
   * @see #getAWTKeyStrokeForEvent(KeyEvent)
   * @see #getAWTKeyStroke(String)
   */
  protected AWTKeyStroke(char keyChar, int keyCode, int modifiers,
                         boolean onKeyRelease)
  {
    this.keyChar = keyChar;
    this.keyCode = keyCode;
    // No need to call extend(), as only trusted code calls this constructor.
    this.modifiers = modifiers;
    this.onKeyRelease = onKeyRelease;
  }

  /**
   * Registers a new subclass as being the type of keystrokes to generate in
   * the factory methods. This operation flushes the cache of stored keystrokes
   * if the class differs from the current one. The new class must be
   * AWTKeyStroke or a subclass, and must have a no-arg constructor (which may
   * be private).
   *
   * @param subclass the new runtime type of generated keystrokes
   * @throws IllegalArgumentException subclass doesn't have no-arg constructor
   * @throws ClassCastException subclass doesn't extend AWTKeyStroke
   */
  protected static void registerSubclass(final Class subclass)
  {
    if (subclass == null)
      throw new IllegalArgumentException();
    if (subclass.equals(ctor == null ? AWTKeyStroke.class
                        : ctor.getDeclaringClass()))
      return;
    if (subclass.equals(AWTKeyStroke.class))
       {
         cache.clear();
         recent = null;
         ctor = null;
         return;
       }
    try
      {
        ctor = (Constructor) AccessController.doPrivileged
          (new PrivilegedExceptionAction()
            {
              public Object run()
                throws NoSuchMethodException, InstantiationException,
                       IllegalAccessException, InvocationTargetException
              {
                Constructor c = subclass.getDeclaredConstructor(null);
                c.setAccessible(true);
                // Create a new instance, to make sure that we can, and
                // to cause any ClassCastException.
                AWTKeyStroke dummy = (AWTKeyStroke) c.newInstance(null);
                return c;
              }
            });
      }
    catch (PrivilegedActionException e)
      {
        // e.getCause() will not ever be ClassCastException; that should
        // escape on its own.
        throw (RuntimeException)
          new IllegalArgumentException().initCause(e.getCause());
      }
    cache.clear();
    recent = null;
  }

  /**
   * Returns a keystroke representing a typed character.
   *
   * @param keyChar the typed character 
   * @return the specified keystroke
   */
  public static AWTKeyStroke getAWTKeyStroke(char keyChar)
  {
    return getAWTKeyStroke(keyChar, KeyEvent.VK_UNDEFINED, 0, false);
  }

  /**
   * Returns a keystroke representing a typed character with the given
   * modifiers. Note that keyChar is a <code>Character</code> instead of a
   * <code>char</code> to avoid accidental ambiguity with
   * <code>getAWTKeyStroke(int, int)</code>. The modifiers are the bitwise
   * or of the masks found in {@link InputEvent}; the new style (*_DOWN_MASK)
   * is preferred, but the old style will work.
   *
   * @param keyChar the typed character
   * @param modifiers the modifiers, or 0
   * @return the specified keystroke
   * @throws IllegalArgumentException if keyChar is null
   */
  public static AWTKeyStroke getAWTKeyStroke(Character keyChar, int modifiers)
  {
    if (keyChar == null)
      throw new IllegalArgumentException();
    return getAWTKeyStroke(keyChar.charValue(), KeyEvent.VK_UNDEFINED,
                           extend(modifiers), false);
  }

  /**
   * Returns a keystroke representing a pressed or released key event, with
   * the given modifiers. The "virtual key" should be one of the VK_*
   * constants in {@link KeyEvent}. The modifiers are the bitwise or of the
   * masks found in {@link InputEvent}; the new style (*_DOWN_MASK) is
   * preferred, but the old style will work.
   *
   * @param keyCode the virtual key
   * @param modifiers the modifiers, or 0
   * @param release true if this is a key release instead of a key press
   * @return the specified keystroke
   */
  public static AWTKeyStroke getAWTKeyStroke(int keyCode, int modifiers,
                                             boolean release)
  {
    return getAWTKeyStroke(KeyEvent.CHAR_UNDEFINED, keyCode,
                           extend(modifiers), release);
  }

  /**
   * Returns a keystroke representing a pressed key event, with the given
   * modifiers. The "virtual key" should be one of the VK_* constants in
   * {@link KeyEvent}. The modifiers are the bitwise or of the masks found
   * in {@link InputEvent}; the new style (*_DOWN_MASK) is preferred, but the
   * old style will work.
   *
   * @param keyCode the virtual key
   * @param modifiers the modifiers, or 0
   * @return the specified keystroke
   */
  public static AWTKeyStroke getAWTKeyStroke(int keyCode, int modifiers)
  {
    return getAWTKeyStroke(KeyEvent.CHAR_UNDEFINED, keyCode,
                           extend(modifiers), false);
  }

  /**
   * Returns a keystroke representing what caused the key event.
   *
   * @param event the key event to convert
   * @return the specified keystroke, or null if the event is invalid
   * @throws NullPointerException if event is null
   */
  public static AWTKeyStroke getAWTKeyStrokeForEvent(KeyEvent event)
  {
    switch (event.id)
      {
      case KeyEvent.KEY_TYPED:
        return getAWTKeyStroke(event.getKeyChar(), KeyEvent.VK_UNDEFINED,
                               extend(event.getModifiersEx()), false);
      case KeyEvent.KEY_PRESSED:
        return getAWTKeyStroke(KeyEvent.CHAR_UNDEFINED, event.getKeyCode(),
                               extend(event.getModifiersEx()), false);
      case KeyEvent.KEY_RELEASED:
        return getAWTKeyStroke(KeyEvent.CHAR_UNDEFINED, event.getKeyCode(),
                               extend(event.getModifiersEx()), true);
      default:
        return null;
      }
  }

  /**
   * Parses a string and returns the keystroke that it represents. The syntax
   * for keystrokes is listed below, with tokens separated by an arbitrary
   * number of spaces:
   * <pre>
   * keyStroke := &lt;modifiers&gt;* ( &lt;typedID&gt; | &lt;codeID&gt; )
   * modifiers := ( shift | control | ctrl | meta | alt
   *                | button1 | button2 | button3 )
   * typedID := typed &lt;single Unicode character&gt;
   * codeID := ( pressed | released )? &lt;name&gt;
   * name := &lt;the KeyEvent field name less the leading "VK_"&gt;
   * </pre>
   *
   * <p>Note that the grammar is rather weak, and not all valid keystrokes
   * can be generated in this manner (for example, a typed space, or anything
   * with the alt-graph modifier!). The output of AWTKeyStroke.toString()
   * will not meet the grammar. If pressed or released is not specified,
   * pressed is assumed. Examples:<br>
   * <code>
   * "INSERT" =&gt; getAWTKeyStroke(KeyEvent.VK_INSERT, 0);<br>
   * "control DELETE" =&gt;
   *    getAWTKeyStroke(KeyEvent.VK_DELETE, InputEvent.CTRL_MASK);<br>
   * "alt shift X" =&gt; getAWTKeyStroke(KeyEvent.VK_X,
   *    InputEvent.ALT_MASK | InputEvent.SHIFT_MASK);<br>
   * "alt shift released X" =&gt; getAWTKeyStroke(KeyEvent.VK_X,
   *    InputEvent.ALT_MASK | InputEvent.SHIFT_MASK, true);<br>
   * "typed a" =&gt; getAWTKeyStroke('a');
   * </code>      
   *
   * @param s the string to parse
   * @throws IllegalArgumentException if s is null or cannot be parsed
   * @return the specified keystroke
   */
  public static AWTKeyStroke getAWTKeyStroke(String s)
  {
    if (s == null)
      throw new IllegalArgumentException("null argument");
    StringTokenizer t = new StringTokenizer(s, " ");
    if (! t.hasMoreTokens())
      throw new IllegalArgumentException("no tokens '" + s + "'");
    int modifiers = 0;
    boolean released = false;
    String token = null;
    do
      {
        token = t.nextToken();
        if ("shift".equals(token))
          modifiers |= KeyEvent.SHIFT_DOWN_MASK;
        else if ("ctrl".equals(token) || "control".equals(token))
          modifiers |= KeyEvent.CTRL_DOWN_MASK;
        else if ("meta".equals(token))
          modifiers |= KeyEvent.META_DOWN_MASK;
        else if ("alt".equals(token))
          modifiers |= KeyEvent.ALT_DOWN_MASK;
        else if ("button1".equals(token))
          modifiers |= KeyEvent.BUTTON1_DOWN_MASK;
        else if ("button2".equals(token))
          modifiers |= KeyEvent.BUTTON2_DOWN_MASK;
        else if ("button3".equals(token))
          modifiers |= KeyEvent.BUTTON3_DOWN_MASK;
        else if ("typed".equals(token))
          {
            if (t.hasMoreTokens())
              {
                token = t.nextToken();
                if (! t.hasMoreTokens() && token.length() == 1)
                  return getAWTKeyStroke(token.charAt(0),
                                         KeyEvent.VK_UNDEFINED, modifiers,
                                         false);
              }
            throw new IllegalArgumentException("Invalid 'typed' argument '"
			    		       + s + "'");
          }
        else if ("pressed".equals(token))
          {
            if (t.hasMoreTokens())
              token = t.nextToken();
            break;
          }
        else if ("released".equals(token))
          {
            released = true;
            if (t.hasMoreTokens())
              token = t.nextToken();
            break;
          }
        else
          break;
      }
    while (t.hasMoreTokens());
    // Now token contains the VK name we must parse.
    Integer code = (Integer) vktable.get(token);
    if (code == null)
      throw new IllegalArgumentException("Unknown token '" + token
					 + "' in '" + s + "'");
    if (t.hasMoreTokens())
      throw new IllegalArgumentException("Too many tokens: " + s);
    return getAWTKeyStroke(KeyEvent.CHAR_UNDEFINED, code.intValue(),
                           modifiers, released);
  }

  /**
   * Returns the character of this keystroke, if it was typed.
   *
   * @return the character value, or CHAR_UNDEFINED
   * @see #getAWTKeyStroke(char)
   */
  public final char getKeyChar()
  {
    return keyChar;
  }

  /**
   * Returns the virtual key code of this keystroke, if it was pressed or
   * released. This will be a VK_* constant from KeyEvent.
   *
   * @return the virtual key code value, or VK_UNDEFINED
   * @see #getAWTKeyStroke(int, int)
   */
  public final int getKeyCode()
  {
    return keyCode;
  }

  /**
   * Returns the modifiers for this keystroke. This will be a bitwise or of
   * constants from InputEvent; it includes the old style masks for shift,
   * control, alt, meta, and alt-graph (but not button1); as well as the new
   * style of extended modifiers for all modifiers.
   *
   * @return the modifiers
   * @see #getAWTKeyStroke(Character, int)
   * @see #getAWTKeyStroke(int, int)
   */
  public final int getModifiers()
  {
    return modifiers;
  }

  /**
   * Tests if this keystroke is a key release.
   *
   * @return true if this is a key release
   * @see #getAWTKeyStroke(int, int, boolean)
   */
  public final boolean isOnKeyRelease()
  {
    return onKeyRelease;
  }

  /**
   * Returns the AWT event type of this keystroke. This is one of
   * {@link KeyEvent#KEY_TYPED}, {@link KeyEvent#KEY_PRESSED}, or
   * {@link KeyEvent#KEY_RELEASED}.
   *
   * @return the key event type
   */
  public final int getKeyEventType()
  {
    return keyCode == KeyEvent.VK_UNDEFINED ? KeyEvent.KEY_TYPED
      : onKeyRelease ? KeyEvent.KEY_RELEASED : KeyEvent.KEY_PRESSED;
  }

  /**
   * Returns a hashcode for this key event. It is not documented, but appears
   * to be: <code>(getKeyChar() + 1) * (getKeyCode() + 1)
   * * (getModifiers() + 1) * 2 + (isOnKeyRelease() ? 1 : 2)</code>.
   *
   * @return the hashcode
   */
  public int hashCode()
  {
    return (keyChar + 1) * (keyCode + 1) * (modifiers + 1) * 2
      + (onKeyRelease ? 1 : 2);
  }

  /**
   * Tests two keystrokes for equality.
   *
   * @param o the object to test
   * @return true if it is equal
   */
  public final boolean equals(Object o)
  {
    if (! (o instanceof AWTKeyStroke))
      return false;
    AWTKeyStroke s = (AWTKeyStroke) o;
    return this == o || (keyChar == s.keyChar && keyCode == s.keyCode
                         && modifiers == s.modifiers
                         && onKeyRelease == s.onKeyRelease);
  }

  /**
   * Returns a string representation of this keystroke. For typed keystrokes,
   * this is <code>"keyChar " + KeyEvent.getKeyModifiersText(getModifiers())
   + getKeyChar()</code>; for pressed and released keystrokes, this is
   * <code>"keyCode " + KeyEvent.getKeyModifiersText(getModifiers())
   * + KeyEvent.getKeyText(getKeyCode())
   * + (isOnKeyRelease() ? "-R" : "-P")</code>.
   *
   * @return a string representation
   */
  public String toString()
  {
    if (keyCode == KeyEvent.VK_UNDEFINED)
      return "keyChar " + KeyEvent.getKeyModifiersText(modifiers) + keyChar;
    return "keyCode " + KeyEvent.getKeyModifiersText(modifiers)
      + KeyEvent.getKeyText(keyCode) + (onKeyRelease ? "-R" : "-P");
  }

  /**
   * Returns a cached version of the deserialized keystroke, if available.
   *
   * @return a cached replacement
   * @throws ObjectStreamException if something goes wrong
   */
  protected Object readResolve() throws ObjectStreamException
  {
    AWTKeyStroke s = (AWTKeyStroke) cache.get(this);
    if (s != null)
      return s;
    cache.put(this, this);
    return this;
  }

  /**
   * Gets the appropriate keystroke, creating one if necessary.
   *
   * @param keyChar the keyChar
   * @param keyCode the keyCode
   * @param modifiers the modifiers
   * @param release true for key release
   * @return the specified keystroke
   */
  private static AWTKeyStroke getAWTKeyStroke(char keyChar, int keyCode,
                                              int modifiers, boolean release)
  {
    // Check level 0 cache.
    AWTKeyStroke stroke = recent; // Avoid thread races.
    if (stroke != null && stroke.keyChar == keyChar
        && stroke.keyCode == keyCode && stroke.modifiers == modifiers
        && stroke.onKeyRelease == release)
      return stroke;
    // Create a new object, on the assumption that if it has a match in the
    // cache, the VM can easily garbage collect it as it is temporary.
    Constructor c = ctor; // Avoid thread races.
    if (c == null)
      stroke = new AWTKeyStroke(keyChar, keyCode, modifiers, release);
    else
      try
        {
          stroke = (AWTKeyStroke) c.newInstance(null);
          stroke.keyChar = keyChar;
          stroke.keyCode = keyCode;
          stroke.modifiers = modifiers;
          stroke.onKeyRelease = release;
        }
      catch (Exception e)
        {
          throw (Error) new InternalError().initCause(e);
        }
    // Check level 1 cache.
    AWTKeyStroke cached = (AWTKeyStroke) cache.get(stroke);
    if (cached == null)
      cache.put(stroke, stroke);
    else
      stroke = cached;
    return recent = stroke;
  }

  /**
   * Converts the modifiers to the appropriate format.
   *
   * @param mod the modifiers to convert
   * @return the adjusted modifiers
   */
  private static int extend(int mod)
  {
    if ((mod & (KeyEvent.SHIFT_MASK | KeyEvent.SHIFT_DOWN_MASK)) != 0)
      mod |= KeyEvent.SHIFT_MASK | KeyEvent.SHIFT_DOWN_MASK;
    if ((mod & (KeyEvent.CTRL_MASK | KeyEvent.CTRL_DOWN_MASK)) != 0)
      mod |= KeyEvent.CTRL_MASK | KeyEvent.CTRL_DOWN_MASK;
    if ((mod & (KeyEvent.META_MASK | KeyEvent.META_DOWN_MASK)) != 0)
      mod |= KeyEvent.META_MASK | KeyEvent.META_DOWN_MASK;
    if ((mod & (KeyEvent.ALT_MASK | KeyEvent.ALT_DOWN_MASK)) != 0)
      mod |= KeyEvent.ALT_MASK | KeyEvent.ALT_DOWN_MASK;
    if ((mod & (KeyEvent.ALT_GRAPH_MASK | KeyEvent.ALT_GRAPH_DOWN_MASK)) != 0)
      mod |= KeyEvent.ALT_GRAPH_MASK | KeyEvent.ALT_GRAPH_DOWN_MASK;
    if ((mod & KeyEvent.BUTTON1_MASK) != 0)
      mod |= KeyEvent.BUTTON1_DOWN_MASK;
    return mod & MODIFIERS_MASK;
  }
} // class AWTKeyStroke
