// Properties - Property list representation.

/* Copyright (C) 1998, 1999, 2000  Free Software Foundation

   This file is part of libgcj.

This software is copyrighted work licensed under the terms of the
Libgcj License.  Please consult the file "LIBGCJ_LICENSE" for
details.  */

package java.util;

import java.io.BufferedWriter;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.io.OutputStream;
import java.io.OutputStreamWriter;
import java.io.PrintStream;
import java.io.PrintWriter;
import java.io.PushbackReader;

/**
 * @author Tom Tromey <tromey@cygnus.com>
 * @date October 26, 1998.
 */

/* Written using "Java Class Libraries", 2nd edition, ISBN 0-201-31002-3
 * Status: Complete to JDK 1.2.
 */

public class Properties extends Hashtable
{
  protected Properties defaults;

  public String getProperty (String propName)
    {
      return getProperty (propName, null);
    }

  public String getProperty (String propName, String defVal)
    {
      String r = (String) get (propName);
      if (r == null)
	{
	  if (defaults != null)
	    r = defaults.getProperty(propName, defVal);
	  else
	    r = defVal;
	}
      return r;
    }

  public Object setProperty (String key, String value)
  {
    return put (key, value);
  }

  public void list (PrintStream out)
    {
      Enumeration e = propertyNames ();
      while (e.hasMoreElements())
	{
	  String key = (String) e.nextElement();
	  String value = getProperty(key);
	  if (value != null)
	    {
	      if (value.length() > 40)
		{
		  // JDK compatibility.
		  value = value.substring(0, 37) + "...";
		}
	      out.print(key);
	      out.print("=");
	      out.println(value);
	    }
	}
    }

  public void list (PrintWriter writer)
    {
      Enumeration e = propertyNames ();
      while (e.hasMoreElements())
	{
	  String key = (String) e.nextElement();
	  String value = getProperty(key);
	  if (value != null)
	    {
	      if (value.length() > 40)
		{
		  // JDK compatibility.
		  value = value.substring(0, 37) + "...";
		}
	      writer.print(key);
	      writer.print("=");
	      writer.println(value);
	    }
	}
    }

  private final boolean skip_ws (PushbackReader reader) throws IOException
    {
      while (true)
	{
	  int c = reader.read();
	  if (c == -1)
	    return false;
	  // FIXME: we use our own definition of whitespace.
	  // Character.isWhitespace includes newlines, which we don't
	  // want.  Character.isSpaceChar doesn't include \t.
	  if (c != ' ' && c != '\t')
	    {
	      reader.unread(c);
	      return true;
	    }
	}
    }

  // Note: this method needs to be rewritten for JDK 1.2.
  // We rather arbitrarily decide that an EOF in the middle of a line
  // means that the whole line should be ignored.  The spec doesn't
  // specifically address this, but this interpretation seems valid.
  public synchronized void load (InputStream in) throws IOException
    {
      PushbackReader reader = new PushbackReader (new InputStreamReader (in));

      StringBuffer key = new StringBuffer ();
      StringBuffer value = new StringBuffer ();

    nextLine:
      while (true)
	{
	  key.setLength(0);
	  value.setLength(0);

	  // Skip leading whitespace.
	  if (! skip_ws (reader))
	    return;

	  // Read key until key terminator.
	  boolean first_char = true;
	  int c;
	  while (true)
	    {
	      c = reader.read();
	      if (c == -1)
		return;
	      if (c == '\\')
		{
		  first_char = false;
		  c = reader.read();
		  if (c == -1)
		    return;
		}

	      // If we found a comment, just read to end of line and
	      // then keep going.
	      if (first_char == true && (c == '#' || c == '!'))
		{
		  while (c != -1 && c != '\r' && c != '\n')
		    c = reader.read();
		  if (c == -1)
		    return;
		  continue nextLine;
		}

	      if (c == '\r' || c == '\n')
		{
		  if (first_char)
		    continue nextLine;
		  reader.unread(c);
		  break;
		}
	      // FIXME: again, our own definition of whitespace.
	      if (c == ' ' || c == '\t' || c == ':' || c == '=')
		break;

	      first_char = false;
	      key.append((char) c);
	    }

	  // Found end of key.  Skip whitespace.  If the terminator
	  // was whitespace, also skip a single instance of a "real"
	  // terminator, and then more whitespace.
	  if (! skip_ws (reader))
	    return;
	  if (c != ':' && c != '=')
	    {
	      c = reader.read();
	      if (c == -1)
		return;
	      if (c == ':' || c == '=')
		{
		  // Skip more whitespace.
		  if (! skip_ws (reader))
		    return;
		}
	      else
		reader.unread(c);
	    }

	  // Now read the value.
	  while (true)
	    {
	      c = reader.read();
	      if (c == -1)
		return;
	      if (c == '\r' || c == '\n')
		break;
	      if (c == '\\')
		{
		  c = reader.read();
		  switch (c)
		    {
		    case -1:
		      return;
		    case 't':
		      c = '\t';
		      break;
		    case 'r':
		      c = '\r';
		      break;
		    case 'n':
		      c = '\n';
		      break;
		    case 'u':
		      c = 0;
		      for (int i = 0; i < 4; ++i)
			{
			  int x = reader.read();
			  if (x == -1)
			    return;
			  int d = Character.digit((char) x, 16);
			  // We follow JDK here: invalid characters
			  // are treated as terminators.
			  if (d == -1)
			    {
			      value.append((char) c);
			      c = x;
			      break;
			    }
			  c <<= 4;
			  c |= d;
			}
		      break;
		    default:
		      // Nothing.
		    }
		}
	      value.append((char) c);
	    }

	  put (key.toString(), value.toString());
	}
    }

  public Properties ()
    {
      defaults = null;
    }

  public Properties (Properties defs)
    {
      defaults = defs;
    }

  private final void addHashEntries (Hashtable base)
    {
      if (defaults != null)
	defaults.addHashEntries(base);
      Enumeration keys = keys ();
      while (keys.hasMoreElements())
	base.put(keys.nextElement(), base);
    }

  public Enumeration propertyNames ()
    {
      // We make a new Hashtable that holds all the keys.  Then we
      // return an enumeration for this hash.  We do this because we
      // don't want modifications to be reflected in the enumeration
      // (per JCL), and because there doesn't seem to be a
      // particularly better way to ensure that duplicates are
      // ignored.
      Hashtable t = new Hashtable ();
      addHashEntries (t);
      return t.keys();
    }

  public synchronized void save (OutputStream out, String comment)
  {
    try
      {
	store (out, comment);
      }
    catch (IOException _)
      {
      }
  }

  public synchronized void store (OutputStream out, String comment)
    throws IOException
  {
      // Use a buffer because writing a single string through
      // OutputStreamWriter is fairly expensive.
      BufferedWriter output
	= new BufferedWriter (new OutputStreamWriter (out));
      String newline = System.getProperty("line.separator");

      if (comment != null)
	{
	  // We just lose if COMMENT contains a newline.  This is
	  // what JDK 1.1 does.
	  output.write("#");
	  output.write(comment);
	  output.write(newline);
	}
      output.write("# ");
      output.write(new Date().toString());
      output.write(newline);

      Enumeration keys = keys ();
      while (keys.hasMoreElements())
	{
	  String key = (String) keys.nextElement();
	  String value = (String) get (key);

	  // FIXME: JCL says that the key can contain many Unicode
	  // characters.  But it also doesn't say we should encode
	  // it in any way.
	  // FIXME: if key contains ':', '=', or whitespace, must
	  // quote it here.  Note that JDK 1.1 does not do this.
	  output.write(key);
	  output.write("=");

	  boolean leading = true;
	  for (int i = 0; i < value.length(); ++i)
	    {
	      boolean new_lead = false;
	      char c = value.charAt(i);
	      switch (c)
		{
		case '\n':
		  output.write("\\n");
		  break;
		case '\r':
		  output.write("\\r");
		  break;
		case '\t':
		  output.write("\\t");
		  break;
		case '\\':
		  output.write("\\\\");
		  break;

		case '#':
		case '!':
		case '=':
		case ':':
		  output.write("\\");
		  output.write(c);
		  break;

		case ' ':
		  new_lead = leading;
		  if (leading)
		    output.write("\\");
		  output.write(c);
		  break;

		default:
		  if (c < '\u0020' || c > '\u007e')
		    {
		      output.write("\\u");
		      output.write(Character.forDigit(c >>> 12, 16));
		      output.write(Character.forDigit((c >>> 8) & 0xff,
						      16));
		      output.write(Character.forDigit((c >>> 4) & 0xff,
						      16));
		      output.write(Character.forDigit(c & 0xff, 16));
		    }
		  else
		    output.write(c);
		}
	      leading = new_lead;
	    }
	  output.write(newline);
	}

      output.flush();
  }
}
