/* PersistentMap.java -- The persistent object naming map
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


package gnu.classpath.tools.orbd;

import gnu.CORBA.NamingService.NamingMap;

import java.io.BufferedOutputStream;
import java.io.BufferedReader;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.InputStreamReader;
import java.io.OutputStream;
import java.util.Iterator;
import java.util.Map;

import org.omg.CORBA.ORB;
import org.omg.CosNaming.NameComponent;
import org.omg.CosNaming.NamingContextPackage.AlreadyBound;
import org.omg.CosNaming.NamingContextPackage.InvalidName;

/**
 * The persistent object naming map for the persistent naming service. The
 * inherited (super.) naming map implementation is transient and is used as a
 * cache. During the normal work, the naming map does not read from the disk,
 * just stores the changes there. Map only reads from the disk when it starts.
 * 
 * @author Audrius Meskauskas, Lithuania (AudriusA@Bioinformatics.org)
 */
public class PersistentMap
    extends NamingMap
{
  /**
   * The data entry.
   */
  public static class Entry
  {
    String id;

    String kind;

    String ior;

    /**
     * Get the name component node.
     */
    public NameComponent getComponent()
    {
      return new NameComponent(id, kind);
    }

    /**
     * Write the naming map entry to the output stream.
     */
    public void write(OutputStream out) throws IOException
    {
      // Format: id.kind <eoln> ior <eoln><eoln>
      out.write(getKey(id, kind).getBytes());
      out.write('\n');
      out.write(ior.getBytes());
      out.write('\n');
      out.close();
    }

    /**
     * Read the name component from the input stream
     */
    public boolean read(BufferedReader in) throws IOException
    {
      String key = in.readLine();
      String xior = in.readLine();

      if (key != null && xior != null)
        {
          if (key.length() < 2)
            {
              // A single char key cannot have the kind part.
              id = key;
              kind = "";
            }
          else
            {
              // Search for the id/kind splitter, dot:
              int iks = - 1;
              for (int i = 1; i < key.length(); i++)
                {
                  if (key.charAt(i) == '.')
                    // The id is separated from kind by dot, unless preceeded by
                    // the
                    // escape character, \.
                    if (key.charAt(i - 1) != '\\')
                      {
                        iks = i;
                        break;
                      }
                }

              // May also end by dot, if the kind field is missing.
              if (iks < 0)
                {
                  id = key;
                  kind = "";
                }
              else if (iks == key.length() - 1)
                {
                  id = key.substring(0, key.length() - 1);
                  kind = "";
                }
              else
                {
                  id = key.substring(0, iks);
                  kind = key.substring(iks + 1);
                }
            }
          ior = xior;
          return true;
        }
      else
        return false;
    }

    /**
     * Get the key value from the name component.
     * 
     * @param id the component id
     * @param kind the component kind
     * @return the key value
     */
    public String getKey(String id, String kind)
    {
      StringBuilder b = new StringBuilder(id.length() + 8);
      appEscaping(b, id);
      b.append('.');
      if (kind != null && kind.length() > 0)
        appEscaping(b, kind);
      return b.toString();
    }

    /**
     * Append the contents of the string to this string buffer, inserting the
     * escape sequences, where required.
     * 
     * @param b a buffer to append the contents to.
     * @param s a string to append.
     */
    void appEscaping(StringBuilder b, String s)
    {
      char c;
      for (int i = 0; i < s.length(); i++)
        {
          c = s.charAt(i);
          switch (c)
            {
            case '.':
            case '/':
            case '\\':
              b.append('\\');
              b.append(c);
              break;

            default:
              b.append(c);
              break;
            }
        }
    }
  }

  /**
   * The file, where the persistent naming map stores the information. The
   * format of this file is n*(id LF kind LF ior LFLF).
   */
  public final File file;

  /**
   * The naming service ORB, used to obtain and produce the object stringified
   * references.
   */
  ORB orb;
  
  /**
   * If true, all existing data on the file system are discarded.
   */
  boolean reset;

  /**
   * Create the persistent map that stores information in the given file.
   * 
   * @param an_orb the naming service ORB, used to obtain and produce the object
   *          stringified references.
   * @param mapFile the file, where the persistent information is stored.
   * @param a_reset if true, the previous naming data are discarded. If false
   *          (normally expected), they are loaded from the persistent memory to
   *          provide the persistence.
   */
  public PersistentMap(ORB an_orb, File mapFile, boolean a_reset)
  {
    super();
    orb = an_orb;
    file = mapFile;
    reset = a_reset;

    // Initialise the persistent map with existing data.
    if (file.exists() && ! reset)
      {

        BufferedReader in;
        try
          {
            FileInputStream fin = new FileInputStream(file);
            in = new BufferedReader(new InputStreamReader(fin));
            Entry e = new Entry();
            boolean ok;

            while (e.read(in))
              {
                org.omg.CORBA .Object object = string_to_object(e.ior);
                orb.connect(object);
                map.put(e.getComponent(), object);
              }
          }
        catch (Exception ex)
          {
            InternalError ierr = new InternalError(file.getAbsolutePath());
            ierr.initCause(ex);
            throw ierr;
          }
      }
  }
  
  /**
   * Restore object from its string description.
   * 
   * @param description the string, describing the object
   * 
   * @return the object.
   */
  protected org.omg.CORBA.Object string_to_object(String description)
  {
    return orb.string_to_object(description);
  }
  
  /**
   * Convert the object to its string description
   * 
   * @param object the object to convert
   * @return the string description of the object
   */
  protected String object_to_string(org.omg.CORBA .Object object)
  {
      return orb.object_to_string(object);    
  }

  /**
   * Put the given GIOP object, specifying the given name as a key. If the entry
   * with the given name already exists, or if the given object is already
   * mapped under another name, the {@link AlreadyBound} exception will be
   * thrown.
   * 
   * @param name the name
   * @param object the object
   */
  public void bind(NameComponent name, org.omg.CORBA.Object object)
      throws AlreadyBound, InvalidName
  {
    if (!containsKey(name))
      {
        super.bind(name, object);
        register(name, object);
      }
    else
      throw new AlreadyBound(name.id + "." + name.kind);
  }

  /**
   * Put the given CORBA object, specifying the given name as a key. Remove all
   * pre - existing mappings for the given name and object.
   * 
   * @param name the name.
   * @param object the object
   */
  public void rebind(NameComponent name, org.omg.CORBA.Object object)
      throws InvalidName
  {
    if (containsKey(name))
      {
        org.omg.CORBA.Object existing = get(name);
        String ior = object_to_string(object);
        String xior = object_to_string(existing);
        
        // Same name and same ior - nothing to do.
        if (ior.equals(xior))
          return;
        else
          remove(name);
      }

    Iterator iter = entries().iterator();
    Map.Entry item;

    // Remove the existing mapping for the given object, if present.
    while (iter.hasNext())
      {
        item = (Map.Entry) iter.next();
        if (item.getValue().equals(object))
          iter.remove();
      }

    map.put(name, object);
    register(name, object);
  }

  /**
   * Removes the given name, if present.
   * 
   * @param name a name to remove.
   */
  public void remove(NameComponent name)
  {
    super.remove(name);
    unregister(name);
  }

  /**
   * Register this name - object pair in the persistent storage.
   * 
   * @param name the name.
   * @param object the object
   */
  public void register(NameComponent name, org.omg.CORBA.Object object)
  {
    // If this key is already known, and this is the same object,
    // then return without action.
    String ior = object_to_string(object);

    synchronized (file)
      {
        try
          {
            FileOutputStream fou;

            if (! file.exists())
              fou = new FileOutputStream(file);
            else
              fou = new FileOutputStream(file, true);

            Entry e = new Entry();
            e.id = name.id;
            e.kind = name.kind;
            e.ior = ior;
            e.write(fou);
            fou.close();
          }
        catch (Exception e)
          {
            InternalError ierr = new InternalError(file.getAbsolutePath());
            ierr.initCause(e);
            throw ierr;
          }
      }
  }

  /**
   * Remove this name from the persistent storage.
   * 
   * @param name the name to remove
   */
  public void unregister(NameComponent name)
  {
    synchronized (file)
      {
        try
          {
            File nf = new File(file.getParent(), file.getName() + "_t");
            FileInputStream fin = new FileInputStream(file);
            FileOutputStream fou = new FileOutputStream(nf);
            BufferedOutputStream ou = new BufferedOutputStream(fou);

            BufferedReader in = new BufferedReader(new InputStreamReader(fin));
            String s;
            String nk = name.kind;
            if (nk == null)
              nk = "";

            Entry e = new Entry();

            while (e.read(in))
              {
                if (e.id.equals(name.id) && e.kind.equals(nk))
                  {
                    // Do nothing - skip.
                  }
                else
                  {
                    e.write(ou);
                  }
              }

            File deleteIt = new File(file.getParent(), file.getName() + "_d");
            if (deleteIt.exists())
              deleteIt.delete();

            if (! file.renameTo(deleteIt))
              throw new IOException(file.getAbsolutePath() + " rename failed");

            if (! nf.renameTo(file))
              throw new IOException(file.getAbsolutePath() + " rename failed");
          }
        catch (Exception e)
          {
            InternalError ierr = new InternalError(file.getAbsolutePath());
            ierr.initCause(e);
            throw ierr;
          }
      }
  }
}
