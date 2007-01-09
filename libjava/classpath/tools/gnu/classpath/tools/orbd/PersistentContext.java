/* PersistentContext.java -- The persistent naming context.
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

import gnu.CORBA.NamingService.NameTransformer;
import gnu.CORBA.NamingService.TransientContext;

import java.io.File;

import org.omg.CORBA.ORB;
import org.omg.CosNaming.NameComponent;
import org.omg.CosNaming.NamingContext;
import org.omg.CosNaming.NamingContextPackage.AlreadyBound;
import org.omg.CosNaming.NamingContextPackage.CannotProceed;
import org.omg.CosNaming.NamingContextPackage.InvalidName;
import org.omg.CosNaming.NamingContextPackage.NotFound;

/**
 * This class implements the persistent naming service, defined by
 * {@link NamingContext}. The 'persistent' means that the service remembers the
 * mappings, stored between restarts.
 * 
 * @author Audrius Meskauskas, Lithuania (AudriusA@Bioinformatics.org)
 */
public class PersistentContext
    extends TransientContext
{
  /**
   * Use serial version UID for interoperability.
   */
  private static final long serialVersionUID = 2;
  
  /**
   * The folder, where the persistent context information is stored.
   */
  File contextFolder;
  
  /**
   * The uinque context identifier.
   */
  static long num = System.currentTimeMillis();
  
  /**
   * The naming service orb.
   */
  ORB orb;
  
  /**
   * Create the persistent naming context that will store the files in the given
   * folder of the local file system. This method also connects object to the
   * passed ORB.
   * 
   * @param an_orb the naming service ORB, used to obtain and produce the object
   *          stringified references.
   * @param folder the folder, where the persistent information is stored.
   * @param reset if true, the previous naming data are discarded. If false
   *          (normally expected), they are loaded from the persistent memory to
   *          provide the persistence.
   */
  public PersistentContext(ORB an_orb, File folder, boolean reset)
  {
    super(
         new PersistentContextMap(an_orb, new File(folder, "contexts.txt"), reset),
         new PersistentMap(an_orb, new File(folder, "objects.txt"), reset));         
    contextFolder = folder;
    folder.mkdirs();
    orb = an_orb;
    orb.connect(this);
  }
  
  /**
   * Get the unique context number;
   * 
   * @return the context number
   */
  static synchronized String getNum()
  {
    return Long.toHexString(num++);
  }
  
  /**
   * Create new persistent context.
   */
  public NamingContext new_context()
  {
    File ctxFolder = new File(contextFolder, "ctx_"+getNum());
    return new PersistentContext(orb, ctxFolder, true);
  }
  
  /**
   * Create a new context and give it a given name (bound it) in the current
   * context. The method benefits from passing the better readable context name.
   * 
   * @param a_name the name being given to the new context.
   * @return the newly created context.
   * @throws AlreadyBound if the name is already in use.
   * @throws InvalidName if the name has zero length or otherwise invalid.
   */
  public NamingContext bind_new_context(NameComponent[] a_name)
      throws NotFound, AlreadyBound, CannotProceed, InvalidName
  {
    if (named_contexts.containsKey(a_name[0])
        || named_objects.containsKey(a_name[0]))
      throw new AlreadyBound();

    NameTransformer transformer = new NameTransformer();

    File ctxFolder = new File(contextFolder,
                              transformer.toString(a_name).replace('/', '.')
                                  + ".v" + getNum());

    NamingContext child = new PersistentContext(orb, ctxFolder, true);
    bind_context(a_name, child);
    return child;
  }  
}
