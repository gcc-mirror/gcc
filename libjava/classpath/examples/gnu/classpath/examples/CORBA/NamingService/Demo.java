/* Demo.java -- Shows how to use Classpath transient naming service.
   Copyright (C) 2005 Free Software Foundation, Inc.

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


package gnu.classpath.examples.CORBA.NamingService;

import gnu.CORBA.IOR;
import gnu.CORBA.NamingService.NamingServiceTransient;

import org.omg.CORBA.ORB;
import org.omg.CORBA.Object;
import org.omg.CosNaming.Binding;
import org.omg.CosNaming.BindingHolder;
import org.omg.CosNaming.BindingIterator;
import org.omg.CosNaming.BindingIteratorHolder;
import org.omg.CosNaming.BindingListHolder;
import org.omg.CosNaming.NameComponent;
import org.omg.CosNaming.NamingContext;
import org.omg.CosNaming.NamingContextExt;
import org.omg.CosNaming.NamingContextExtHelper;
import org.omg.CosNaming.NamingContextHelper;

/**
 * A simple test of the naming service.
 *
 * The main class of the GNU Classpath transient naming service is
 * {@link gnu.CORBA.NamingService}. This class must be started
 * before starting this example.
 *
 * This example should interoperate as with GNU Classpath naming
 * service, as with Sun Microsystems transient and persistent
 * naming services, included in releases 1.3 and 1.4 (tnameserv and
 * orbd). To work with this example, the naming service must
 * be started on the local host, at the port 900.
 *
 * The persistent naming service is currently under development.
 *
 * @author Audrius Meskauskas, Lithuania (AudriusA@Bioinformatics.org)
 */
public class Demo
{
  public static void main(String[] an_args)
  {
    // We create the following naming graph:
    // <ROOT CONTEXT>
    //  |
    //  +--- <c.d context>
    //  |     |
    //  |     +--- obj
    //  |
    //  +--- xobj
    //
    // Where both obj and xobj are CORBA objects, representing the
    // default naming service.
    //
    System.out.println("Starting the GNU Classpath " +
                       "built-in transient naming service"
                      );
    
    final String[] args = an_args;

    new Thread()
      {
        public void run()
        {
          NamingServiceTransient.main(args);
        }
      }.start();

    System.out.println("Waiting for three seconds for naming service to start:");
    try
      {
        Thread.sleep(3000);
      }
    catch (InterruptedException ex)
      {
      }

    try
      {
        ORB orb = ORB.init(args, null);

        Object no = orb.resolve_initial_references("NameService");

        System.out.println("Naming service IOR:" + orb.object_to_string(no));

        System.out.println(IOR.parse(orb.object_to_string(no)));

        NamingContextExt namer = NamingContextExtHelper.narrow(no);

        System.out.println("Naming service: " + namer.getClass().getName());

        NamingContext second = namer.new_context();

        namer.rebind_context(namer.to_name("c.d"), second);
        namer.rebind(namer.to_name("xobj"), no);

        second.rebind(namer.to_name("obj"), no);

        NamingContext nsec =
          NamingContextHelper.narrow(namer.resolve_str("c.d"));

        System.out.println(namer.resolve(namer.to_name("c.d/obj")));

        // In all cases, this must be the same object (the naming
        // service itself).
        System.out.println(nsec.resolve(new NameComponent[]
                                        {
                                          new NameComponent("obj", "")
                                        }
                                       )
                          );
        System.out.println(namer.resolve_str("xobj"));

        // In all cases, this must be the same object (the naming
        // service itself).
        System.out.println(namer.resolve(new NameComponent[]
                                         {
                                           new NameComponent("c", "d"),
                                           new NameComponent("obj", "")
                                         }
                                        )
                          );

        System.out.println(namer.resolve_str("c.d/obj"));

        System.out.println("Test binding list iterator:");

        BindingListHolder lh = new BindingListHolder();
        BindingIteratorHolder lih = new BindingIteratorHolder();

        namer.list(0, lh, lih);

        BindingIterator iter = lih.value;
        BindingHolder binding = new BindingHolder();

        while (iter.next_one(binding))
          {
            Binding b = binding.value;
            System.out.println("NAME: " + namer.to_string(b.binding_name) +
                               " TYPE " + b.binding_type.value()
                              );
          }

        System.out.println("Testing binding list:");

        iter.destroy();

        namer.list(Integer.MAX_VALUE, lh, lih);

        for (int i = 0; i < lh.value.length; i++)
          {
            Binding b = lh.value [ i ];
            System.out.println("NAME: " + namer.to_string(b.binding_name) +
                               " TYPE " + b.binding_type.value()
                              );
          }
      }
    catch (Exception ex)
      {
        ex.printStackTrace();
        System.exit(1);
      }

    System.exit(0);
  }
}
