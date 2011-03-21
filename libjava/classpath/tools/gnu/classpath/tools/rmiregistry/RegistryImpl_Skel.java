/* RegistryImpl_Skel.java
   Copyright (C) 2002, 2006 Free Software Foundation, Inc.

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


package gnu.classpath.tools.rmiregistry;

import java.io.IOException;
import java.io.ObjectInput;
import java.io.ObjectOutput;
import java.rmi.MarshalException;
import java.rmi.Remote;
import java.rmi.UnmarshalException;
import java.rmi.server.Operation;
import java.rmi.server.RemoteCall;
import java.rmi.server.SkeletonMismatchException;

/**
 * This skeleton supports unlikely cases when the naming service is
 * contacted from other interoperable java implementation that still uses
 * the old style skeleton-dependent invocations.
 */
public final class RegistryImpl_Skel
    implements java.rmi.server.Skeleton
{
    private static final long interfaceHash = 4905912898345647071L;

    /**
     * Repeated multiple times.
     */
    static final String EUM = "error unmarshalling arguments for Registry";

    /**
     * Repeated multiple times.
     */
    static final String EMR = "error marshalling return from Registry";

    private static final Operation[] operations =
      {
         new Operation("void bind(java.lang.String, Remote"),
         new Operation("java.lang.String[] list("),
         new Operation("Remote lookup(java.lang.String"),
         new Operation("void rebind(java.lang.String, Remote"),
         new Operation("void unbind(java.lang.String")
      };

    public Operation[] getOperations()
  {
    return ((Operation[]) operations.clone());
  }

    public void dispatch(Remote obj, RemoteCall call,
                       int opnum, long hash) throws java.lang.Exception
  {
    if (opnum < 0)
      {
        if (hash == 7583982177005850366L)
          opnum = 0;
        else if (hash == 2571371476350237748L)
          opnum = 1;
        else if (hash == -7538657168040752697L)
          opnum = 2;
        else if (hash == -8381844669958460146L)
          opnum = 3;
        else if (hash == 7305022919901907578L)
          opnum = 4;
        else
          throw new SkeletonMismatchException("interface hash mismatch");
      }
    else if (hash != interfaceHash)
      throw new SkeletonMismatchException("interface hash mismatch");

    RegistryImpl server = (RegistryImpl) obj;
    switch (opnum)
      {
      case 0:
      {
        java.lang.String $param_0;
        Remote $param_1;
        try
          {
            ObjectInput in = call.getInputStream();
            $param_0 = (java.lang.String) in.readObject();
            $param_1 = (Remote) in.readObject();

          }
        catch (IOException e)
          {
            throw new UnmarshalException(EUM, e);
          }
        catch (java.lang.ClassCastException e)
          {
            throw new UnmarshalException(EUM, e);
          }
        finally
          {
            call.releaseInputStream();
          }
        server.bind($param_0, $param_1);
        try
          {
            ObjectOutput out = call.getResultStream(true);
          }
        catch (IOException e)
          {
            throw new MarshalException(EMR, e);
          }
        break;
      }

      case 1:
      {
        try
          {
            ObjectInput in = call.getInputStream();

          }
        catch (IOException e)
          {
            throw new UnmarshalException(EUM, e);
          }
        finally
          {
            call.releaseInputStream();
          }
        java.lang.String[] $result = server.list();
        try
          {
            ObjectOutput out = call.getResultStream(true);
            out.writeObject($result);
          }
        catch (IOException e)
          {
            throw new MarshalException(EMR, e);
          }
        break;
      }

      case 2:
      {
        java.lang.String $param_0;
        try
          {
            ObjectInput in = call.getInputStream();
            $param_0 = (java.lang.String) in.readObject();

          }
        catch (IOException e)
          {
            throw new UnmarshalException(EUM, e);
          }
        catch (java.lang.ClassCastException e)
          {
            throw new UnmarshalException(EUM, e);
          }
        finally
          {
            call.releaseInputStream();
          }
        Remote $result = server.lookup($param_0);
        try
          {
            ObjectOutput out = call.getResultStream(true);
            out.writeObject($result);
          }
        catch (IOException e)
          {
            throw new MarshalException(EMR, e);
          }
        break;
      }

      case 3:
      {
        java.lang.String $param_0;
        Remote $param_1;
        try
          {
            ObjectInput in = call.getInputStream();
            $param_0 = (java.lang.String) in.readObject();
            $param_1 = (Remote) in.readObject();

          }
        catch (IOException e)
          {
            throw new UnmarshalException(EUM, e);
          }
        catch (java.lang.ClassCastException e)
          {
            throw new UnmarshalException(EUM, e);
          }
        finally
          {
            call.releaseInputStream();
          }
        server.rebind($param_0, $param_1);
        try
          {
            ObjectOutput out = call.getResultStream(true);
          }
        catch (IOException e)
          {
            throw new MarshalException(EMR, e);
          }
        break;
      }

      case 4:
      {
        java.lang.String $param_0;
        try
          {
            ObjectInput in = call.getInputStream();
            $param_0 = (java.lang.String) in.readObject();

          }
        catch (IOException e)
          {
            throw new UnmarshalException(EUM, e);
          }
        catch (java.lang.ClassCastException e)
          {
            throw new UnmarshalException(EUM, e);
          }
        finally
          {
            call.releaseInputStream();
          }
        server.unbind($param_0);
        try
          {
            ObjectOutput out = call.getResultStream(true);
          }
        catch (IOException e)
          {
            throw new MarshalException(EMR, e);
          }
        break;
      }

      default:
        throw new UnmarshalException("invalid method number");
      }
  }
}
