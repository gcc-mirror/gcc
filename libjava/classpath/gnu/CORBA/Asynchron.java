/* Asynchron.java --
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


package gnu.CORBA;

import org.omg.CORBA.Request;
import org.omg.CORBA.WrongTransaction;

import java.util.Iterator;
import java.util.LinkedList;

/**
 * Handles the asynchronous dynamic invocations.
 *
 * @author Audrius Meskauskas, Lithuania (AudriusA@Bioinformatics.org)
 */
public class Asynchron
{
  LinkedList sent = new LinkedList();

  /**
   * Send multiple prepared requests one way, do not caring about the answer.
   * The messages, containing requests, will be marked, indicating that
   * the sender is not expecting to get a reply.
   *
   * @param requests the prepared array of requests.
   *
   * @see Request#send_oneway()
   */
  public void send_multiple_requests_oneway(Request[] requests)
  {
    for (int i = 0; i < requests.length; i++)
      {
        requests [ i ].send_oneway();
      }
  }

  /**
   * Send multiple prepared requests expecting to get a reply. All requests
   * are send in parallel, each in its own separate thread. When the
   * reply arrives, it is stored in the agreed fields of the corresponing
   * request data structure. If this method is called repeatedly,
   * the new requests are added to the set of the currently sent requests,
   * but the old set is not discarded.
   *
   * @param requests the prepared array of requests.
   *
   * @see #poll_next_response()
   * @see #get_next_response()
   * @see Request#send_deferred()
   */
  public void send_multiple_requests_deferred(Request[] requests)
  {
    synchronized (sent)
      {
        for (int i = 0; i < requests.length; i++)
          {
            sent.add(requests [ i ]);

            // TODO Reuse threads that are instantiated in the method below,
            // one thread per call.
            requests [ i ].send_deferred();
          }
      }
  }

  /**
   * Find if any of the requests that have been previously sent with
   * {@link #send_multiple_requests_deferred}, have a response yet.
   *
   * @return true if there is at least one response to the previously
   * sent request, false otherwise.
   */
  public boolean poll_next_response()
  {
    synchronized (sent)
      {
        Iterator iter = sent.iterator();
        Request r;
        while (iter.hasNext())
          {
            r = (Request) iter.next();
            if (r.poll_response())
              return true;
          }
      }
    return false;
  }

  /**
   * Get the next instance with a response being received. If all currently
   * sent responses not yet processed, this method pauses till at least one of
   * them is complete. If there are no requests currently sent, the method
   * pauses till some request is submitted and the response is received.
   * This strategy is identical to the one accepted by Suns 1.4 ORB
   * implementation.
   *
   * The returned response is removed from the list of the currently
   * submitted responses and is never returned again.
   *
   * @return the previously sent request that now contains the received
   * response.
   *
   * @throws WrongTransaction If the method was called from the transaction
   * scope different than the one, used to send the request. The exception
   * can be raised only if the request is implicitly associated with some
   * particular transaction.
   */
  public Request get_next_response()
                            throws WrongTransaction
  {
    // The hard-coded waiting times for the incremental waiter.
    // TODO it is possible to write more tricky system where the
    // requests notify the Asynchron when they are complete.
    // Wait for 5 ms intially.
    int wait = 8;

    // Double the waiting time
    int INC = 2;

    // Do not increase if the waiting time is already over 500 ms.
    int MAX = 500;
    while (true)
      {
        synchronized (sent)
          {
            Iterator iter = sent.iterator();
            Request r;
            while (iter.hasNext())
              {
                r = (Request) iter.next();
                if (r.poll_response())
                {
                  sent.remove(r);
                  return r;
                }
              }
          }
        try
          {
            Thread.sleep(wait);
            if (wait < MAX)
              wait = wait * INC;
          }
        catch (InterruptedException ex)
          {
          }
      }
  }
}
