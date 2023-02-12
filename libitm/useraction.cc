/* Copyright (C) 2008-2023 Free Software Foundation, Inc.
   Contributed by Richard Henderson <rth@redhat.com>.

   This file is part of the GNU Transactional Memory Library (libitm).

   Libitm is free software; you can redistribute it and/or modify it
   under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 3 of the License, or
   (at your option) any later version.

   Libitm is distributed in the hope that it will be useful, but WITHOUT ANY
   WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
   FOR A PARTICULAR PURPOSE.  See the GNU General Public License for
   more details.

   Under Section 7 of GPL version 3, you are granted additional
   permissions described in the GCC Runtime Library Exception, version
   3.1, as published by the Free Software Foundation.

   You should have received a copy of the GNU General Public License and
   a copy of the GCC Runtime Library Exception along with this program;
   see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see
   <http://www.gnu.org/licenses/>.  */

#include "libitm_i.h"

namespace GTM HIDDEN {

void
gtm_thread::rollback_user_actions(size_t until_size)
{
  for (size_t s = user_actions.size(); s > until_size; s--)
    {
      user_action *a = user_actions.pop();
      if (!a->on_commit)
	a->fn (a->arg);
    }
}


void
gtm_thread::commit_user_actions()
{
  for (vector<user_action>::iterator i = user_actions.begin(),
      ie = user_actions.end(); i != ie; i++)
    {
      if (i->on_commit)
	i->fn (i->arg);
    }
  user_actions.clear();
}

} // namespace GTM

using namespace GTM;

void ITM_REGPARM
_ITM_addUserCommitAction(_ITM_userCommitFunction fn,
			 _ITM_transactionId_t tid, void *arg)
{
  gtm_thread *tx = gtm_thr();
  if (tid != _ITM_noTransactionId)
    GTM_fatal("resumingTransactionId in _ITM_addUserCommitAction must be "
	      "_ITM_noTransactionId");
  gtm_thread::user_action *a = tx->user_actions.push();
  a->fn = fn;
  a->arg = arg;
  a->on_commit = true;
  a->resuming_id = tid;
}


void ITM_REGPARM
_ITM_addUserUndoAction(_ITM_userUndoFunction fn, void * arg)
{
  gtm_thread *tx = gtm_thr();
  gtm_thread::user_action *a = tx->user_actions.push();
  a->fn = fn;
  a->arg = arg;
  a->on_commit = false;
}
