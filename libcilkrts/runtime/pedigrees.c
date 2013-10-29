/* pedigrees.c                  -*-C-*-
 *
 *************************************************************************
 *
 *  @copyright
 *  Copyright (C) 2007-2013, Intel Corporation
 *  All rights reserved.
 *  
 *  @copyright
 *  Redistribution and use in source and binary forms, with or without
 *  modification, are permitted provided that the following conditions
 *  are met:
 *  
 *    * Redistributions of source code must retain the above copyright
 *      notice, this list of conditions and the following disclaimer.
 *    * Redistributions in binary form must reproduce the above copyright
 *      notice, this list of conditions and the following disclaimer in
 *      the documentation and/or other materials provided with the
 *      distribution.
 *    * Neither the name of Intel Corporation nor the names of its
 *      contributors may be used to endorse or promote products derived
 *      from this software without specific prior written permission.
 *  
 *  @copyright
 *  THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
 *  "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
 *  LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
 *  A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
 *  HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT,
 *  INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING,
 *  BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS
 *  OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED
 *  AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
 *  LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY
 *  WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
 *  POSSIBILITY OF SUCH DAMAGE.
 *
 **************************************************************************/

#include "pedigrees.h"
#include "local_state.h"

/*************************************************************
  Pedigree API code.
*************************************************************/

/*
 * C99 requires that every inline function with external linkage have one
 * extern declaration in the program (with the inline definition in scope).
 */
COMMON_PORTABLE
extern void update_pedigree_on_leave_frame(__cilkrts_worker *w,
					   __cilkrts_stack_frame *sf);

void __cilkrts_set_pedigree_leaf(__cilkrts_pedigree *leaf)
{
    __cilkrts_set_tls_pedigree_leaf(leaf);
}

void load_pedigree_leaf_into_user_worker(__cilkrts_worker *w)
{
    __cilkrts_pedigree *pedigree_leaf;
    CILK_ASSERT(w->l->type == WORKER_USER);
    pedigree_leaf = __cilkrts_get_tls_pedigree_leaf(1);
    w->pedigree = *pedigree_leaf;

    // Save a pointer to the old leaf.
    // We'll need to restore it later.
    CILK_ASSERT(w->l->original_pedigree_leaf == NULL);
    w->l->original_pedigree_leaf = pedigree_leaf;
    
    __cilkrts_set_tls_pedigree_leaf(&w->pedigree);
    
    // Check that this new pedigree root has at least two values.
    CILK_ASSERT(w->pedigree.parent);
    CILK_ASSERT(w->pedigree.parent->parent == NULL);
}

void save_pedigree_leaf_from_user_worker(__cilkrts_worker *w)
{
    CILK_ASSERT(w->l->type == WORKER_USER);

    // Existing leaf in tls should be for the current worker.
    // This assert is expensive to check though.
    // CILK_ASSERT(&w->pedigree == __cilkrts_get_tls_pedigree_leaf(0));
    CILK_ASSERT(w->l->original_pedigree_leaf);

    // w should finish with a pedigree node that points to 
    // the same root that we just looked up.

    // TODO: This assert should be valid.
    // But we are removing it now to make exceptions (without pedigrees) work.
    // Currently, reading the pedigree after an exception is caught
    // fails because the pedigree chain not restored correctly. 
    // CILK_ASSERT(w->l->original_pedigree_leaf->next == w->pedigree.parent);
    w->l->original_pedigree_leaf->rank = w->pedigree.rank;

    // Save that leaf pointer back into tls.
    __cilkrts_set_tls_pedigree_leaf(w->l->original_pedigree_leaf);
    // Null out worker's leaf for paranoia.
    w->l->original_pedigree_leaf = NULL;
}



/*
  Local Variables: **
  c-file-style:"bsd" **
  c-basic-offset:4 **
  indent-tabs-mode:nil **
  End: **
*/
