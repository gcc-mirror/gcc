/*****************************************************************************/
/*                                                                           */
/*  Copyright (c) 2012                                                       */
/*  Free Software Foundation, Inc.                                           */
/*  This file is part of the UPC runtime Library.                            */
/*  Written by Gary Funck <gary@intrepid.com>                                */
/*  and Nenad Vukicevic <nenad@intrepid.com>                                 */
/*                                                                           */
/*  Copyright (c) 2004, Michigan Technological University                    */
/*  All rights reserved.                                                     */
/*                                                                           */
/*  Redistribution and use in source and binary forms, with or without       */
/*  modification, are permitted provided that the following conditions       */
/*  are met:                                                                 */
/*                                                                           */
/*  * Redistributions of source code must retain the above copyright         */
/*  notice, this list of conditions and the following disclaimer.            */
/*  * Redistributions in binary form must reproduce the above                */
/*  copyright notice, this list of conditions and the following              */
/*  disclaimer in the documentation and/or other materials provided          */
/*  with the distribution.                                                   */
/*  * Neither the name of the Michigan Technological University              */
/*  nor the names of its contributors may be used to endorse or promote      */
/*  products derived from this software without specific prior written       */
/*  permission.                                                              */
/*                                                                           */
/*  THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS      */
/*  "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT        */
/*  LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A  */
/*  PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER */
/*  OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, */
/*  EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO,      */
/*  PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR       */
/*  PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF   */
/*  LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING     */
/*  NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS       */
/*  SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.             */
/*                                                                           */
/*****************************************************************************/

#ifndef _UPC_COLL_H_
#define _UPC_COLL_H_ 1

/**
 * @file upc_coll.h
 * GUPC Portals4 reduce collectives implementation.
 *
 * @addtogroup COLLECTIVES GUPCR Collectives Functions
 * @{
 */

extern int upc_coll_init_flag;
extern void upc_coll_init (void);

/**
 * Collective reduce storage area. It is used for the portals atomic
 * functions, reduce function storage, and signaling that upper part
 * of the reduced tree is ready for the reduced operation.
 */
struct gupcr_reduce_str
{
  int signal;				/** Signal that parent is ready */
  long double value[GUPCR_TREE_FANOUT];	/** Reduced values from children */
};
typedef shared struct gupcr_reduce_str *gupcr_reduce_str_t;
/** Allocated reduce storage space */
extern gupcr_reduce_str_t gupcr_reduce_storage;

/** @} */
#endif /* upc_coll.h */
