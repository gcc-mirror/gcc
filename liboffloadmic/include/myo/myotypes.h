/*
 * Copyright 2010-2015 Intel Corporation.
 *
 * This library is free software; you can redistribute it and/or modify it
 * under the terms of the GNU Lesser General Public License as published
 * by the Free Software Foundation, version 2.1.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
 * Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public
 * License along with this library; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
 * 02110-1301 USA.
 *
 * Disclaimer: The codes contained in these modules may be specific
 * to the Intel Software Development Platform codenamed Knights Ferry,
 * and the Intel product codenamed Knights Corner, and are not backward
 * compatible with other Intel products. Additionally, Intel will NOT
 * support the codes or instruction set in future products.
 *
 * Intel offers no warranty of any kind regarding the code. This code is
 * licensed on an "AS IS" basis and Intel is not obligated to provide
 * any support, assistance, installation, training, or other services
 * of any kind. Intel is also not obligated to provide any updates,
 * enhancements or extensions. Intel specifically disclaims any warranty
 * of merchantability, non-infringement, fitness for any particular
 * purpose, and any other warranty.
 *
 * Further, Intel disclaims all liability of any kind, including but
 * not limited to liability for infringement of any proprietary rights,
 * relating to the use of the code, even if Intel is notified of the
 * possibility of such liability. Except as expressly stated in an Intel
 * license agreement provided with this code and agreed upon with Intel,
 * no license, express or implied, by estoppel or otherwise, to any
 * intellectual property rights is granted herein.
 */
/**
 Description: Define the types used by APIs of MYO programming.
 */

#ifndef _MYO_TYPES_H_
#define _MYO_TYPES_H_

#include <string.h> /* For size_t */

/** @ingroup MYO
 *  @addtogroup MYOTYPES
@{
* @file myotypes.h
*/

#ifdef __cplusplus
extern "C" {
#endif

/*! MYO Status
 */
typedef enum {
    MYO_SUCCESS = 0,      /*!< Success */
    MYO_ERROR,            /*!< Error */

    MYO_INVALID_ENV,      /*!< Invalid Env */
    MYO_INVALID_ARGUMENT, /*!< Invalid Argument */

    MYO_NOT_INITIALIZED,  /*!< Not Initialized */
    MYO_ALREADY_FINALIZED,/*!< Already Finalized */

    MYO_BUF_ERROR,        /*!< Buffer Error */
    MYO_OUT_OF_RANGE,     /*!< Out of Range */
    MYO_OUT_OF_MEMORY,    /*!< Out of Memory */ 

    MYO_ALREADY_EXISTS,   /*!< Already Exists */

    MYO_EOF,              /*!< EOF */
    MYO_FEATURE_NOT_IMPLEMENTED = -1,  /*!< Feature not implemented (see myoiSupportsFeature(). */
} MyoError;


/*! Arena  Ownership */
typedef enum {
    MYO_ARENA_MINE = 1, /*!< Arena MINE Ownership */ 
    MYO_ARENA_OURS,     /*!< Arena OURS Ownership */
} MyoOwnershipType;

  /*! MYO Features */
typedef enum {
                                                         /*!< EVERY VALUE that is less than MYO_FEATURE_BEGIN is not implemented.       */
  MYO_FEATURE_BEGIN         = 1,                         /*!< The first feature that is supported.                                      */
  MYO_FEATURE_POST_LIB_INIT = MYO_FEATURE_BEGIN,         /*!< Allows specifying a function to be executed immediately                   */
                                                         /*   after myoiLibInit() completes. This feature was implemented in version    */
                                                         /*     3.3 of MPSS.                                                            */
  /* MYO_FEATURE_FUTURE_CAPABILITY     = 2,   at some time in the future, as new features are added to MYO, new enumeration constants   */
  /*                                 will be added to the MyoFeatureType, and the value of the new enumeration constant will be greater */
  /*                                 than the current value of MYO_FEATURE_LAST constant, and then the MYO_FEATURE_LAST constant too,   */
  /*                                 will be changed to be the value of the new enumeration constant.  For example, in April, 2014,     */
  /*                                 the POST_LIB_INIT feature was implemented in version 3.3 of MPSS, and the MYO_FEATURE_BEGIN        */
  /*                                 enumeration constant is the same as the MYO_FEATURE_LAST enumeration constant, and both are equal  */
  /*                                 to 1.                                                                                              */
  /*                                 Suppose in December, 2014, a new feature is added to the MYO library, for version 3.4 of MPSS.     */
  /*                                 Then, MYO_FEATURE_BEGIN enumeration constant will be still the value 1, but the MYO_FEATURE_LAST   */
  /*                                 enumeration constant will be set to 2.                                                             */
  /*                                 At runtime, one client binary can determine if the MYO that is installed is capable of any         */
  /*                                 capability.  For example, suppose a future client binary queries version 3.3 of MYO if it is       */
  /*                                 capable of some future feature.  Version 3.3 of MYO will indicate that the feature is not          */
  /*                                 implemented to the client.  But, conversely, suppose the future client queries version 3.4 of MYO  */
  /*                                 if it is capable of some future feature.  Version 3.4 of MYO will indicate that the feature isd    */
  /*                                 supported.                                                                                         */
  /*                                                                                                                                    */
  /*   Date:        |  MYO_FEATURE_BEGIN: |  MYO_FEATURE_LAST: | MPSS VERSION: | myoiSupportsFeature(MYO_FEATURE_FUTURE_CAPABILITY)     */
  /* ---------------+---------------------+--------------------+---------------+---------------------------------------------------     */
  /* April, 2014    |         1           |         1          |     3.3       | MYO_FEATURE_NOT_IMPLEMENTED                            */
  /* December, 2014 |         1           |         2          |     3.4       | MYO_SUCCESS                                            */
  /* ---------------+---------------------+--------------------+---------------+---------------------------------------------------     */
  MYO_FEATURE_LAST          = MYO_FEATURE_POST_LIB_INIT, /*!< The last feature that is supported.                                       */
                                                         /*!< EVERY VALUE that is greater than MYO_FEATURE_LAST is not implemented.     */
  /*!< EVERY VALUE that is greater than or equal to MYO_FEATURE_BEGIN AND less than or equal to MYO_FEATURE_LAST is implemented.        */
} MyoFeatureType;  /* (For more information, please also see myoiSupportsFeature() function declaration.)  */

/*************************************************************
 *  define the property of MYO Arena 
 ***********************************************************/
#define MYO_CONSISTENCY_MODE            0x3
#define MYO_RELEASE_CONSISTENCY         0x1
#define MYO_STRONG_RELEASE_CONSISTENCY  0x2
#define MYO_STRONG_CONSISTENCY          0x3
#define MYO_UPDATE_ON_DEMAND            0x8
#define MYO_UPDATE_ON_ACQUIRE           0x10
#define MYO_RECORD_DIRTY                0x20
#define MYO_NOT_RECORD_DIRTY            0x40
#define MYO_ONE_VERSION                 0x80
#define MYO_MULTI_VERSIONS              0x100
#define MYO_CONSISTENCY                 0x200
#define MYO_NO_CONSISTENCY              0x400
#define MYO_HOST_TO_DEVICE              0x800
#define MYO_DEVICE_TO_HOST              0x1000
#define MYO_HYBRID_UPDATE               0x2000
typedef unsigned int MyoArena;

typedef void * MyoMutex;
typedef void * MyoSem;
typedef void * MyoBarrier;


#ifdef __cplusplus
}
#endif
#endif // _MYO_TYPES_H_
/*! @} */
