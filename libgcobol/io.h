/*
 * Copyright (c) 2021-2025 Symas Corporation
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions are
 * met:
 *
 * * Redistributions of source code must retain the above copyright
 *   notice, this list of conditions and the following disclaimer.
 * * Redistributions in binary form must reproduce the above
 *   copyright notice, this list of conditions and the following disclaimer
 *   in the documentation and/or other materials provided with the
 *   distribution.
 * * Neither the name of the Symas Corporation nor the names of its
 *   contributors may be used to endorse or promote products derived from
 *   this software without specific prior written permission.
 *
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
 * "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
 * LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
 * A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
 * OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
 * SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
 * LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
 * DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
 * THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
 * (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
 * OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 */

/*
 *  File status key values and meanings
 *   0  Successful completion
 *       0  No further information
 *       2  Duplicate key READ
 *       4  Short/long READ
 *       5  OPEN optional file unavailable
 *       7  Not a tape
 *   1  At-end condition
 *       0  Sequential READ EOF
 *       4  Relative record too big
 *   2  Invalid key condition
 *       1  Sequence error
 *       2  Duplicate key WRITE
 *       3  Record not found
 *       4  Sequential WRITE EOF
 *   3  Permanent error
 *       0  No further information
 *       1  Filename inconsistent with operating system
 *       4  Boundary violation
 *       5  OPEN nonoptional file unavailable
 *       7  OPEN EACCES
 *       8  OPEN file previously closed with lock
 *       9  OPEN wrong file type
 *   4  Logic error condition
 *       1  OPEN file already open
 *       2  CLOSE file not open
 *       3  REWRITE without prior READ
 *       4  REWRITE/WRITE boundary violation
 *       6  READ after failed READ
 *       7  File not open for READ
 *       8  File not open for WRITE
 *       9  File not open for DELETE/REWRITE
 *   9  Implementor-defined
 *       0  VSAM/QSAM close on wrong thread
 *       1  VSAM password failure
 *       2  Logic error
 *       3  Resource unavailable
 *       5  Incomplete file information
 *       6  VSAM no DD statement
 *       7  VSAM File integrity verified
 *       8  OPEN invalid environment variable contents
 */

#ifndef IO_H_
#define IO_H_

enum file_high_t {
  FhSuccess = 0,
  FhAtEnd = 1,
  FhInvKey = 2,
  FhOsError = 3,
  FhLogicError = 4,
  FhImplementor = 9,
};

enum file_status_t {
                    FsSuccess     = FhSuccess,
                    FsDupRead     = (FhSuccess * 10) + 2,   // First digit is 0
                    FsRecordLength= (FhSuccess * 10) + 4,
                    FsUnavail     = (FhSuccess * 10) + 5,
                    FsNotaTape    = (FhSuccess * 10) + 7,

                    FsEofSeq      = (FhAtEnd * 10) + 0,     // First digit is 1
                    FsEofRel      = (FhAtEnd * 10) + 4,

                    FsKeySeq      = (FhInvKey * 10) + 1,    // First digit is 2
                    FsDupWrite    = (FhInvKey * 10) + 2,
                    FsNotFound    = (FhInvKey * 10) + 3,
                    FsEofWrite    = (FhInvKey * 10) + 4,

                    FsOsError     = (FhOsError * 10) + 0,   // First digit is 3
                    FsNameError   = (FhOsError * 10) + 1,
                    FsBoundary    = (FhOsError * 10) + 4,
                    FsNoFile      = (FhOsError * 10) + 5,
                    FsNoAccess    = (FhOsError * 10) + 7,
                    FsCloseLock   = (FhOsError * 10) + 8,
                    FsWrongType   = (FhOsError * 10) + 9,

                    FsLogicErr    = (FhLogicError * 10) + 0,    // First digit is 4
                    FsIsOpen      = (FhLogicError * 10) + 1,
                    FsCloseNotOpen= (FhLogicError * 10) + 2,
                    FsNoRead      = (FhLogicError * 10) + 3,
                    FsBoundWrite  = (FhLogicError * 10) + 4,
                    FsReadError   = (FhLogicError * 10) + 6,
                    FsReadNotOpen = (FhLogicError * 10) + 7,
                    FsNoWrite     = (FhLogicError * 10) + 8,
                    FsNoDelete    = (FhLogicError * 10) + 9,

                    FsWrongThread = (FhImplementor * 10) + 0,   // First digit is 9
                    FsPassword    = (FhImplementor * 10) + 1,
                    FsLogicOther  = (FhImplementor * 10) + 2,
                    FsNoResource  = (FhImplementor * 10) + 3,
                    FsIncomplete  = (FhImplementor * 10) + 5,
                    FsNoDD        = (FhImplementor * 10) + 6,
                    FsVsamOK      = (FhImplementor * 10) + 7,
                    FsBadEnvVar   = (FhImplementor * 10) + 8,

                    FsErrno       = (1000000)                   // This means "map errno to one of the above errors"
};

#define FhNotOkay FsEofSeq  // Values less than 10 mean the data are valid

extern "C" file_status_t __gg__file_status_word(enum file_status_t status,
                                                int error_number);

#endif
