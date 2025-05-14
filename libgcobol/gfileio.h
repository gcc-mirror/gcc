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
#ifndef GFILEIO_H_
#define GFILEIO_H_

// For indexed files, there can be one or more indexes, one per key.
// Each index is one or more fields.

struct file_hole_t
  {
  long location;
  size_t size;
  };

struct file_index_t
    {
    std::multimap<std::vector<unsigned char>, long> key_to_position;
    std::multimap<std::vector<unsigned char>, long>::iterator current_iterator;
    std::multimap<std::vector<unsigned char>, long>::iterator ending_iterator;
    };

class supplemental_t
  {
  public:
    std::vector<file_hole_t>  holes;
    std::vector<file_index_t> indexes;
    std::vector<int>          uniques;
  };

extern "C"
{
void __gg__handle_error(const char *function, const char *msg);

void __gg__file_open(   cblc_file_t *file,
                        char *filename,
                        int mode_char,
                        int is_quoted);

void __gg__file_reopen(cblc_file_t *file, int mode_char);

void __gg__file_close(  cblc_file_t *file, int how );

void __gg__file_read(   cblc_file_t *file,
                        int where);

void __gg__file_write(  cblc_file_t    *file,
                        unsigned char  *location,
                        size_t          length,
                        int             after,
                        int             lines,
                        int             is_random );
}

#endif