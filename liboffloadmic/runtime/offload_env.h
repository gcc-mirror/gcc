/*
    Copyright (c) 2014-2016 Intel Corporation.  All Rights Reserved.

    Redistribution and use in source and binary forms, with or without
    modification, are permitted provided that the following conditions
    are met:

      * Redistributions of source code must retain the above copyright
        notice, this list of conditions and the following disclaimer.
      * Redistributions in binary form must reproduce the above copyright
        notice, this list of conditions and the following disclaimer in the
        documentation and/or other materials provided with the distribution.
      * Neither the name of Intel Corporation nor the names of its
        contributors may be used to endorse or promote products derived
        from this software without specific prior written permission.

    THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
    "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
    LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
    A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
    HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
    SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
    LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
    DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
    THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
    (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
    OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
*/


#ifndef OFFLOAD_ENV_H_INCLUDED
#define OFFLOAD_ENV_H_INCLUDED

#include <list>
#include "offload_util.h"

// data structure and routines to parse MIC user environment and pass to MIC

enum MicEnvVarKind
{
    c_no_mic,         // not MIC env var
    c_mic_var,        // for <mic-prefix>_<var>
    c_mic_card_var,   // for <mic-prefix>_<card-number>_<var>
    c_mic_card_env    // for <mic-prefix>_<card-number>_ENV
};

struct DLL_LOCAL MicEnvVar {
public:
    MicEnvVar() : prefix(0) {}
    ~MicEnvVar();

    void analyze_env_var(char *env_var_string);
    char** create_environ_for_card(int card_num);
    MicEnvVarKind get_env_var_kind(
        char *env_var_string,
        int *card_number,
        char **env_var_name,
        int *env_var_name_length,
        char **env_var_def
    );
    void add_env_var(
        int card_number,
        char *env_var_name,
        int env_var_name_length,
        char *env_var_def
    );

    void set_prefix(const char *pref) {
        prefix = (pref && *pref != '\0') ? pref : 0;
    }

    struct VarValue {
    public:
        char* env_var;
        int   length;
        char* env_var_value;

        VarValue(char* var, int ln, char* value)
        {
            env_var = var;
            length = ln;
            env_var_value = value;
        }
        ~VarValue();
    };

    struct CardEnvVars {
    public:

        int card_number;
        std::list<struct VarValue*> env_vars;

        CardEnvVars() { card_number = any_card; }
        CardEnvVars(int num) { card_number = num; }
        ~CardEnvVars();

        void add_new_env_var(int number, char *env_var, int length,
                             char *env_var_value);
        VarValue* find_var(char* env_var_name, int env_var_name_length);
    };
    static const int any_card;

private:
    void         mic_parse_env_var_list(int card_number, char *env_var_def);
    CardEnvVars* get_card(int number);

    const char *prefix;
    std::list<struct CardEnvVars *> card_spec_list;
    CardEnvVars common_vars;
};

#endif // OFFLOAD_ENV_H_INCLUDED
