# sppp - the Standard Prelude Pre-Processor
#
# Copyright (C) 2025 Jose E. Marchesi
#
# This file is free software; you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation; either version 3 of the License, or
# (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program; see the file COPYING3.  If not see
# <http://www.gnu.org/licenses/>.

function error(msg)
{
    print FILENAME ":" msg | "cat 1>&2"
    exit (1)
}

BEGIN {
    num_alternatives = 0
    in_iter = 0
    template = ""
}

/^[ \t]*\{ Process this file/ {
    print "{ This is auto-generated from " FILENAME ".  Do not edit.  }"
    next
}

/^[ \t]*\{[ \t]*iter[ \t]+/ {
    if (match ($0, /[ \t]*\{[ \t]*iter[ \t]+([a-zA-Z_]+)[\t ]*(\{.*\}[ \t]*){1,}[ \t]*\}/, matches) == 0)
        error(FNR ": invalid iter")

    iter_name = matches[1]
    iter_alts = matches[2]

    # Count and collect alternatives.  #
    iter_num_alternatives = 0
    while (match (iter_alts, /[ \t]*\{([^\}]*)\}/, matches) > 0)
    {      
        iter_num_alternatives++
        iter_alts = substr (iter_alts, RSTART + RLENGTH)
        iter_names[iter_name] = 1
        iterators[iter_name,iter_num_alternatives] = matches[1]
    }

    if (in_iter == 1)
    {
        if (iter_num_alternatives != num_alternatives)
            error(FNR ": invalid number of alternatives in iterator")
    }

    num_alternatives = iter_num_alternatives
    in_iter = 1
    next
}

/^[ \t]*\{[ \t]*reti/ {
    separator = ""
    if (match ($0, /[ \t]*\{[ \t]*reti[ \t]+\{([^\}]*)\}/, matches) > 0)
        separator = matches[1]

    for (nalt = 1; nalt <= num_alternatives; nalt++)
    {
        output = template
        for (iter_name in iter_names)
        {
            while (sub ("\\{" iter_name "\\}", iterators[iter_name,nalt], output)) {}
        }
        printf "%s%s\n", substr (output, 0, length(output) - 1), nalt < num_alternatives ? separator : ""
    }

    for (key in iter_names) delete iter_names[key]
    for (key in iterators) delete iterators[key]
    in_iter = 0;
    template = ""
    num_alternatives = 0
    next
}

in_iter == 1 {
    template = template $0 "\n"
    next
}

{
    print $0
}
