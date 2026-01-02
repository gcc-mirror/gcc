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
    line=$0
    if (match (line, /[ \t\*\{[ \t]*iter[ \t]+/) == 0)
        error(FNR ": invalid iter")
    line = substr (line, RSTART + RLENGTH)
    # Iterator name.  #
    if (match (line, /[a-zA-Z_]+/) == 0)
        error(FNR ": expected iterator name")
    iter_name = substr (line, RSTART, RLENGTH)
    line = substr (line, RSTART + RLENGTH)
    # Blanks.  #
    if (match (line, /[\t ]*/) == 0)
        error(FNR ": invalid iter")
    line = substr (line, RSTART + RLENGTH)
    # Iterator alternatives.  #
    if (match (line, /\{.*\}/) == 0)
        error(FNR ": expected iterator alternatives")
    iter_alts = substr (line, RSTART, RLENGTH)
    line = substr (line, RSTART + RLENGTH)

    # Count and collect alternatives.  #
    iter_num_alternatives = 0
    while (match (iter_alts, /[ \t]*\{([^\}]*)\}/) > 0)
    {
        iter_num_alternatives++
        iter_names[iter_name] = 1
        # Skip prefix.  #
        if (match (iter_alts, /[ \t]*\{/) == 0)
            error(FNR ": invalid iterator alternative")
        iter_alts = substr (iter_alts, RSTART + RLENGTH)
        # Get alternative contents.  #
        if (match (iter_alts, /[^\}]*/) == 0)
            error(FNR ": invalid iterator alternative")
        iterators[iter_name,iter_num_alternatives] = substr (iter_alts, RSTART, RLENGTH)
        iter_alts = substr (iter_alts, RSTART + RLENGTH)
        # Skip trailer.  #
        if (match (iter_alts, /\}/) == 0)
            error(FNR ": invalid iterator alternative")
        iter_alts = substr (iter_alts, RSTART + RLENGTH)
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
    line = $0
    if (match (line, /[ \t]*\{[ \t]*reti[ \t]*\{/) > 0)
    {
        # Extract separator. #
        line = substr (line, RSTART + RLENGTH)
        if (match (line, /[^\}]*/) == 0)
            error(FNR ": invalid separator in reti")
        separator = substr (line, RSTART, RLENGTH)
        line = substr (line, RSTART + RLENGTH)
        # Skip suffix
        if (match (line, /\}/) == 0)
            error(FNR ": expected closing } in reti separator")
    }
    else
    {
        # No separator.  #
        if (match (line, /[ \t]*\{[ \t]*reti[ \t]*\}/) == 0)
            error(FNR ": invalid reti")
    }

    for (nalt = 1; nalt <= num_alternatives; nalt++)
    {
        output = template
        for (iter_name in iter_names)
        {
            while (sub ("\\{" iter_name "\\}", iterators[iter_name,nalt], output)) {}
        }
        if (nalt < num_alternatives)
            sep = separator
        else
            sep = ""
        printf "%s%s\n", substr (output, 0, length(output) - 1), sep
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
