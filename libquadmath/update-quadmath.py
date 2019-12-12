#!/usr/bin/python3
# Update libquadmath code from glibc sources.
# Copyright (C) 2018 Free Software Foundation, Inc.
# This file is part of the libquadmath library.
#
# Libquadmath is free software; you can redistribute it and/or
# modify it under the terms of the GNU Lesser General Public
# License as published by the Free Software Foundation; either
# version 2.1 of the License, or (at your option) any later version.
#
# Libquadmath is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
# Lesser General Public License for more details.
#
# You should have received a copy of the GNU Lesser General Public
# License along with libquadmath; if not, see
# <https://www.gnu.org/licenses/>.

# Usage: update-quadmath.py glibc_srcdir quadmath_srcdir

import argparse
from collections import defaultdict
import os.path
import re


def replace_in_file(repl_map, extra_map, src, dest):
    """Apply the replacements in repl_map, then those in extra_map, to the
    file src, producing dest."""
    with open(src, 'r') as src_file:
        text = src_file.read()
    for re_src, re_repl in sorted(repl_map.items()):
        text = re.sub(re_src, re_repl, text)
    for re_src, re_repl in sorted(extra_map.items()):
        text = re.sub(re_src, re_repl, text)
    text = text.rstrip() + '\n'
    with open(dest, 'w') as dest_file:
        dest_file.write(text)


def update_sources(glibc_srcdir, quadmath_srcdir):
    """Update libquadmath sources."""
    glibc_ldbl128 = os.path.join(glibc_srcdir, 'sysdeps/ieee754/ldbl-128')
    glibc_math = os.path.join(glibc_srcdir, 'math')
    quadmath_math = os.path.join(quadmath_srcdir, 'math')
    float128_h = os.path.join(glibc_srcdir,
                              'sysdeps/ieee754/float128/float128_private.h')
    repl_map = {}
    # Use float128_private.h to get an initial list of names to
    # replace for libquadmath.
    repl_names = {}
    with open(float128_h, 'r') as header:
        for line in header:
            line = line.strip()
            if not line.startswith('#define '):
                continue
            match = re.fullmatch('^#define[ \t]+([a-zA-Z0-9_]+)'
                                 '[ \t]+([a-zA-Z0-9_]+)', line)
            if not match:
                continue
            macro = match.group(1)
            result = match.group(2)
            result = result.replace('f128', 'q')
            result = result.replace('__ieee754_', '')
            if result not in ('__expq_table', '__sincosq_table',
                              '__builtin_signbit'):
                result = result.replace('__', '')
            result = result.replace('_do_not_use', '')
            if result in ('rem_pio2q', 'kernel_sincosq', 'kernel_sinq',
                          'kernel_cosq', 'kernel_tanq', 'gammaq_r',
                          'gamma_productq', 'lgamma_negq', 'lgamma_productq',
                          'lgammaq_r', 'x2y2m1q'):
                # Internal function names, for which the above removal
                # of leading '__' was inappropriate and a leading
                # '__quadmath_' needs adding instead.  In the
                # libquadmath context, lgammaq_r is an internal name.
                result = '__quadmath_' + result
            if result == 'ieee854_float128_shape_type':
                result = 'ieee854_float128'
            if result == 'HUGE_VAL_F128':
                result = 'HUGE_VALQ'
            repl_names[macro] = result
    # More such names that aren't simply defined as object-like macros
    # in float128_private.h.
    repl_names['_Float128'] = '__float128'
    repl_names['SET_RESTORE_ROUNDL'] = 'SET_RESTORE_ROUNDF128'
    repl_names['parts32'] = 'words32'
    for macro in ('GET_LDOUBLE_LSW64', 'GET_LDOUBLE_MSW64',
                  'GET_LDOUBLE_WORDS64', 'SET_LDOUBLE_LSW64',
                  'SET_LDOUBLE_MSW64', 'SET_LDOUBLE_WORDS64'):
        repl_names[macro] = macro.replace('LDOUBLE', 'FLT128')
    # The classication macros are replaced.
    for macro in ('FP_NAN', 'FP_INFINITE', 'FP_ZERO', 'FP_SUBNORMAL',
                  'FP_NORMAL'):
        repl_names[macro] = 'QUAD' + macro
    for macro in ('fpclassify', 'signbit', 'isnan', 'isinf', 'issignaling'):
        repl_names[macro] = macro + 'q'
    repl_names['isfinite'] = 'finiteq'
    # Map comparison macros to the __builtin forms.
    for macro in ('isgreater', 'isgreaterequal', 'isless', 'islessequal',
                  'islessgreater', 'isunordered'):
        repl_names[macro] = '__builtin_' + macro
    # Replace macros used in type-generic templates in glibc.
    repl_names['FLOAT'] = '__float128'
    repl_names['CFLOAT'] = '__complex128'
    repl_names['M_NAN'] = 'nanq ("")'
    repl_names['M_HUGE_VAL'] = 'HUGE_VALQ'
    repl_names['INFINITY'] = '__builtin_inf ()'
    for macro in ('MIN_EXP', 'MAX_EXP', 'MIN', 'MAX', 'MANT_DIG', 'EPSILON'):
        repl_names['M_%s' % macro] = 'FLT128_%s' % macro
    for macro in ('COPYSIGN', 'FABS', 'SINCOS', 'SCALBN', 'LOG1P', 'ATAN2',
                  'COSH', 'EXP', 'HYPOT', 'LOG', 'SINH', 'SQRT'):
        repl_names['M_%s' % macro] = macro.lower() + 'q'
    # Each such name is replaced when it appears as a whole word.
    for macro in repl_names:
        repl_map[r'\b%s\b' % macro] = repl_names[macro]
    # Also replace the L macro for constants; likewise M_LIT and M_MLIT.
    repl_map[r'\bL *\((.*?)\)'] = r'\1Q'
    repl_map[r'\bM_LIT *\((.*?)\)'] = r'\1Q'
    repl_map[r'\bM_MLIT *\((.*?)\)'] = r'\1q'
    # M_DECL_FUNC and M_SUF need similar replacements.
    repl_map[r'\bM_DECL_FUNC *\((?:__)?(?:ieee754_)?(.*?)\)'] = r'\1q'
    repl_map[r'\bM_SUF *\((?:__)?(?:ieee754_)?(.*?)\)'] = r'\1q'
    # Further adjustments are then needed for certain internal
    # functions called via M_SUF.
    repl_map[r'\bx2y2m1q\b'] = '__quadmath_x2y2m1q'
    repl_map[r'\bkernel_casinhq\b'] = '__quadmath_kernel_casinhq'
    # Replace calls to __set_errno.
    repl_map[r'\b__set_errno *\((.*?)\)'] = r'errno = \1'
    # Eliminate glibc diagnostic macros.
    repl_map[r' *\bDIAG_PUSH_NEEDS_COMMENT;'] = ''
    repl_map[r' *\bDIAG_IGNORE_NEEDS_COMMENT *\(.*?\);'] = ''
    repl_map[r' *\bDIAG_POP_NEEDS_COMMENT;'] = ''
    # Different names used in union.
    repl_map[r'\.d\b'] = '.value'
    repl_map[r'\bunion ieee854_float128\b'] = 'ieee854_float128'
    # Calls to alias and hidden_def macros are all eliminated.
    for macro in ('strong_alias', 'weak_alias', 'libm_alias_ldouble',
                  'declare_mgen_alias', 'declare_mgen_finite_alias',
                  'libm_hidden_def', 'mathx_hidden_def'):
        repl_map[r'\b%s *\(.*?\);?' % macro] = ''
    # Replace all #includes with a single include of quadmath-imp.h.
    repl_map['(\n+#include[^\n]*)+\n+'] = '\n\n#include "quadmath-imp.h"\n\n'
    # Omitted from this list because code comes from more than one
    # glibc source file: rem_pio2.
    ldbl_files = {
        'e_acoshl.c': 'acoshq.c', 'e_acosl.c': 'acosq.c',
        's_asinhl.c': 'asinhq.c', 'e_asinl.c': 'asinq.c',
        'e_atan2l.c': 'atan2q.c', 'e_atanhl.c': 'atanhq.c',
        's_atanl.c': 'atanq.c', 's_cbrtl.c': 'cbrtq.c', 's_ceill.c': 'ceilq.c',
        's_copysignl.c': 'copysignq.c', 'e_coshl.c': 'coshq.c',
        's_cosl.c': 'cosq.c', 'k_cosl.c': 'cosq_kernel.c',
        's_erfl.c': 'erfq.c', 's_expm1l.c': 'expm1q.c', 'e_expl.c': 'expq.c',
        't_expl.h': 'expq_table.h', 's_fabsl.c': 'fabsq.c',
        's_finitel.c': 'finiteq.c', 's_floorl.c': 'floorq.c',
        's_fmal.c': 'fmaq.c', 'e_fmodl.c': 'fmodq.c', 's_frexpl.c': 'frexpq.c',
        'e_lgammal_r.c': 'lgammaq.c', 'lgamma_negl.c': 'lgammaq_neg.c',
        'lgamma_productl.c': 'lgammaq_product.c', 'e_hypotl.c': 'hypotq.c',
        'e_ilogbl.c': 'ilogbq.c', 's_isinfl.c': 'isinfq.c',
        's_isnanl.c': 'isnanq.c', 's_issignalingl.c': 'issignalingq.c',
        'e_j0l.c': 'j0q.c', 'e_j1l.c': 'j1q.c', 'e_jnl.c': 'jnq.c',
        's_llrintl.c': 'llrintq.c', 's_llroundl.c': 'llroundq.c',
        'e_log10l.c': 'log10q.c', 's_log1pl.c': 'log1pq.c',
        'e_log2l.c': 'log2q.c', 's_logbl.c': 'logbq.c', 'e_logl.c': 'logq.c',
        's_lrintl.c': 'lrintq.c', 's_lroundl.c': 'lroundq.c',
        's_modfl.c': 'modfq.c', 's_nearbyintl.c': 'nearbyintq.c',
        's_nextafterl.c': 'nextafterq.c', 'e_powl.c': 'powq.c',
        'e_remainderl.c': 'remainderq.c', 's_remquol.c': 'remquoq.c',
        's_rintl.c': 'rintq.c', 's_roundl.c': 'roundq.c',
        's_scalblnl.c': 'scalblnq.c', 's_scalbnl.c': 'scalbnq.c',
        's_signbitl.c': 'signbitq.c', 't_sincosl.c': 'sincos_table.c',
        's_sincosl.c': 'sincosq.c', 'k_sincosl.c': 'sincosq_kernel.c',
        'e_sinhl.c': 'sinhq.c', 's_sinl.c': 'sinq.c',
        'k_sinl.c': 'sinq_kernel.c', 's_tanhl.c': 'tanhq.c',
        's_tanl.c': 'tanq.c', 'k_tanl.c': 'tanq_kernel.c',
        'e_gammal_r.c': 'tgammaq.c', 'gamma_productl.c': 'tgammaq_product.c',
        's_truncl.c': 'truncq.c', 'x2y2m1l.c': 'x2y2m1q.c'
        }
    template_files = {
        's_cacosh_template.c': 'cacoshq.c', 's_cacos_template.c': 'cacosq.c',
        's_casinh_template.c': 'casinhq.c',
        'k_casinh_template.c': 'casinhq_kernel.c',
        's_casin_template.c': 'casinq.c', 's_catanh_template.c': 'catanhq.c',
        's_catan_template.c': 'catanq.c', 's_ccosh_template.c': 'ccoshq.c',
        's_cexp_template.c': 'cexpq.c', 'cimag_template.c': 'cimagq.c',
        's_clog10_template.c': 'clog10q.c', 's_clog_template.c': 'clogq.c',
        'conj_template.c': 'conjq.c', 's_cproj_template.c': 'cprojq.c',
        'creal_template.c': 'crealq.c', 's_csinh_template.c': 'csinhq.c',
        's_csin_template.c': 'csinq.c', 's_csqrt_template.c': 'csqrtq.c',
        's_ctanh_template.c': 'ctanhq.c', 's_ctan_template.c': 'ctanq.c',
        'e_exp2_template.c': 'exp2q.c', 's_fdim_template.c': 'fdimq.c',
        's_fmax_template.c': 'fmaxq.c', 's_fmin_template.c': 'fminq.c',
        's_ldexp_template.c': 'ldexpq.c'
        }
    # Some files have extra substitutions to apply.
    extra_maps = defaultdict(dict)
    extra_maps['expq.c'] = {r'#include "quadmath-imp\.h"\n':
                            '#include "quadmath-imp.h"\n'
                            '#include "expq_table.h"\n'}
    extra_maps['ilogbq.c'] = {r'#include "quadmath-imp\.h"\n':
                              '#include <math.h>\n'
                              '#include "quadmath-imp.h"\n'
                              '#ifndef FP_ILOGB0\n'
                              '# define FP_ILOGB0 INT_MIN\n'
                              '#endif\n'
                              '#ifndef FP_ILOGBNAN\n'
                              '# define FP_ILOGBNAN INT_MAX\n'
                              '#endif\n',
                              r'return ([A-Z0-9_]+);':
                              r'{ errno = EDOM; feraiseexcept (FE_INVALID); '
                              r'return \1; }'}
    extra_maps['lgammaq.c'] = {r'#include "quadmath-imp\.h"\n':
                               '#include "quadmath-imp.h"\n'
                               '#ifdef HAVE_MATH_H_SIGNGAM\n'
                               '# include <math.h>\n'
                               '#endif\n'
                               '__float128\n'
                               'lgammaq (__float128 x)\n'
                               '{\n'
                               '#ifndef HAVE_MATH_H_SIGNGAM\n'
                               '  int signgam;\n'
                               '#endif\n'
                               '  return __quadmath_lgammaq_r (x, &signgam);\n'
                               '}\n'}
    extra_maps['tgammaq.c'] = {r'#include "quadmath-imp\.h"\n':
                               '#include "quadmath-imp.h"\n'
                               '__float128\n'
                               'tgammaq (__float128 x)\n'
                               '{\n'
                               '  int sign;\n'
                               '  __float128 ret;\n'
                               '  ret = __quadmath_gammaq_r (x, &sign);\n'
                               '  return sign < 0 ? -ret : ret;\n'
                               '}\n'}
    for src, dest in ldbl_files.items():
        replace_in_file(repl_map, extra_maps[dest],
                        os.path.join(glibc_ldbl128, src),
                        os.path.join(quadmath_math, dest))
    for src, dest in template_files.items():
        replace_in_file(repl_map, extra_maps[dest],
                        os.path.join(glibc_math, src),
                        os.path.join(quadmath_math, dest))

def main():
    parser = argparse.ArgumentParser(description='Update libquadmath code.')
    parser.add_argument('glibc_srcdir', help='glibc source directory')
    parser.add_argument('quadmath_srcdir', help='libquadmath source directory')
    args = parser.parse_args()
    update_sources(args.glibc_srcdir, args.quadmath_srcdir)


if __name__ == '__main__':
    main()
