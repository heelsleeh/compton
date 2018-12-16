// SPDX-License-Identifier: MIT
// Copyright (c) 2011-2013, Christopher Jeffrey
// Copyright (c) 2013 Richard Grenville <pyxlcy@gmail.com>

#include <stdlib.h>
#include <stdbool.h>

#include "compiler.h"
#include "common.h"
#include "utils.h"
#include "c2.h"
#include "string_utils.h"
#include "log.h"

#include "config.h"

/**
 * Parse a long number.
 */
bool
parse_long(const char *s, long *dest) {
  const char *endptr = NULL;
  long val = strtol(s, (char **) &endptr, 0);
  if (!endptr || endptr == s) {
    printf_errf("(\"%s\"): Invalid number.", s);
    return false;
  }
  while (isspace(*endptr))
    ++endptr;
  if (*endptr) {
    printf_errf("(\"%s\"): Trailing characters.", s);
    return false;
  }
  *dest = val;
  return true;
}

/**
 * Parse a floating-point number in matrix.
 */
const char *
parse_matrix_readnum(const char *src, double *dest) {
  char *pc = NULL;
  double val = strtod(src, &pc);
  if (!pc || pc == src) {
    printf_errf("(\"%s\"): No number found.", src);
    return src;
  }

  while (*pc && (isspace(*pc) || ',' == *pc))
    ++pc;

  *dest = val;

  return pc;
}

/**
 * Parse a matrix.
 */
xcb_render_fixed_t *
parse_matrix(session_t *ps, const char *src, const char **endptr) {
  int wid = 0, hei = 0;
  const char *pc = NULL;

  // Get matrix width and height
  {
    double val = 0.0;
    if (src == (pc = parse_matrix_readnum(src, &val)))
      goto err1;
    src = pc;
    wid = val;
    if (src == (pc = parse_matrix_readnum(src, &val)))
      goto err1;
    src = pc;
    hei = val;
  }

  // Validate matrix width and height
  if (wid <= 0 || hei <= 0) {
    printf_errf("(): Invalid matrix width/height.");
    goto err1;
  }
  if (!(wid % 2 && hei % 2)) {
    printf_errf("(): Width/height not odd.");
    goto err1;
  }
  if (wid > 16 || hei > 16)
    printf_errf("(): Matrix width/height too large, may slow down"
                "rendering, and/or consume lots of memory");

  // Allocate memory
  auto matrix = ccalloc(wid * hei + 2, xcb_render_fixed_t);
  if (!matrix) {
    printf_errf("(): Failed to allocate memory for matrix.");
    goto err1;
  }

  // Read elements
  {
    int skip = hei / 2 * wid + wid / 2;
    bool hasneg = false;
    for (int i = 0; i < wid * hei; ++i) {
      // Ignore the center element
      if (i == skip) {
        matrix[2 + i] = DOUBLE_TO_XFIXED(0);
        continue;
      }
      double val = 0;
      if (src == (pc = parse_matrix_readnum(src, &val)))
        goto err2;
      src = pc;
      if (val < 0) hasneg = true;
      matrix[2 + i] = DOUBLE_TO_XFIXED(val);
    }
    if (BKEND_XRENDER == ps->o.backend && hasneg)
      printf_errf("(): A convolution kernel with negative values "
          "may not work properly under X Render backend.");
  }

  // Detect trailing characters
  for ( ;*pc && ';' != *pc; ++pc)
    if (!isspace(*pc) && ',' != *pc) {
      printf_errf("(): Trailing characters in matrix string.");
      goto err2;
    }

  // Jump over spaces after ';'
  if (';' == *pc) {
    ++pc;
    while (*pc && isspace(*pc))
      ++pc;
  }

  // Require an end of string if endptr is not provided, otherwise
  // copy end pointer to endptr
  if (endptr)
    *endptr = pc;
  else if (*pc) {
    printf_errf("(): Only one matrix expected.");
    goto err2;
  }

  // Fill in width and height
  matrix[0] = DOUBLE_TO_XFIXED(wid);
  matrix[1] = DOUBLE_TO_XFIXED(hei);

  return matrix;

err2:
  free(matrix);
err1:
  return NULL;
}

/**
 * Normalize a convolution kernel.
 */
static inline void
normalize_conv_kern(int wid, int hei, xcb_render_fixed_t *kern) {
  double sum = 0.0;
  for (int i = 0; i < wid * hei; ++i)
    sum += XFIXED_TO_DOUBLE(kern[i]);
  double factor = 1.0 / sum;
  for (int i = 0; i < wid * hei; ++i)
    kern[i] = DOUBLE_TO_XFIXED(XFIXED_TO_DOUBLE(kern[i]) * factor);
}

/**
 * Parse a convolution kernel.
 */
xcb_render_fixed_t *
parse_conv_kern(session_t *ps, const char *src, const char **endptr) {
  xcb_render_fixed_t *res = parse_matrix(ps, src, endptr);

  if (!res)
    return res;

  int w = XFIXED_TO_DOUBLE(res[0]), h = XFIXED_TO_DOUBLE(res[1]);
  res[h/2*w+w/2+2] = DOUBLE_TO_XFIXED(1.0);
  normalize_conv_kern(w, h, res+2);
  return res;
}

/**
 * Parse a list of convolution kernels.
 */
bool
parse_conv_kern_lst(session_t *ps, const char *src, xcb_render_fixed_t **dest, int max) {
  static const struct {
    const char *name;
    const char *kern_str;
  } CONV_KERN_PREDEF[] = {
    { "3x3box", "3,3,1,1,1,1,1,1,1,1," },
    { "5x5box", "5,5,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1," },
    { "7x7box", "7,7,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1," },
    { "3x3gaussian", "3,3,0.243117,0.493069,0.243117,0.493069,0.493069,0.243117,0.493069,0.243117," },
    { "5x5gaussian", "5,5,0.003493,0.029143,0.059106,0.029143,0.003493,0.029143,0.243117,0.493069,0.243117,0.029143,0.059106,0.493069,0.493069,0.059106,0.029143,0.243117,0.493069,0.243117,0.029143,0.003493,0.029143,0.059106,0.029143,0.003493," },
    { "7x7gaussian", "7,7,0.000003,0.000102,0.000849,0.001723,0.000849,0.000102,0.000003,0.000102,0.003493,0.029143,0.059106,0.029143,0.003493,0.000102,0.000849,0.029143,0.243117,0.493069,0.243117,0.029143,0.000849,0.001723,0.059106,0.493069,0.493069,0.059106,0.001723,0.000849,0.029143,0.243117,0.493069,0.243117,0.029143,0.000849,0.000102,0.003493,0.029143,0.059106,0.029143,0.003493,0.000102,0.000003,0.000102,0.000849,0.001723,0.000849,0.000102,0.000003," },
    { "9x9gaussian", "9,9,0.000000,0.000000,0.000001,0.000006,0.000012,0.000006,0.000001,0.000000,0.000000,0.000000,0.000003,0.000102,0.000849,0.001723,0.000849,0.000102,0.000003,0.000000,0.000001,0.000102,0.003493,0.029143,0.059106,0.029143,0.003493,0.000102,0.000001,0.000006,0.000849,0.029143,0.243117,0.493069,0.243117,0.029143,0.000849,0.000006,0.000012,0.001723,0.059106,0.493069,0.493069,0.059106,0.001723,0.000012,0.000006,0.000849,0.029143,0.243117,0.493069,0.243117,0.029143,0.000849,0.000006,0.000001,0.000102,0.003493,0.029143,0.059106,0.029143,0.003493,0.000102,0.000001,0.000000,0.000003,0.000102,0.000849,0.001723,0.000849,0.000102,0.000003,0.000000,0.000000,0.000000,0.000001,0.000006,0.000012,0.000006,0.000001,0.000000,0.000000," },
    { "11x11gaussian", "11,11,0.000000,0.000000,0.000000,0.000000,0.000000,0.000000,0.000000,0.000000,0.000000,0.000000,0.000000,0.000000,0.000000,0.000000,0.000001,0.000006,0.000012,0.000006,0.000001,0.000000,0.000000,0.000000,0.000000,0.000000,0.000003,0.000102,0.000849,0.001723,0.000849,0.000102,0.000003,0.000000,0.000000,0.000000,0.000001,0.000102,0.003493,0.029143,0.059106,0.029143,0.003493,0.000102,0.000001,0.000000,0.000000,0.000006,0.000849,0.029143,0.243117,0.493069,0.243117,0.029143,0.000849,0.000006,0.000000,0.000000,0.000012,0.001723,0.059106,0.493069,0.493069,0.059106,0.001723,0.000012,0.000000,0.000000,0.000006,0.000849,0.029143,0.243117,0.493069,0.243117,0.029143,0.000849,0.000006,0.000000,0.000000,0.000001,0.000102,0.003493,0.029143,0.059106,0.029143,0.003493,0.000102,0.000001,0.000000,0.000000,0.000000,0.000003,0.000102,0.000849,0.001723,0.000849,0.000102,0.000003,0.000000,0.000000,0.000000,0.000000,0.000000,0.000001,0.000006,0.000012,0.000006,0.000001,0.000000,0.000000,0.000000,0.000000,0.000000,0.000000,0.000000,0.000000,0.000000,0.000000,0.000000,0.000000,0.000000,0.000000," },
  };
  for (unsigned int i = 0;
      i < sizeof(CONV_KERN_PREDEF) / sizeof(CONV_KERN_PREDEF[0]); ++i)
    if (!strcmp(CONV_KERN_PREDEF[i].name, src))
      return parse_conv_kern_lst(ps, CONV_KERN_PREDEF[i].kern_str, dest, max);

  int i = 0;
  const char *pc = src;

  // Free old kernels
  for (i = 0; i < max; ++i) {
    free(dest[i]);
    dest[i] = NULL;
  }

  // Continue parsing until the end of source string
  i = 0;
  while (pc && *pc && i < max - 1) {
    if (!(dest[i++] = parse_conv_kern(ps, pc, &pc)))
      return false;
  }

  if (i > 1) {
    printf_errf("(): You are seeing this message because your are using multipass\n"
        "blur. Please report an issue to us so we know multipass blur is actually been used.\n"
        "Otherwise it might be removed in future releases");
  }

  if (*pc) {
    printf_errf("(): Too many blur kernels!");
    return false;
  }

  return true;
}

/**
 * Parse a X geometry.
 *
 * ps->root_width and ps->root_height must be valid
 */
bool
parse_geometry(session_t *ps, const char *src, region_t *dest) {
  pixman_region32_clear(dest);
  if (!src)
    return true;
  if (!ps->root_width || !ps->root_height)
    return true;

  geometry_t geom = { .wid = ps->root_width, .hei = ps->root_height, .x = 0, .y = 0 };
  long val = 0L;
  char *endptr = NULL;

  src = skip_space(src);
  if (!*src)
    goto parse_geometry_end;

  // Parse width
  // Must be base 10, because "0x0..." may appear
  if (!('+' == *src || '-' == *src)) {
    val = strtol(src, &endptr, 10);
    assert(endptr);
    if (src != endptr) {
      geom.wid = val;
      if (geom.wid < 0) {
        printf_errf("(\"%s\"): Invalid width.", src);
        return false;
      }
      src = endptr;
    }
    src = skip_space(src);
  }

  // Parse height
  if ('x' == *src) {
    ++src;
    val = strtol(src, &endptr, 10);
    assert(endptr);
    if (src != endptr) {
      geom.hei = val;
      if (geom.hei < 0) {
        printf_errf("(\"%s\"): Invalid height.", src);
        return false;
      }
      src = endptr;
    }
    src = skip_space(src);
  }

  // Parse x
  if ('+' == *src || '-' == *src) {
    val = strtol(src, &endptr, 10);
    if (endptr && src != endptr) {
      geom.x = val;
      if (*src == '-')
        geom.x += ps->root_width - geom.wid;
      src = endptr;
    }
    src = skip_space(src);
  }

  // Parse y
  if ('+' == *src || '-' == *src) {
    val = strtol(src, &endptr, 10);
    if (endptr && src != endptr) {
      geom.y = val;
      if (*src == '-')
        geom.y += ps->root_height - geom.hei;
      src = endptr;
    }
    src = skip_space(src);
  }

  if (*src) {
    printf_errf("(\"%s\"): Trailing characters.", src);
    return false;
  }

parse_geometry_end:
  pixman_region32_union_rect(dest, dest, geom.x, geom.y, geom.wid, geom.hei);
  return true;
}

/**
 * Parse a list of opacity rules.
 */
bool parse_rule_opacity(session_t *ps, const char *src) {
  // Find opacity value
  char *endptr = NULL;
  long val = strtol(src, &endptr, 0);
  if (!endptr || endptr == src) {
    printf_errf("(\"%s\"): No opacity specified?", src);
    return false;
  }
  if (val > 100 || val < 0) {
    printf_errf("(\"%s\"): Opacity %ld invalid.", src, val);
    return false;
  }

  // Skip over spaces
  while (*endptr && isspace(*endptr))
    ++endptr;
  if (':' != *endptr) {
    printf_errf("(\"%s\"): Opacity terminator not found.", src);
    return false;
  }
  ++endptr;

  // Parse pattern
  // I hope 1-100 is acceptable for (void *)
  return c2_parse(ps, &ps->o.opacity_rules, endptr, (void *) val);
}

/**
 * Add a pattern to a condition linked list.
 */
bool
condlst_add(session_t *ps, c2_lptr_t **pcondlst, const char *pattern) {
  if (!pattern)
    return false;

  if (!c2_parse(ps, pcondlst, pattern, NULL))
    exit(1);

  return true;
}

void parse_config(session_t *ps, bool *shadow_enable, bool *fading_enable,
    win_option_mask_t *winopt_mask) {
#ifdef CONFIG_LIBCONFIG
  parse_config_libconfig(ps, shadow_enable, fading_enable, winopt_mask);
#endif

  // Apply default wintype options that does not depends on global options.
  // For example, wintype shadow option will depend on the global shadow
  // option, so it is not set here.
  //
  // Except desktop windows are always drawn without shadow.
  if (!winopt_mask[WINTYPE_DESKTOP].shadow) {
    winopt_mask[WINTYPE_DESKTOP].shadow = true;
    ps->o.wintype_option[WINTYPE_DESKTOP].shadow = false;
  }

  // Focused/unfocused state only apply to a few window types, all other windows
  // are always considered focused.
  const wintype_t nofocus_type[] =
    { WINTYPE_UNKNOWN, WINTYPE_NORMAL, WINTYPE_UTILITY };
  for (unsigned long i = 0; i < ARR_SIZE(nofocus_type); i++) {
    if (!winopt_mask[nofocus_type[i]].focus) {
      winopt_mask[nofocus_type[i]].focus = true;
      ps->o.wintype_option[nofocus_type[i]].focus = false;
    }
  }
  for (unsigned long i = 0; i < NUM_WINTYPES; i++) {
    if (!winopt_mask[i].focus) {
      winopt_mask[i].focus = true;
      ps->o.wintype_option[i].focus = true;
    }
    if (!winopt_mask[i].full_shadow) {
      winopt_mask[i].full_shadow = true;
      ps->o.wintype_option[i].full_shadow = false;
    }
    if (!winopt_mask[i].redir_ignore) {
      winopt_mask[i].redir_ignore = true;
      ps->o.wintype_option[i].redir_ignore = false;
    }
    if (!winopt_mask[i].opacity) {
      winopt_mask[i].opacity = true;
      // Opacity is not set to a concrete number here because the opacity logic
      // is complicated, and needs an "unset" state
      ps->o.wintype_option[i].opacity = NAN;
    }
  }
}
