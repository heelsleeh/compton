// SPDX-License-Identifier: MIT
// Copyright (c)

// Throw everything in here.


// === Includes ===

#include <math.h>
#include <sys/select.h>
#include <limits.h>
#include <unistd.h>
#include <getopt.h>
#include <locale.h>
#include <signal.h>

#ifdef CONFIG_VSYNC_DRM
#include <fcntl.h>
// We references some definitions in drm.h, which could also be found in
// /usr/src/linux/include/drm/drm.h, but that path is probably even less
// reliable than libdrm
#include <drm.h>
#include <sys/ioctl.h>
#include <errno.h>
#endif

#include <X11/Xutil.h>
#include <pixman.h>
#ifdef CONFIG_OPENGL
#include "opengl.h" // XXX clean up
#endif
#include "common.h"
#include "backend/backend.h"
#include "win.h"
#include "x.h"
#include "c2.h"
#include "log.h" // XXX clean up
#include "render.h"

// == Functions ==
// TODO move static inline functions that are only used in compton.c, into
//      compton.c

// inline functions must be made static to compile correctly under clang:
// http://clang.llvm.org/compatibility.html#inline

void add_damage(session_t *ps, const region_t *damage);

long determine_evmask(session_t *ps, Window wid, win_evmode_t mode);

xcb_window_t
find_client_win(session_t *ps, xcb_window_t w);

win *find_toplevel2(session_t *ps, Window wid);

void map_win(session_t *ps, Window id);

/**
 * Subtract two unsigned long values.
 *
 * Truncate to 0 if the result is negative.
 */
static inline unsigned long __attribute__((const))
sub_unslong(unsigned long a, unsigned long b) {
  return (a > b) ? a - b : 0;
}

/**
 * Set a <code>switch_t</code> array of all unset wintypes to true.
 */
static inline void
wintype_arr_enable_unset(switch_t arr[]) {
  wintype_t i;

  for (i = 0; i < NUM_WINTYPES; ++i)
    if (UNSET == arr[i])
      arr[i] = ON;
}

/**
 * Check if a window ID exists in an array of window IDs.
 *
 * @param arr the array of window IDs
 * @param count amount of elements in the array
 * @param wid window ID to search for
 */
static inline bool
array_wid_exists(const Window *arr, int count, Window wid) {
  while (count--) {
    if (arr[count] == wid) {
      return true;
    }
  }

  return false;
}

/**
 * Destroy a condition list.
 */
static inline void
free_wincondlst(c2_lptr_t **pcondlst) {
  while ((*pcondlst = c2_free_lptr(*pcondlst)))
    continue;
}

#ifndef CONFIG_OPENGL
static inline void
free_paint_glx(session_t *ps, void *p) {}
static inline void
free_win_res_glx(session_t *ps, win *w) {}
#endif

/**
 * Create a XTextProperty of a single string.
 */
static inline XTextProperty *
make_text_prop(session_t *ps, char *str) {
  XTextProperty *pprop = ccalloc(1, XTextProperty);

  if (XmbTextListToTextProperty(ps->dpy, &str, 1,  XStringStyle, pprop)) {
    cxfree(pprop->value);
    free(pprop);
    pprop = NULL;
  }

  return pprop;
}


/**
 * Set a single-string text property on a window.
 */
static inline bool
wid_set_text_prop(session_t *ps, Window wid, Atom prop_atom, char *str) {
  XTextProperty *pprop = make_text_prop(ps, str);
  if (!pprop) {
    printf_errf("(\"%s\"): Failed to make text property.", str);
    return false;
  }

  XSetTextProperty(ps->dpy, wid, pprop, prop_atom);
  cxfree(pprop->value);
  cxfree(pprop);

  return true;
}

/**
 * Dump an drawable's info.
 */
static inline void
dump_drawable(session_t *ps, Drawable drawable) {
  Window rroot = None;
  int x = 0, y = 0;
  unsigned width = 0, height = 0, border = 0, depth = 0;
  if (XGetGeometry(ps->dpy, drawable, &rroot, &x, &y, &width, &height,
        &border, &depth)) {
    printf_dbgf("(%#010lx): x = %u, y = %u, wid = %u, hei = %d, b = %u, d = %u\n", drawable, x, y, width, height, border, depth);
  }
  else {
    printf_dbgf("(%#010lx): Failed\n", drawable);
  }
}

// vim: set et sw=2 :
