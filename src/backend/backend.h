// SPDX-License-Identifier: MPL-2.0
// Copyright (c) 2018, Yuxuan Shui <yshuiv7@gmail.com>

#pragma once
#include "common.h"
#include "region.h"

typedef struct backend_info {

  // ===========    Initialization    ===========

  void *(*init)(session_t *ps)
    __attribute__((nonnull(1)));
  void (*deinit)(void *backend_data, session_t *ps)
    __attribute__((nonnull(1, 2)));

  /// Called when rendering will be stopped for an unknown amount of
  /// time (e.g. screen is unredirected). Free some resources.
  void (*pause)(void *backend_data, session_t *ps);

  /// Called before rendering is resumed
  void (*resume)(void *backend_data, session_t *ps);

  /// Called when root property changed, returns the new
  /// backend_data. Even if the backend_data changed, all
  /// the existing win_data returned by prepare_win should
  /// remain valid.
  ///
  /// Optional
  void *(*root_change)(void *backend_data, session_t *ps);

  // ===========      Rendering      ============

  /// Called before any compose() calls.
  ///
  /// Usually the backend should clear the buffer, or paint a background
  /// on the buffer (usually the wallpaper).
  ///
  /// Optional?
  void (*prepare)(void *backend_data, session_t *ps,
    const region_t *reg_paint);

  /// Paint the content of the window onto the (possibly buffered)
  /// target picture. Always called after render_win(). Maybe called
  /// multiple times between render_win() and finish_render_win().
  /// The origin is the top left of the window, exclude the shadow,
  /// (dst_x, dst_y) refers to where the origin should be in the target
  /// buffer.
  void (*compose)(void *backend_data, session_t *ps,
    win *w, void *win_data,
    int dst_x, int dst_y,
    const region_t *reg_paint);

  /// Blur a given region on of the target.
  bool (*blur)(void *backend_data, session_t *ps, double opacity, const region_t *)
    __attribute__((nonnull(1, 2, 4)));

  /// Present the buffered target picture onto the screen. If target
  /// is not buffered, this should be NULL.
  ///
  /// Optional
  void (*present)(void *backend_data, session_t *ps)
    __attribute__((nonnull(1, 2)));

  /**
   * Render the content of a window into an opaque
   * data structure. Dimming, shadow and color inversion is handled
   * here.
   *
   * This function is allowed to allocate additional resource needed
   * for rendering.
   *
   * Params:
   *    reg_paint = the paint region, meaning painting should only
   *                be happening within that region. It's in global
   *                coordinates. If NULL, the region of paint is the
   *                whole screen.
   */
  void (*render_win)(void *backend_data, session_t *ps,
    win *w, void *win_data, const region_t *reg_paint);

  /// Free resource allocated for rendering. After this function is
  /// called, compose() won't be called before render_win is called
  /// another time.
  ///
  /// Optional
  void (*finish_render_win)(void *backend_data, session_t *ps,
    win *w, void *win_data);

  // ============ Resource management ===========


  // XXX Thoughts: calling release_win and prepare_win for every config notify
  //     is wasteful, since there can be multiple such notifies per drawing.
  //     But if we don't, it can mean there will be a state where is window is
  //     mapped and visible, but there is no win_data attached to it. We don't
  //     want to break that assumption.

  /// Create a structure to stored additional data needed for rendering a
  /// window, later used for render() and compose().
  ///
  /// Backend can assume this function will only be called with visible
  /// InputOutput windows, and only be called when screen is redirected.
  ///
  /// Backend can assume size, shape and visual of the window won't change between
  /// prepare_win() and release_win().
  void *(*prepare_win)(void *backend_data, session_t *ps, win *w)
    __attribute__((nonnull(1, 2, 3)));

  /// Free resources allocated by prepare()
  void (*release_win)(void *backend_data, session_t *ps, win *w, void *win_data)
    __attribute__((nonnull(1, 2, 3)));

  // ===========        Query         ===========

  /// Return if a window has transparent content. Guaranteed to only
  /// be called after render_win is called.
  bool (*is_win_transparent)(void *backend_data, win *w, void *win_data)
    __attribute__((nonnull(1, 2)));

  /// Return if the frame window has transparent content. Guaranteed to
  /// only be called after render_win is called.
  bool (*is_frame_transparent)(void *backend_data, win *w, void *win_data)
    __attribute__((nonnull(1, 2)));
} backend_info_t;

extern backend_info_t xrender_backend;
extern backend_info_t *backend_list[NUM_BKEND];

bool default_is_win_transparent(void *, win *, void *);
bool default_is_frame_transparent(void *, win *, void *);
