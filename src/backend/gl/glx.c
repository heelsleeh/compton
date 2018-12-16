// SPDX-License-Identifier: MIT
/*
 * Compton - a compositor for X11
 *
 * Based on `xcompmgr` - Copyright (c) 2003, Keith Packard
 *
 * Copyright (c) 2011-2013, Christopher Jeffrey
 * See LICENSE-mit for more information.
 *
 */

#include <GL/glx.h>
#include "backend/backend.h"
#include "backend/gl/gl_common.h"

/// @brief Wrapper of a GLX FBConfig.
typedef struct {
  GLXFBConfig cfg;
  GLint texture_fmt;
  GLint texture_tgts;
  bool y_inverted;
} glx_fbconfig_t;

struct _glx_win_data {
  gl_texture_t texture;
  GLXPixmap glpixmap;
  xcb_pixmap_t pixmap;
};

struct _glx_data {
  int glx_event;
  int glx_error;
  GLXContext ctx;
  gl_cap_t cap;

  void (*glXBindTexImage)(Display *display, GLXDrawable drawable, int buffer,
    const int *attrib_list);
  void (*glXReleaseTexImage)(Display *display, GLXDrawable drawable, int buffer);

};

/**
 * Check if a GLX extension exists.
 */
static inline bool
glx_has_extension(session_t *ps, const char *ext) {
  const char *glx_exts = glXQueryExtensionsString(ps->dpy, ps->scr);
  if (!glx_exts) {
    printf_errf("(): Failed get GLX extension list.");
    return false;
  }

  int len = strlen(ext);
  char *found = strstr(glx_exts, ext);
  if (!found)
    printf_errf("(): Missing GLX extension %s.", ext);

  // Make sure extension names are not crazy...
  assert(found[len] == ' ' || found[len] == 0);
  return found != NULL;
}

/**
 * @brief Release binding of a texture.
 */
void
glx_release_pixmap(struct _glx_data *gd, Display *dpy, struct _glx_win_data *wd) {
  // Release binding
  if (wd->glpixmap && wd->texture.texture) {
    glBindTexture(wd->texture.target, wd->texture.texture);
    gd->glXReleaseTexImage(dpy, wd->glpixmap, GLX_FRONT_LEFT_EXT);
    glBindTexture(wd->texture.target, 0);
  }

  // Free GLX Pixmap
  if (wd->glpixmap) {
    glXDestroyPixmap(dpy, wd->glpixmap);
    wd->glpixmap = 0;
  }

  gl_check_err();
}

/**
 * Free a glx_texture_t.
 */
static void
glx_release_win(struct _glx_data *gd, Display *dpy, gl_texture_t *ptex) {
  glx_release_pixmap(gd, dpy, ptex);
  glDeleteTextures(1, &ptex->texture);

  // Free structure itself
  free(ptex);
}

/**
 * Free GLX part of win.
 */
static inline void
free_win_res_glx(session_t *ps, win *w) {
  /*free_paint_glx(ps, &w->paint);*/
  /*free_paint_glx(ps, &w->shadow_paint);*/
  /*free_glx_bc(ps, &w->glx_blur_cache);*/
}
>>>>>>> 4bc5ef8... wip glx backend:src/backend/gl/glx.c

static inline int
glx_cmp_fbconfig_cmpattr(session_t *ps,
    const glx_fbconfig_t *pfbc_a, const glx_fbconfig_t *pfbc_b,
    int attr) {
  int attr_a = 0, attr_b = 0;

  // TODO: Error checking
  glXGetFBConfigAttrib(ps->dpy, pfbc_a->cfg, attr, &attr_a);
  glXGetFBConfigAttrib(ps->dpy, pfbc_b->cfg, attr, &attr_b);

  return attr_a - attr_b;
}

/**
 * Compare two GLX FBConfig's to find the preferred one.
 */
static int
glx_cmp_fbconfig(session_t *ps,
    const glx_fbconfig_t *pfbc_a, const glx_fbconfig_t *pfbc_b) {
  int result = 0;

  if (!pfbc_a)
    return -1;
  if (!pfbc_b)
    return 1;
  int tmpattr;

  // Avoid 10-bit colors
  glXGetFBConfigAttrib(ps->dpy, pfbc_a->cfg, GLX_RED_SIZE, &tmpattr);
  if (tmpattr != 8)
    return -1;

  glXGetFBConfigAttrib(ps->dpy, pfbc_b->cfg, GLX_RED_SIZE, &tmpattr);
  if (tmpattr != 8)
    return 1;

#define P_CMPATTR_LT(attr) { if ((result = glx_cmp_fbconfig_cmpattr(ps, pfbc_a, pfbc_b, (attr)))) return -result; }
#define P_CMPATTR_GT(attr) { if ((result = glx_cmp_fbconfig_cmpattr(ps, pfbc_a, pfbc_b, (attr)))) return result; }

  P_CMPATTR_LT(GLX_BIND_TO_TEXTURE_RGBA_EXT);
  P_CMPATTR_LT(GLX_DOUBLEBUFFER);
  P_CMPATTR_LT(GLX_STENCIL_SIZE);
  P_CMPATTR_LT(GLX_DEPTH_SIZE);
  P_CMPATTR_GT(GLX_BIND_TO_MIPMAP_TEXTURE_EXT);

  return 0;
}

/**
 * @brief Update the FBConfig of given depth.
 */
static inline void
glx_update_fbconfig_bydepth(session_t *ps, int depth, glx_fbconfig_t *pfbcfg) {
  // Make sure the depth is sane
  if (depth < 0 || depth > OPENGL_MAX_DEPTH)
    return;

  // Compare new FBConfig with current one
  if (glx_cmp_fbconfig(ps, ps->psglx->fbconfigs[depth], pfbcfg) < 0) {
#ifdef DEBUG_GLX
    printf_dbgf("(%d): %#x overrides %#x, target %#x.\n", depth, (unsigned) pfbcfg->cfg, (ps->psglx->fbconfigs[depth] ? (unsigned) ps->psglx->fbconfigs[depth]->cfg: 0), pfbcfg->texture_tgts);
#endif
    if (!ps->psglx->fbconfigs[depth]) {
      ps->psglx->fbconfigs[depth] = cmalloc(glx_fbconfig_t);
    }
    (*ps->psglx->fbconfigs[depth]) = *pfbcfg;
  }
}

/**
 * Get GLX FBConfigs for all depths.
 */
static bool
glx_update_fbconfig(session_t *ps) {
  // Acquire all FBConfigs and loop through them
  int nele = 0;
  GLXFBConfig* pfbcfgs = glXGetFBConfigs(ps->dpy, ps->scr, &nele);

  for (GLXFBConfig *pcur = pfbcfgs; pcur < pfbcfgs + nele; pcur++) {
    glx_fbconfig_t fbinfo = {
      .cfg = *pcur,
      .texture_fmt = 0,
      .texture_tgts = 0,
      .y_inverted = false,
    };
    int id = (int) (pcur - pfbcfgs);
    int depth = 0, depth_alpha = 0, val = 0;

    // Skip over multi-sampled visuals
    // http://people.freedesktop.org/~glisse/0001-glx-do-not-use-multisample-visual-config-for-front-o.patch
#ifdef GLX_SAMPLES
    if (Success == glXGetFBConfigAttrib(ps->dpy, *pcur, GLX_SAMPLES, &val)
        && val > 1)
      continue;
#endif

    if (Success != glXGetFBConfigAttrib(ps->dpy, *pcur, GLX_BUFFER_SIZE, &depth)
        || Success != glXGetFBConfigAttrib(ps->dpy, *pcur, GLX_ALPHA_SIZE, &depth_alpha)) {
      printf_errf("(): Failed to retrieve buffer size and alpha size of FBConfig %d.", id);
      continue;
    }
    if (Success != glXGetFBConfigAttrib(ps->dpy, *pcur, GLX_BIND_TO_TEXTURE_TARGETS_EXT, &fbinfo.texture_tgts)) {
      printf_errf("(): Failed to retrieve BIND_TO_TEXTURE_TARGETS_EXT of FBConfig %d.", id);
      continue;
    }

    int visualdepth = 0;
    {
      XVisualInfo *pvi = glXGetVisualFromFBConfig(ps->dpy, *pcur);
      if (!pvi) {
        // On nvidia-drivers-325.08 this happens slightly too often...
        // printf_errf("(): Failed to retrieve X Visual of FBConfig %d.", id);
        continue;
      }
      visualdepth = pvi->depth;
      cxfree(pvi);
    }

    bool rgb = false;
    bool rgba = false;

    if (depth >= 32 && depth_alpha && Success == glXGetFBConfigAttrib(ps->dpy, *pcur, GLX_BIND_TO_TEXTURE_RGBA_EXT, &val) && val)
      rgba = true;

    if (Success == glXGetFBConfigAttrib(ps->dpy, *pcur, GLX_BIND_TO_TEXTURE_RGB_EXT, &val) && val)
      rgb = true;

    if (Success == glXGetFBConfigAttrib(ps->dpy, *pcur, GLX_Y_INVERTED_EXT, &val))
      fbinfo.y_inverted = val;

    {
      int tgtdpt = depth - depth_alpha;
      if (tgtdpt == visualdepth && tgtdpt < 32 && rgb) {
        fbinfo.texture_fmt = GLX_TEXTURE_FORMAT_RGB_EXT;
        glx_update_fbconfig_bydepth(ps, tgtdpt, &fbinfo);
      }
    }

    if (depth == visualdepth && rgba) {
      fbinfo.texture_fmt = GLX_TEXTURE_FORMAT_RGBA_EXT;
      glx_update_fbconfig_bydepth(ps, depth, &fbinfo);
    }
  }

  cxfree(pfbcfgs);

  // Sanity checks
  if (!ps->psglx->fbconfigs[ps->depth]) {
    printf_errf("(): No FBConfig found for default depth %d.", ps->depth);
    return false;
  }

  if (!ps->psglx->fbconfigs[32]) {
    printf_errf("(): No FBConfig found for depth 32. Expect crazy things.");
  }

#ifdef DEBUG_GLX
  printf_dbgf("(): %d-bit: %#3x, 32-bit: %#3x\n",
      ps->depth, (int) ps->psglx->fbconfigs[ps->depth]->cfg,
      (int) ps->psglx->fbconfigs[32]->cfg);
#endif

  return true;
}

static inline XVisualInfo *
get_visualinfo_from_visual(session_t *ps, xcb_visualid_t visual) {
}

#ifdef DEBUG_GLX_DEBUG_CONTEXT
static inline GLXFBConfig
get_fbconfig_from_visualinfo(session_t *ps, const XVisualInfo *visualinfo) {
  int nelements = 0;
  GLXFBConfig *fbconfigs = glXGetFBConfigs(ps->dpy, visualinfo->screen,
      &nelements);
  for (int i = 0; i < nelements; ++i) {
    int visual_id = 0;
    if (Success == glXGetFBConfigAttrib(ps->dpy, fbconfigs[i], GLX_VISUAL_ID, &visual_id)
        && visual_id == visualinfo->visualid)
      return fbconfigs[i];
  }

  return NULL;
}

static void
glx_debug_msg_callback(GLenum source, GLenum type,
    GLuint id, GLenum severity, GLsizei length, const GLchar *message,
    GLvoid *userParam) {
  printf_dbgf("(): source 0x%04X, type 0x%04X, id %u, severity 0x%0X, \"%s\"\n",
      source, type, id, severity, message);
}
#endif

/**
 * Initialize OpenGL.
 */
void * glx_init(session_t *ps) {
  bool success = false;
  auto gd = ccalloc(1, struct _glx_data);
  XVisualInfo *pvis = NULL;

  // Check for GLX extension
  if (!glXQueryExtension(ps->dpy, &gd->glx_event, &gd->glx_error)) {
    printf_errf("(): No GLX extension.");
    goto end;
  }

  // Get XVisualInfo
  int nitems = 0;
  XVisualInfo vreq = { .visualid = ps->vis };
  pvis = XGetVisualInfo(ps->dpy, VisualIDMask, &vreq, &nitems);
  if (!pvis) {
    printf_errf("(): Failed to acquire XVisualInfo for current visual.");
    goto end;
  }

  // Ensure the visual is double-buffered
  int value = 0;
  if (glXGetConfig(ps->dpy, pvis, GLX_USE_GL, &value) || !value) {
    printf_errf("(): Root visual is not a GL visual.");
    goto end;
  }

  if (glXGetConfig(ps->dpy, pvis, GLX_DOUBLEBUFFER, &value) || !value) {
    printf_errf("(): Root visual is not a double buffered GL visual.");
    goto end;
  }

  // Ensure GLX_EXT_texture_from_pixmap exists
  if (!glx_hasglxext(ps, "GLX_EXT_texture_from_pixmap"))
    goto glx_init_end;

  // Initialize GLX data structure
  if (!ps->psglx) {
    static const glx_session_t CGLX_SESSION_DEF = CGLX_SESSION_INIT;
    ps->psglx = cmalloc(glx_session_t);
    memcpy(ps->psglx, &CGLX_SESSION_DEF, sizeof(glx_session_t));

    for (int i = 0; i < MAX_BLUR_PASS; ++i) {
      glx_blur_pass_t *ppass = &ps->psglx->blur_passes[i];
      ppass->unifm_factor_center = -1;
      ppass->unifm_offset_x = -1;
      ppass->unifm_offset_y = -1;
    }
  }

// Get GLX context
gd->context = glXCreateContext(ps->dpy, pvis, None, GL_TRUE);

if (!gd->context) {
  printf_errf("(): Failed to get GLX context.");
  goto end;
}

// Attach GLX context
if (!glXMakeCurrent(ps->dpy, get_tgt_window(ps), psglx->context)) {
  printf_errf("(): Failed to attach GLX context.");
  goto glx_init_end;
}

#ifdef DEBUG_GLX_DEBUG_CONTEXT
    {
      f_DebugMessageCallback p_DebugMessageCallback =
        (f_DebugMessageCallback)
        glXGetProcAddress((const GLubyte *) "glDebugMessageCallback");
      if (!p_DebugMessageCallback) {
        printf_errf("(): Failed to get glDebugMessageCallback(0.");
        goto glx_init_end;
      }
      p_DebugMessageCallback(glx_debug_msg_callback, ps);
    }
#endif

  }

  // Ensure we have a stencil buffer. X Fixes does not guarantee rectangles
  // in regions don't overlap, so we must use stencil buffer to make sure
  // we don't paint a region for more than one time, I think?
  if (need_render && !ps->o.glx_no_stencil) {
    GLint val = 0;
    glGetIntegerv(GL_STENCIL_BITS, &val);
    if (!val) {
      printf_errf("(): Target window doesn't have stencil buffer.");
      goto glx_init_end;
    }
  }

  // Check GL_ARB_texture_non_power_of_two, requires a GLX context and
  // must precede FBConfig fetching
  gd->has_texture_non_power_of_two = gl_has_extension(ps,
    "GL_ARB_texture_non_power_of_two");

  // Acquire function addresses
#if 0
  psglx->glStringMarkerGREMEDY = (f_StringMarkerGREMEDY)
    glXGetProcAddress((const GLubyte *) "glStringMarkerGREMEDY");
  psglx->glFrameTerminatorGREMEDY = (f_FrameTerminatorGREMEDY)
    glXGetProcAddress((const GLubyte *) "glFrameTerminatorGREMEDY");
#endif

  psglx->glXBindTexImageProc = (f_BindTexImageEXT)
    glXGetProcAddress((const GLubyte *) "glXBindTexImageEXT");
  psglx->glXReleaseTexImageProc = (f_ReleaseTexImageEXT)
    glXGetProcAddress((const GLubyte *) "glXReleaseTexImageEXT");
  if (!psglx->glXBindTexImageProc || !psglx->glXReleaseTexImageProc) {
    printf_errf("(): Failed to acquire glXBindTexImageEXT() / glXReleaseTexImageEXT().");
    goto glx_init_end;
  }

  // Acquire FBConfigs
  if (!glx_update_fbconfig(ps))
    goto end;

  // Render preparations
  glx_on_root_change(ps);

  glDisable(GL_DEPTH_TEST);
  glDepthMask(GL_FALSE);
  glTexEnvi(GL_TEXTURE_ENV, GL_TEXTURE_ENV_MODE, GL_REPLACE);
  glDisable(GL_BLEND);

  if (!ps->o.glx_no_stencil) {
    // Initialize stencil buffer
    glClear(GL_STENCIL_BUFFER_BIT);
    glDisable(GL_STENCIL_TEST);
    glStencilMask(0x1);
    glStencilFunc(GL_EQUAL, 0x1, 0x1);
  }

  // Clear screen
  glClearColor(0.0f, 0.0f, 0.0f, 1.0f);
  // glClear(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT);
  // glXSwapBuffers(ps->dpy, get_tgt_window(ps));

  success = true;

end:
  cxfree(pvis);

  if (!success) {
    glx_destroy(gd);
    return NULL;
  }

  return gd;
}

/**
 * Destroy GLX related resources.
 */
void
glx_destroy(session_t *ps) {
  if (!ps->psglx)
    return;

  // Free all GLX resources of windows
  for (win *w = ps->list; w; w = w->next)
    free_win_res_glx(ps, w);

  // Free GLSL shaders/programs
  for (int i = 0; i < MAX_BLUR_PASS; ++i) {
    glx_blur_pass_t *ppass = &ps->psglx->blur_passes[i];
    if (ppass->frag_shader)
      glDeleteShader(ppass->frag_shader);
    if (ppass->prog)
      glDeleteProgram(ppass->prog);
  }

  glx_free_prog_main(ps, &ps->o.glx_prog_win);

  glx_check_err(ps);

  // Free FBConfigs
  for (int i = 0; i <= OPENGL_MAX_DEPTH; ++i) {
    free(ps->psglx->fbconfigs[i]);
    ps->psglx->fbconfigs[i] = NULL;
  }

  // Destroy GLX context
  if (ps->psglx->context) {
    glXDestroyContext(ps->dpy, ps->psglx->context);
    ps->psglx->context = NULL;
  }

  free(ps->psglx);
  ps->psglx = NULL;
}

/**
 * Reinitialize GLX.
 */
bool
glx_reinit(session_t *ps, bool need_render) {
  // Reinitialize VSync as well
  vsync_deinit(ps);

  glx_destroy(ps);
  if (!glx_init(ps, need_render)) {
    printf_errf("(): Failed to initialize GLX.");
    return false;
  }

  if (!vsync_init(ps)) {
    printf_errf("(): Failed to initialize VSync.");
    return false;
  }

  return true;
}

/**
 * Initialize GLX blur filter.
 */
bool
glx_init_blur(session_t *ps) {
  assert(ps->o.blur_kerns[0]);

  // Allocate PBO if more than one blur kernel is present
  if (ps->o.blur_kerns[1]) {
    // Try to generate a framebuffer
    GLuint fbo = 0;
    glGenFramebuffers(1, &fbo);
    if (!fbo) {
      printf_errf("(): Failed to generate Framebuffer. Cannot do "
          "multi-pass blur with GLX backend.");
      return false;
    }
    glDeleteFramebuffers(1, &fbo);
  }

  {
    char *lc_numeric_old = strdup(setlocale(LC_NUMERIC, NULL));
    // Enforce LC_NUMERIC locale "C" here to make sure decimal point is sane
    // Thanks to hiciu for reporting.
    setlocale(LC_NUMERIC, "C");

    static const char *FRAG_SHADER_BLUR_PREFIX =
      "#version 110\n"
      "%s"
      "uniform float offset_x;\n"
      "uniform float offset_y;\n"
      "uniform float factor_center;\n"
      "uniform %s tex_scr;\n"
      "\n"
      "void main() {\n"
      "  vec4 sum = vec4(0.0, 0.0, 0.0, 0.0);\n";
    static const char *FRAG_SHADER_BLUR_ADD =
      "  sum += float(%.7g) * %s(tex_scr, vec2(gl_TexCoord[0].x + offset_x * float(%d), gl_TexCoord[0].y + offset_y * float(%d)));\n";
    static const char *FRAG_SHADER_BLUR_ADD_GPUSHADER4 =
      "  sum += float(%.7g) * %sOffset(tex_scr, vec2(gl_TexCoord[0].x, gl_TexCoord[0].y), ivec2(%d, %d));\n";
    static const char *FRAG_SHADER_BLUR_SUFFIX =
      "  sum += %s(tex_scr, vec2(gl_TexCoord[0].x, gl_TexCoord[0].y)) * factor_center;\n"
      "  gl_FragColor = sum / (factor_center + float(%.7g));\n"
      "}\n";

    const bool use_texture_rect = !ps->psglx->has_texture_non_power_of_two;
    const char *sampler_type = (use_texture_rect ?
        "sampler2DRect": "sampler2D");
    const char *texture_func = (use_texture_rect ?
        "texture2DRect": "texture2D");
    const char *shader_add = FRAG_SHADER_BLUR_ADD;
    char *extension = strdup("");
    if (use_texture_rect)
      mstrextend(&extension, "#extension GL_ARB_texture_rectangle : require\n");
    if (ps->o.glx_use_gpushader4) {
      mstrextend(&extension, "#extension GL_EXT_gpu_shader4 : require\n");
      shader_add = FRAG_SHADER_BLUR_ADD_GPUSHADER4;
    }

    for (int i = 0; i < MAX_BLUR_PASS && ps->o.blur_kerns[i]; ++i) {
      xcb_render_fixed_t *kern = ps->o.blur_kerns[i];
      if (!kern)
        break;

      glx_blur_pass_t *ppass = &ps->psglx->blur_passes[i];

      // Build shader
      {
        int wid = XFIXED_TO_DOUBLE(kern[0]), hei = XFIXED_TO_DOUBLE(kern[1]);
        int nele = wid * hei - 1;
        unsigned int len = strlen(FRAG_SHADER_BLUR_PREFIX) +
                           strlen(sampler_type) +
                           strlen(extension) +
                           (strlen(shader_add) + strlen(texture_func) + 42) * nele +
                           strlen(FRAG_SHADER_BLUR_SUFFIX) +
                           strlen(texture_func) + 12 + 1;
        char *shader_str = ccalloc(len, char);
        if (!shader_str) {
          printf_errf("(): Failed to allocate %d bytes for shader string.", len);
          return false;
        }
        {
          char *pc = shader_str;
          sprintf(pc, FRAG_SHADER_BLUR_PREFIX, extension, sampler_type);
          pc += strlen(pc);
          assert(strlen(shader_str) < len);

          double sum = 0.0;
          for (int j = 0; j < hei; ++j) {
            for (int k = 0; k < wid; ++k) {
              if (hei / 2 == j && wid / 2 == k)
                continue;
              double val = XFIXED_TO_DOUBLE(kern[2 + j * wid + k]);
              if (0.0 == val)
                continue;
              sum += val;
              sprintf(pc, shader_add, val, texture_func, k - wid / 2, j - hei / 2);
              pc += strlen(pc);
              assert(strlen(shader_str) < len);
            }
          }

          sprintf(pc, FRAG_SHADER_BLUR_SUFFIX, texture_func, sum);
          assert(strlen(shader_str) < len);
        }
        ppass->frag_shader = glx_create_shader(GL_FRAGMENT_SHADER, shader_str);
        free(shader_str);
      }

      if (!ppass->frag_shader) {
        printf_errf("(): Failed to create fragment shader %d.", i);
        return false;
      }

      // Build program
      ppass->prog = glx_create_program(&ppass->frag_shader, 1);
      if (!ppass->prog) {
        printf_errf("(): Failed to create GLSL program.");
        return false;
      }

      // Get uniform addresses
#define P_GET_UNIFM_LOC(name, target) { \
      ppass->target = glGetUniformLocation(ppass->prog, name); \
      if (ppass->target < 0) { \
        printf_errf("(): Failed to get location of %d-th uniform '" name "'. Might be troublesome.", i); \
      } \
    }

      P_GET_UNIFM_LOC("factor_center", unifm_factor_center);
      if (!ps->o.glx_use_gpushader4) {
        P_GET_UNIFM_LOC("offset_x", unifm_offset_x);
        P_GET_UNIFM_LOC("offset_y", unifm_offset_y);
      }

#undef P_GET_UNIFM_LOC
    }
    free(extension);

    // Restore LC_NUMERIC
    setlocale(LC_NUMERIC, lc_numeric_old);
    free(lc_numeric_old);
  }


  glx_check_err(ps);

  return true;
}

/**
 * Bind a X pixmap to an OpenGL texture.
 */
bool
glx_render_win(session_t *ps, glx_texture_t **pptex, xcb_pixmap_t pixmap,
    unsigned width, unsigned height, unsigned depth) {
  if (ps->o.backend != BKEND_GLX && ps->o.backend != BKEND_XR_GLX_HYBRID)
    return true;

  if (!pixmap) {
    printf_errf("(%#010x): Binding to an empty pixmap. This can't work.", pixmap);
    return false;
  }

  glx_texture_t *ptex = *pptex;
  bool need_release = true;

  // Allocate structure
  if (!ptex) {
    static const glx_texture_t GLX_TEX_DEF = {
      .texture = 0,
      .glpixmap = 0,
      .pixmap = 0,
      .target = 0,
      .width = 0,
      .height = 0,
      .depth = 0,
      .y_inverted = false,
    };

    ptex = cmalloc(glx_texture_t);
    memcpy(ptex, &GLX_TEX_DEF, sizeof(glx_texture_t));
    *pptex = ptex;
  }

  // Release pixmap if parameters are inconsistent
  if (ptex->texture && ptex->pixmap != pixmap) {
    glx_release_pixmap(ps, ptex);
  }

  // Create GLX pixmap
  if (!ptex->glpixmap) {
    need_release = false;

    // Retrieve pixmap parameters, if they aren't provided
    if (!(width && height && depth)) {
      Window rroot = None;
      int rx = 0, ry = 0;
      unsigned rbdwid = 0;
      if (!XGetGeometry(ps->dpy, pixmap, &rroot, &rx, &ry,
            &width, &height, &rbdwid, &depth)) {
        printf_errf("(%#010x): Failed to query Pixmap info.", pixmap);
        return false;
      }
      if (depth > OPENGL_MAX_DEPTH) {
        printf_errf("(%d): Requested depth higher than %d.", depth,
            OPENGL_MAX_DEPTH);
        return false;
      }
    }

    const glx_fbconfig_t *pcfg = ps->psglx->fbconfigs[depth];
    if (!pcfg) {
      printf_errf("(%d): Couldn't find FBConfig with requested depth.", depth);
      return false;
    }

    // Choose a suitable texture target for our pixmap.
    // Refer to GLX_EXT_texture_om_pixmap spec to see what are the mean
    // of the bits in texture_tgts
    GLenum tex_tgt = 0;
    if (GLX_TEXTURE_2D_BIT_EXT & pcfg->texture_tgts
        && ps->psglx->has_texture_non_power_of_two)
      tex_tgt = GLX_TEXTURE_2D_EXT;
    else if (GLX_TEXTURE_RECTANGLE_BIT_EXT & pcfg->texture_tgts)
      tex_tgt = GLX_TEXTURE_RECTANGLE_EXT;
    else if (!(GLX_TEXTURE_2D_BIT_EXT & pcfg->texture_tgts))
      tex_tgt = GLX_TEXTURE_RECTANGLE_EXT;
    else
      tex_tgt = GLX_TEXTURE_2D_EXT;

#ifdef DEBUG_GLX
    printf_dbgf("(): depth %d, tgt %#x, rgba %d\n", depth, tex_tgt,
        (GLX_TEXTURE_FORMAT_RGBA_EXT == pcfg->texture_fmt));
#endif

    GLint attrs[] = {
        GLX_TEXTURE_FORMAT_EXT,
        pcfg->texture_fmt,
        GLX_TEXTURE_TARGET_EXT,
        tex_tgt,
        0,
    };

    ptex->glpixmap = glXCreatePixmap(ps->dpy, pcfg->cfg, pixmap, attrs);
    ptex->pixmap = pixmap;
    ptex->target = (GLX_TEXTURE_2D_EXT == tex_tgt ? GL_TEXTURE_2D:
        GL_TEXTURE_RECTANGLE);
    ptex->width = width;
    ptex->height = height;
    ptex->depth = depth;
    ptex->y_inverted = pcfg->y_inverted;
  }
  if (!ptex->glpixmap) {
    printf_errf("(): Failed to allocate GLX pixmap.");
    return false;
  }

  glEnable(ptex->target);

  // Create texture
  if (!ptex->texture) {
    need_release = false;

    GLuint texture = 0;
    glGenTextures(1, &texture);
    glBindTexture(ptex->target, texture);

    glTexParameteri(ptex->target, GL_TEXTURE_MIN_FILTER, GL_NEAREST);
    glTexParameteri(ptex->target, GL_TEXTURE_MAG_FILTER, GL_NEAREST);
    glTexParameteri(ptex->target, GL_TEXTURE_WRAP_S, GL_CLAMP_TO_EDGE);
    glTexParameteri(ptex->target, GL_TEXTURE_WRAP_T, GL_CLAMP_TO_EDGE);

    glBindTexture(ptex->target, 0);

    ptex->texture = texture;
  }
  if (!ptex->texture) {
    printf_errf("(): Failed to allocate texture.");
    return false;
  }

  glBindTexture(ptex->target, ptex->texture);

  // The specification requires rebinding whenever the content changes...
  // We can't follow this, too slow.
  if (need_release)
    ps->psglx->glXReleaseTexImageProc(ps->dpy, ptex->glpixmap, GLX_FRONT_LEFT_EXT);

  ps->psglx->glXBindTexImageProc(ps->dpy, ptex->glpixmap, GLX_FRONT_LEFT_EXT, NULL);

  // Cleanup
  glBindTexture(ptex->target, 0);
  glDisable(ptex->target);

  glx_check_err(ps);

  return true;
}

#if 0
/**
 * Preprocess function before start painting.
 */
void
glx_paint_pre(session_t *ps, region_t *preg) {
  ps->psglx->z = 0.0;
  // glClear(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT);

  // Get buffer age
  bool trace_damage = (ps->o.glx_swap_method < 0 || ps->o.glx_swap_method > 1);

  // Trace raw damage regions
  region_t newdamage;
  pixman_region32_init(&newdamage);
  if (trace_damage)
    copy_region(&newdamage, preg);

  // We use GLX buffer_age extension to decide which pixels in
  // the back buffer is reusable, and limit our redrawing
  int buffer_age = 0;

  // Query GLX_EXT_buffer_age for buffer age
  if (ps->o.glx_swap_method == SWAPM_BUFFER_AGE) {
    unsigned val = 0;
    glXQueryDrawable(ps->dpy, get_tgt_window(ps),
        GLX_BACK_BUFFER_AGE_EXT, &val);
    buffer_age = val;
  }

  // Buffer age too high
  if (buffer_age > CGLX_MAX_BUFFER_AGE + 1)
    buffer_age = 0;

  assert(buffer_age >= 0);

  if (buffer_age) {
    // Determine paint area
      for (int i = 0; i < buffer_age - 1; ++i)
        pixman_region32_union(preg, preg, &ps->all_damage_last[i]);
  } else
    // buffer_age == 0 means buffer age is not available, paint everything
    copy_region(preg, &ps->screen_reg);

  if (trace_damage) {
    // XXX use a circular queue instead of memmove
    pixman_region32_fini(&ps->all_damage_last[CGLX_MAX_BUFFER_AGE - 1]);
    memmove(ps->all_damage_last + 1, ps->all_damage_last,
        (CGLX_MAX_BUFFER_AGE - 1) * sizeof(region_t *));
    ps->all_damage_last[0] = newdamage;
  }

  glx_set_clip(ps, preg);

#ifdef DEBUG_GLX_PAINTREG
  glx_render_color(ps, 0, 0, ps->root_width, ps->root_height, 0, *preg, NULL);
#endif

  glx_check_err(ps);
}
#endif

backend_info_t glx_backend = {
  .init = glx_init,

};
