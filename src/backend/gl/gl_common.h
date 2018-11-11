#pragma once
#include <GL/gl.h>
#include <GL/glext.h>

#include "common.h"

typedef struct {
  /// GLSL program.
  GLuint prog;
  /// Location of uniform "opacity" in window GLSL program.
  GLint unifm_opacity;
  /// Location of uniform "invert_color" in blur GLSL program.
  GLint unifm_invert_color;
  /// Location of uniform "tex" in window GLSL program.
  GLint unifm_tex;
} gl_shader_t;

typedef struct {
  /// Fragment shader for blur.
  GLuint frag_shader;
  /// GLSL program for blur.
  GLuint prog;
  /// Location of uniform "offset_x" in blur GLSL program.
  GLint unifm_offset_x;
  /// Location of uniform "offset_y" in blur GLSL program.
  GLint unifm_offset_y;
  /// Location of uniform "factor_center" in blur GLSL program.
  GLint unifm_factor_center;
} gl_blur_pass_t;

/// @brief Wrapper of a binded GLX texture.
typedef struct gl_texture {
  GLuint texture;
  GLenum target;
  unsigned width;
  unsigned height;
  unsigned depth;
  bool y_inverted;
} gl_texture_t;

#define GL_PROG_MAIN_INIT { \
  .prog = 0, \
  .unifm_opacity = -1, \
  .unifm_invert_color = -1, \
  .unifm_tex = -1, \
}

GLuint
gl_create_shader(GLenum shader_type, const char *shader_str);
GLuint
gl_create_program(const GLuint * const shaders, int nshaders);
GLuint
gl_create_program_from_str(const char *vert_shader_str,
    const char *frag_shader_str);

bool
gl_load_prog_main(session_t *ps,
    const char *vshader_str, const char *fshader_str,
    gl_shader_t *pprogram);

unsigned char *
gl_take_screenshot(session_t *ps, int *out_length);

/**
 * Get a textual representation of an OpenGL error.
 */
static inline const char *
gl_get_err_str(GLenum err) {
  switch (err) {
    CASESTRRET(GL_NO_ERROR);
    CASESTRRET(GL_INVALID_ENUM);
    CASESTRRET(GL_INVALID_VALUE);
    CASESTRRET(GL_INVALID_OPERATION);
    CASESTRRET(GL_INVALID_FRAMEBUFFER_OPERATION);
    CASESTRRET(GL_OUT_OF_MEMORY);
    CASESTRRET(GL_STACK_UNDERFLOW);
    CASESTRRET(GL_STACK_OVERFLOW);
  }
  return NULL;
}

/**
 * Check for GLX error.
 *
 * http://blog.nobel-joergensen.com/2013/01/29/debugging-opengl-using-glgeterror/
 */
static inline void
gl_check_err_(const char *func, int line) {
  GLenum err = GL_NO_ERROR;

  while (GL_NO_ERROR != (err = glGetError())) {
    fprintf(stderr, "%s():%d: GLX error ", func, line);
    const char *errtext = gl_get_err_str(err);
    if (errtext) {
      printf_dbg("%s\n", errtext);
    }
    else {
      printf_dbg("%d\n", err);
    }
  }
}

#define gl_check_err() gl_check_err_(__func__, __LINE__)

/**
 * Check if a GLX extension exists.
 */
static inline bool
gl_has_extension(session_t *ps, const char *ext) {
  GLint nexts = 0;
  glGetIntegerv(GL_NUM_EXTENSIONS, &nexts);
  if (!nexts) {
    printf_errf("(): Failed get GL extension list.");
    return false;
  }

  for (int i = 0; i < nexts; i++) {
    const char *exti = (const char *) glGetStringi(GL_EXTENSIONS, i);
    if (strcmp(ext, exti) == 0)
      return true;
  }
  printf_errf("(): Missing GL extension %s.", ext);
  return false;
}

#define P_PAINTREG_START(var) do { \
  region_t reg_new; \
  int nrects; \
  const rect_t *rects; \
  pixman_region32_init_rect(&reg_new, dx, dy, width, height); \
  pixman_region32_intersect(&reg_new, &reg_new, (region_t *)reg_tgt); \
  rects = pixman_region32_rectangles(&reg_new, &nrects); \
  glBegin(GL_QUADS); \
 \
  for (int ri = 0; ri < nrects; ++ri) { \
    rect_t var = rects[ri];

#define P_PAINTREG_END() \
  } \
  glEnd(); \
  pixman_region32_fini(&reg_new); \
} while(0)
