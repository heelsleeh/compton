#include <stdbool.h>
#include <GL/gl.h>
#include <GL/glext.h>

#include "common.h"

#include "backend/gl/gl_common.h"

GLuint
gl_create_shader(GLenum shader_type, const char *shader_str) {
#ifdef DEBUG_GLX_GLSL
  printf_errf("(): ===\n%s\n===\n", shader_str);
  fflush(stdout);
#endif

  bool success = false;
  GLuint shader = glCreateShader(shader_type);
  if (!shader) {
    printf_errf("(): Failed to create shader with type %#x.", shader_type);
    goto end;
  }
  glShaderSource(shader, 1, &shader_str, NULL);
  glCompileShader(shader);

  // Get shader status
  {
    GLint status = GL_FALSE;
    glGetShaderiv(shader, GL_COMPILE_STATUS, &status);
    if (GL_FALSE == status) {
      GLint log_len = 0;
      glGetShaderiv(shader, GL_INFO_LOG_LENGTH, &log_len);
      if (log_len) {
        char log[log_len + 1];
        glGetShaderInfoLog(shader, log_len, NULL, log);
        printf_errf("(): Failed to compile shader with type %d: %s",
            shader_type, log);
      }
      goto end;
    }
  }

  success = true;

end:
  if (shader && !success) {
    glDeleteShader(shader);
    shader = 0;
  }

  return shader;
}

GLuint
gl_create_program(const GLuint * const shaders, int nshaders) {
  bool success = false;
  GLuint program = glCreateProgram();
  if (!program) {
    printf_errf("(): Failed to create program.");
    goto end;
  }

  for (int i = 0; i < nshaders; ++i)
    glAttachShader(program, shaders[i]);
  glLinkProgram(program);

  // Get program status
  {
    GLint status = GL_FALSE;
    glGetProgramiv(program, GL_LINK_STATUS, &status);
    if (GL_FALSE == status) {
      GLint log_len = 0;
      glGetProgramiv(program, GL_INFO_LOG_LENGTH, &log_len);
      if (log_len) {
        char log[log_len + 1];
        glGetProgramInfoLog(program, log_len, NULL, log);
        printf_errf("(): Failed to link program: %s", log);
      }
      goto end;
    }
  }
  success = true;

end:
  if (program) {
    for (int i = 0; i < nshaders; ++i)
      glDetachShader(program, shaders[i]);
  }
  if (program && !success) {
    glDeleteProgram(program);
    program = 0;
  }

  return program;
}

/**
 * @brief Create a program from vertex and fragment shader strings.
 */
GLuint
gl_create_program_from_str(const char *vert_shader_str,
    const char *frag_shader_str) {
  GLuint vert_shader = 0;
  GLuint frag_shader = 0;
  GLuint prog = 0;

  if (vert_shader_str)
    vert_shader = gl_create_shader(GL_VERTEX_SHADER, vert_shader_str);
  if (frag_shader_str)
    frag_shader = gl_create_shader(GL_FRAGMENT_SHADER, frag_shader_str);

  {
    GLuint shaders[2];
    unsigned int count = 0;
    if (vert_shader)
      shaders[count++] = vert_shader;
    if (frag_shader)
      shaders[count++] = frag_shader;
    assert(count <= sizeof(shaders) / sizeof(shaders[0]));
    if (count)
      prog = gl_create_program(shaders, count);
  }

  if (vert_shader)
    glDeleteShader(vert_shader);
  if (frag_shader)
    glDeleteShader(frag_shader);

  return prog;
}

/**
 * @brief Get tightly packed RGB888 data from GL front buffer.
 *
 * Don't expect any sort of decent performance.
 *
 * @returns tightly packed RGB888 data of the size of the screen,
 *          to be freed with `free()`
 */
unsigned char *
gl_take_screenshot(session_t *ps, int *out_length) {
  int length = 3 * ps->root_width * ps->root_height;
  GLint unpack_align_old = 0;
  glGetIntegerv(GL_UNPACK_ALIGNMENT, &unpack_align_old);
  assert(unpack_align_old > 0);
  glPixelStorei(GL_UNPACK_ALIGNMENT, 1);
  unsigned char *buf = cmalloc(length, unsigned char);
  glReadBuffer(GL_FRONT);
  glReadPixels(0, 0, ps->root_width, ps->root_height, GL_RGB,
      GL_UNSIGNED_BYTE, buf);
  glReadBuffer(GL_BACK);
  glPixelStorei(GL_UNPACK_ALIGNMENT, unpack_align_old);
  if (out_length)
    *out_length = sizeof(unsigned char) * length;
  return buf;
}

/**
 * @brief Render a region with texture data.
 */
bool
gl_compose(const gl_texture_t *ptex,
  int x, int y, int dx, int dy, int width, int height, int z,
  double opacity, bool argb, bool neg,
  const region_t *reg_tgt, const gl_shader_t *shader)
{
  if (!ptex || !ptex->texture) {
    printf_errf("(): Missing texture.");
    return false;
  }

  //argb = argb || (GLX_TEXTURE_FORMAT_RGBA_EXT ==
  //    ps->psglx->fbconfigs[ptex->depth]->texture_fmt);
  bool dual_texture = false;

  // It's required by legacy versions of OpenGL to enable texture target
  // before specifying environment. Thanks to madsy for telling me.
  glEnable(ptex->target);

  // Enable blending if needed
  if (opacity < 1.0 || argb) {

    glEnable(GL_BLEND);

    // Needed for handling opacity of ARGB texture
    glTexEnvi(GL_TEXTURE_ENV, GL_TEXTURE_ENV_MODE, GL_MODULATE);

    // This is all weird, but X Render is using premultiplied ARGB format, and
    // we need to use those things to correct it. Thanks to derhass for help.
    glBlendFunc(GL_ONE, GL_ONE_MINUS_SRC_ALPHA);
    glColor4f(opacity, opacity, opacity, opacity);
  }

  // Programmable path
  assert(shader->prog);
  glUseProgram(shader->prog);
  if (shader->unifm_opacity >= 0)
    glUniform1f(shader->unifm_opacity, opacity);
  if (shader->unifm_invert_color >= 0)
    glUniform1i(shader->unifm_invert_color, neg);
  if (shader->unifm_tex >= 0)
    glUniform1i(shader->unifm_tex, 0);

#ifdef DEBUG_GLX
  printf_dbgf("(): Draw: %d, %d, %d, %d -> %d, %d (%d, %d) z %d\n", x, y, width, height, dx, dy, ptex->width, ptex->height, z);
#endif

  // Bind texture
  glBindTexture(ptex->target, ptex->texture);
  if (dual_texture) {
    glActiveTexture(GL_TEXTURE1);
    glBindTexture(ptex->target, ptex->texture);
    glActiveTexture(GL_TEXTURE0);
  }

  // Painting
  P_PAINTREG_START(crect) {
    // Calculate texture coordinates
    GLfloat texture_x1 = (double) (crect.x1 - dx + x);
    GLfloat texture_y1 = (double) (crect.y1 - dy + y);
    GLfloat texture_x2 = texture_x1 + (double) (crect.x2 - crect.x1);
    GLfloat texture_y2 = texture_y1 + (double) (crect.y2 - crect.y1);

    if (GL_TEXTURE_2D == ptex->target) {
      // GL_TEXTURE_2D coordinates are 0-1
      texture_x1 /= ptex->width;
      texture_y1 /= ptex->height;
      texture_x2 /= ptex->width;
      texture_y2 /= ptex->height;
    }

    // Vertex coordinates
    GLint vx1 = crect.x1;
    GLint vy1 = crect.y1;
    GLint vx2 = crect.x2;
    GLint vy2 = crect.y2;

    // X pixmaps might be Y inverted, invert the texture coordinates
    if (ptex->y_inverted) {
      texture_y1 = 1.0 - texture_y1;
      texture_y2 = 1.0 - texture_y2;
    }

#ifdef DEBUG_GLX
    printf_dbgf("(): Rect %d: %f, %f, %f, %f -> %d, %d, %d, %d\n", ri, rx, ry, rxe, rye, rdx, rdy, rdxe, rdye);
#endif

    GLfloat texture_x[] = { texture_x1, texture_x2, texture_x2, texture_x1 };
    GLfloat texture_y[] = { texture_y1, texture_y1, texture_y2, texture_y2 };
    GLint vx[] = { vx1, vx2, vx2, vx1 };
    GLint vy[] = { vy1, vy1, vy2, vy2 };

    for (int i = 0; i < 4; i++) {
      glTexCoord2f(texture_x[i], texture_y[i]);
      glVertex3i(vx[i], vy[i], z);
    }

  } P_PAINTREG_END();

  // Cleanup
  glBindTexture(ptex->target, 0);
  glColor4f(0.0f, 0.0f, 0.0f, 0.0f);
  glTexEnvi(GL_TEXTURE_ENV, GL_TEXTURE_ENV_MODE, GL_REPLACE);
  glDisable(GL_BLEND);
  glDisable(GL_COLOR_LOGIC_OP);
  glDisable(ptex->target);

  if (dual_texture) {
    glActiveTexture(GL_TEXTURE1);
    glBindTexture(ptex->target, 0);
    glDisable(ptex->target);
    glActiveTexture(GL_TEXTURE0);
  }

  glUseProgram(0);

  gl_check_err();

  return true;
}

bool
gl_dim_reg(session_t *ps, int dx, int dy, int width, int height, float z,
    GLfloat factor, const region_t *reg_tgt) {
  // It's possible to dim in glx_render(), but it would be over-complicated
  // considering all those mess in color negation and modulation
  glEnable(GL_BLEND);
  glBlendFunc(GL_ONE, GL_ONE_MINUS_SRC_ALPHA);
  glColor4f(0.0f, 0.0f, 0.0f, factor);

  {
    P_PAINTREG_START(crect) {
      // XXX what does all of these variables mean?
      GLint rdx = crect.x1;
      GLint rdy = ps->root_height - crect.y1;
      GLint rdxe = rdx + (crect.x2 - crect.x1);
      GLint rdye = rdy - (crect.y2 - crect.y1);

      glVertex3i(rdx, rdy, z);
      glVertex3i(rdxe, rdy, z);
      glVertex3i(rdxe, rdye, z);
      glVertex3i(rdx, rdye, z);
    }
    P_PAINTREG_END();
  }

  glEnd();

  glColor4f(0.0f, 0.0f, 0.0f, 0.0f);
  glDisable(GL_BLEND);

  gl_check_err();

  return true;
}
