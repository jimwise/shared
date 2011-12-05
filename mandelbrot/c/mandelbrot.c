#include <complex.h>
#include <stdarg.h>
#include <stdio.h>
#include <stdlib.h>
#include <math.h>

#include <png.h>

int
escape (double complex c, unsigned int cap) {
  double complex z = 0.0 + (0.0 * I);
  for (int i=0; i<cap; i++) {
    z = cpow(z, 2.0) + c;
    if (cabs(z) > 2.0) {
      return i;
    }
  }
  return 0;
}

void
err (const char *s, ...) {
  va_list ap;
  va_start(ap, s);
  vfprintf(stderr, s, ap);
  fputc('\n', stderr);
  exit(EXIT_FAILURE);
}

/* adapted from: http://zarb.org/~gc/html/libpng.html */
/* assumes 8-bit-per-sample RGB data in rows */
int
write_png (const char *fname, png_bytep *rows, int width, int height) {
  FILE *fp = fopen(fname, "wb");
  if (!fp)
    err("could not open file for writing: %s", fname);

  png_structp png_ptr = png_create_write_struct(PNG_LIBPNG_VER_STRING, NULL, NULL, NULL);

  if (!png_ptr)
    err("png_create_write_struct failed");

  png_infop info_ptr = png_create_info_struct(png_ptr);
  if (!info_ptr)
    err("png_create_info_struct failed");

  if (setjmp(png_jmpbuf(png_ptr)))
    err("failed while writing png file");

  png_init_io(png_ptr, fp);

  png_set_IHDR(png_ptr, info_ptr, width, height,
	       8, PNG_COLOR_TYPE_RGB, PNG_INTERLACE_NONE,
	       PNG_COMPRESSION_TYPE_BASE, PNG_FILTER_TYPE_BASE);
  png_write_info(png_ptr, info_ptr);

  png_write_image(png_ptr, rows);

  png_write_end(png_ptr, NULL);
  png_destroy_write_struct(&png_ptr, &info_ptr);

  fclose(fp);
  return 1;
}

const int density_factor = 16;

unsigned int
color (unsigned int i, unsigned int cap) {
  return (density_factor * i * (unsigned int)(0xffffff / cap)) % 0xffffff;
}

png_byte red (unsigned int c) {return c & 0xff;}
png_byte green (unsigned int c) {return (c & 0xff00) >> 8;}
png_byte blue (unsigned int c) {return (c & 0xff0000) >> 16;}

png_bytep *
image(unsigned int cap, unsigned int width, unsigned int height,
      double xmin, double xmax, double ymin, double ymax) {
  png_bytep *rows = malloc(sizeof(png_bytep) * height);
  if (!rows)
    err("could not allocate image space");
  for (int i=0; i<height; i++) {
    rows[i] = malloc(3 * width * sizeof(png_byte));
    if (!rows[i])
      err("could not allocate row");
  }

  double xstep = (double)fabs(xmax - xmin) / (double)width;
  double ystep = (double)fabs(ymax - ymin) / (double)height;

  for (int y=0; y<height; y++) {
    double y1 = y * ystep + ymin;
    for (int x=0; x<width; x++) {
      double x1 = x * xstep + xmin;
      unsigned int c = color(escape(x1 + (y1 * I), cap), cap);
      png_byte *pix = &(rows[y][x*3]);
	
      pix[0] = red(c);
      pix[1] = green(c);
      pix[2] = blue(c);
    }
  }
  return rows;
}

void
free_image(png_bytep *rows, unsigned int height) {
  for (int i=0; i<height; i++)
    free(rows[i]);
  free(rows);
}

int
main (int argc, char **argv) {
  const unsigned int width = 1280, height = 800, cap = 1000;

  png_bytep *full = image(cap, width, height, -2.5, 1.0, -1.0, 1.0);
  write_png("mandelbrot-c.png", full, width, height);
  free_image(full, height);

  png_bytep *zoomed = image(cap, width, height, -0.5, 0.5, 0.0, 0.75);
  write_png("mandelzoom1-c.png", zoomed, width, height);
  free_image(zoomed, height);

  exit(0);
}
