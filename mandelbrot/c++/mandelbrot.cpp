#include <complex>
#include <cmath>

#include <png++/png.hpp>

using std::complex;

namespace {
  const int density_factor = 16;
  const int cap = 1000;
}

// class MandelImage : public png::image<png::rgb_pixel> {
//   MandelImage(size_t width, size_t height, double xmin, double xmax,
// 	      double ymin, double ymax) : png::image<png::rgb_pixel>(width, height) {

// };

int
escape (complex<double> c) {
  complex<double> z(0.0, 0.0);
  for (unsigned int i=0; i<cap; i++) {
    z = pow(z, 2) + c;
    if (abs(z) > 2.0)
      return i;
  }
  return 0;
}

png::byte red (unsigned int c) {return c & 0xff;}
png::byte green (unsigned int c) {return (c & 0xff00) >> 8;}
png::byte blue (unsigned int c) {return (c & 0xff0000) >> 16;}

png::rgb_pixel
pixel (unsigned int i) {
  unsigned int c = (density_factor * i * (unsigned int)(0xffffff / cap)) % 0xffffff;
  return png::rgb_pixel(red(c), green(c), blue(c));
}

png::image<png::rgb_pixel>
image(unsigned int width, unsigned int height,
      double xmin, double xmax, double ymin, double ymax) {
  png::image<png::rgb_pixel> image(width, height);
  double xstep = (double)fabs(xmax - xmin) / (double)width;
  double ystep = (double)fabs(ymax - ymin) / (double)height;

  for (unsigned int y=0; y<height; y++) {
    double y1 = y * ystep + ymin;
    for (unsigned int x=0; x<width; x++) {
      double x1 = x * xstep + xmin;
      image[y][x] = pixel(escape(complex<double>(x1, y1)));
    }
  }
  return image;
}

int
main (int argc, char **argv) {
  const unsigned int width = 1280, height = 800;

  png::image< png::rgb_pixel > full = image(width, height, -2.5, 1.0, -1.0, 1.0);
  full.write("mandelbrot-c++.png");

  png::image< png::rgb_pixel > zoomed = image(width, height, -0.5, 0.5, 0.0, 0.75);
  zoomed.write("mandelzoom1-c++.png");

  exit(0);
}
