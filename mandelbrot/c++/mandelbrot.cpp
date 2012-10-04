#include <complex>
#include <cmath>

#include <png++/png.hpp>

using std::complex;

const int density_factor = 16;
const int cap = 1000;

class MandelImage : public png::image<png::rgb_pixel> {
public:
  MandelImage(size_t width, size_t height, double xmin = -2.5, double xmax = 1.0,
	      double ymin = -1.0, double ymax = 1.0) : png::image<png::rgb_pixel>(width, height) {
    double xstep = (double)fabs(xmax - xmin) / (double)width;
    double ystep = (double)fabs(ymax - ymin) / (double)height;

    for (unsigned int y=0; y<height; y++) {
      double y1 = y * ystep + ymin;
      for (unsigned int x=0; x<width; x++) {
	double x1 = x * xstep + xmin;
	set_pixel(x, y, pixel(escape(complex<double>(x1, y1))));
      }
    }
  }

private:
  int escape (complex<double> c) {
    complex<double> z(0.0, 0.0);
    for (unsigned int i=0; i<cap; i++) {
      z = pow(z, 2) + c;
      if (abs(z) > 2.0)
	return i;
    }
    return 0;
  }

  static png::byte red (unsigned int c) {return c & 0xff;}
  static png::byte green (unsigned int c) {return (c & 0xff00) >> 8;}
  static png::byte blue (unsigned int c) {return (c & 0xff0000) >> 16;}

  static png::rgb_pixel pixel (unsigned int i) {
    unsigned int c = (density_factor * i * (unsigned int)(0xffffff / cap)) % 0xffffff;
    return png::rgb_pixel(red(c), green(c), blue(c));
  }
};

int
main (int argc, char **argv) {
  const unsigned int width = 1280, height = 800;

  MandelImage(width, height).write("mandelbrot-c++.png");
  MandelImage(width, height, -0.5, 0.5, 0.0, 0.75).write("mandelzoom1-c++.png");
  exit(0);
}
