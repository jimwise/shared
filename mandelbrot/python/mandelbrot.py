#!/opt/csw/bin/python

import Image, ImageDraw

class Mandelbrot:
  def __init__(self, cap):
    self.cap = cap

  def escape(self, c):
    z = complex(0, 0)
    for it in range(self.cap + 1):
      if abs(z) > 2.0:
        return it
      z = (z ** 2) + c
    return 0

class MandelImage(Mandelbrot):
  def __init__(self, width, height, cap):
    # super(MandelImage).__init__(cap)
    self.cap = cap
    self.width = width
    self.height = height
    self.scalec = 0xffffff / cap
    self.zoom()
    self.density_factor = 16

  def zoom(self, xmin=-2.5, xmax=1.0, ymin=-1.0, ymax=1.0):
    self.xmin = xmin
    self.xmax = xmax
    self.ymin = ymin
    self.ymax = ymax
    self.xstep = abs(xmax - xmin) / self.width
    self.ystep = abs(ymax - ymin) / self.height
    return self

  def image(self):
    img = Image.new("RGB", (self.width, self.height))
    draw = ImageDraw.Draw(img)
    for y in range(self.height):
      for x in range(self.width):
        c = complex(self.scalex(x), (self.scaley(y)))
        it = self.escape(c)
        draw.point((x, y), self.color(it))
    return img
  
  def scalex(self, n):
    return self.xmin + (n * self.xstep)

  def scaley(self, n):
    return self.ymin + (n * self.ystep)

  def color(self, n):
    if n == 0:
      return (0,0,0) 
    n = (n * self.scalec * self.density_factor) % 0xffffff
    r =  (n & 0xff)
    g =  ((n & 0xff00) >> 8)
    b =  ((n & 0xff0000) >> 16)
    return (r,g,b)

MandelImage(1280,800,1000).image().save("mandelbrot-python.png")
MandelImage(1280,800,1000).zoom(-0.5, 0.5, 0, 0.75).image().save("mandelzoom1-python.png")
