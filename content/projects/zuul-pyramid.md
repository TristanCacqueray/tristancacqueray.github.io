---
title: Zuul Pyramid
date: 2019-09-24
tags: [zuul]
---

A 3D rendering of the [[zuul]] logo.

<iframe width="560" height="315" src="https://www.youtube.com/embed/fxP3VbAP-X8" title="YouTube video player" frameborder="0" allow="accelerometer; autoplay; clipboard-write; encrypted-media; gyroscope; picture-in-picture" allowfullscreen></iframe>

The GLSL shader:

```glsl
/* Zuul-ci pyramid logo in 3D
   Copyright © 2019 Tristan De Cacqueray
   SPDX short identifier: MIT & CC-BY-NC-SA-4.0

   Code is mainly based on
   https://www.shadertoy.com/view/Xds3zN
   Uploaded by iq in 2013-03-25

    and

   https://www.shadertoy.com/view/ldKGWW
   Uploaded by wjbgrafx on 2016-02-04

   More resources:
   http://www.iquilezles.org/www/material/nvscene2008/rwwtt.pdf
   http://www.iquilezles.org/www/articles/distfunctions/distfunctions.htm
*/

// Set to 1 if too slow
#define AA 4

// Uncomment to add scene elements
// #define SKY
// #define MONSTER
// #define FLOOR

// Params
uniform vec3 iResolution;
uniform float iTime;
uniform vec4 iMouse;

const vec3 cam = vec3(0., -.2, -2.);
#define PI 3.141592653589793
#define PITCH 0.428
#define YAW -0.8
#define DEG60 0.866025

// Primitives
const mat3 rotX180 = mat3(1.0,     0.0,     0.0,
                          0.0,  cos(PI), sin(PI),
                          0.0, -sin(PI), cos(PI));

float smin(float a, float b, float k) {
  float h = clamp(0.5 + 0.5 * (b - a) / k, 0.0, 1.0);
  return mix(b, a, h) - k * h * (1.0 - h);
}

vec2 opU(vec2 d1, vec2 d2) {
  return (d1.x < d2.x) ? d1 : d2;
}

float pMirror(inout float p, float dist) {
  float s = sign(p);
  p = abs(p) - dist;
  return s;
}

float sdPlane(vec3 p) {
  return p.y;
}

float sdTriPrism(vec3 p, vec2 h) {
  vec3 q = abs(p);
  return max(q.z - h.y,
         max(q.x * 0.4 + p.y * 0.5, -p.y) - h.x * 0.5);
}

float sdPrismZ(vec3 p, float angleRads, float height, float depth) {
  vec3 q = abs(p);
  return max(q.z - depth,
         max(q.x * angleRads + p.y * 0.5, -p.y) - height * 0.5);
}

float sdPrismX(vec3 p, float angleRads, float height, float depth) {
    vec3 q = abs(p);
    return max(q.x - depth,
           max(q.z * angleRads + p.y * 0.5, -p.y) - height * 0.5);
}

float sdPyramid(vec3 p, float angleRads, float height, float depth) {
  vec3 q = abs(p);
  return max(sdPrismX(p, angleRads, height, depth),
             sdPrismZ(p, angleRads, height, depth));
}

float sdBox(vec3 p, vec3 b) {
  vec3 d = abs(p) - b;
  return min(max(d.x, max(d.y, d.z)), 0.0) + length(max(d, 0.0));
}

float dot2(vec3 v) {
  return dot(v,v);
}

float udTriangle(vec3 p, vec3 a, vec3 b, vec3 c) {
  vec3 ba = b - a; vec3 pa = p - a;
  vec3 cb = c - b; vec3 pb = p - b;
  vec3 ac = a - c; vec3 pc = p - c;
  vec3 nor = cross(ba, ac);

  return sqrt(
    (sign(dot(cross(ba,nor),pa)) +
     sign(dot(cross(cb,nor),pb)) +
     sign(dot(cross(ac,nor),pc)) < 2.0)
     ?
     min(min(
     dot2(ba*clamp(dot(ba,pa)/dot2(ba),0.0,1.0)-pa),
     dot2(cb*clamp(dot(cb,pb)/dot2(cb),0.0,1.0)-pb)),
     dot2(ac*clamp(dot(ac,pc)/dot2(ac),0.0,1.0)-pc))
     :
     dot(nor,pa)*dot(nor,pa)/dot2(nor));
}

// Zuul logo primitives
float sdHollowPyramid(vec3 p) {
  float pyramid = sdPyramid(p, DEG60, 1.0, 1.0);
  pyramid = max(pyramid, -sdPyramid(p - vec3(0.0, 0., 0.5), DEG60, .9, .9));
  pyramid = max(pyramid, -sdBox(p, vec3(.45, 0.45, 0.45)));
  return pyramid;
}

float sdHollowBox(vec3 pos, vec3 size, float hole) {
  float box = sdBox(pos, size);
  box = max(box, -sdBox(pos, size * vec3(2., hole, hole)));
  box = max(box, -sdBox(pos, size * vec3(hole, hole, 2.)));
  return box;
}

float sdInnerFrame(vec3 pos) {
  float left  = sdHollowBox(pos + vec3(0.06, .1, .0), vec3(.39, .48, .45), 0.87);
  float right = sdHollowBox(pos + vec3(.0, .1, -.06), vec3(.45, .48, .39), 0.87);
  return left < right ? left : right;
}

float sdPilar(vec3 pos) {
  return sdBox(pos + vec3(.000, .15, -.424), vec3(.026, .35, .026));
}

float sdRoof(vec3 pos) {
 return sdTriPrism((pos - vec3(.44, .492, .0)) * rotX180, vec2(.05, .45));
}

float sdZuul(vec3 pos) {
  vec3 p = pos;
  pMirror(p.x, 0.0);
  pMirror(p.z, 0.0);
  float pyramid = sdHollowPyramid(p);
  float mainBox = sdHollowBox(p, vec3(.45, .517, .45), 0.87);
  float innerBox = sdInnerFrame(p);
  float lowerPlateau = sdBox(p - vec3(.0, .21, .0), vec3(.45, .026, .45));
  float pilar = sdPilar(p);
  float entranceWall = udTriangle(
      p, vec3(.78, -.45, .45), vec3(.39, -.45, .451), vec3(.39, .34, .451));
  float roof = sdRoof(p);

  float closer = pyramid;
  closer = smin(closer, lowerPlateau, .01);
  closer = smin(closer, entranceWall, .01);
  closer = closer < mainBox ? closer : mainBox;
  closer = closer < roof ? closer : roof;
  closer = closer < innerBox ? closer : innerBox;
  closer = closer < pilar ? closer : pilar;
  float socle = sdBox(pos + vec3(.0, .6, .0), vec3(2., .1, 2.));
  return max(closer, -socle);
}

#ifdef MONSTER
// Julia - Quaternion
// https://www.shadertoy.com/view/MsfGRr
const int numIterations = 11;

vec4 qsqr(vec4 a) {
  return vec4(a.x*a.x - a.y*a.y - a.z*a.z - a.w*a.w,
              2.0*a.x*a.y,
              2.0*a.x*a.z,
              2.0*a.x*a.w);
}

float juliaQ(vec3 p, vec4 c) {
  vec4 z = vec4(p, 0.0) * 3.6;
  float md2 = 20.0;
  float mz2 = dot(z, z);

  vec4 trap = vec4(abs(z.xyz), dot(z, z));

  for (int i=0; i < numIterations; i++) {
    md2 *= 4.0 * mz2;
    z = qsqr(z) + c;
    trap = min(trap, vec4(abs(z.xyz), dot(z, z)));
    mz2 = dot(z, z);
    if (mz2 > 4.0)
      break;
  }
  return 0.25 * sqrt(mz2 / md2) * log(mz2);
}
#endif

// The scene
vec2 scene(vec3 pos) {
  vec2 res = vec2(sdZuul(pos), 29.);
  #ifdef FLOOR
  float plane = sdPlane(pos + vec3(.0, 1., .0));
  // Does not work well, but tries to remove the floor when under it...
  if (plane > .0)
    res = opU(res, vec2(plane, 1.));
  #endif
  #ifdef MONSTER
  vec4 c = 0.45*cos(vec4(0.5,3.9,1.4,1.1) + (iTime*1.5)*vec4(1.2,1.7,1.3,2.5)) - vec4(0.3,0.0,0.0,0.0);
  res = opU(res, vec2(juliaQ(pos + vec3(.0, .13, .0), c), 49.));
  #endif
  return res;
}

vec3 calcNormal(vec3 pos) {
  vec2 e = vec2(1.0, -1.0) * 0.5773 * 0.0005;
  return normalize(e.xyy * scene(pos + e.xyy).x +
                   e.yyx * scene(pos + e.yyx).x +
                   e.yxy * scene(pos + e.yxy).x +
                   e.xxx * scene(pos + e.xxx).x);
}

float calcAO(vec3 pos, vec3 nor) {
  float occ = 0.0;
  float sca = 1.0;
  for (int i=0; i < 5; i++) {
    float hr = 0.01 + 0.12 * float(i) / 4.0;
    vec3 aopos = nor * hr + pos;
    float dd = scene(aopos).x;
    occ += -(dd - hr) * sca;
    sca *= 0.95;
  }
  return clamp(1.0 - 3.0 * occ, 0.0, 1.0);
}

vec2 castRay(vec3 ro, vec3 rd) {
  const float tmin = 0.2;
  const float tmax = 20.0;
  float t = tmin;
  float m;
  for (int i=0; i < 300; i++) {
    vec2 res = scene(ro + rd * t);
    if (res.x < 0.0001 ||
        t > tmax) break;
    t += res.x;
    m = res.y;
  }
  if (t > tmax) m = -1.0;
  return vec2(t, m);
}

// Improved soft shadow by Sebastian Aaltonen
// From: http://www.iquilezles.org/www/articles/rmshadows/rmshadows.htm
float calcSoftshadow(vec3 ro, vec3 rd, float mint, float maxt) {
  float k = 32.;
  float res = 1.0;
  float ph = 1e20;
  for (float t=mint; t < maxt;) {
    float h = scene(ro + rd * t).x;
    if (h < 0.001)
      return 0.0;
    float y = h * h / (2.0 * ph);
    float d = sqrt(h * h - y * y);
    res = min(res, k * d / max(0.0, t-y));
    ph = h;
    t += h;
  }
  return res;
}

#ifdef FLOOR
// http://iquilezles.org/www/articles/checkerfiltering/checkerfiltering.htm
float checkersGradBox(vec2 p) {
    // filter kernel
    vec2 w = fwidth(p) + 0.001;
    // analytical integral (box filter)
    vec2 i = 2.0*(abs(fract((p-0.5*w)*0.5)-0.5)-abs(fract((p+0.5*w)*0.5)-0.5))/w;
    // xor pattern
    return 0.5 - 0.5*i.x*i.y;
}
#endif

#ifdef SKY
vec3 skyColor(vec3 rd) {
  float offset = (.95 - clamp(rd.y, 0.0, 0.2))*.7;
  return vec3(.5)+vec3(.5)*cos(6.28*(vec3(1.)*offset+vec3(0.0,0.10,0.20)));
}
#endif

vec3 render(vec3 ro, vec3 rd) {
  vec3 col = vec3(.0);
  #ifdef SKY
  col = skyColor(rd);
  #endif
  vec2 res = castRay(ro, rd);
  float t = res.x;
  float m = res.y;

  if (m > 0.) {
    vec3 pos = ro + t * rd;
    vec3 nor = calcNormal(pos);
    vec3 ref = reflect(rd, nor);
    col = 0.45 + 0.35*sin(vec3(0.0001,0.004,0.04)*(m-1.0));
    #ifdef FLOOR
    if (m < 1.5) {
            float f = checkersGradBox(5.0*pos.xz);
            col = 0.01 + f*vec3(0.1);
    }
    #endif
    // lightning
    float occ = calcAO(pos, nor);
    vec3  lig = normalize(vec3(0.25, 0.7, -1.));
    vec3  hal = normalize(lig - rd);
    float amb = clamp(0.5 + 0.5 * nor.y, 0.0, 1.0);
    float dif = clamp(dot(nor, lig), 0.0, 1.0);
    float bac = clamp(dot(nor, normalize(vec3(-lig.x, 0.0, -lig.z))),
                      0.0, 1.0)*clamp(1.0 - pos.y, 0.0, 1.0);
    float dom = smoothstep(-0.2, 0.2, ref.y);
    float fre = pow(clamp(1.0+dot(nor,rd),0.0,1.0), 2.0);

    dif *= calcSoftshadow(pos, lig, 0.02, 2.5) * 0.2;
    dom *= calcSoftshadow(pos, ref, 0.02, 2.5);

    float spe = pow(clamp(dot(nor, hal), 0.0, 1.0), 16.0);
    spe *= dif * (0.04 + 0.96 * pow(clamp(1.0 + dot(hal, rd), 0.0, 1.0), 5.0));

    vec3 lin = vec3(0.0);
    lin += 3.30 * dif * vec3(1.00, 0.80, 0.55);
    lin += 0.40 * amb * vec3(0.40, 0.60, 1.00) * occ;
    lin += 0.40 * dom * vec3(0.40, 0.60, 1.00) * occ;
    lin += 0.50 * bac * vec3(0.25, 0.25, 0.25) * occ;
    lin += 0.25 * fre * vec3(1.00, 1.00, 1.00) * occ;
    col *= lin;
    col += 50.0 * spe * vec3(1.00, 0.90, 0.70);
    col = mix(col,  vec3(.0), 1.0 - exp(-0.0002 * t * t * t));
  }
  #ifdef SKY
  // fog
  float rayDist = length(ro + rd * t);
  col = mix(col, skyColor(rd), 1.0 - 1.0 / exp(rayDist * 0.05));
  #endif
  return vec3(clamp(col, 0.0, 1.0));
}

float smoothStair(float frame) {
  return frame - sin(frame) / 1.9;
}

mat3 camRotation() {
  float yaw, pitch;
  if (iMouse.z > 0.0) {
    yaw = (iMouse.x / iResolution.x - 0.5) * 4.;
    pitch = (iMouse.y / iResolution.y - 0.5) * 4.;
  } else {
    yaw = -.35 + smoothStair(iTime * 2.) / 2.;
    pitch = PITCH;
  }
  return mat3(1.0,        0.0,        0.0,
              0.0, cos(pitch), -sin(pitch),
              0.0, sin(pitch),  cos(pitch)) *
         mat3(cos(yaw),   0.0,  sin(yaw),
              0.0,        1.0,      0.0,
             -sin(yaw),   0.0,  cos(yaw));
}

void mainImage(out vec4 fragColor, in vec2 fragCoord) {
  mat3 rot = camRotation();
  vec3 tot = vec3(0.0);
#if AA > 1
  for(int m=0; m < AA; m++)
  for(int n=0; n < AA; n++) {
    vec2 o = vec2(float(m), float(n)) / float(AA) - 0.5;
    vec2 uv = (gl_FragCoord.xy+o) / iResolution.xy*2.-1.;
#else
    vec2 uv = gl_FragCoord.xy / iResolution.xy*2.-1.;
#endif

    uv.y *= iResolution.y / iResolution.x;

    vec3 dir = normalize(vec3(uv, 1.)) * rot;
    vec3 pos = cam * rot;

    vec3 col = render(pos, dir);
    // gamma
    col = pow(col, vec3(0.4));

    tot += col;
#if AA > 1
  }
  tot /= float(AA * AA);
#endif
  fragColor = vec4(tot, 1.0);
}

void main(void) {
  mainImage(gl_FragColor, gl_FragCoord.xy);
}
```

The renderer:

```python
# Zuul-ci pyramid logo in 3D
# SPDX short identifier: MIT

"""Render the zuul.glsl shader to a zuul.mp4 file at 25 fps"""

from subprocess import Popen
import numpy as np
from glumpy import app, gl, gloo
from PIL import Image

RES = [500, 500]
ROTATION = 2
FPS = 25

vertex = "attribute vec2 p; void main(void) {gl_Position = vec4(p, 0.0, 1.0);}"
fragment = open("zuul.glsl").read()
window = app.Window(width=RES[0], height=RES[1])
program = gloo.Program(vertex, fragment, count=4)
program['p'] = [(-1, -1), (-1, +1), (+1, -1), (+1, +1)]
program['iResolution'] = RES + [0]
gl.glEnable(gl.GL_BLEND)
gl.glBlendFunc(gl.GL_SRC_ALPHA, gl.GL_ONE_MINUS_SRC_ALPHA)
backend = app.__backend__
clock = app.__init__(backend=backend, framerate=FPS)
pixels = np.zeros((RES[0], RES[1] * 3), dtype=np.uint8)

for i in range(int(np.pi * ROTATION * FPS)):
    window.activate()
    window.clear()
    program["iTime"] = i / FPS
    program.draw(gl.GL_TRIANGLE_STRIP)
    gl.glReadPixels(
        0, 0, RES[0], RES[1], gl.GL_RGB, gl.GL_UNSIGNED_BYTE, pixels)
    image = Image.frombytes(
        "RGB", RES, np.ascontiguousarray(np.flip(pixels, 0)))
    image.save("%03d.png" % (i + 1), 'png')
    backend.process(clock.tick())

Popen(["ffmpeg", "-y", "-r", str(FPS), "-i", "%03d.png", "-pix_fmt", "yuv420p",
       "zuul.mp4"]).wait()
```
