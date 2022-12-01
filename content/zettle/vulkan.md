---
title: Vulkan
---

Vulkan is a new API by the Khronos group (known for OpenGL) that provides a much better abstraction of modern graphics cards.

## Coordinate system

For vulkan, the Normalized Device Coordinates (NDC) are:

```
  (-1, -1)    (1, -1)
      +--------+
      |        |
      |        |
      |        |
      |        |
      +--------+
  (-1, 1)     (1, 1)
```

- Relevant article: [Keeping the Blue Side Up: Coordinate Conventions for OpenGL, Metal and Vulkan](https://hacksoflife.blogspot.com/2019/04/keeping-blue-side-up-coordinate.html)


- [[vulkan-triangle-winding]]#
