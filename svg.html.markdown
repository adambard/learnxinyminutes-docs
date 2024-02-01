---
language: svg
filename: learn-svg.svg
contributors:
    - ["Kaamkiya", "https://github.com/Kaamkiya"]
---

Scalable Vector Graphics (SVGs) are used to make images and graphics. Unlike the PNG and JPG image formats,
SVG images don't store all the individual pixels: they store the shapes. That's why they scale well.


```xhtml
<!-- SVGs are made in XML format, so they should be fairly easy to learn
for HTML, XHTML, and XML users -->

<svg xmlns="https://w3.org/2000/svg" viewBox="0 0 400 400" width="400" height="400">
  <!-- Around this comment is the wrapper for an SVG
  the xmlns attribute specifies which SVG standard to use
  width and height attributes are obvious
  viewBox is where you zoom in on, in a way. It's what you see.
  In this example, viewBox="0 0 400 400" means you see from coordinates
  0, 0 to 400, 400 (the entire SVG)

  We'll see more of this later-->

  <rect x="30" y="50" height="73" width="62" fill="red"></rect>
  <!-- that is a rectangle. fill is the fill color, but the rest of the
  attributes are quite obvious -->

  
</svg>

```

### Further Reading

<!-- Add some resources -->
