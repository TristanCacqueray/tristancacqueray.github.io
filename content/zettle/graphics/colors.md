# colors

## The "Matrix Color" formula

$(r, g, b) = (r ^\frac{3}{2}, g ^\frac{4}{5}, b ^\frac{3}{2})$

… brought to you by https://twitter.com/iquilezles/status/1440847977560494084

## Mixing two textures

```
screen = 1.0 - (1.0 - tex1.rgb) * (1.0 - tex2.rgb);
// or
screen = tex1.rgb + tex2.rgb - tex1.rgb * tex2.rgb;
```

… brought to you by https://twitter.com/XorDev/status/1781032636699332752
