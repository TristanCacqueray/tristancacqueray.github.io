# Mockingbird

A lambda calculus / combinatory explaination.

| Bird        | λ calculus   | Haskell        |
|-------------|--------------|----------------|
| Idiot       | λa.a         | id             |
| Mockingbird | λf.ff        | _              |
| Kestrel     | λab.a        | const          |
| Kite        | λab.b        | const id       |
| Cardinal    | λfab.fba     | flip           |
| Bluebird    | λfga.f(ga)   | (.)            |
| Thrush      | λaf.fa       | flip id        |
| Vireo       | λabf.fab     | flip . flip id |
| Blackbird   | λfgab.f(gab) | (.) . (.)      |

S.K calculus

- S := λabc.ac(bc)
- K := λab.a
- I := SKK


## References

- Video: [Lambda Calculus - Fundamentals of Lambda Calculus & Functional Programming in JavaScript](https://www.youtube.com/watch?v=3VQ382QG-y4)
