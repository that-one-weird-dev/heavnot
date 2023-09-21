# Heavnot
A very simple interpreter for a very simple language, made to learn the very simple language of Ocaml.
Did i already say it was simple?

## Examples of this simple language
```rust
Vec2 is { x: float, y: float }
Vec3 is { x: float, y: float }

Position is enum {
  D0
  D1: { x: float }
  D2: Vec2
  D3: Vec3
}

fn main() {
  vec2 := Position::D2({ x: 10., y: 20. });
  vec3 := Position::D3({ x: 20., y: 30. });

  // this is a comment... in all other languages because this one does not support them at the moment (will never probably...)
  str := match vec2 {
    D0 v => { "" }
    D1 v => { "float" }
    D2 v => { "vec2" }
    v => { "vec3" }
  };

  print(str)
}

```

Can you guess where i took inspiration for the syntax? YES TYPESCRIPT. wait you said rust, ehhhh...
maybe you're right but it wasn't on purpose.

## Running the simple language
```
dune exec heavnot
```
This runs the bin project (it executes the heavnot-src/test1.hvn script at the moment)
