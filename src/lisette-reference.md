# Lisette Language Reference

Lisette is a Rust-inspired language that compiles to Go.
Repo: https://github.com/ivov/lisette | Homepage: https://lisette.run

## File Extensions

- `.lis` — source files
- `.d.lis` — declaration files (type stubs for Go interop)

## Comments

```
// line comment
/// doc comment
```

## Imports

```
import "go:fmt"           // Go stdlib
import "go:encoding/json" // nested Go packages
import "commands"          // local module
```

## Variables

```
let x = 42                          // immutable
let mut y = 0                       // mutable
const MAX: int = 100                // constant
let mut tags: Slice<string> = []    // explicit type
```

## Functions

```
fn area(r: float64) -> float64 {
  3.14 * r * r
}

pub fn load() -> Result<Slice<Task>, string> { ... }

// closures
items.filter(|t| t.status == Status.Done)
items.fold(0, |sum, t| sum + t.tags.length())

// generic
fn Max<T: Comparable>(a: T, b: T) -> T

// variadic (Go interop)
fn Log(format: string, args: VarArgs<Unknown>)
```

## Structs

```
#[json]
pub struct Task {
  pub id: int,
  pub title: string,
  pub priority: Priority,
  pub status: Status,
  pub tags: Slice<string>,
}

// newtype
pub struct ID(uint64)

// struct update syntax (spread)
Task { status: Status.Done, ..t }
```

## Impl Blocks

```
impl Task {
  pub fn new(id: int, title: string) -> Task {
    Task { id, title, status: Status.Pending }
  }

  pub fn display(self) -> string { ... }

  // mutable receiver
  fn Increment(self: Ref<Counter>) { ... }
}
```

## Enums

```
pub enum Priority {
  Low,
  Medium,
  High,
}

pub enum Status {
  Pending,
  Done,
  Cancelled(string),        // tuple variant
}

pub enum Shape {
  Circle(float64),
  Rectangle { width: float64, height: float64 },  // named fields
}
```

## Pattern Matching

```
// match expression
let result = match command {
  "add" => commands.add(os.Args),
  "list" => commands.list(),
  other => Err(f"unknown: '{other}'"),
}

// enum destructuring
match shape {
  Shape.Circle(r) => 3.14 * r * r,
  Shape.Rectangle { width, height } => width * height,
}

// if let
if let Err(msg) = result { ... }

// let-else
let Some(command) = os.Args.get(1) else {
  return
}
```

## Error Handling

```
// Result type
fn load_config(path: string) -> Result<Cfg, error> {
  let file = os.Open(path)?       // ? propagates error
  defer file.Close()
  let data = io.ReadAll(file)?
  parse_yaml(data)
}

// matching on Result
match json.Unmarshal(bytes, &items) {
  Ok(_) => Ok(items),
  Err(e) => Err(f"failed: {e}"),
}
```

## Option Type

```
match scores.get("alice") {
  Some(score) => fmt.Println(score),
  None => fmt.Println("not found"),
}
```

## Strings

```
"regular string"
r"raw string, no escapes"
f"interpolated: {expr} and {obj.method()}"
'c'   // char literal
```

## Pipeline Operator

```
let slug = "  Hello World  "
  |> strings.TrimSpace
  |> strings.ToLower
  |> strings.ReplaceAll(" ", "-")
```

## Control Flow

```
if condition { ... } else { ... }

while i < args.length() { ... }

for item in items { ... }

loop { ... }    // infinite loop

break
continue
return
```

## Concurrency

```
// typed channels
let (tx, rx) = Channel.new<string>().split()
let ch = Channel.new<bool>()

// spawn task (goroutine)
task {
  tx.send("hello")
  tx.close()
}

// receive
match rx.receive() {
  Some(msg) => fmt.Println(msg),
  None => fmt.Println("closed"),
}

// iterate channel
for msg in ch { ... }
```

## Interfaces

```
pub interface Stringer {
  fn String() -> string
}

pub interface ReadWriter {
  fn Read(p: Slice<byte>) -> (int, Option<error>)
  fn Write(p: Slice<byte>) -> (int, Option<error>)
}
```

## Generics

```
enum Option<T> { Some(T), None }
enum Result<T, E> { Ok(T), Err(E) }

pub struct ConstrainedPair<K: Comparable, V> {
  pub Key: K,
  pub Value: V,
}

pub interface Transformer<T> {
  fn Transform(data: T) -> T
}
```

## Type Aliases

```
pub type Handler = fn(string) -> Result<string, error>
pub type AliasIntSlice = Slice<int>
```

## Attributes

```
#[json]                   // JSON marshaling
#[go(comma_ok)]           // Go interop hint
#[go(array_return)]
#[allow(unused_value)]    // compiler hint
```

## Built-in Types

Primitives: `int`, `uint`, `float32`, `float64`, `string`, `bool`, `byte`, `rune`, `error`
Sized: `i8`-`i64`, `u8`-`u64`, `f32`, `f64`, `usize`, `isize`
Collections: `Slice<T>`, `Map<K, V>`
References: `Ref<T>`, `Option<Ref<T>>`
Special: `Option<T>`, `Result<T, E>`, `Channel<T>`, `VarArgs<T>`

## Operators

- Arithmetic: `+`, `-`, `*`, `/`, `%`
- Comparison: `==`, `!=`, `<`, `>`, `<=`, `>=`
- Logical: `&&`, `||`, `!`
- Bitwise: `&`, `|`, `^`, `<<`, `>>`
- Pipeline: `|>`
- Arrow: `->` (return type), `=>` (match arm)
- Range: `..`, `..=`
- Error propagation: `?`
- Spread: `..expr` (in struct literals)
- Assignment: `=`, `+=`, `-=`, `*=`, `/=`, `%=`

## Numeric Literals

```
42          // decimal
0xFF        // hex
0o644       // octal
0b1010      // binary
3.14        // float
1_000_000   // underscore separators
2.0i        // complex (imaginary)
```

## Visibility

`pub` keyword makes items public. Without it, items are module-private.

## Defer

```
defer file.Close()    // runs at end of scope, like Go
```

## LSP

The `lis` CLI includes an LSP server: `lis lsp`
