# Structs

In veld, you can define a nominal product type with the `struct` keyword. By declaring a struct, you can define an opaque description of data whose instances will only identify as  concretely as the type that it is ‘constructed’ as in a non-behavioral context (see ~kinds~).

here is an example:
```veld
struct Point
    x: f32
    y: f32
end
```

you can also define a struct more tersely with parenthesis, like so:
``` veld
struct Point(x: f32, y: f32)
```

In this case, the property names are optional. You can omit them if you want, as long as all members are either named or not named. That means our `Point` could also be defined like so:
```veld
struct Point(f32, f32)
```
However, note that in this case, the members can only be accessed by their index in the struct definition, meaning our first `f32` will be 'called’ 0, and the second will be 'called' 1 (see <u>tuples</u>).
