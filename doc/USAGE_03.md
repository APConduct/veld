```veld
# Veld code
mod math
pub fn add(a: i32, b: i32) -> i32 => a + b
```

Would compile to Lua as:

```lua
-- Compiled Lua
local math = {}
function math.add(a, b)
    return a + b
end
return math
```

And imports:

```veld
# Veld code
import math.{add, subtract}
```

Would compile as:

```lua
-- Compiled Lua
local math = require("math")
local add, subtract = math.add, math.subtract
```

## Additional Module Features

1. **Module initialization code**
   ```veld
   mod config

   # Module initialization code runs when imported
   println~("Config module loaded");

   pub let settings = load_settings()
   ```

2. **Re-exports**
   ```veld
   mod graphics

   import math.Vector2
   pub(Vector2)  # Re-export Vector2 from the math module
   ```

3. **Conditional exports**
   ```veld
   mod platform

   @when(target = "web")
   pub fn get_platform() -> str "web" end

   @when(target = "desktop")
   pub fn get_platform() -> str "desktop" end
   ```


## More on the Macro System

```veld
# Declarative macro definition (uses tilde)
macro~ vec
    () => new_vec(),
    ($elem:expr) => do
        let mut temp = new_vec()
        temp.push($elem)
        temp
    end
end

# Procedural macro definition (no tilde)
@comptime fn derive(trait_name)
    match trait_name
        "Debug" => generate_debug_impl(),
        "Clone" => generate_clone_impl(),
    end
end

# Usage examples
let numbers = vec~(1, 2, 3)  # Declarative macro with tilde

@derive(Debug, Clone)         # Procedural macro as attribute
struct Point
    x: f64,
    y: f64,
end
```
