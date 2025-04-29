```veld
-- Veld code
mod math
pub fn add(a: i32, b: i32) -> i32 = a + b;
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
-- Veld code
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

   -- Module initialization code runs when imported
   println~("Config module loaded");

   pub let settings = load_settings();
   ```

2. **Re-exports**
   ```veld
   mod graphics

   import math.Vector2
   pub(Vector2)  -- Re-export Vector2 from the math module
   ```

3. **Conditional exports**
   ```veld
   mod platform

   @when(target = "web")
   pub fn get_platform() -> str = "web";

   @when(target = "desktop")
   pub fn get_platform() -> str = "desktop";
   ```
