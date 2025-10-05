Following the book "Writing an Interpreter in Go" by Thorsten Ball, but in Zig.

# Run the repl
```bash
zig build run
```

and then we can type some code, for example:
```
╰─λ zig build run
>> let a = 5;
5
>> let b = a > 3;
true
>> let c = a * 99;
495
>> if (b) { 10 } else { 1 };
10
>> let d = if (c > a) { 99 } else { 100 } ;
99
>> d
99
>> d * c * a;
245025
```

# Run the Tests
```bash
zig build test
```
