# Higher Order Functions

The general idea is to treat functions as variables. This allows functions to
be passed into (and returned from other functions). Once this is integrated
into the language, generics are needed to implment generalised pre-defined
functions such as map, fold, reduce, and filter. Alongside, anonymous functions
(i.e. lambdas) could also be implemented.

### Example I (Proposed Usage)
Here, the map is not generalisd, is user defined and only supports functions of
type `func(int, int)` on an `int[]` list. Note that the next paragraph
describes the new `func` base type.

```
macro X 5;

begin
  # Identity function
  int id(int x) is 
    return x;
  end

  # Increment function
  int inc(int x) is
    return x + 1;
  end

  # Decrement function
  int dec(int x) is
    return x + 1;
  end

  # Map function
  int[] map(func(int, int) f, int[] list) is
    int i = 0;
    int[] result = newarray(int, len list);
    while i < len list
    do 
      result[i] = call f(a[i]);
    done;
    return result;
  end
  
  # Examples of map usage
  int[] a = [1,2,3,4,X];
  int[] x = call map(dec, call map(inc, a));
  int[] y = call map(id, [1,X,2,X]);

  println a
  println x # Should be the same as a
  println y
end
```

Here's another example where the `apply` function returns the result of
applying the passed-in function on the passed-in value.

```
begin
  int apply (func(int, int) g, int x) is
    return call g(x);
  end
  int id (int x) is
    return x;
  end
  
  func(int, int) f = id;
  int y = call apply(f, 10);
  println y;
end
```

Note that here, the symbol (and function) tables are as follows:

```
Function Table:
1. id    :: func(int, int)                 |-> label(“id”)
2. apply :: func(func(int, int), int, int) |-> label(“apply”)

Symbol table for program:
1. f     :: func(int, int)
2. id    :: func(int, int)
3. apply :: func(func(int, int), int, int)

Symbol table for apply:
1. g :: func(int, int)
2. x :: int
```

### Specification

One could think of the function type as being similar to a tuple, where the
first part represents a variable length list of arguments and the last
represents the return type of the function. Note that to support this, the base
type is modified as follows:

```
basic: int 
     | char
     | string
     | bool
     | pair
     | void
     | func
     
func : ((basic)*, basic)
```
### Example II (Function Overloading & Anonymous Functions)

The following code block demonstrates a result of combining the three
extensions along with showcasing the different ways `map` could be used.  

```
begin
  # Map function - int to int
  int[] map(func(int, int) f, int[] list) is
    int i = 0;
    int[] result = newarray(int, len list);
    while i < len list
    do 
      result[i] = call f(a[i]);
    done;
    return result;
  end

  # Area of a square
  int area(int s) is
    return area(s, s);
  end

  # Area of a rectangle
  int area(int l, int w) is
    return l * w;
  end

  int[] sides = [2,4,6,8];
  int[] areas = newarray(len sides);
  func(int, int) f = area;
  
  # Different ways of using map:
  areas = call map(area, sides);                 # -> Actual fn. name    + Variable identifier (list)
  areas = call map(area, [2,4,6,8]);             # -> Actual fn. name    + Actual list
  areas = call map(f, sides);                    # -> Aliased fn. name   + Variable identifier (list)
  areas = call map(f, [2,4,6,8]);                # -> Aliased fn. name   + Actual list
  areas = call map((int x -> x * x), sides);     # -> Anonymous function + Variable identifier (list)
  areas = call map((int x -> x * x), [2,4,6,8]); # -> Anonymous function + Actual list

  println areas;
end
```

Note the difference between an actual and aliased function name; both point
to the same function label internally.
