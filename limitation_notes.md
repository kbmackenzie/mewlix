# Meowscript Limitation Logs

1. Boxes cannot be included in trails dynamically. This means all of these examples are invalid:

```
(~( ^.x.^) BOX!! [ x: 1, y: 0 ]).x 	-- This throws an exception.
get_box().x  				-- This also throws an exception.
```

2. Meowscript does not have a 'char' data type. That means the 'take' and 'drop' style functions return a list of strings when used on strings, instead of returning a string by itself.


