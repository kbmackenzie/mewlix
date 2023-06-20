
A RegexOutput record type:

	data RegexOutput =
	{ getOutput :: Text
	, getGroups :: [RegexOutput] }

A Regex opaque type (a newtype wrapper):

	newtype Regex = Regex [RegexAST]

A RegexBuilder typeclass with the functions:

	-- Necessary:
	buildEither :: a -> Either Text Regex
	match :: Regex -> a -> RegexOutput

	-- Additional:
	build :: HasCallstack a => a -> Regex
	isMatch :: Regex -> a -> Bool

	<.*> :: Regex -> a -> RegexOutput
	<.*> = match

	<.?> :: Regex -> a -> Bool
	<.?> = isMatch

To build it, only the 'buildEither' and 'match' functions are needed.

Note: The 'build' function is a **partial function** and throws an error if the Regex string is not syntatically correct. If you want a safe alternative, use 'buildEither', which returns a neat error message on a syntax error.
**The reason why 'build' is partial** is because most people using Regex check their strings beforehand, and forcing them to constantly double-check a Regex string they __know__ is correct would be really annoying.
Additionally, 'build' being a partial function makes defining compiled Regex constants a lot easier and prettier!

If for some reason you want/need to double-check syntax when using Regex (such as when receiving strings as input from a user), use 'buildEither'.


THIS IS HOW IT WOULD LOOK:
------

emailRegex :: Regex
emailRegex = build "[a-zA-Z0-9]+@[a-zA-Z0-9]+\\.[a-zA-Z0-9.]"

validateEmail :: String -> Bool
validateEmail = isMatch emailRegex

------
