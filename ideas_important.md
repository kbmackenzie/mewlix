hello!
ideas:

1. (liftIO . readIORef) and similar functions should **never** be used outside of Environment.hs.
	a. todo: add functions to environment.hs to remedy that
	b. todo: replace all uses of 'liftIO' anywhere with Environment.hs functions
2. let ExceptT have (MeowException, Text.Text) tuple for exceptions instead of just the exception text. this makes try/catch easier
3. add try/catch block, named differently: 'watch/catch'
	a. watch/catch blocks can have multiple 'catch' blocks with different exceptions!
	b. a user can specify the exception name by string, or just use the keywords 'laser pointer' to catch all!

[important]
1. allow returning IORefs from functions as Prims, wrap them as 'MeowRef'!
2. replace **all trails** with sequential de-referencing
3. treat the '.' operator as a proper operator now instead of acting like it's something that should be unwrapped into a list! it's an operator, it performs an operation, and that's that!

[semi-important]
1. add std.meows and other standard library files as 'data-files' in Cabal!!
2. let the base library always be included, let additional libraries on top of the base library be included more easily!

[extremely important]
1. pipe all meowscript exceptions to stderror instead of stdout .
2. have important state flags and values stored in the Evaluator monad. it'll be called 'MeowState'.
3. pipe to sockets in order to be able to call meowscript from anywhere. have socket number be an argument in meowr and a value in GlobalMeow
4. rename 'lookUpVal'' to lookUpRef and 'peekAsObject' to 'peekRefAsObject'.


[new ideas]
1. filepathT
2. meowr as folder
3. box["item"] accessor
4. watch/catch
5. let function names be expressions!!!!!!!!!!! let them have KEYS!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
6. above is DONE!!!!! HOWEVER!!!!!!!!!!!!!!!!!!! NOW I HAVE TO MAYBE MAKE FUNCTION ORDER MATTER AGAIN MAYBE?????????????????????


[changelog]
1. added box["item"] operator, but it cannot be chained! keep this in mind.
