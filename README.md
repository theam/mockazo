# Mockazo 👃
_Mock your records of functions with ease_

[![CircleCI](https://circleci.com/gh/theam/mockazo.svg?style=svg&circle-token=e80d08cb0d9855a774709311335f4e29ca40f5de)](https://circleci.com/gh/theam/mockazo)
[![PRs Welcome](https://img.shields.io/badge/PRs-welcome-brightgreen.svg?style=flat-square)](http://makeapullrequest.com) 
[![Average time to resolve an issue](http://isitmaintained.com/badge/resolution/theam/mockazo.svg)](http://isitmaintained.com/project/theam/mockazo "Average time to resolve an issue")
[![GitHub license](https://img.shields.io/github/license/theam/mockazo.svg)](https://github.com/theam/mockazo/blob/master/LICENSE)
[![GitHub release](https://img.shields.io/github/release/theam/mockazo.svg)](https://GitHub.com/theam/mockazo/releases/)
[![Open Source Love png1](https://badges.frapsoft.com/os/v1/open-source.png?v=103)](https://github.com/ellerbrock/open-source-badges/)

One approach to structure a Haskell is using records of functions, sometimes called
handles, services, or as we like to call them, components.

Mockazo provides a way of mocking components with ease and to verify that they executed
the proper operations with the proper results.

# Adding it to your project

Add the `mockazo` dependency to your `package.yaml` **or** your `cabal` file.

If you use Stack, you also need to add `multistate-0.8.0.2` to your `extra-deps`
section in the `stack.yaml` file.

In your tests, `import Data.Component.Mock`, and you are ready to roll!

# Some restrictions

For Mockazo to work properly, we need that you do a little tweaking on your component
definitions:

## Parametrize the return context

It is common practice to make component methods return values in the `IO` context.
This might looks straightforward, but when mocking comes into place, it is much easier
to work in other contexts.

Imagine that we have a simple logging component:

```haskell
data Component = Component
  { logInfo  :: Text -> IO ()
  , logWarn  :: Text -> IO ()
  , logError :: Text -> IO ()
  }
```

For Mockazo to work properly, we parametrize the context of execution:

```haskell
data Component context = Component
  { logInfo  :: Text -> context ()
  , logWarn  :: Text -> context ()
  , logError :: Text -> context ()
  }
```

This not only makes testing easier, but also makes your code much more robust,
because when defining a function that uses this component, we are unable to execute
any other kind of code that runs in another execution contexts (like a colleague
calling `launchMissiles :: IO ()`).

## All methods must return something in a context

Generally, we use Components to model pieces of our application that perform side effects,
so adding a field that contains some static piece of data doesn't make much sense.

If you really need to do this, we recommend you that you create a companion `Configuration`
type, with all of these values, and leave the component for the side effect operations only.

If you **really** need the value inside of the component, wrap it in `context`.

Mockazo, expects all the methods to be effectful. So it will choke on a return value that
is not wrapped in the context.

So, instead of doing:

```haskell
data Component context = Component
  { foo :: Text
  }
```

Do this:

```haskell
data Component context = Component
  { foo :: context Text
  }
```

# Creating your first mock

Let's suppose that we want to mock the logging component from the first example:

```haskell
data Component context = Component
  { logInfo  :: Text -> context ()
  , logWarn  :: Text -> context ()
  , logError :: Text -> context ()
  }
```

Create a separate module for the mock (we recommend you to do it in `test/Mock`, and the
name of the module should match the name of the component module).

After that, we create the mock (following the advice, we make it in `test/Mock/Logging.hs`):

```haskell
module Mock.Logging where
import Logging   -- We import the component *UNQUALIFIED*
makeMock ''Component
```

That's it! (Yes, really)

If for some reason, you want to add the export list to the mock module (your compiler is
complaining), you can fix it like this:

```haskell
module Mock.Logging (Action(..), Component(..), mock, test) where
import Logging
makeMock ''Component
```

# Testing a function that calls our component

Suppose that somewhere we have a function `importantOperation`
that looks like this:

```haskell
importantOperation :: Monad context => Logging.Component context -> context ()
importantOperation Logging.Component{..} = do
  logInfo "info"
  logWarn "warn"
  logError "error"
```

We want to assure that these operations are run in order and with the
appropriate arguments. We can write a test for it by using Mockazo's little DSL:

```haskell
import qualified Mock.Logging as Logging

let loggingMock = Logging.mock

-- ... somewhere our test framework

runMock
  $ withActions
    [ Logging.LogInfo  "info"  :-> ()
    , Logging.LogWarn  "warn"  :-> ()
    , Logging.LogError "error" :-> ()
    ]
  $ importantOperation loggingMock
```

We tell the test to run a function with a mock using `runMock`.

After that, we specify the actions that we expect to be run, and what they return,
using the `:->` operator, inside of a `withActions` block.

Finally, we run the function that we want to test, by passing the mocked component to it.

# Functions that depend on multiple components

Suppose that `importantOperation` depended on two, three, or whatever more components.

The great stuff about Mockazo, is that you can chain as many `withActions` blocks as
you want, passing the expected operations for each one of the mocks.

Suppose that apart from the `Logging` component, we had another called `UserFetch`.
We could also mock it in the same way we did with `Logging`, and add the expected
operations too:

```haskell
import qualified Mock.Logging as Logging
import qualified Mock.UserFetch as UserFetch

let loggingMock   = Logging.mock
let userFetchMock = UserFetch.mock

-- ... somewhere our test framework

runMock
  $ withActions
    [ Logging.LogInfo  "info"  :-> ()
    , Logging.LogWarn  "warn"  :-> ()
    , Logging.LogError "error" :-> ()
    ]
  $ withActions
    [ UserFetch.Connect "localhost"  :-> ()
    , UserFetch.Fetch :-> User { name = "mike", password = "absolutely-encrypted" }
    ]
  $ importantOperation loggingMock userFetchMock
```

# Acknowledgements

Mockazo is heavily inspired by [`monad-mock`](https://github.com/cjdev/monad-mock/). It wouldn't have been possible to create this package without it's existence.

To all of the authors and contributors of `monad-mock`:

### **Thank you!**
