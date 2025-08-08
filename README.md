# magik-company: Company backend for Magik files

## Content

1. [Requirements](#requirements)
2. [Installation](#installation)
3. [Features](#features)
4. [Limitations](#limitations)

## Requirements

This package requires:

- Magik mode with a version 0.5.1 or higher. [magik-mode](https://melpa.org/#/magik-mode)
- Emacs version 29.1 or higher.

## Installation

This package is available on [MELPA](https://melpa.org/).
See [Emacs Wiki](https://www.emacswiki.org/emacs/InstallingPackages) for instructions on how to set up and install packages.

The alternative, and recommended, way of installing this backend is using [use-package](https://github.com/jwiegley/use-package):

```emacs-lisp
(use-package magik-company
  :hook (magik-ts-mode . magik-company-mode)
        (magik-session-mode . magik-company-mode))
```

## Features

### Caching

Caching is used to minimalize the usage of your local resources.
Any time one transmits to magik (buffer or local) the cache is refreshed.
magik-company-reload-cache can be used to manually refresh the cache.

Expect a slight delay in typing after using f2-b, f2-RET or magik-company-reload-cache.

When typing on a new line the buffer local cache (only for magik-ts-mode) is refreshed.

### Yasnippet completion

When a yasnippet is found it is automatically completed when the autocomplete is done.
When a method is completed the parameters are automatically inserted as yasnippet.
This can be toggled with:

```emacs-lisp
(setq magik-company-insert-params nil/t)
```

Similarly optional parameters (default off) and the gather parameter (default on) can be inserted into the parameter snippet.
Configure with:

```emacs-lisp
(setq magik-company-insert-optional-params nil/t)
(setq magik-company-insert-gather-param nil/t)
```

### Variables

all variables that are assigned with "<<" in your current scope are available for completion
Parameters will also be seen as variables within a scope.
Loop variables are also available within the loop scope as variable.

### Methods

When a type is available all methods on this type are available
The type is available when:

- _self is used within a method and the exemplar is loaded in the session
- an object from the objects candidates is used
- a parameter with a type_doc comment is used
- a variable assigned by one of the basic types: {integer, float, char16_vector, simple_vector}
- a variable assigned with a new* method

### Method annotations

When a method is being shown as a candidate, the following information will be shown:

- (I) if the method is an iterator method
- <param1, param2> the parameters of the function
- <param1, _optional param2> the optional parameters of the function
- <param1, _gather param2> the gather parameters of a function.

Showing parameters, optional or gather (all default on) can be configured with:

```emacs-lisp
(setq magik-company-show-optional-params-annotation nil/t)
(setq magik-company-show-gather-param-annotation nil/t)
(setq magik-company-show-params-annotation nil/t)
```

### Variable/Object annotations

When a variable/object is being shown as a candidate, the following information will be shown:

- (Y) if the candidate is a yasnippet.
- <package_name> the origin package of the global if available.

### Objects

All loaded in exemplars will be available as objects.
All yassnippets will be seen as objects.

### Slots

Slots defined on the exemplar in this scope, slots have to be defined in the file.

## Limitations

Currently this is bound to one session, when there are multiple sessions running there is no control over which session is being used.
Magik is soft typed so quite often it is hard to determine the current exemplar to use for completion.
