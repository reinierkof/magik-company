# magik-company: Company backend for Magik files

## Content

1. [Requirements](#requirements)
2. [Installation](#installation)
3. [Features](#features)
4. [Limitations](#limitations)

## Requirements

This package requires:
Magik mode with a version 0.4.1 or higher. (https://melpa.org/#/magik-mode)
Emacs version 29.1 or higher.

## Installation

This package is available on [MELPA](https://melpa.org/).
See [Emacs Wiki](https://www.emacswiki.org/emacs/InstallingPackages) for instructions on how to set up and install packages.

The alternative, and recommended, way of installing [magik-mode](https://github.com/roadrunner1776/magik) is using [use-package](https://github.com/jwiegley/use-package):

```emacs-lisp
(use-package magik-company
  :after company
  :config
  (add-to-list 'company-backends 'company-magik))
```

## Features

### Caching
Caching is used to minimalize the usage of your local resources.
Any time one transmits to magik (buffer or local) the cache is refreshed.
magik-company-reload-cache can be used to manually refresh the cache.

### variables
all variables that are assigned with "<<" in your current scope are available for completion
Parameters will also be seen as variables within a scope.

### methods
When a type is available all methods on this type are available
The type is available when:
- _self is used within a method and the exemplar is loaded in the session
- an object from the objects candidates is used
- a parameter with a type_doc comment is used
- a variable assigned by one of the basic types: {integer, float, char16_vector, simple_vector}
- a variable assigned with a new* method

### objects
All loaded in exemplars will be available as objects.

### slots
Slots defined in the current file on the current exemplar in scope will be available as slots.

## Limitations

Currently this is bound to one session, when there are multiple sessions running there is no control over which session is being used.
Magik is soft typed so quite often it is hard to determine the current exemplar to use for completion.
