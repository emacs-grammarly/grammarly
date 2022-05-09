[![License: GPL v3](https://img.shields.io/badge/License-GPL%20v3-blue.svg)](https://www.gnu.org/licenses/gpl-3.0)
[![MELPA](https://melpa.org/packages/grammarly-badge.svg)](https://melpa.org/#/grammarly)
[![MELPA Stable](https://stable.melpa.org/packages/grammarly-badge.svg)](https://stable.melpa.org/#/grammarly)

<a href="https://app.grammarly.com/"><img align="right" src="./etc/logo.png" width="100" height="100"></a>

# grammarly
> Grammarly API interface.

[![CI](https://github.com/emacs-grammarly/grammarly/actions/workflows/test.yml/badge.svg)](https://github.com/emacs-grammarly/grammarly/actions/workflows/test.yml)

## 🔨 Examples

Below is an simple example that how you can use this library for calling
Grammarly API interface.

```el
(require 'grammarly)

(defun test-on-message (data)
  "On message callback with DATA."
  (message "[DATA] %s" data))

;; Set callback for receiving data.
(add-to-list 'grammarly-on-message-function-list 'test-on-message)

;; Send check text request.
(grammarly-check-text "Hello World")
```

## 💸 Using a Paid Grammarly Account

You will need the set the following variable in order to use paid version
of Grammarly!

```el
(setq grammarly-username "username@email.com")  ; Your Grammarly Username
(setq grammarly-password "password")  ; Your Grammarly Password
```

If you use `auth-source` to manage your secrets, you can add this to your
`.authinfo.gpg` file:

``` 
machine grammarly.com login <your@email.com> pass <your-password>
```

And instead of directly setting `grammarly-username` and `grammarly-password`, 
you can call the helper function `grammarly-load-from-authinfo`.

``` el
(grammarly-load-from-authinfo)

;; Or, if you have multiple Grammarly accounts:
(grammarly-load-from-authinfo "your@email.com")
```

## 🔗 References

* [grammarly-api](https://github.com/dexterleng/grammarly-api)
* [reverse-engineered-grammarly-api](https://github.com/c0nn3r/reverse-engineered-grammarly-api)
* [grammarly (vscode)](https://github.com/znck/grammarly)

## 📝 Todo List

- [ ] Support multiple requests at the same time.

## Contribute

[![PRs Welcome](https://img.shields.io/badge/PRs-welcome-brightgreen.svg)](http://makeapullrequest.com)
[![Elisp styleguide](https://img.shields.io/badge/elisp-style%20guide-purple)](https://github.com/bbatsov/emacs-lisp-style-guide)

If you would like to contribute to this project, you may either
clone and make pull requests to this repository. Or you can
clone the project and establish your own branch of this tool.
Any methods are welcome!
