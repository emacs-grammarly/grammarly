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

## 🛠️ Contribute

[![PRs Welcome](https://img.shields.io/badge/PRs-welcome-brightgreen.svg)](http://makeapullrequest.com)
[![Elisp styleguide](https://img.shields.io/badge/elisp-style%20guide-purple)](https://github.com/bbatsov/emacs-lisp-style-guide)
[![Donate on paypal](https://img.shields.io/badge/paypal-donate-1?logo=paypal&color=blue)](https://www.paypal.me/jcs090218)
[![Become a patron](https://img.shields.io/badge/patreon-become%20a%20patron-orange.svg?logo=patreon)](https://www.patreon.com/jcs090218)

If you would like to contribute to this project, you may either
clone and make pull requests to this repository. Or you can
clone the project and establish your own branch of this tool.
Any methods are welcome!

### 🔬 Development

To run the test locally, you will need the following tools:

- [Eask](https://emacs-eask.github.io/)
- [Make](https://www.gnu.org/software/make/) (optional)

Install all dependencies and development dependencies:

```sh
$ eask install-deps --dev
```

To test the package's installation:

```sh
$ eask package
$ eask install
```

To test compilation:

```sh
$ eask compile
```

**🪧 The following steps are optional, but we recommend you follow these lint results!**

The built-in `checkdoc` linter:

```sh
$ eask lint checkdoc
```

The standard `package` linter:

```sh
$ eask lint package
```

*📝 P.S. For more information, find the Eask manual at https://emacs-eask.github.io/.*

## ⚜️ License

This program is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program.  If not, see <https://www.gnu.org/licenses/>.

See [`LICENSE`](./LICENSE.txt) for details.
