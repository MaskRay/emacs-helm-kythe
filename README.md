# helm-kythe.el

## Introduction

`helm-kythe.el` is a helm interface for Google Kythe.

## Requirements

* `emacs >= 25`
* `dash >= 2.12.0`
* `helm >= 2.0`

## Usage

`helm-kythe-mode`: enable `helm-kythe-mode`. You will see `Kythe` in mode line.

`helm-kythe-apply-decorations`: `helm-kythe.el` uses text properties to mark definitions and references. When `helm-kythe-mode` is enabled, this command will be called automatically to fetch cross references information through HTTP API provided by `/opt/kythe/tools/http_server`. Call this command to update text properties in current buffer.

`helm-kythe-find-definitions`

`helm-kythe-find-references`

`helm-kythe-dwim`: find references if at the definition, otherwise find definitions.

`helm-kythe-imenu`: list toplevel definitions in current buffer.

`helm-kythe-resume`: resurrect previously invoked `helm-kythe` command.

## Suggested key mapping

`helm-kythe.el` provides suggested key mapping if `helm-kythe-suggested-key-mapping` is non-nil. The prefix key is `helm-kythe-prefix-key (default: C-c k)` .

|Key         |Command                          |
|:-----------|:--------------------------------|
|Prefix `a`  | `helm-kythe-apply-decorations`  |
|Prefix `d`  | `helm-kythe-find-definitions`   |
|Prefix `i`  | `helm-kythe-imenu`              |
|Prefix `l`  | `helm-kythe-resume`             |
|Prefix `r`  | `helm-kythe-find-references`    |

## Sample configuration

```elisp
(add-hook 'c++-mode-hook 'helm-kythe-mode)
(add-hook 'c-mode-hook 'helm-kythe-mode)
(add-hook 'haskell-mode-hook 'helm-kythe-mode)

(custom-set-variables
 ;; helm-kythe.el talks to /opt/kythe/tools/http_server . This is where http_server listens to.
 '(helm-kythe-http-server-url "http://127.0.0.1:8080")
 ;; If the jump site (`kythe://?path=a/b/c.hs`) cannot be find relative to current project, try `/tmp/haskell-package-root/a/b/c.hs`.
 '(helm-kythe-file-search-paths '("/tmp/haskell-package-root"))
 )
```

![](images/helm-kythe-haskell.gif)
