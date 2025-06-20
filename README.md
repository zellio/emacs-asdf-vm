<!-- README.md --- ASDF-VM Readme

Author: Zachary Elliott <contact@zell.io>
Maintainer: Zachary Elliott <contact@zell.io>
Version: 0.1.0

This program is free software: you can redistribute it and/or modify it under
the terms of the GNU General Public License as published by the Free Software
Foundation, either version 3 of the License, or (at your option) any later
version.

This program is distributed in the hope that it will be useful, but WITHOUT
ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
FOR A PARTICULAR PURPOSE. See the GNU General Public License for more details.

You should have received a copy of the GNU General Public License along with
this program. If not, see <https://www.gnu.org/licenses/>.
-->

# asdf-vm-mode

An [ASDF-VM][asdf-vm] porcelain inside [Emacs][emacs].

## Overview

[asdf-vm][asdf-vm] is a tool version management tool. The stated goal is to
provide a unified interface for installing and managing the various tools
needed for programming and systems management. As a lot of those tools will be
used within [Emacs][emacs] it makes sense to give it a nice interface.

`emacs-asdf-vm` provides first class status for [ASDF-VM][asdf-vm] within
[Emacs][emacs]. Providing both, user friendly command interfaces to the myriad
CLI options, in addition to seamlessly integrating the installed tools.

## Installation

`emacs-asdf-vm` is available on [MELPA][melpa] and can be installed via your
favourite package manager, if you would rather not use [MELPA][melpa], there
are manual options:

### Straight

```elisp
(straight-use-package
  '(asdf-vm :type git :host github :repo "zellio/emacs-asdf-vm"))
```

### Elpaca

```elisp
(elpaca
  (asdf-vm :host github :repo "zellio/emacs-asdf-vm"))
```

### Fully Manual

Either download the code as a [tarbar][tar] or [zip][zip] archive

```bash
mkdir emacs-asdf-vm

curl \
	--proto '=https' --tlsv1.2 --silent --show-error --location --fail \
	--header 'accept: application/octet-stream' \
	--url 'https://github.com/zellio/emacs-asdf-vm/archive/refs/heads/main.tar.gz' \
	--output - | tar xvzf --strip-components=1 --directory=emacs-asdf-vm
```

or via [git][git]

```bash
git clone ssh://git@github.com/zellio/emacs-asdf-vm.git emacs-asdf-vm
```

Then add that directory to your load path and require the library

```elisp
(add-to-list 'load-path "/path/to/directory/emacs-asdf-vm")
(require 'asdf-vm)
```

## Configuration

### Customization Variables

In addition to exposing all of the [ASDF-VM][asdf-vm] tool's configuration
values, `emacs-asdf-vm` has some configuration options of its own:

| Variable                             | Default                                                  | Description                                                                                      |
|:-------------------------------------|:--------------------------------------------------------:|:-------------------------------------------------------------------------------------------------|
| asdf-vm-config-file                  | `(getenv "ASDF_CONFIG_FILE")`                            | Path to the `.asdfrc` configuration file. Must be an absolute path.                              |
| asdf-vm-tool-versions-filename       | `(getenv "ASDF_TOOL_VERSIONS_FILENAME")`                 | The filename of the file storing the tool names and versions.                                    |
| asdf-vm-dir                          | `(getenv "ASDF_DIR")`                                    | The location of asdf core scripts. Must be an absolute path.                                     |
| asdf-vm-data-dir                     | `(getenv "ASDF_DATA_DIR")`                               | The location where asdf will install plugins, shims and tool versions. Must be an absolute path. |
| asdf-vm-concurrency                  | `(getenv "ASDF_CONCURRENCY")`                            | Number of cores to use when compiling the source code.                                           |
| asdf-vm-help-buffer-name             | `"*asdf-vm-help*"`                                       | Display buffer for `asdf-vm-help` response.                                                      |
| asdf-vm-help-fill-column-width       | `fill-column`                                            | Column width for `asdf-vm-help` display buffer formatting.                                       |
| asdf-vm-installer-prefix             | `(asdf-vm-installer-prefix-default)`                     | Installation prefix for `asdf-vm-installer`.                                                     |
| asdf-vm-installer-data-root-dir      | `(expand-file-name "share" asdf-vm-installer-prefix)`    | Installation bindir for `asdf-vm-installer`.                                                     |
| asdf-vm-installer-src-dir            | `(expand-file-name "share" asdf-vm-installer-prefix)`    | Installation bindir for `asdf-vm-installer`.                                                     |
| asdf-vm-installer-exec-prefix        | `asdf-vm-installer-prefix`                               | Installation exec prefix for `asdf-vm-installer`.                                                |
| asdf-vm-installer-bin-dir            | `(expand-file-name "bin" asdf-vm-installer-exec-prefix)` | Installation bindir for `asdf-vm-installer`.                                                     |
| asdf-vm-installer-git-executable     | `(executable-find "git")`                                | Path to git executable used in [ASDF-VM][asdf-vm] installation.                                  |
| asdf-vm-installer-git-arguments      | `nil`                                                    | Optional extra arguments to be passed to git execution on every call.                            |
| asdf-vm-installer-github-url         | `"https://github.com/asdf-vm/asdf"`                      | Source url for [ASDF-VM][asdf-vm] installation.                                                  |
| asdf-vm-installer-git-url            | `(concat asdf-vm-installer-github-url ".git")`           | Source url for [ASDF-VM][asdf-vm] installation.                                                  |
| asdf-vm-installer-system             | `nil`                                                    | Operating system for [ASDF-VM][asdf-vm] installation.                                            |
| asdf-vm-installer-architecture       | `nil`                                                    | Hardware architecture for [ASDF-VM][asdf-vm] installation.                                       |
| asdf-vm-installer-md5sum-executable  | `(executable-find "md5sum")`                             | Path to md5sum executable used in [ASDF-VM][asdf-vm] installation.                               |
| asdf-vm-installer-md5sum-arguments   | `nil`                                                    | Optional extra arguments to be passed to md5sum execution on every call.                         |
| asdf-vm-installer-tar-executable     | `(executable-find "tar")`                                | Path to tar executable used in [ASDF-VM][asdf-vm] installation.                                  |
| asdf-vm-installer-tar-arguments      | `nil`                                                    | Optional extra arguments to be passed to tar execution on every call.                            |
| asdf-vm-mode-line-format             | `"(A)"`                                                  | How `asdf-vm-mode` will indicate activity in the mode line.                                      |
| asdf-vm-mode-keymap-prefix           | `"C-c a"`                                                | Keymode map prefix for `asdf-vm-mode`.                                                           |
| asdf-vm-path-injection-behaviour     | `'prepend`                                               | Control how [ASDF-VM][asdf-vm] updates the variable `exec-path`.                                 |
| asdf-vm-plugin-github-url            | `"https://github.com/asdf-vm/asdf-plugins"`              | Source url for [ASDF-VM][asdf-vm] installation.                                                  |
| asdf-vm-plugin-repository-path       | `asdf-vm--plugin-index-directory`                        | Source url for [ASDF-VM][asdf-vm] installation.                                                  |
| asdf-vm-process-executable           | `(executable-find "asdf")`                               | Path to [ASDF-VM][asdf-vm] command line tool.                                                    |
| asdf-vm-process-executable-arguments | `nil`                                                    | [ASDF-VM][asdf-vm] command line tool execution arguments.                                        |
| asdf-vm-process-buffer-name          | `"*asdf-vm*"`                                            | Host buffer name for `asdf-vm-process` queue.                                                    |
| asdf-vm-process-stderr-buffer-name   | `"*asdf-vm-stderr*"`                                     | Host buffer name for [ASDF-VM][asdf-vm] process stderr.                                          |
|                                      |                                                          |                                                                                                  |

###

Once installed, all [ASDF-VM][asdf-vm] command are available to be called
interactively. The commands are prefixed with `asdf-vm-`.

<!--
```elisp
(asdf-vm-mode +1)
```

or via hook at startup

```elisp
(add-hook 'emacs-startup-hook #'asdf-vm-mode-enable)
```
-->

## Contributing

Bug reports and pull requests are welcome on [GitHub][github-self]. This
project is intended to be a safe, welcoming space for collaboration, and
contributors are expected to adhere to the [Contributor Covenant][covenant]
code of conduct.

## License

Copyright (c) 2025 Zachary Elliott

Distributed under the GNU General Public License

This program is free software: you can redistribute it and/or modify it under
the terms of the GNU General Public License as published by the Free Software
Foundation, either version 3 of the License, or (at your option) any later
version.

This program is distributed in the hope that it will be useful, but WITHOUT
ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
FOR A PARTICULAR PURPOSE. See the GNU General Public License for more details.

You should have received a copy of the GNU General Public License along with
this program. If not, see <[https://www.gnu.org/licenses/][licenses]>.

<!-- Links -->

[asdf-vm]: https://asdf-vm.com/
[covenant]: http://contributor-covenant.org
[emacs]: https://www.gnu.org/software/emacs/
[git]: https://git-scm.com/
[github-self]: https://github.com/zellio/emacs-asdf-vm
[licenses]: https://www.gnu.org/licenses/
[melpa]: https://melpa.org/#/asdf-vm
[tar]: https://www.gnu.org/software/tar/
[zip]: https://www.loc.gov/preservation/digital/formats/fdd/fdd000354.shtml

<!-- README.md ends here  -->
